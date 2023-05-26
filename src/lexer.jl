module MDMLexer

export Lexer, gettoken!, Kind, is_kw, is_error, is_section, Kind

using StringViews
using Parsers
using EnumX

########
# Kind #
########
@enumx T=KindEnum Kind begin
    LPAREN
    RPAREN
    NUMBER
    COLON
    STAR
    IDENTIFIER
    HASH
    EOF
    UNKNOWN

    begin_keywords
        BEGIN_HEADER
        END_HEADER
        USER_INPUTS
        ICCAP_INPUTS
        ICCAP_OUTPUTS
        ICCAP_VALUES

        begin_modes
            V
            I
            C
            G
            T
            S
            H
            Z
            Y
            K
            A
            F
            P
            U
            W
        end_modes

        M # <type> can be M, S, B
        B

        begin_sweeps
            LIN
            LOG
            SYNC
            LIST
            CON
            AC
            HB
            EXP
            PULSE
            PWL
            SFFM
            SIN
            TDR
            SEG
        end_sweeps

        BEGIN_DB
        END_DB
        USER_VAR
        ICCAP_VAR
    end_keywords

    begin_errors
        ERROR_LEX_STRING
        ERROR_LEX_NUMBER
        ERROR_UNEXPECTED_CHARACTER
        ERROR_NONASCII_IDENTIFIER
    end_errors
end

function syntax_description(k::Kind.KindEnum)
    k == Kind.LPAREN     ? "`(`" :
    k == Kind.RPAREN     ? "`)`" :
    k == Kind.NUMBER     ? "number literal" :
    k == Kind.STAR       ? "`*`" :
    k == Kind.EOF        ? "end of file" :
    k == Kind.IDENTIFIER ? "identifier" :
    is_kw(k)        ? string(k) :
    error("internal error: unknown kind: $k")
end


is_kw(kind::Kind.KindEnum)    = Kind.begin_keywords < kind < Kind.end_keywords
is_error(kind::Kind.KindEnum) = Kind.begin_errors < kind < Kind.end_errors
is_sweep(kind::Kind.KindEnum) = Kind.begin_sweeps < kind < Kind.end_sweeps
is_mode(kind::Kind.KindEnum)  = Kind.begin_modes < kind < Kind.end_modes
is_type(kind::Kind.KindEnum)  = kind == Kind.M || kind == Kind.S || kind == Kind.B

const kws = Dict{String, Kind.KindEnum}()
let
    for kind in instances(Kind.KindEnum)
        str = string(kind)
        if is_kw(kind) && !startswith("begin_", str) && !startswith("end_", str)
            kws[str] = kind
        end
    end
end


#########
# Token #
#########
struct Token
    kind::Kind.KindEnum
    poslen::Parsers.PosLen
    val::Float64
end
Token(kind, poslen) = Token(kind, poslen, NaN64)

#########
# Lexer #
#########
mutable struct Lexer{B <: Union{AbstractVector{UInt8}, IO}}
    #=const=#  buf::B
    #=const=#  len::Int
    #=const=#  opts::Parsers.Options
    pos::Int
    startmark::Int
end

create_lexer(buf, len, ) = Lexer(buf, len, Parsers.Options(; quoted=true, escapechar='\\', delim=' ', wh1 = 0x7f, wh2 = 0x7f), 1, 0)
function Base.seek(l::Lexer, pos)
    Parsers.fastseek!(l.buf, pos-1)
    l.pos = pos
    l.startmark = pos
end

Lexer(str::String)              = create_lexer(codeunits(str), sizeof(str))
Lexer(io::IO)                   = create_lexer(io, 0)
Lexer(v::AbstractVector{UInt8}) = create_lexer(v, sizeof(v))

eof(l::Lexer)                           = eof(l.buf, l.pos)
eof(b::AbstractVector{UInt8}, pos::Int) = pos > length(b)
eof(source::IO, _::Int)                 = Base.eof(source)

getstring(tok::Token, l::Lexer)           = getstring(tok.poslen, l)
getstring(plen::Parsers.PosLen, l::Lexer) = Parsers.getstring(l.buf, plen, l.opts.e)

getstringview(tok::Token, l::Lexer)                 = getstringview(tok.poslen, l)
getstringview(plen::Parsers.PosLen, l::Lexer)       = StringView(view(l.buf, plen.pos:plen.pos+plen.len-1))
getstringview(plen::Parsers.PosLen, l::Lexer{<:IO}) = getstring(plen, l)


# Error #
function generate_error_desc(t::Token, l::Lexer)
    @assert is_error(t.kind)
    if t.kind == Kind.ERROR_LEX_STRING
        return "encountered an error when reading a string literal"
    elseif t.kind == Kind.ERROR_LEX_NUMBER
        return "encountered an error when reading a number literal"
    elseif t.kind == Kind.ERROR_UNEXPECTED_CHARACTER
        return "unexpected character"
    elseif t.kind == Kind.ERROR_NONASCII_IDENTIFIER
        str = getstring(t, l)
        return "non-ascii letter encountered in identifier or keyword"
    end
end

# Peek #
unsafe_peekbyte(from::IO, _::Int)                      = UInt8(Base.peek(from))
unsafe_peekbyte(from::IOBuffer, _::Int)                = @inbounds from.data[from.ptr]
unsafe_peekbyte(from::AbstractVector{UInt8}, pos::Int) = @inbounds from[pos]
function peekbyte(l::Lexer)
    @assert !eof(l)
    return unsafe_peekbyte(l.buf, l.pos)
end

# incr! #
incr!(from::IO)         = Base.read(from, UInt8)
incr!(from::IOBuffer)   = from.ptr += 1
incr!(_::AbstractArray) = nothing

function incr!(l::Lexer)
    incr!(l.buf)
    l.pos += 1
end

function skipwhitespace(l::Lexer)
    while !eof(l)
        c = peekbyte(l)
        if !(c == UInt8(' ') || c == UInt8('\n') || c == UInt8('\r')) # TODO: EOF check
            return c
        end
        incr!(l)
    end
    return
end

function skipcomment(l::Lexer)
    while !eof(l)
        c = peekbyte(l)
        if c == UInt8('\n') # TODO: EOF check
            incr!(l)
            return peekbyte(l)
        end
        incr!(l)
    end
    return
end

function gettoken!(l::Lexer)
    c = skipwhitespace(l)
    if c !== nothing && Char(c) == '!'
        c = skipcomment(l)
    end

    l.startmark = l.pos
    c === nothing && return Token(Kind.EOF, Parsers.PosLen(l.startmark, 0))
    if isdigit(Char(c)) || c == UInt8('-')
        r = Parsers.xparse(Float64, l.buf, l.pos, l.len, l.opts)
        typ = Parsers.invalid(r.code) ? Kind.ERROR_LEX_NUMBER : Kind.NUMBER
        l.pos += r.tlen
        return Token(typ, Parsers.PosLen(l.startmark, l.pos - l.startmark), r.val)
    elseif c == UInt8('(')
        incr!(l)
        return Token(Kind.LPAREN, Parsers.PosLen(l.startmark, 1))
    elseif c == UInt8(')')
        incr!(l)
        return Token(Kind.RPAREN, Parsers.PosLen(l.startmark, 1))
    elseif c == UInt8('#')
        incr!(l)
        return Token(Kind.HASH, Parsers.PosLen(l.startmark, 1))
    elseif c == UInt8('*')
        incr!(l)
        return Token(Kind.STAR, Parsers.PosLen(l.startmark, 1))
    elseif c == UInt8(':')
        incr!(l)
        return Token(Kind.COLON, Parsers.PosLen(l.startmark, 1))
    elseif isletter(Char(c))
        return lex_identifier(l)
    else
        incr!(l)
        error("unexpected character $(Char(c))")
        return Token(Kind.ERROR_UNEXPECTED_CHARACTER, Parsers.PosLen(l.startmark, 1))
    end
end

function lex_identifier(l::Lexer)
    while (!eof(l) && (c = Char(peekbyte(l)); isletter(c) || isdigit(c) || c == '_'))
        if !isascii(c)
            return Token(Kind.ERROR_NONASCII_IDENTIFIER, Parsers.PosLen(l.startmark, l.pos - l.startmark - 1))
        end
        incr!(l)
    end
    plen = Parsers.PosLen(l.startmark, l.pos - l.startmark)
    str = getstringview(plen, l)

    kind = get(kws, str, nothing)
    kind === nothing && return Token(Kind.IDENTIFIER, plen)
    return Token(kind, plen)
end

end
