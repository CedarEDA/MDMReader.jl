module MDMParser
using ..MDMLexer
using ..MDMLexer:  MDMLexer, Lexer, Token, Kind, gettoken!

using Mmap
using EnumX
using DataFrames

#########
# Types #
#########
mutable struct Parser{L <: Lexer}
    #=const=# l::L
    nt::Token
    filename::Union{Nothing, String}
end
function Parser(source; filename::Union{String, Nothing}=nothing)
    l = Lexer(source)
    nt = gettoken!(l)
    return Parser(l, nt, filename)
end

@enumx T=ModeEnum Mode begin
    V
    U
    I
    P
    W
    F
    T
    N
    C
    G
    S
    H
    Z
    K
    A
    Y
end

@enumx T=SweepTypeEnum SweepType begin
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
end

@enumx T=TypeEnum Type begin
    M
    S
    B
end

struct UserInput
    name::Symbol
    sweep_type::SweepType.SweepTypeEnum
    sweep_type_options_list::Vector
end

struct IccapInput
    name::Symbol
    mode::Mode.ModeEnum
    mode_options_list::Vector
    sweep_type::SweepType.SweepTypeEnum
    sweep_type_options_list::Vector
end

struct IccapOutput
    name::Symbol
    mode::Mode.ModeEnum
    mode_options_list::Vector
    unit
    compliance
    type::Type.TypeEnum
end

struct IccapValues
    value_name::Symbol
    value::Float64 # ?
end


struct Header
    user_inputs::Union{Nothing, Vector{UserInput}}
    iccap_inputs::Union{Nothing, Vector{IccapInput}}
    iccap_outputs::Union{Nothing, Vector{IccapOutput}}
    iccap_values::Union{Nothing, Vector{IccapValues}}
end

mutable struct MDMFile
    #=const=#  header::Header
    #=const=#  data::DataFrame
end


########
# Take #
########
@inline take(p::Parser, kind::Kind.KindEnum) = take(p, (kind,))

@inline function take(p::Parser, kinds::NTuple{<:Any, Kind.KindEnum})
    if !(p.nt.kind in kinds)
        MDMerror(p, kinds)
    end
    return take(p)
end
@inline function take(p::Parser)
    nt = p.nt
    if MDMLexer.is_error(nt.kind)
        throw(MDMParserError(p, nothing, "got an error token"))
    end
    p.nt = gettoken!(p.l)
    nt
end

function take_identifier(p::Parser)
    if p.nt.kind !== Kind.IDENTIFIER && !is_kw(p.nt.kind)
        throw(MDMParserError(p, nothing, "wrong identifier"))
    else
        return take(p)
    end
end

function take_get_symbol(p::Parser)
    tok = take_identifier(p)
    return Symbol(MDMLexer.getstringview(tok, p.l))
end

function take_integer(p::Parser)
    tok = take(p, Kind.NUMBER)
    v = tok.val
    if isinteger(v)
        return Int(v)
    else
        throw(MDMParserError(p, "expected number to be an integer"))
    end
end

@inline function accept(p::Parser, k::Kind.KindEnum)
    if p.nt.kind == k
        take(p)
        return true
    else
        return false
    end
end


@noinline MDMerror(p::Parser, kind::Kind.KindEnum) = MDMerror(p, (kind,))
@noinline MDMerror(p::Parser, kinds::NTuple{<:Any, Kind.KindEnum}) = throw(MDMParserError(p, kinds, nothing))


###########
# Parsing #
###########

function parse_mdmfile(file::String)
    if !isfile(file)
        error("No such file: $(repr(file))")
    end
    io = Mmap.mmap(file)
    p = Parser(io; filename=file)

    header = parse_header(p)
    dbs = parse_dbs(p, header)
    return MDMFile(header, dbs)
    error("unreachable")
end


##########
# Header #
##########

function parse_header(p::Parser)
    take(p, Kind.BEGIN_HEADER)
    user_inputs = nothing
    iccap_inputs = nothing
    iccap_outputs = nothing
    iccap_values = nothing
    while p.nt.kind !== Kind.END_HEADER
        if accept(p, Kind.USER_INPUTS)
            user_inputs = parse_user_inputs(p)
        elseif accept(p, Kind.ICCAP_INPUTS)
            iccap_inputs = parse_iccap_inputs(p)
        elseif accept(p, Kind.ICCAP_OUTPUTS)
            iccap_outputs = parse_iccap_outputs(p)
        elseif accept(p, Kind.ICCAP_VALUES)
            iccap_values = parse_iccap_values(p)
        else
            MDMerror(p, (Kind.USER_INPUTS, Kind.ICCAP_INPUTS, Kind.ICCAP_OUTPUTS, Kind.ICCAP_VALUES))
        end
    end
    take(p, Kind.END_HEADER)
    return Header(user_inputs, iccap_inputs, iccap_outputs, iccap_values)
end


function parse_sweep(p::Parser)
    sweep = accept(p, Kind.LIN  ) ? SweepType.LIN :
            accept(p, Kind.LOG  ) ? SweepType.LOG :
            accept(p, Kind.SYNC ) ? SweepType.SYNC :
            accept(p, Kind.LIST ) ? SweepType.LIST :
            accept(p, Kind.CON  ) ? SweepType.CON :
            accept(p, Kind.AC   ) ? SweepType.AC :
            accept(p, Kind.HB   ) ? SweepType.HB :
            accept(p, Kind.EXP  ) ? SweepType.EXP :
            accept(p, Kind.PULSE) ? SweepType.PULSE :
            accept(p, Kind.PWL  ) ? SweepType.PWL :
            accept(p, Kind.SFFM ) ? SweepType.SFFM :
            accept(p, Kind.SIN  ) ? SweepType.SIN :
            accept(p, Kind.TDR  ) ? SweepType.TDR :
            accept(p, Kind.SEG  ) ? SweepType.SEG :
            throw(MDMParserError(p, "expected a sweep token"))

    sweep_options_list = Vector() # TODO: Element type

    if sweep == SweepType.LIN
        sweep_order = take_integer(p)
        start = take(p, Kind.NUMBER).val
        stop = take(p, Kind.NUMBER).val
        number_of_points = take_integer(p)
        step_size = take(p, Kind.NUMBER).val
        push!(sweep_options_list, sweep_order, start, stop, number_of_points, step_size)
    elseif sweep == SweepType.LOG
        error("unimplemented sweep type: $sweep")
    elseif sweep == SweepType.SYNC
        ratio = take(p, Kind.NUMBER).val
        offset = take_integer(p)
        master_sweep = take_get_symbol(p)
        push!(sweep_options_list, ratio, offset, master_sweep)
    elseif sweep == SweepType.LIST
        error("unimplemented sweep type: $sweep")
    elseif sweep == SweepType.CON
        val = take(p, Kind.NUMBER).val
        push!(sweep_options_list, val)
    elseif sweep == SweepType.AC
        error("unimplemented sweep type: $sweep")
    elseif sweep == SweepType.HB
        error("unimplemented sweep type: $sweep")
    elseif sweep == SweepType.EXP
        error("unimplemented sweep type: $sweep")
    elseif sweep == SweepType.PULSE
        error("unimplemented sweep type: $sweep")
    elseif sweep == SweepType.PWL
        error("unimplemented sweep type: $sweep")
    elseif sweep == SweepType.SFFM
        error("unimplemented sweep type: $sweep")
    elseif sweep == SweepType.SIN
        error("unimplemented sweep type: $sweep")
    elseif sweep == SweepType.TDR
        error("unimplemented sweep type: $sweep")
    elseif sweep == SweepType.SEG
        error("unimplemented sweep type: $sweep")
    else
        throw(MDMParserError(p, "unexpected sweep type `$(sweep)` in input"))
    end
    return sweep, sweep_options_list
end

function parse_mode(p::Parser; input::Bool)
    mode = accept(p, Kind.V) ? Mode.V :
           accept(p, Kind.U) ? Mode.U :
           accept(p, Kind.I) ? Mode.I :
           accept(p, Kind.P) ? Mode.P :
           accept(p, Kind.W) ? Mode.W :
           accept(p, Kind.F) ? Mode.F :
           accept(p, Kind.T) ? Mode.T :
           accept(p, Kind.N) ? Mode.N :
           accept(p, Kind.C) ? Mode.C :
           accept(p, Kind.G) ? Mode.G :
           accept(p, Kind.S) ? Mode.S :
           accept(p, Kind.H) ? Mode.H :
           accept(p, Kind.Z) ? Mode.Z :
           accept(p, Kind.K) ? Mode.K :
           accept(p, Kind.A) ? Mode.A :
           accept(p, Kind.Y) ? Mode.Y :
           throw(MDMParserError(p, "expected a mode token"))

    mode_options_list = Vector() # TODO: Element type
    if input
        if mode == Mode.V || mode == Mode.U
            node_plus = take_get_symbol(p)
            node_minus = take_get_symbol(p)
            unit = take_get_symbol(p)
            compliance = take(p, Kind.NUMBER).val
            push!(mode_options_list, node_plus, node_minus, unit, compliance)
        elseif mode == Mode.I
            error("unimplemented mode: input $mode")
        elseif mode == Mode.P
            error("unimplemented mode: input $mode")
        elseif mode == Mode.W
            error("unimplemented mode: input $mode")
        elseif mode == Mode.F || mode == Mode.T
            error("unimplemented mode: input $mode")
        else
            throw(MDMParserError(p, "unexpected mode type `$(mode)` in input"))
        end
    else
        if mode == Mode.V ||mode == Mode.N || mode == Mode.U
            error("unimplemented mode: output $mode")
        elseif mode == Mode.I
            to_node = take_get_symbol(p)
            from_node = take_get_symbol(p)
            push!(mode_options_list, to_node, from_node)
        elseif mode == Mode.C || mode == Mode.G
            error("unimplemented mode: output $mode")
        elseif mode == Mode.T
            error("unimplemented mode: output $mode")
        elseif mode in (Mode.S, Mode.H, Mode.Z, Mode.K, Mode.A, Mode.Y)
            error("unimplemented mode: output $mode")
        else
            throw(MDMParserError(p, "unexpected mode type `$(mode)` in output"))
        end
    end
    return mode, mode_options_list
end


function parse_iccap_inputs(p)
    inputs = IccapInput[]
    while !is_kw(p.nt.kind)
        name = take_get_symbol(p)
        mode, mode_options_list = parse_mode(p; input=true)
        sweep_type, sweep_type_options_list = parse_sweep(p)
        input = IccapInput(name, mode, mode_options_list, sweep_type, sweep_type_options_list)
        push!(inputs, input)
    end
    return inputs
end


function parse_iccap_outputs(p)
    outputs = IccapOutput[]
    while !is_kw(p.nt.kind)
        name = take_get_symbol(p)
        mode, mode_options_list = parse_mode(p; input=false)
        unit = take_get_symbol(p) # TODO: should be optional
        if p.nt.kind == Kind.M || p.nt.kind == Kind.S || p.nt.kind == Kind.B
            compliance = nothing
        else
            compliance = take(p, Kind.NUMBER).val
        end
        type = accept(p, Kind.M) ? Type.M :
               accept(p, Kind.S) ? Type.S :
               accept(p, Kind.B) ? Type.B :
               throw(MDMParserError(p, "unexpected type `$(type)` in iccap output"))

        output = IccapOutput(name, mode, mode_options_list, unit, compliance, type)
        push!(outputs, output)
    end
    return outputs
end


#############
# Databases #
#############

function parse_dbs(p, header)
    data, ids = parse_db_first(p)
    while !MDMLexer.eof(p.l)
        parse_db(p, data, ids)
    end
    data = reshape(data, length(ids), :)
    # Bad cache behavior here
    return DataFrame(; (ids[i] => data[i, :] for i in eachindex(ids))...)
end

function parse_db_first(p)
    ids = Symbol[]
    vals = Float64[]
    take(p, Kind.BEGIN_DB)
    while p.nt.kind != Kind.HASH
        take(p, Kind.ICCAP_VAR)
        id = take_get_symbol(p)
        push!(ids, id)
        val = take(p, Kind.NUMBER).val
        push!(vals, val)
    end

    n_cols = 0
    take(p, Kind.HASH)
    while p.nt.kind !== Kind.NUMBER
        n_cols += 1
        push!(ids, take_get_symbol(p))
    end

    data = Float64[]

    read_data!(data, vals, n_cols, p)

    take(p, Kind.END_DB)
    return data, ids
end

function parse_db(p, data, ids)
    i = 1
    take(p, Kind.BEGIN_DB)
    vals = Float64[]
    while p.nt.kind != Kind.HASH
        take(p, Kind.ICCAP_VAR)
        id = take_get_symbol(p)
        if ids[i] !== id
            throw(MDMParserError(p, "unexpected id name"))
        end
        i += 1
        val = take(p, Kind.NUMBER).val
        push!(vals, val)
    end

    n_cols = 0
    take(p, Kind.HASH)
    while p.nt.kind !== Kind.NUMBER
        n_cols += 1
        id = take_get_symbol(p)
        if ids[i] !== id
            throw(MDMParserError(p, "unexpected id name"))
        end
        i += 1
    end


    read_data!(data, vals, n_cols, p)

    take(p, Kind.END_DB)
    return data, ids
end

function read_data!(data::Vector{Float64}, vals::Vector{Float64}, n_cols::Int, p::Parser)
    while p.nt.kind != Kind.END_DB
        # Read a line
        append!(data, vals)
        for _ in 1:n_cols
            push!(data, take(p, Kind.NUMBER).val)
        end
        # Add the stuff from
    end
end

##########
# Errors #
##########

struct MDMParserError
    p::Parser
    expected::Union{NTuple{<:Any, Kind.KindEnum}, Nothing}
    msg::Union{Nothing, String}
end
MDMParserError(p::Parser, msg::String) = MDMParserError(p, nothing, msg)

function point_to_line(str::AbstractString, a::Int, b::Int, context)
    @assert b >= a
    a = thisind(str, a)
    b = thisind(str, b)
    pos = 1
    io1 = IOContext(IOBuffer(), context)
    io2 = IOContext(IOBuffer(), context)
    while true
        if a <= pos <= b
            printstyled(io2, "^"; color=Base.error_color(), bold=true)
        else
            print(io2, " ")
        end
        it = iterate(str, pos)
        it === nothing && break
        c, pos = it
        c == '\n' && break
        print(io1, c)
    end
    return String(take!(io1.io)), String(take!(io2.io))
end

function Base.showerror(io::IO, e::MDMParserError)
    p = e.p
    l = p.l
    t = p.nt

    error_pos = l.pos - 1

    # Compute lineno
    seek(l, 1)
    lineno = 1
    while l.pos != error_pos
        if MDMLexer.peekbyte(l) == UInt8('\n')
            lineno += 1
        end
        MDMLexer.incr!(l)
    end

    f = something(p.filename, "none")
    printstyled(io, "MDM Parser error: "; color=Base.error_color())
    if e.msg !== nothing
        printstyled(io, e.msg, "\n"; bold=true)
    elseif MDMLexer.is_error(t.kind)
        desc = MDMLexer.generate_error_desc(t, l)
        printstyled(io, desc, "\n"; bold=true)
    else
        got_str = MDMLexer.syntax_description(t.kind)
        ex_str = join(MDMLexer.syntax_description.(e.expected), ", ", " or ")
        printstyled(io, "unexpected token: $got_str, expected $ex_str \n"; bold=true)
    end
    printstyled(io, " --> "; color=:cyan)
    printstyled(io, f, ':', lineno, '\n')

    while l.pos != 1
        seek(l, l.pos-1)
        if MDMLexer.peekbyte(l) == UInt8('\n')
            break
        end
    end
    start_pos = l.pos == 1 ? 1 : l.pos + 1

    lineio = IOBuffer()
    if l.pos != 1
        MDMLexer.incr!(l)
    end
    while !MDMLexer.eof(l)
        c = MDMLexer.peekbyte(l)
        if c == UInt8('\n')
            break
        end
        MDMLexer.incr!(l)
        write(lineio, c)
    end
    line = String(take!(lineio))
    # TODO: Figure out max(1, ...)
    a, b = point_to_line(line, max(1, t.poslen.pos - start_pos - 1),  t.poslen.pos - start_pos + t.poslen.len, io)

    tw = textwidth(string(lineno))
    ind = " "^tw
    printstyled(io, " $ind|\n"; color=:cyan)
    printstyled(io, lineno, " |"; color=:cyan)
    print(io, " "); println(io, a)
    printstyled(io, " $ind| "; color=:cyan)
    println(io, b)
    return
end

end # module
