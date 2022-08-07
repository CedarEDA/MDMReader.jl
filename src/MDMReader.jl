module MDMReader

include("lexer.jl")
include("parser.jl")

# Doing it like this makes tab complete work
const parse_mdmfile        = MDMParser.parse_mdmfile

end
