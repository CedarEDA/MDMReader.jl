# MDMReader

MDMReader.jl is a Julia package to read ["MDM" files](https://people.ece.ubc.ca/robertor/Links_files/Files/ICCAP-2008-doc/icug/icug136.html). It is a work in progress and the API should not be considered stable.

# API

Calling `parse_mdmfile(file::String)` parses the content of an MDM file.
The header is found in the `header` field and the data in the `data` field.

The data is stored as a data frame with the data points as rows.

```julia
julia> using MDMReader

julia> mdm = MDMReader.parse_mdmfile("test/test.mdm");

julia> mdm.data
222×7 DataFrame
 Row │ VS       VB       VD       VG       IG          ID           IB
     │ Float64  Float64  Float64  Float64  Float64     Float64      Float64
─────┼─────────────────────────────────────────────────────────────────────────
   1 │     0.0      0.0      0.1     0.0    4.421e-10  -7.641e-10   -1.3915e-8
   2 │     0.0      0.0      0.1     0.05   2.6524e-9   5.054e-10   -7.547e-9
   3 │     0.0      0.0      0.1     0.1   -2.3342e-9   7.845e-10    1.4195e-8
   4 │     0.0      0.0      0.1     0.15   2.5825e-9  -7.61e-10    -1.4968e-8
...
```

# Work to do:

## Correctness:

The minimum to parse the files in https://github.com/google/skywater-pdk-sky130-raw-data/tree/main/sky130_fd_pr/cells has been implemented. There are still many missing pieces for full coverage like including the other modes and sweep types, as well as handling complex numbers.

## Performance

The parser currently parses all data eagerly into a dataframe. However, the data format is engineered so that random access in the file can be done efficiently. For larger files it might be required to move to a more query based API instead of eagerly reading all data.

## Ergonomics

Make better show methods for the data types.
