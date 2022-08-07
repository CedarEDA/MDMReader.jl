using MDMReader

using Test
mdm =  MDMReader.parse_mdmfile(joinpath(@__DIR__, "test.mdm"))
i1 = mdm.header.iccap_inputs[1]
@test i1.mode == MDMReader.MDMParser.Mode.V
@test i1.mode_options_list == [:B, :GROUND, :SMU4, 0.001]
@test i1.sweep_type == MDMReader.MDMParser.SweepType.LIN
@test i1.sweep_type_options_list == [1, 0, 1.8, 37, 0.05]


o1 = mdm.header.iccap_outputs[1]
@test o1.mode == MDMReader.MDMParser.Mode.I
@test o1.mode_options_list == [:B, :GROUND]

data = mdm.data
@test names(data) == ["VS", "VB", "VD", "VG", "IG", "ID", "IB"]

@test first(data.VG) == 0
@test last(data.VG) == 1.8

@test first(data.IG) == 4.421e-010
@test last(data.IG) == -6.61e-010

@test first(data.ID) == -7.641e-010
@test last(data.ID) == 0.00079815

@test first(data.IB) == -1.3915e-008
@test last(data.IB) == -4.3582e-008

@test first(data.VS) == 0.0
@test last(data.VS) == 0.0

@test first(data.VB) == 0.0
@test last(data.VB) == -1.8

@test first(data.VD) == 0.1
@test last(data.VD) == 1.8
