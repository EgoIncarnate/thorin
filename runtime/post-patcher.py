#!/usr/bin/env python3
import sys, re, os
rttype, basename = sys.argv[1:]

if rttype in ("nvvm", "spir"):
    # we need to patch
    result = []
    filename = basename+"."+rttype
    if os.path.isfile(filename):
        with open(filename) as f:
            for line in f:
                # patch to opaque identity functions
                if rttype=="spir":
                    m = re.match('^declare cc75 (.*) @(magic_.*_id)\((.*)\)\n$', line)
                else:
                    m = re.match('^declare (.*) @(magic_.*_id)\((.*)\)\n$', line)
                if m is not None:
                    ty1, fname, ty2 = m.groups()
                    assert ty1 == ty2, "Argument and return types of magic IDs must match"
                    print("Patching magic ID {0}".format(fname))
                    # emit definition instead
                    if rttype=="spir":
                        result.append('define cc75 {0} @{1}({0} %name) {{\n'.format(ty1, fname))
                    else:
                        result.append('define {0} @{1}({0} %name) {{\n'.format(ty1, fname))
                    result.append('  ret {0} %name\n'.format(ty1))
                    result.append('}\n')
                # get rid of attributes
                elif 'attributes #' in line:
                    print("Removing attribute declarations")
                    # ignore this line
                    pass
                # it's a normal line, keep it but apply substitutions
                else:
                    line = re.sub('#[0-9]+', '', line)
                    result.append(line)
        # we have the patched thing, write it
        with open(filename, "w") as f:
            for line in result:
                f.write(line)

if rttype in ("cuda", "opencl"):
    # we need to patch
    result = []
    if rttype == "cuda":
        filename = basename+"."+"cu"
    else:
        filename = basename+"."+"cl"
    if os.path.isfile(filename):
        with open(filename) as f:
            for line in f:
                # patch to opaque identity functions
                m = re.match('^(.*) = (magic_.*_id)\((.*)\);\n$', line)
                if m is not None:
                    lhs, fname, arg = m.groups()
                    print("Patching magic ID {0}".format(fname))
                    # emit definition instead
                    result.append('{0} = {1};\n'.format(lhs, arg))
                else:
                    result.append(line)
        # we have the patched thing, write it
        with open(filename, "w") as f:
            for line in result:
                f.write(line)

# another pass to add the ldg, minmax and consorts to the nvvm file
nvvm_defs = {
  "ldg_f32" : """define float @ldg_f32(float addrspace(1)* %addr) {
    %1 = call float asm "ld.global.nc.f32 $0, [$1];", "=f, l" (float addrspace(1)* %addr)
    ret float %1
}
""",
  "ldg_i32" : """define i32 @ldg_i32(i32 addrspace(1)* %addr) {
    %1 = call i32 asm "ld.global.nc.i32 $0, [$1];", "=f, l" (i32 addrspace(1)* %addr)
    ret i32 %1
}
""",
  "ldg4_f32" : """define <4 x float> @ldg4_f32(<4 x float>* %addr) {
    %1 = call {float, float, float, float} asm "ld.global.nc.v4.f32 {$0, $1, $2, $3}, [$4];", "=f,=f,=f,=f, l" (<4 x float>* %addr)
    %2 = extractvalue {float, float, float, float} %1, 0
    %3 = extractvalue {float, float, float, float} %1, 1
    %4 = extractvalue {float, float, float, float} %1, 2
    %5 = extractvalue {float, float, float, float} %1, 3
    %6 = insertelement <4 x float> undef, float %2, i32 0
    %7 = insertelement <4 x float> %6, float %3, i32 1
    %8 = insertelement <4 x float> %7, float %4, i32 2
    %9 = insertelement <4 x float> %8, float %5, i32 3
    ret <4 x float> %9
}
""",
  "ldg4_u32" : """define <4 x u32> @ldg4_u32(<4 x u32>* %addr) {
    %1 = call {u32, u32, u32, u32} asm "ld.global.nc.v4.f32 {$0, $1, $2, $3}, [$4];", "=r,=r,=r,=r, l" (<4 x u32>* %addr)
    %2 = extractvalue {u32, u32, u32, u32} %1, 0
    %3 = extractvalue {u32, u32, u32, u32} %1, 1
    %4 = extractvalue {u32, u32, u32, u32} %1, 2
    %5 = extractvalue {u32, u32, u32, u32} %1, 3
    %6 = insertelement <4 x u32> undef, u32 %2, i32 0
    %7 = insertelement <4 x u32> %6, u32 %3, i32 1
    %8 = insertelement <4 x u32> %7, u32 %4, i32 2
    %9 = insertelement <4 x u32> %8, u32 %5, i32 3
    ret <4 x u32> %9
}
""",
  "ldg4_i32" : """define <4 x i32> @ldg4_i32(<4 x i32>* %addr) {
    %1 = call {i32, i32, i32, i32} asm "ld.global.nc.v4.s32 {$0, $1, $2, $3}, [$4];", "=r,=r,=r,=r, l" (<4 x i32>* %addr)
    %2 = extractvalue {i32, i32, i32, i32} %1, 0
    %3 = extractvalue {i32, i32, i32, i32} %1, 1
    %4 = extractvalue {i32, i32, i32, i32} %1, 2
    %5 = extractvalue {i32, i32, i32, i32} %1, 3
    %6 = insertelement <4 x i32> undef, i32 %2, i32 0
    %7 = insertelement <4 x i32> %6, i32 %3, i32 1
    %8 = insertelement <4 x i32> %7, i32 %4, i32 2
    %9 = insertelement <4 x i32> %8, i32 %5, i32 3
    ret <4 x i32> %9
}
""",
  "maxmax" : """define i32 @maxmax(i32 %a, i32 %b, i32 %c) {
    %1 = call i32 asm "vmax.s32.s32.s32.max $0, $1, $2, $3;", "=r, r, r, r" (i32 %a, i32 %b, i32 %c)
    ret i32 %1
}
""",
  "minmin" : """define i32 @minmin(i32 %a, i32 %b, i32 %c) {
    %1 = call i32 asm "vmin.s32.s32.s32.min $0, $1, $2, $3;", "=r, r, r, r" (i32 %a, i32 %b, i32 %c)
    ret i32 %1
}
""",
  "minmax" : """define i32 @minmax(i32 %a, i32 %b, i32 %c) {
    %1 = call i32 asm "vmin.s32.s32.s32.max $0, $1, $2, $3;", "=r, r, r, r" (i32 %a, i32 %b, i32 %c)
    ret i32 %1
}
""",
  "maxmin" : """define i32 @maxmin(i32 %a, i32 %b, i32 %c) {
    %1 = call i32 asm "vmax.s32.s32.s32.min $0, $1, $2, $3;", "=r, r, r, r" (i32 %a, i32 %b, i32 %c)
    ret i32 %1
}
""",
  "ballot" : """define i32 @ballot(i32 %a) {
    %1 = call i32 asm
        "{ .reg .pred %p1;
            setp.ne.u32 %p1, $1, 0;
            vote.ballot.b32 $0, %p1;
        }", "=r, r" (i32 %a)
    ret i32 %1
}
""",
  "__all" : """define i32 @__all(i32 %a) {
    %1 = call i32 asm
        "{ .reg .pred %p1;
            setp.ne.s32 %p1, $1, 0;
            vote.all.pred $0, %p1;
        }", "=b, r" (i32 %a)
    ret i32 %1
}
""",
  "shfldown" : """define float @shfldown(float %a, i32 %b, i32 %c) {
    %1 = call float asm "shfl.down.b32 $0, $1, $2, $3;", "=f, f, r, r" (float %a, i32 %b, i32 %c)
    ret float %1
}
""",
  "shflup" : """define float @shflup(float %a, i32 %b, i32 %c) {
    %1 = call float asm "shfl.up.b32 $0, $1, $2, $3;", "=f, f, r, r" (float %a, i32 %b, i32 %c)
    ret float %1
}
""",
  "shflidx" : """define float @shflidx(float %a, i32 %b, i32 %c) {
    %1 = call float asm "shfl.idx.b32 $0, $1, $2, $3;", "=f, f, r, r" (float %a, i32 %b, i32 %c)
    ret float %1
}
""",
  "shflidx_u32" : """define u32 @shflidx(u32 %a, i32 %b, i32 %c) {
    %1 = call u32 asm "shfl.idx.b32 $0, $1, $2, $3;", "=r, r, r, r" (u32 %a, i32 %b, i32 %c)
    ret u32 %1
}
""",
  "shfldn_add" : """define float @shfldn_add(float %a, i32 %b, i32 %c) {
    %1 = call float asm
     "{ .reg .pred p;
        .reg .f32 r0;
        shfl.down.b32 r0|p, $1, $2, $3;
        @p add.f32 r0, r0, $1;
        mov.f32 $0, r0;
     }", "=f, f, r, r" (float %a, i32 %b, i32 %c)

    ret float %1
}
""",
  "shflup_add": """define float @shflup_add(float %a, i32 %b, i32 %c) {
    %1 = call float asm
      "{ .reg .f32 r0;
         .reg .pred p;
         shfl.up.b32 r0|p, $1, $2, $3;
         @p add.f32 r0, r0, $1;
         mov.f32 $0, r0;
      }", "=f, f, r, r" (float %a, i32 %b, i32 %c)

      ret float %1
}
""",
  "shflup_add_u32": """define i32 @shflup_add_u32(i32 %a, i32 %b, i32 %c) {
    %1 = call i32 asm sideeffect
      "{ .reg .u32 r0;
         .reg .pred p;
         shfl.up.b32 r0|p, $1, $2, $3;
         @p add.u32 r0, r0, $1;
         mov.u32 $0, r0;
      }", "=r, r, r, r" (i32 %a, i32 %b, i32 %c)

      ret i32 %1
}
""",
  "lane_id" : """define i32 @lane_id() {
    %1 = call i32 asm "mov.u32 $0, %laneid;", "=r" ()
    ret i32 %1
}
""",
  "clock" : """define i64 @clock() {
    %1 = call i64 asm sideeffect "mov.u32 $0, %clock;", "=r" ()
    ret i64 %1
}
""",
  "bfe" : """define i32 @bfe(i32 %a, i32 %b, i32 %c) {
    %1 = call i32 asm sideeffect "bfe.u32 $0, $1, $2, $3;", "=r, r, r, r" (i32 %a, i32 %b, i32 %c)
    ret i32 %1
}
""",
  "lane_mask_ge" : """define i32 @lane_mask_ge() {
    %1 = call i32 asm sideeffect "mov.u32 $0, %lanemask_ge;", "=r" ()
    ret i32 %1
}
""",
  "st2_cg_s32" : """define i32 @st2_cg_s32(<2 x i32>* %ptr, <2 x i32> %val) {
    %1 = extractelement <2 x i32> %val, i32 0
    %2 = extractelement <2 x i32> %val, i32 1
    call void asm sideeffect "st.cg.v2.s32 [$0], {$1, $2};", "l, r, r" (<2 x i32>* %ptr, i32 %1, i32 %2)
    ret i32 0
  }
""",
  "ld2_cg_s32" : """define <2 x i32> @ld2_cg_s32(<2 x i32>* %ptr) {
    %1 = call {i32, i32} asm sideeffect "ld.cg.v2.s32 {$0, $1}, [$2];", "=r,=r, l" (<2 x i32>* %ptr)
    %2 = extractvalue {i32, i32} %1, 0
    %3 = extractvalue {i32, i32} %1, 1
    %4 = insertelement <2 x i32> undef, i32 %2, i32 0
    %5 = insertelement <2 x i32> %4, i32 %3, i32 1
    ret <2 x i32> %5
  }
""",
  "st_vol_f32" : """define i32 @st_vol_f32(float* %ptr, float %val) {
    call void asm sideeffect "st.volatile.f32 [$0], $1;", "l, r" (float* %ptr, i32 %val)
    ret i32 0
  }
""",
  "ld_vol_f32" : """define f32 @ld_vol_f32(float* %ptr) {
    %1 = call f32 asm sideeffect "ld.volatile.f32 $0, [$1];", "=r, l" (float* %ptr)
    ret f32 %1
  }
"""
}

if rttype == "nvvm":
    result = []
    filename = basename+".nvvm"
    if os.path.isfile(filename):
        with open(filename) as f:
            for line in f:
                matched = False

                for (func, code) in nvvm_defs.items():
                    m = re.match('^declare (.*) (@' + func + ')\((.*)\)\n$', line)
                    if m is not None:
                        result.append(code)
                        matched = True
                        break

                if not matched:
                    result.append(line)

        with open(filename, "w") as f:
            for line in result:
                f.write(line)
