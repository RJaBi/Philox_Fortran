MODULE Philox
   USE philox2x32_logic, ONLY: &
      philox2x32_bumpkey, &
      philox2x32_round, &
      philox2x32_R, &
      philox2x32_7, philox2x32_10, &
      philox2x32_trace, &
      philox2x32_7_trace, philox2x32_10_trace
   USE philox2x64_logic, ONLY: &
      philox2x64_bumpkey, &
      philox2x64_round, &
      philox2x64_R, &
      philox2x64_7, philox2x64_10, &
      philox2x64_trace, &
      philox2x64_7_trace, philox2x64_10_trace
   USE philox4x32_logic, ONLY: &
      philox4x32_bumpkey, &
      philox4x32_round, &
      philox4x32_R, &
      philox4x32_7, philox4x32_10, &
      philox4x32_trace, &
      philox4x32_7_trace, philox4x32_10_trace
   USE philox4x64_logic, ONLY: &
      philox4x64_bumpkey, &
      philox4x64_round, &
      philox4x64_R, &
      philox4x64_7, philox4x64_10, &
      philox4x64_trace, &
      philox4x64_7_trace, philox4x64_10_trace
   USE philox_kinds, ONLY: C64, C32
   USE philox_uniform, ONLY: &
      philox_uniform_cc, philox_uniform_co, &
      philox_uniform_oc, philox_uniform_oo, &
      philox_fill_uniform_cc, philox_fill_uniform_co, &
      philox_fill_uniform_oc, philox_fill_uniform_oo, &
      philox_u01_cc, philox_u01_co, philox_u01_oc, philox_u01_oo
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE

   PUBLIC :: &
      philox2x32_bumpkey, philox2x32_round, philox2x32_R, &
      philox2x32_7, philox2x32_10, &
      philox2x32_trace, philox2x32_7_trace, philox2x32_10_trace, &
      philox4x32_bumpkey, philox4x32_round, philox4x32_R, &
      philox4x32_7, philox4x32_10, &
      philox4x32_trace, philox4x32_7_trace, philox4x32_10_trace, &
      philox2x64_bumpkey, philox2x64_round, philox2x64_R, &
      philox2x64_7, philox2x64_10, &
      philox2x64_trace, philox2x64_7_trace, philox2x64_10_trace, &
      philox4x64_bumpkey, philox4x64_round, philox4x64_R, &
      philox4x64_7, philox4x64_10, &
      philox4x64_trace, philox4x64_7_trace, philox4x64_10_trace, &
      philox_uniform_cc, philox_uniform_co, &
      philox_uniform_oc, philox_uniform_oo, &
      philox_fill_uniform_cc, philox_fill_uniform_co, &
      philox_fill_uniform_oc, philox_fill_uniform_oo, &
      philox_u01_cc, philox_u01_co, philox_u01_oc, philox_u01_oo, &
      C64, C32

END MODULE Philox
