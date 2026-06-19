MODULE philox_types
   USE philox_kinds, ONLY: C32, C64
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE

   PUBLIC :: philox2x32_ctr, philox2x32_key
   PUBLIC :: philox4x32_ctr, philox4x32_key

   PUBLIC :: philox2x64_ctr, philox2x64_key
   PUBLIC :: philox4x64_ctr, philox4x64_key

   TYPE :: philox2x32_ctr
      INTEGER(C32) :: v(2)
   END TYPE philox2x32_ctr

   TYPE :: philox2x32_key
      INTEGER(C32) :: v(1)
   END TYPE philox2x32_key

   TYPE :: philox4x32_ctr
      INTEGER(C32) :: v(4)
   END TYPE philox4x32_ctr

   TYPE :: philox4x32_key
      INTEGER(C32) :: v(2)
   END TYPE philox4x32_key

   TYPE :: philox2x64_ctr
      INTEGER(C64) :: v(2)
   END TYPE philox2x64_ctr

   TYPE :: philox2x64_key
      INTEGER(C64) :: v(1)
   END TYPE philox2x64_key

   TYPE :: philox4x64_ctr
      INTEGER(C64) :: v(4)
   END TYPE philox4x64_ctr

   TYPE :: philox4x64_key
      INTEGER(C64) :: v(2)
   END TYPE philox4x64_key

END MODULE philox_types
