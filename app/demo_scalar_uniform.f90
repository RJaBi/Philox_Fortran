PROGRAM demo_scalar_uniform
   USE ISO_C_BINDING, ONLY: C_DOUBLE
   USE philox, ONLY: philox_uniform_co, philox_uniform_oo, C64
   IMPLICIT NONE(TYPE, EXTERNAL)

   INTEGER(C64) :: counter(4), key(2)
   REAL(C_DOUBLE) :: x, y

   counter = 0_C64
   key = [123_C64, 456_C64]

   x = philox_uniform_co(counter, key, 1_C64, 0.0_C_DOUBLE, 1.0_C_DOUBLE)
   y = philox_uniform_oo(counter, key, 2_C64, -1.0_C_DOUBLE, 1.0_C_DOUBLE)

   PRINT *, "x in [0,1): ", x
   PRINT *, "y in (-1,1): ", y
END PROGRAM demo_scalar_uniform
