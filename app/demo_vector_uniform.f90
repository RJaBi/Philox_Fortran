PROGRAM demo_vector_uniform
   USE ISO_C_BINDING, ONLY: C_DOUBLE
   USE philox, ONLY: philox_fill_uniform_cc, C64
   IMPLICIT NONE(TYPE, EXTERNAL)

   INTEGER(C64) :: counter(4), key(2)
   REAL(C_DOUBLE) :: x(8)

   counter = 0_C64
   key = 0_C64

   CALL philox_fill_uniform_cc(counter, key, 1_C64, 2.0_C_DOUBLE, 3.0_C_DOUBLE, x)

   PRINT *, x
END PROGRAM demo_vector_uniform
