PROGRAM example_fill
  USE iso_c_binding, ONLY: c_double
  USE philox, ONLY: philox_fill_uniform_cc, C64
  IMPLICIT NONE(TYPE, EXTERNAL)
  INTEGER(kind=C64) :: counter(4)
  INTEGER(kind=C64) :: key(2)
  REAL(kind=c_double) :: a, b
  REAL(kind=c_double), ALLOCATABLE :: x(:)

  ALLOCATE(x(8))
  ! Example counter/key (choose deterministic values for reproducibility)
  counter = [0_C64, 0_C64, 0_C64, 1_C64]
  key = [12345_C64, 67890_C64]
  a = 0.0_c_double
  b = 1.0_c_double

  CALL philox_fill_uniform_cc(counter, key, 1_C64, a, b, x)

  PRINT *, 'Generated values:'
  PRINT *, x

  DEALLOCATE(x)
END PROGRAM example_fill
