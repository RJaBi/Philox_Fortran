PROGRAM test_trace_philox64
   USE ISO_C_BINDING, ONLY: C64 => C_INT64_T
   USE philox, ONLY: philox2x64_10_trace, philox4x64_10_trace
   IMPLICIT NONE(TYPE, EXTERNAL)

   INTERFACE
      SUBROUTINE philox2x64_10_trace_ref(ctr, key, out) BIND(C)
        IMPORT :: C64
        IMPLICIT NONE(TYPE, EXTERNAL)
         INTEGER(C64), INTENT(IN) :: ctr(2)
         INTEGER(C64), INTENT(IN) :: key(1)
         INTEGER(C64), INTENT(OUT) :: out(2, 10)
      END SUBROUTINE philox2x64_10_trace_ref

      SUBROUTINE philox4x64_10_trace_ref(ctr, key, out) BIND(C)
        IMPORT :: C64
        IMPLICIT NONE(TYPE, EXTERNAL)
         INTEGER(C64), INTENT(IN) :: ctr(4)
         INTEGER(C64), INTENT(IN) :: key(2)
         INTEGER(C64), INTENT(OUT) :: out(4, 10)
      END SUBROUTINE philox4x64_10_trace_ref
   END INTERFACE

   CALL check_2x64([0_C64, 0_C64], [0_C64], "2x64 zero")
   CALL check_4x64([0_C64, 0_C64, 0_C64, 0_C64], [0_C64, 0_C64], "4x64 zero")

CONTAINS

   SUBROUTINE check_2x64(ctr, key, label)
      INTEGER(C64), INTENT(IN) :: ctr(2), key(1)
      CHARACTER(*), INTENT(IN) :: label
      INTEGER(C64) :: f(10, 2), c(2, 10)
      INTEGER :: i

      CALL philox2x64_10_trace(ctr, key, f)
      CALL philox2x64_10_trace_ref(ctr, key, c)

      PRINT *, TRIM(label)
      DO i = 1, 10
         WRITE (*, '("Round ",I2)') i
         WRITE (*, '("F:",2(1X,Z16.16))') f(i, :)
         WRITE (*, '("C:",2(1X,Z16.16))') c(:, i)
         IF (.NOT. ALL(f(i, :) == c(:, i))) THEN
            ERROR STOP "2x64 trace mismatch"
         END IF
      END DO
   END SUBROUTINE check_2x64

   SUBROUTINE check_4x64(ctr, key, label)
      INTEGER(C64), INTENT(IN) :: ctr(4), key(2)
      CHARACTER(*), INTENT(IN) :: label
      INTEGER(C64) :: f(10, 4), c(4, 10)
      INTEGER :: i

      CALL philox4x64_10_trace(ctr, key, f)
      CALL philox4x64_10_trace_ref(ctr, key, c)

      PRINT *, TRIM(label)
      DO i = 1, 10
         WRITE (*, '("Round ",I2)') i
         WRITE (*, '("F:",4(1X,Z16.16))') f(i, :)
         WRITE (*, '("C:",4(1X,Z16.16))') c(:, i)
         IF (.NOT. ALL(f(i, :) == c(:, i))) THEN
            ERROR STOP "4x64 trace mismatch"
         END IF
      END DO
   END SUBROUTINE check_4x64

END PROGRAM test_trace_philox64
