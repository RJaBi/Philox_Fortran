PROGRAM test_trace_philox32
   USE ISO_C_BINDING, ONLY: C32 => C_INT32_T
   USE philox, ONLY: philox2x32_10_trace, philox4x32_10_trace
   IMPLICIT NONE(TYPE, EXTERNAL)

   INTERFACE
      SUBROUTINE philox2x32_10_trace_ref(ctr, key, out) BIND(C)
        IMPORT :: C32
        IMPLICIT NONE(TYPE, EXTERNAL)
         INTEGER(C32), INTENT(IN) :: ctr(2)
         INTEGER(C32), INTENT(IN) :: key(1)
         INTEGER(C32), INTENT(OUT) :: out(2, 10)
      END SUBROUTINE philox2x32_10_trace_ref

      SUBROUTINE philox4x32_10_trace_ref(ctr, key, out) BIND(C)
        IMPORT :: C32
        IMPLICIT NONE(TYPE, EXTERNAL)
         INTEGER(C32), INTENT(IN) :: ctr(4)
         INTEGER(C32), INTENT(IN) :: key(2)
         INTEGER(C32), INTENT(OUT) :: out(4, 10)
      END SUBROUTINE philox4x32_10_trace_ref
   END INTERFACE

   CALL check_2x32([0_C32, 0_C32], [0_C32], "2x32 zero")
   CALL check_4x32([0_C32, 0_C32, 0_C32, 0_C32], [0_C32, 0_C32], "4x32 zero")

CONTAINS

   SUBROUTINE check_2x32(ctr, key, label)
      INTEGER(C32), INTENT(IN) :: ctr(2), key(1)
      CHARACTER(*), INTENT(IN) :: label
      INTEGER(C32) :: f(10, 2), c(2, 10)
      INTEGER :: i

      CALL philox2x32_10_trace(ctr, key, f)
      CALL philox2x32_10_trace_ref(ctr, key, c)

      PRINT *, TRIM(label)
      DO i = 1, 10
         WRITE (*, '("Round ",I2)') i
         WRITE (*, '("F:",2(1X,Z8.8))') f(i, :)
         WRITE (*, '("C:",2(1X,Z8.8))') c(:, i)
         IF (.NOT. ALL(f(i, :) == c(:, i))) THEN
            ERROR STOP "2x32 trace mismatch"
         END IF
      END DO
   END SUBROUTINE check_2x32

   SUBROUTINE check_4x32(ctr, key, label)
      INTEGER(C32), INTENT(IN) :: ctr(4), key(2)
      CHARACTER(*), INTENT(IN) :: label
      INTEGER(C32) :: f(10, 4), c(4, 10)
      INTEGER :: i

      CALL philox4x32_10_trace(ctr, key, f)
      CALL philox4x32_10_trace_ref(ctr, key, c)

      PRINT *, TRIM(label)
      DO i = 1, 10
         WRITE (*, '("Round ",I2)') i
         WRITE (*, '("F:",4(1X,Z8.8))') f(i, :)
         WRITE (*, '("C:",4(1X,Z8.8))') c(:, i)
         IF (.NOT. ALL(f(i, :) == c(:, i))) THEN
            ERROR STOP "4x32 trace mismatch"
         END IF
      END DO
   END SUBROUTINE check_4x32

END PROGRAM test_trace_philox32
