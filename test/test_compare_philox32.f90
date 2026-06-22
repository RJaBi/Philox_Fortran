PROGRAM test_compare_philox32
   USE ISO_C_BINDING, ONLY: C32 => C_INT32_T
   USE philox, ONLY: philox2x32_10, philox4x32_10
   IMPLICIT NONE(TYPE, EXTERNAL)

   INTERFACE
      SUBROUTINE philox2x32_10_ref(ctr, key, out) BIND(C)
        IMPORT :: C32
        IMPLICIT NONE(TYPE, EXTERNAL)
         INTEGER(C32), INTENT(IN) :: ctr(2)
         INTEGER(C32), INTENT(IN) :: key(1)
         INTEGER(C32), INTENT(OUT) :: out(2)
      END SUBROUTINE philox2x32_10_ref

      SUBROUTINE philox4x32_10_ref(ctr, key, out) BIND(C)
        IMPORT :: C32
        IMPLICIT NONE(TYPE, EXTERNAL)
         INTEGER(C32), INTENT(IN) :: ctr(4)
         INTEGER(C32), INTENT(IN) :: key(2)
         INTEGER(C32), INTENT(OUT) :: out(4)
      END SUBROUTINE philox4x32_10_ref
   END INTERFACE

   LOGICAL :: ok

   ok = .TRUE.

   CALL run_2x32_case([0_C32, 0_C32], [0_C32], ok, "2x32 zero")
   CALL run_2x32_case([ &
                      INT(z'01234567', C32), INT(z'89ABCDEF', C32)], &
                      [INT(z'13579BDF', C32)], ok, "2x32 mixed")

   CALL run_4x32_case([0_C32, 0_C32, 0_C32, 0_C32], [0_C32, 0_C32], ok, "4x32 zero")
   CALL run_4x32_case([ &
                      INT(z'01234567', C32), INT(z'89ABCDEF', C32), &
                      INT(z'0F1E2D3C', C32), INT(z'4B5A6978', C32)], &
                      [INT(z'13579BDF', C32), INT(z'2468ACE0', C32)], ok, "4x32 mixed")

   IF (ok) THEN
      PRINT *, "All philox32 comparisons passed."
   ELSE
      ERROR STOP "philox32 comparison failed"
   END IF

CONTAINS

   SUBROUTINE run_2x32_case(ctr, key, ok, label)
      INTEGER(C32), INTENT(IN) :: ctr(2), key(1)
      LOGICAL, INTENT(INOUT) :: ok
      CHARACTER(*), INTENT(IN) :: label

      INTEGER(C32) :: f(2), c(2)

      CALL philox2x32_10(ctr, key, f)
      CALL philox2x32_10_ref(ctr, key, c)

      IF (ALL(f == c)) THEN
         PRINT '(A,": PASS")', TRIM(label)
      ELSE
         ok = .FALSE.
         PRINT '(A,": FAIL")', TRIM(label)
         WRITE (*, '("  F:",2(1X,Z8.8))') f
         WRITE (*, '("  C:",2(1X,Z8.8))') c
      END IF
   END SUBROUTINE run_2x32_case

   SUBROUTINE run_4x32_case(ctr, key, ok, label)
      INTEGER(C32), INTENT(IN) :: ctr(4), key(2)
      LOGICAL, INTENT(INOUT) :: ok
      CHARACTER(*), INTENT(IN) :: label

      INTEGER(C32) :: f(4), c(4)

      CALL philox4x32_10(ctr, key, f)
      CALL philox4x32_10_ref(ctr, key, c)

      IF (ALL(f == c)) THEN
         PRINT '(A,": PASS")', TRIM(label)
      ELSE
         ok = .FALSE.
         PRINT '(A,": FAIL")', TRIM(label)
         WRITE (*, '("  F:",4(1X,Z8.8))') f
         WRITE (*, '("  C:",4(1X,Z8.8))') c
      END IF
   END SUBROUTINE run_4x32_case

END PROGRAM test_compare_philox32
