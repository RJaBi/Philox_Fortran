PROGRAM test_compare_philox64
   USE ISO_C_BINDING, ONLY: C64 => C_INT64_T
   USE philox, ONLY: philox2x64_10, philox4x64_10
   IMPLICIT NONE(TYPE, EXTERNAL)

   INTERFACE
      SUBROUTINE philox2x64_10_ref(ctr, key, out) BIND(C)
        IMPORT :: C64
        IMPLICIT NONE(TYPE, EXTERNAL)
         INTEGER(C64), INTENT(IN) :: ctr(2)
         INTEGER(C64), INTENT(IN) :: key(1)
         INTEGER(C64), INTENT(OUT) :: out(2)
      END SUBROUTINE philox2x64_10_ref

      SUBROUTINE philox4x64_10_ref(ctr, key, out) BIND(C)
        IMPORT :: C64
        IMPLICIT NONE(TYPE, EXTERNAL)
         INTEGER(C64), INTENT(IN) :: ctr(4)
         INTEGER(C64), INTENT(IN) :: key(2)
         INTEGER(C64), INTENT(OUT) :: out(4)
      END SUBROUTINE philox4x64_10_ref
   END INTERFACE

   LOGICAL :: ok

   ok = .TRUE.

   CALL run_2x64_case([0_C64, 0_C64], [0_C64], ok, "2x64 zero")
   CALL run_2x64_case([ &
                      INT(z'0123456789ABCDEF', C64), &
                      INT(z'0F1E2D3C4B5A6978', C64)], &
                      [INT(z'13579BDF2468ACE0', C64)], ok, "2x64 mixed")

   CALL run_4x64_case([0_C64, 0_C64, 0_C64, 0_C64], [0_C64, 0_C64], ok, "4x64 zero")
   CALL run_4x64_case([ &
                      INT(z'0123456789ABCDEF', C64), &
                      INT(z'0F1E2D3C4B5A6978', C64), &
                      INT(z'1122334455667788', C64), &
                      INT(z'99AABBCCDDEEFF00', C64)], &
                      [ &
                      INT(z'13579BDF2468ACE0', C64), &
                      INT(z'FEDCBA9876543210', C64)], ok, "4x64 mixed")

   IF (ok) THEN
      PRINT *, "All philox64 comparisons passed."
   ELSE
      ERROR STOP "philox64 comparison failed"
   END IF

CONTAINS

   SUBROUTINE run_2x64_case(ctr, key, ok, label)
      INTEGER(C64), INTENT(IN) :: ctr(2), key(1)
      LOGICAL, INTENT(INOUT) :: ok
      CHARACTER(*), INTENT(IN) :: label

      INTEGER(C64) :: f(2), c(2)

      CALL philox2x64_10(ctr, key, f)
      CALL philox2x64_10_ref(ctr, key, c)

      IF (ALL(f == c)) THEN
         PRINT '(A,": PASS")', TRIM(label)
      ELSE
         ok = .FALSE.
         PRINT '(A,": FAIL")', TRIM(label)
         WRITE (*, '("  F:",2(1X,Z16.16))') f
         WRITE (*, '("  C:",2(1X,Z16.16))') c
      END IF
   END SUBROUTINE run_2x64_case

   SUBROUTINE run_4x64_case(ctr, key, ok, label)
      INTEGER(C64), INTENT(IN) :: ctr(4), key(2)
      LOGICAL, INTENT(INOUT) :: ok
      CHARACTER(*), INTENT(IN) :: label

      INTEGER(C64) :: f(4), c(4)

      CALL philox4x64_10(ctr, key, f)
      CALL philox4x64_10_ref(ctr, key, c)

      IF (ALL(f == c)) THEN
         PRINT '(A,": PASS")', TRIM(label)
      ELSE
         ok = .FALSE.
         PRINT '(A,": FAIL")', TRIM(label)
         WRITE (*, '("  F:",4(1X,Z16.16))') f
         WRITE (*, '("  C:",4(1X,Z16.16))') c
      END IF
   END SUBROUTINE run_4x64_case

END PROGRAM test_compare_philox64
