PROGRAM test_compare_philox64_expand
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

   ! ---------------------------------------------------------------------------
   ! 2x64 cases
   ! ---------------------------------------------------------------------------
   CALL run_2x64_case([0_C64, 0_C64], [0_C64], ok, "2x64 zero")

   CALL run_2x64_case([1_C64, 0_C64], [0_C64], ok, "2x64 low bit")
   CALL run_2x64_case([INT(z'8000000000000000', C64), 0_C64], [0_C64], ok, "2x64 high bit")

   CALL run_2x64_case([ &
                      INT(z'FFFFFFFFFFFFFFFF', C64), INT(z'FFFFFFFFFFFFFFFF', C64)], &
                      [INT(z'FFFFFFFFFFFFFFFF', C64)], ok, "2x64 all ones")

   CALL run_2x64_case([ &
                      INT(z'AAAAAAAAAAAAAAAA', C64), INT(z'5555555555555555', C64)], &
                      [INT(z'0123456789ABCDEF', C64)], ok, "2x64 alternating")

   CALL run_2x64_case([ &
                      INT(z'0123456789ABCDEF', C64), &
                      INT(z'0F1E2D3C4B5A6978', C64)], &
                      [INT(z'13579BDF2468ACE0', C64)], ok, "2x64 mixed")

   CALL run_2x64_case([ &
                      INT(z'FFFFFFFFFFFFFFFE', C64), &
                      INT(z'FFFFFFFFFFFFFFFD', C64)], &
                      [INT(z'FFFFFFFFFFFFFFFC', C64)], ok, "2x64 near all ones")

   CALL run_2x64_case([ &
                      INT(z'DEADBEEFCAFEBABE', C64), &
                      INT(z'1122334455667788', C64)], &
                      [INT(z'0BADF00D0D15EA5E', C64)], ok, "2x64 arbitrary")

   ! ---------------------------------------------------------------------------
   ! 4x64 cases
   ! ---------------------------------------------------------------------------
   CALL run_4x64_case([0_C64, 0_C64, 0_C64, 0_C64], [0_C64, 0_C64], ok, "4x64 zero")

   CALL run_4x64_case([1_C64, 0_C64, 0_C64, 0_C64], [0_C64, 0_C64], ok, "4x64 low bit")
   CALL run_4x64_case([ &
                      INT(z'8000000000000000', C64), 0_C64, &
                      0_C64, 0_C64], &
                      [0_C64, 0_C64], ok, "4x64 high bit")

   CALL run_4x64_case([ &
                      INT(z'FFFFFFFFFFFFFFFF', C64), INT(z'FFFFFFFFFFFFFFFF', C64), &
                      INT(z'FFFFFFFFFFFFFFFF', C64), INT(z'FFFFFFFFFFFFFFFF', C64)], &
                      [INT(z'FFFFFFFFFFFFFFFF', C64), INT(z'FFFFFFFFFFFFFFFF', C64)], &
                      ok, "4x64 all ones")

   CALL run_4x64_case([ &
                      INT(z'AAAAAAAAAAAAAAAA', C64), INT(z'5555555555555555', C64), &
                      INT(z'AAAAAAAAAAAAAAAA', C64), INT(z'5555555555555555', C64)], &
                      [INT(z'0123456789ABCDEF', C64), INT(z'FEDCBA9876543210', C64)], &
                      ok, "4x64 alternating")

   CALL run_4x64_case([ &
                      INT(z'0123456789ABCDEF', C64), &
                      INT(z'0F1E2D3C4B5A6978', C64), &
                      INT(z'1122334455667788', C64), &
                      INT(z'99AABBCCDDEEFF00', C64)], &
                      [ &
                      INT(z'13579BDF2468ACE0', C64), &
                      INT(z'FEDCBA9876543210', C64)], ok, "4x64 mixed")

   CALL run_4x64_case([ &
                      INT(z'FFFFFFFFFFFFFFFE', C64), INT(z'0000000000000001', C64), &
                      INT(z'FFFFFFFFFFFFFFFD', C64), INT(z'0000000000000002', C64)], &
                      [INT(z'FFFFFFFFFFFFFFFC', C64), INT(z'0000000000000003', C64)], &
                      ok, "4x64 carry stress")

   CALL run_4x64_case([ &
                      INT(z'DEADBEEFCAFEBABE', C64), INT(z'1122334455667788', C64), &
                      INT(z'0BADF00D0D15EA5E', C64), INT(z'8877665544332211', C64)], &
                      [INT(z'0102030405060708', C64), INT(z'F0E0D0C0B0A09080', C64)], &
                      ok, "4x64 arbitrary")

   CALL run_4x64_case([ &
                      INT(z'0001020304050607', C64), INT(z'08090A0B0C0D0E0F', C64), &
                      INT(z'F0E0D0C0B0A09080', C64), INT(z'7060504030201000', C64)], &
                      [INT(z'13579BDF2468ACE0', C64), INT(z'0F0E0D0C0B0A0908', C64)], &
                      ok, "4x64 lane scramble")

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
         WRITE (*, '("  ctr:",2(1X,Z16.16))') ctr
         WRITE (*, '("  key:",1(1X,Z16.16))') key
         WRITE (*, '("    F:",2(1X,Z16.16))') f
         WRITE (*, '("    C:",2(1X,Z16.16))') c
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
         WRITE (*, '("  ctr:",4(1X,Z16.16))') ctr
         WRITE (*, '("  key:",2(1X,Z16.16))') key
         WRITE (*, '("    F:",4(1X,Z16.16))') f
         WRITE (*, '("    C:",4(1X,Z16.16))') c
      END IF
   END SUBROUTINE run_4x64_case

END PROGRAM test_compare_philox64_expand
