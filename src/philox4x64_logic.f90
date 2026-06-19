MODULE philox4x64_logic
   USE philox_common, ONLY: u64, mulhilo64
   USE philox_constants_64, ONLY: &
      PHILOX4X64_M0, PHILOX4X64_M1, &
      PHILOX4X64_W0, PHILOX4X64_W1
   USE philox_kinds, ONLY: C64
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE

   PUBLIC :: philox4x64_bumpkey
   PUBLIC :: philox4x64_round
   PUBLIC :: philox4x64_R
   PUBLIC :: philox4x64_7, philox4x64_10
   PUBLIC :: philox4x64_trace
   PUBLIC :: philox4x64_7_trace, philox4x64_10_trace

CONTAINS

   PURE SUBROUTINE philox4x64_bumpkey(k)
      INTEGER(C64), INTENT(INOUT) :: k(2)

      k(1) = u64(k(1) + PHILOX4X64_W0)
      k(2) = u64(k(2) + PHILOX4X64_W1)
   END SUBROUTINE philox4x64_bumpkey

   PURE SUBROUTINE philox4x64_round(c, k)
      INTEGER(C64), INTENT(INOUT) :: c(4)
      INTEGER(C64), INTENT(IN) :: k(2)

      INTEGER(C64) :: hi0, lo0, hi1, lo1
      INTEGER(C64) :: old(4)

      old = c

      CALL mulhilo64(old(1), PHILOX4X64_M0, hi0, lo0)
      CALL mulhilo64(old(3), PHILOX4X64_M1, hi1, lo1)

      c(1) = IEOR(IEOR(hi1, old(2)), k(1))
      c(2) = lo1
      c(3) = IEOR(IEOR(hi0, old(4)), k(2))
      c(4) = lo0
   END SUBROUTINE philox4x64_round

   PURE SUBROUTINE philox4x64_R(counter, key, nrounds, out)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER, INTENT(IN) :: nrounds
      INTEGER(C64), INTENT(OUT) :: out(4)

      INTEGER(C64) :: c(4), k(2)
      INTEGER :: i

      c = counter
      k = key

      DO i = 1, nrounds
         CALL philox4x64_round(c, k)
         IF (i < nrounds) CALL philox4x64_bumpkey(k)
      END DO

      out = c
   END SUBROUTINE philox4x64_R

   PURE SUBROUTINE philox4x64_7(counter, key, out)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(OUT) :: out(4)
      CALL philox4x64_R(counter, key, 7, out)
   END SUBROUTINE philox4x64_7

   PURE SUBROUTINE philox4x64_10(counter, key, out)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(OUT) :: out(4)

      CALL philox4x64_R(counter, key, 10, out)
   END SUBROUTINE philox4x64_10

   PURE SUBROUTINE philox4x64_trace(counter, key, nrounds, out)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER, INTENT(IN) :: nrounds
      INTEGER(C64), INTENT(OUT) :: out(nrounds, 4)

      INTEGER(C64) :: c(4), k(2)
      INTEGER :: i

      c = counter
      k = key

      DO i = 1, nrounds
         CALL philox4x64_round(c, k)
         out(i, :) = c
         IF (i < nrounds) CALL philox4x64_bumpkey(k)
      END DO
   END SUBROUTINE philox4x64_trace

   PURE SUBROUTINE philox4x64_7_trace(counter, key, out)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(OUT) :: out(7, 4)

      CALL philox4x64_trace(counter, key, 7, out)
   END SUBROUTINE philox4x64_7_trace

   PURE SUBROUTINE philox4x64_10_trace(counter, key, out)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(OUT) :: out(10, 4)

      CALL philox4x64_trace(counter, key, 10, out)
   END SUBROUTINE philox4x64_10_trace

END MODULE philox4x64_logic
