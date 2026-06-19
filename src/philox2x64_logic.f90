MODULE philox2x64_logic
   USE philox_common, ONLY: u64, mulhilo64
   USE philox_constants_64, ONLY: PHILOX2X64_M0, PHILOX2X64_W0
   USE philox_kinds, ONLY: C64
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE

   PUBLIC :: philox2x64_bumpkey
   PUBLIC :: philox2x64_round
   PUBLIC :: philox2x64_R
   PUBLIC :: philox2x64_7, philox2x64_10
   PUBLIC :: philox2x64_trace
   PUBLIC :: philox2x64_7_trace, philox2x64_10_trace

CONTAINS

   PURE SUBROUTINE philox2x64_bumpkey(k)
      INTEGER(C64), INTENT(INOUT) :: k(1)

      k(1) = u64(k(1) + PHILOX2X64_W0)
   END SUBROUTINE philox2x64_bumpkey

   PURE SUBROUTINE philox2x64_round(c, k)
      INTEGER(C64), INTENT(INOUT) :: c(2)
      INTEGER(C64), INTENT(IN) :: k(1)

      INTEGER(C64) :: hi0, lo0
      INTEGER(C64) :: old0, old1

      old0 = c(1)
      old1 = c(2)

      CALL mulhilo64(old0, PHILOX2X64_M0, hi0, lo0)

      c(1) = IEOR(IEOR(hi0, old1), k(1))
      c(2) = lo0
   END SUBROUTINE philox2x64_round

   PURE SUBROUTINE philox2x64_R(counter, key, nrounds, out)
      INTEGER(C64), INTENT(IN) :: counter(2), key(1)
      INTEGER, INTENT(IN) :: nrounds
      INTEGER(C64), INTENT(OUT) :: out(2)

      INTEGER(C64) :: c(2), k(1)
      INTEGER :: i

      c = counter
      k = key

      DO i = 1, nrounds
         CALL philox2x64_round(c, k)
         IF (i < nrounds) CALL philox2x64_bumpkey(k)
      END DO

      out = c
   END SUBROUTINE philox2x64_R

   PURE SUBROUTINE philox2x64_7(counter, key, out)
      INTEGER(C64), INTENT(IN) :: counter(2), key(1)
      INTEGER(C64), INTENT(OUT) :: out(2)

      CALL philox2x64_R(counter, key, 7, out)
   END SUBROUTINE philox2x64_7

   PURE SUBROUTINE philox2x64_10(counter, key, out)
      INTEGER(C64), INTENT(IN) :: counter(2), key(1)
      INTEGER(C64), INTENT(OUT) :: out(2)

      CALL philox2x64_R(counter, key, 10, out)
   END SUBROUTINE philox2x64_10

   PURE SUBROUTINE philox2x64_trace(counter, key, nrounds, out)
      INTEGER(C64), INTENT(IN) :: counter(2), key(1)
      INTEGER, INTENT(IN) :: nrounds
      INTEGER(C64), INTENT(OUT) :: out(nrounds, 2)

      INTEGER(C64) :: c(2), k(1)
      INTEGER :: i

      c = counter
      k = key

      DO i = 1, nrounds
         CALL philox2x64_round(c, k)
         out(i, :) = c
         IF (i < nrounds) CALL philox2x64_bumpkey(k)
      END DO
   END SUBROUTINE philox2x64_trace

   PURE SUBROUTINE philox2x64_7_trace(counter, key, out)
      INTEGER(C64), INTENT(IN) :: counter(2), key(1)
      INTEGER(C64), INTENT(OUT) :: out(7, 2)

      CALL philox2x64_trace(counter, key, 7, out)
   END SUBROUTINE philox2x64_7_trace

   PURE SUBROUTINE philox2x64_10_trace(counter, key, out)
      INTEGER(C64), INTENT(IN) :: counter(2), key(1)
      INTEGER(C64), INTENT(OUT) :: out(10, 2)

      CALL philox2x64_trace(counter, key, 10, out)
   END SUBROUTINE philox2x64_10_trace

END MODULE philox2x64_logic
