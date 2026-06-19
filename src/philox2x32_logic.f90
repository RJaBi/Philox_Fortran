MODULE philox2x32_logic
   USE philox_common, ONLY: u32, mulhilo32
   USE philox_constants_32, ONLY: PHILOX2X32_M0, PHILOX2X32_W0
   USE philox_kinds, ONLY: C32, C64
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE

   PUBLIC :: philox2x32_bumpkey
   PUBLIC :: philox2x32_round
   PUBLIC :: philox2x32_R
   PUBLIC :: philox2x32_7, philox2x32_10
   PUBLIC :: philox2x32_trace
   PUBLIC :: philox2x32_7_trace, philox2x32_10_trace

CONTAINS

   PURE SUBROUTINE philox2x32_bumpkey(k)
      INTEGER(C32), INTENT(INOUT) :: k(1)

      k(1) = u32(INT(k(1), C64) + INT(PHILOX2X32_W0, C64))
   END SUBROUTINE philox2x32_bumpkey

   PURE SUBROUTINE philox2x32_round(c, k)
      INTEGER(C32), INTENT(INOUT) :: c(2)
      INTEGER(C32), INTENT(IN) :: k(1)

      INTEGER(C32) :: hi0, lo0
      INTEGER(C32) :: old0, old1

      old0 = c(1)
      old1 = c(2)

      CALL mulhilo32(old0, PHILOX2X32_M0, hi0, lo0)

      !c(1) = u32(int(ieor(hi0, old1), C64) + int(k(1), C64))
      c(1) = IEOR(IEOR(hi0, old1), k(1))
      c(2) = lo0
   END SUBROUTINE philox2x32_round

   PURE SUBROUTINE philox2x32_R(counter, key, nrounds, out)
      INTEGER(C32), INTENT(IN) :: counter(2), key(1)
      INTEGER, INTENT(IN) :: nrounds
      INTEGER(C32), INTENT(OUT) :: out(2)

      INTEGER(C32) :: c(2), k(1)
      INTEGER :: i

      c = counter
      k = key

      DO i = 1, nrounds
         CALL philox2x32_round(c, k)
         IF (i < nrounds) CALL philox2x32_bumpkey(k)
      END DO

      out = c
   END SUBROUTINE philox2x32_R

   PURE SUBROUTINE philox2x32_7(counter, key, out)
      INTEGER(C32), INTENT(IN) :: counter(2), key(1)
      INTEGER(C32), INTENT(OUT) :: out(2)

      CALL philox2x32_R(counter, key, 7, out)
   END SUBROUTINE philox2x32_7

   PURE SUBROUTINE philox2x32_10(counter, key, out)
      INTEGER(C32), INTENT(IN) :: counter(2), key(1)
      INTEGER(C32), INTENT(OUT) :: out(2)

      CALL philox2x32_R(counter, key, 10, out)
   END SUBROUTINE philox2x32_10

   PURE SUBROUTINE philox2x32_trace(counter, key, nrounds, out)
      INTEGER(C32), INTENT(IN) :: counter(2), key(1)
      INTEGER, INTENT(IN) :: nrounds
      INTEGER(C32), INTENT(OUT) :: out(nrounds, 2)

      INTEGER(C32) :: c(2), k(1)
      INTEGER :: i

      c = counter
      k = key

      DO i = 1, nrounds
         CALL philox2x32_round(c, k)
         out(i, :) = c
         IF (i < nrounds) CALL philox2x32_bumpkey(k)
      END DO
   END SUBROUTINE philox2x32_trace

   PURE SUBROUTINE philox2x32_7_trace(counter, key, out)
      INTEGER(C32), INTENT(IN) :: counter(2), key(1)
      INTEGER(C32), INTENT(OUT) :: out(7, 2)

      CALL philox2x32_trace(counter, key, 7, out)
   END SUBROUTINE philox2x32_7_trace

   PURE SUBROUTINE philox2x32_10_trace(counter, key, out)
      INTEGER(C32), INTENT(IN) :: counter(2), key(1)
      INTEGER(C32), INTENT(OUT) :: out(10, 2)

      CALL philox2x32_trace(counter, key, 10, out)
   END SUBROUTINE philox2x32_10_trace

END MODULE philox2x32_logic
