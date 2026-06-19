MODULE philox4x32_logic
   USE philox_common, ONLY: u32, mulhilo32
   USE philox_constants_32, ONLY: &
      PHILOX4X32_M0, PHILOX4X32_M1, &
      PHILOX4X32_W0, PHILOX4X32_W1
   USE philox_kinds, ONLY: C32, C64
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE

   PUBLIC :: philox4x32_bumpkey
   PUBLIC :: philox4x32_round
   PUBLIC :: philox4x32_R
   PUBLIC :: philox4x32_7, philox4x32_10
   PUBLIC :: philox4x32_trace
   PUBLIC :: philox4x32_7_trace, philox4x32_10_trace

CONTAINS

   PURE SUBROUTINE philox4x32_bumpkey(k)
      INTEGER(C32), INTENT(INOUT) :: k(2)

      k(1) = u32(INT(k(1), C64) + INT(PHILOX4X32_W0, C64))
      k(2) = u32(INT(k(2), C64) + INT(PHILOX4X32_W1, C64))
   END SUBROUTINE philox4x32_bumpkey

   PURE SUBROUTINE philox4x32_round(c, k)
      INTEGER(C32), INTENT(INOUT) :: c(4)
      INTEGER(C32), INTENT(IN) :: k(2)

      INTEGER(C32) :: hi0, lo0, hi1, lo1
      INTEGER(C32) :: old(4)

      old = c

      CALL mulhilo32(old(1), PHILOX4X32_M0, hi0, lo0)
      CALL mulhilo32(old(3), PHILOX4X32_M1, hi1, lo1)

      c(1) = IEOR(IEOR(hi1, old(2)), k(1))
      c(2) = lo1
      c(3) = IEOR(IEOR(hi0, old(4)), k(2))
      c(4) = lo0
   END SUBROUTINE philox4x32_round

   PURE SUBROUTINE philox4x32_R(counter, key, nrounds, out)
      INTEGER(C32), INTENT(IN) :: counter(4), key(2)
      INTEGER, INTENT(IN) :: nrounds
      INTEGER(C32), INTENT(OUT) :: out(4)

      INTEGER(C32) :: c(4), k(2)
      INTEGER :: i

      c = counter
      k = key

      DO i = 1, nrounds
         CALL philox4x32_round(c, k)
         IF (i < nrounds) CALL philox4x32_bumpkey(k)
      END DO

      out = c
   END SUBROUTINE philox4x32_R

   PURE SUBROUTINE philox4x32_7(counter, key, out)
      INTEGER(C32), INTENT(IN) :: counter(4), key(2)
      INTEGER(C32), INTENT(OUT) :: out(4)

      CALL philox4x32_R(counter, key, 7, out)
   END SUBROUTINE philox4x32_7

   PURE SUBROUTINE philox4x32_10(counter, key, out)
      INTEGER(C32), INTENT(IN) :: counter(4), key(2)
      INTEGER(C32), INTENT(OUT) :: out(4)

      CALL philox4x32_R(counter, key, 10, out)
   END SUBROUTINE philox4x32_10

   PURE SUBROUTINE philox4x32_trace(counter, key, nrounds, out)
      INTEGER(C32), INTENT(IN) :: counter(4), key(2)
      INTEGER, INTENT(IN) :: nrounds
      INTEGER(C32), INTENT(OUT) :: out(nrounds, 4)

      INTEGER(C32) :: c(4), k(2)
      INTEGER :: i

      c = counter
      k = key

      DO i = 1, nrounds
         CALL philox4x32_round(c, k)
         out(i, :) = c
         IF (i < nrounds) CALL philox4x32_bumpkey(k)
      END DO
   END SUBROUTINE philox4x32_trace

   PURE SUBROUTINE philox4x32_7_trace(counter, key, out)
      INTEGER(C32), INTENT(IN) :: counter(4), key(2)
      INTEGER(C32), INTENT(OUT) :: out(7, 4)

      CALL philox4x32_trace(counter, key, 7, out)
   END SUBROUTINE philox4x32_7_trace

   PURE SUBROUTINE philox4x32_10_trace(counter, key, out)
      INTEGER(C32), INTENT(IN) :: counter(4), key(2)
      INTEGER(C32), INTENT(OUT) :: out(10, 4)

      CALL philox4x32_trace(counter, key, 10, out)
   END SUBROUTINE philox4x32_10_trace

END MODULE philox4x32_logic
