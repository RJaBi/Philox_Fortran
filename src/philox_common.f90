MODULE philox_common
   USE philox_kinds, ONLY: C32, C64, MASK32, MASK64
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE

   PUBLIC :: u32, u64, mulhilo32, mulhilo64
   INTEGER(C32), PARAMETER :: MASK16_32 = INT(z'FFFF', C32)
   INTEGER(C64), PARAMETER :: MASK16_64 = INT(z'FFFF', C64)
   INTEGER(C64), PARAMETER :: TWO32 = SHIFTL(1_C64, 32)

CONTAINS
   PURE ELEMENTAL FUNCTION bits32(x) RESULT(r)
      INTEGER(C64), INTENT(IN) :: x
      INTEGER(C32) :: r
      INTEGER(C64) :: y
      y = IAND(x, MASK32)
      IF (BTEST(y, 31)) THEN
         r = INT(y - TWO32, C32)
      ELSE
         r = INT(y, C32)
      END IF
   END FUNCTION bits32

   PURE ELEMENTAL FUNCTION u32(x) RESULT(r)
      INTEGER(C64), INTENT(IN) :: x
      INTEGER(C32) :: r
      r = bits32(x)
   END FUNCTION u32

   PURE ELEMENTAL FUNCTION u64(x) RESULT(r)
      INTEGER(C64), INTENT(IN) :: x
      INTEGER(C64) :: r
      r = IAND(x, MASK64)
   END FUNCTION u64

   PURE SUBROUTINE mulhilo32(a, b, hi, lo)
      INTEGER(C32), INTENT(IN) :: a, b
      INTEGER(C32), INTENT(OUT) :: hi, lo
      INTEGER(C32) :: a0, a1, b0, b1
      INTEGER(C64) :: p00, p01, p10, p11
      INTEGER(C64) :: mid0, carry
      INTEGER(C64) :: lo64, hi64
      a0 = IAND(a, MASK16_32)
      a1 = IAND(SHIFTR(a, 16), MASK16_32)
      b0 = IAND(b, MASK16_32)
      b1 = IAND(SHIFTR(b, 16), MASK16_32)
      p00 = INT(a0, C64) * INT(b0, C64)
      p01 = INT(a0, C64) * INT(b1, C64)
      p10 = INT(a1, C64) * INT(b0, C64)
      p11 = INT(a1, C64) * INT(b1, C64)
      mid0 = SHIFTR(p00, 16) + IAND(p01, MASK16_64) + IAND(p10, MASK16_64)
      carry = SHIFTR(mid0, 16)
      lo64 = IOR(IAND(p00, MASK16_64), &
                 SHIFTL(IAND(mid0, MASK16_64), 16))
      hi64 = p11 + SHIFTR(p01, 16) + SHIFTR(p10, 16) + carry
      lo = bits32(lo64)
      hi = bits32(hi64)
   END SUBROUTINE mulhilo32
   PURE SUBROUTINE mulhilo64(a, b, hi, lo)
      INTEGER(C64), INTENT(IN) :: a, b
      INTEGER(C64), INTENT(OUT) :: hi, lo

      INTEGER(C64) :: aa(0:3), bb(0:3)
      INTEGER(C64) :: acc(0:7)
      INTEGER(C64) :: carry
      INTEGER :: i, j, k
      aa(0) = IAND(a, MASK16_64)
      aa(1) = IAND(SHIFTR(a, 16), MASK16_64)
      aa(2) = IAND(SHIFTR(a, 32), MASK16_64)
      aa(3) = IAND(SHIFTR(a, 48), MASK16_64)
      bb(0) = IAND(b, MASK16_64)
      bb(1) = IAND(SHIFTR(b, 16), MASK16_64)
      bb(2) = IAND(SHIFTR(b, 32), MASK16_64)
      bb(3) = IAND(SHIFTR(b, 48), MASK16_64)
      acc = 0_C64
      DO i = 0, 3
         DO j = 0, 3
            acc(i + j) = acc(i + j) + aa(i) * bb(j)
         END DO
      END DO
      DO k = 0, 6
         carry = SHIFTR(acc(k), 16)
         acc(k) = IAND(acc(k), MASK16_64)
         acc(k + 1) = acc(k + 1) + carry
      END DO
      acc(7) = IAND(acc(7), MASK16_64)
      lo = IOR(IOR(IAND(acc(0), MASK16_64), SHIFTL(IAND(acc(1), MASK16_64), 16)), &
               IOR(SHIFTL(IAND(acc(2), MASK16_64), 32), SHIFTL(IAND(acc(3), MASK16_64), 48)))

      hi = IOR(IOR(IAND(acc(4), MASK16_64), SHIFTL(IAND(acc(5), MASK16_64), 16)), &
               IOR(SHIFTL(IAND(acc(6), MASK16_64), 32), SHIFTL(IAND(acc(7), MASK16_64), 48)))
   END SUBROUTINE mulhilo64

END MODULE philox_common
