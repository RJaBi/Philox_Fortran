MODULE philox_uniform
   USE ISO_C_BINDING, ONLY: C_DOUBLE
   USE philox4x64_logic, ONLY: philox4x64_10
   USE philox_kinds, ONLY: MASK32, C64
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE

   PUBLIC :: philox_uniform_cc, philox_uniform_co
   PUBLIC :: philox_uniform_oc, philox_uniform_oo
   PUBLIC :: philox_fill_uniform_cc, philox_fill_uniform_co
   PUBLIC :: philox_fill_uniform_oc, philox_fill_uniform_oo

   PUBLIC :: philox_u01_cc, philox_u01_co, philox_u01_oc, philox_u01_oo

   INTEGER(C64), PARAMETER :: TWO32 = SHIFTL(1_C64, 32)
   INTEGER(C64), PARAMETER :: TWO53 = SHIFTL(1_C64, 53)
   INTEGER(C64), PARAMETER :: TWO53M1 = TWO53 - 1_C64

   REAL(C_DOUBLE), PARAMETER :: INV_TWO53 = 1.0_C_DOUBLE / real(TWO53, C_DOUBLE)
   REAL(C_DOUBLE), PARAMETER :: INV_TWO53M1 = 1.0_C_DOUBLE / real(TWO53M1, C_DOUBLE)

CONTAINS

   ! ===========================================================================
   ! Public scalar API
   ! ===========================================================================
   PURE FUNCTION philox_uniform_co(counter, key, idx, a, b) RESULT(x)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(IN) :: idx
      REAL(C_DOUBLE), INTENT(IN) :: a, b
      REAL(C_DOUBLE) :: x
      INTEGER(C64) :: word

      ! Precondition: idx >= 1, b >= a
      word = word_from_index(counter, key, idx)
      x = affine_map(a, b, philox_u01_co(word))
   END FUNCTION philox_uniform_co

   PURE FUNCTION philox_uniform_oc(counter, key, idx, a, b) RESULT(x)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(IN) :: idx
      REAL(C_DOUBLE), INTENT(IN) :: a, b
      REAL(C_DOUBLE) :: x
      INTEGER(C64) :: word

      ! Precondition: idx >= 1, b >= a
      word = word_from_index(counter, key, idx)
      x = affine_map(a, b, philox_u01_oc(word))
   END FUNCTION philox_uniform_oc

   PURE FUNCTION philox_uniform_oo(counter, key, idx, a, b) RESULT(x)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(IN) :: idx
      REAL(C_DOUBLE), INTENT(IN) :: a, b
      REAL(C_DOUBLE) :: x
      INTEGER(C64) :: j, word, m
      INTEGER(C64), PARAMETER :: TWO53 = SHIFTL(1_C64, 53)
      REAL(C_DOUBLE), PARAMETER :: INV_TWO53 = 1.0_C_DOUBLE / real(TWO53, C_DOUBLE)

      ! Precondition: b > a, idx >= 1
      j = 0_C64
      DO
         word = word_from_index(counter, key, idx + j)
         m = top53(word)
         IF (m /= 0_C64) EXIT
         j = j + 1_C64
      END DO
      x = a + (b - a) * (real(m, C_DOUBLE) * INV_TWO53)
   END FUNCTION philox_uniform_oo

!!
!!  pure function philox_uniform_oo(counter, key, idx, a, b) result(x)
!!    integer(C64), intent(in) :: counter(4), key(2)
!!    integer(C64), intent(in) :: idx
!!    real(c_double), intent(in) :: a, b
!!    real(c_double) :: x
!!    integer(C64) :: word
!!
!!    ! Precondition: idx >= 1, b > a
!!    word = word_from_index(counter, key, idx)
!!    x = affine_map(a, b, philox_u01_oo(word))
!!  end function philox_uniform_oo
!!

   PURE FUNCTION philox_uniform_cc(counter, key, idx, a, b) RESULT(x)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(IN) :: idx
      REAL(C_DOUBLE), INTENT(IN) :: a, b
      REAL(C_DOUBLE) :: x
      INTEGER(C64) :: word

      ! Precondition: idx >= 1, b >= a
      ! If a == b, this returns exactly a.
      word = word_from_index(counter, key, idx)
      x = affine_map(a, b, philox_u01_cc(word))
   END FUNCTION philox_uniform_cc

   ! ===========================================================================
   ! Public vector-fill API
   ! ===========================================================================
   PURE SUBROUTINE philox_fill_uniform_co(counter, key, start_idx, a, b, x)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(IN) :: start_idx
      REAL(C_DOUBLE), INTENT(IN) :: a, b
      REAL(C_DOUBLE), INTENT(OUT) :: x(:)
      INTEGER :: i

      ! Precondition: start_idx >= 1, b >= a
      DO CONCURRENT(i=1:SIZE(x))
         x(i) = philox_uniform_co(counter, key, start_idx + INT(i - 1, C64), a, b)
      END DO
   END SUBROUTINE philox_fill_uniform_co

   PURE SUBROUTINE philox_fill_uniform_oc(counter, key, start_idx, a, b, x)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(IN) :: start_idx
      REAL(C_DOUBLE), INTENT(IN) :: a, b
      REAL(C_DOUBLE), INTENT(OUT) :: x(:)
      INTEGER :: i

      ! Precondition: start_idx >= 1, b >= a
      DO CONCURRENT(i=1:SIZE(x))
         x(i) = philox_uniform_oc(counter, key, start_idx + INT(i - 1, C64), a, b)
      END DO
   END SUBROUTINE philox_fill_uniform_oc

   PURE SUBROUTINE philox_fill_uniform_oo(counter, key, start_idx, a, b, x)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(IN) :: start_idx
      REAL(C_DOUBLE), INTENT(IN) :: a, b
      REAL(C_DOUBLE), INTENT(OUT) :: x(:)
      INTEGER :: i

      ! Precondition: start_idx >= 1, b > a
      DO CONCURRENT(i=1:SIZE(x))
         x(i) = philox_uniform_oo(counter, key, start_idx + INT(i - 1, C64), a, b)
      END DO
   END SUBROUTINE philox_fill_uniform_oo

   PURE SUBROUTINE philox_fill_uniform_cc(counter, key, start_idx, a, b, x)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(IN) :: start_idx
      REAL(C_DOUBLE), INTENT(IN) :: a, b
      REAL(C_DOUBLE), INTENT(OUT) :: x(:)
      INTEGER :: i

      ! Precondition: start_idx >= 1, b >= a
      DO CONCURRENT(i=1:SIZE(x))
         x(i) = philox_uniform_cc(counter, key, start_idx + INT(i - 1, C64), a, b)
      END DO
   END SUBROUTINE philox_fill_uniform_cc

   ! ===========================================================================
   ! Core stateless word selection
   ! ===========================================================================
   PURE FUNCTION word_from_index(counter, key, idx) RESULT(word)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(IN) :: idx
      INTEGER(C64) :: word

      INTEGER(C64) :: ctr_i(4), block(4)
      INTEGER(C64) :: block_idx, lane

      ! idx is 1-based for Fortran callers
      block_idx = (idx - 1_C64) / 4_C64
      lane = MODULO(idx - 1_C64, 4_C64) + 1_C64

      ctr_i = counter_add_blocks_4x64(counter, block_idx)
      CALL philox4x64_10(ctr_i, key, block)

      word = block(INT(lane))
   END FUNCTION word_from_index

   ! ===========================================================================
   ! Counter advancement by a number of 4-word blocks
   ! 256-bit little-endian counter represented as 4 x 64-bit words.
   ! ===========================================================================
   PURE FUNCTION counter_add_blocks_4x64(counter, nblocks) RESULT(out)
      INTEGER(C64), INTENT(IN) :: counter(4)
      INTEGER(C64), INTENT(IN) :: nblocks
      INTEGER(C64) :: out(4)

      INTEGER(C64) :: limbs(0:7)
      INTEGER(C64) :: add0, add1, carry
      INTEGER :: k

      ! Split counter into 8 x 32-bit limbs, little-endian
      limbs(0) = IAND(counter(1), MASK32)
      limbs(1) = IAND(SHIFTR(counter(1), 32), MASK32)
      limbs(2) = IAND(counter(2), MASK32)
      limbs(3) = IAND(SHIFTR(counter(2), 32), MASK32)
      limbs(4) = IAND(counter(3), MASK32)
      limbs(5) = IAND(SHIFTR(counter(3), 32), MASK32)
      limbs(6) = IAND(counter(4), MASK32)
      limbs(7) = IAND(SHIFTR(counter(4), 32), MASK32)

      add0 = IAND(nblocks, MASK32)
      add1 = IAND(SHIFTR(nblocks, 32), MASK32)

      limbs(0) = limbs(0) + add0
      carry = SHIFTR(limbs(0), 32)
      limbs(0) = IAND(limbs(0), MASK32)

      limbs(1) = limbs(1) + add1 + carry
      carry = SHIFTR(limbs(1), 32)
      limbs(1) = IAND(limbs(1), MASK32)

      DO k = 2, 7
         limbs(k) = limbs(k) + carry
         carry = SHIFTR(limbs(k), 32)
         limbs(k) = IAND(limbs(k), MASK32)
      END DO

      out(1) = IOR(SHIFTL(limbs(1), 32), limbs(0))
      out(2) = IOR(SHIFTL(limbs(3), 32), limbs(2))
      out(3) = IOR(SHIFTL(limbs(5), 32), limbs(4))
      out(4) = IOR(SHIFTL(limbs(7), 32), limbs(6))
   END FUNCTION counter_add_blocks_4x64

   ! ===========================================================================
   ! Unit-interval mappings from a raw 64-bit word
   ! ===========================================================================
   PURE ELEMENTAL FUNCTION philox_u01_co(word) RESULT(u)
      INTEGER(C64), INTENT(IN) :: word
      REAL(C_DOUBLE) :: u
      INTEGER(C64) :: m

      ! [0,1)
      m = IAND(SHIFTR(word, 11), TWO53M1)
      u = real(m, C_DOUBLE) * INV_TWO53
   END FUNCTION philox_u01_co

   PURE ELEMENTAL FUNCTION philox_u01_oc(word) RESULT(u)
      INTEGER(C64), INTENT(IN) :: word
      REAL(C_DOUBLE) :: u
      INTEGER(C64) :: m

      ! (0,1]
      m = IAND(SHIFTR(word, 11), TWO53M1)
      u = real(m + 1_C64, C_DOUBLE) * INV_TWO53
   END FUNCTION philox_u01_oc

!!  pure elemental function philox_u01_oo(word) result(u)
!!    integer(C64), intent(in) :: word
!!    real(c_double) :: u
!!    integer(C64) :: m
!!
!!    ! (0,1)
!!    m = iand(shiftr(word, 11), TWO53M1)
!!    u = (real(m, c_double) + 0.5_c_double) * INV_TWO53
!!  end function philox_u01_oo

   PURE ELEMENTAL FUNCTION philox_u01_oo(word) RESULT(u)
      INTEGER(C64), INTENT(IN) :: word
      REAL(C_DOUBLE) :: u
      INTEGER(C64), PARAMETER :: TWO53 = SHIFTL(1_C64, 53)
      INTEGER(C64), PARAMETER :: TWO53P2 = TWO53 + 2_C64
      INTEGER(C64), PARAMETER :: TWO53M1 = TWO53 - 1_C64
      INTEGER(C64) :: m
      m = IAND(SHIFTR(word, 11), TWO53M1)
      u = real(m + 1_C64, C_DOUBLE) / real(TWO53P2, C_DOUBLE)
   END FUNCTION philox_u01_oo

   PURE ELEMENTAL FUNCTION philox_u01_cc(word) RESULT(u)
      INTEGER(C64), INTENT(IN) :: word
      REAL(C_DOUBLE) :: u
      INTEGER(C64) :: m

      ! [0,1]
      m = IAND(SHIFTR(word, 11), TWO53M1)
      u = real(m, C_DOUBLE) * INV_TWO53M1
   END FUNCTION philox_u01_cc

   ! ===========================================================================
   ! Affine map [0,1] -> [a,b]
   ! ===========================================================================
   PURE ELEMENTAL FUNCTION affine_map(a, b, u) RESULT(x)
      REAL(C_DOUBLE), INTENT(IN) :: a, b, u
      REAL(C_DOUBLE) :: x

      x = a + (b - a) * u
   END FUNCTION affine_map

   PURE ELEMENTAL FUNCTION top53(word) RESULT(m)
      INTEGER(C64), INTENT(IN) :: word
      INTEGER(C64) :: m
      INTEGER(C64), PARAMETER :: TWO53M1 = SHIFTL(1_C64, 53) - 1_C64
      m = IAND(SHIFTR(word, 11), TWO53M1)
   END FUNCTION top53

END MODULE philox_uniform
