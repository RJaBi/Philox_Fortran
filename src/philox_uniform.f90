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

   INTEGER(kind=c64), PARAMETER :: TWO53   = SHIFTL(1_C64, 53)
   INTEGER(kind=c64), PARAMETER :: TWO53M1 = TWO53 - 1_C64

   REAL(kind=C_DOUBLE), PARAMETER :: INV_TWO53   = 1.0_C_DOUBLE / REAL(TWO53,   C_DOUBLE)
   REAL(kind=C_DOUBLE), PARAMETER :: INV_TWO53M1 = 1.0_C_DOUBLE / REAL(TWO53M1, C_DOUBLE)

   ! Chunk size used internally by fill routines to avoid allocating a full-size
   ! integer temporary for very large arrays.
   INTEGER, PARAMETER :: PHILOX_FILL_CHUNK = 256

CONTAINS

   ! ===========================================================================
   ! Public scalar API
   ! ===========================================================================

   PURE FUNCTION philox_uniform_co(counter, key, idx, a, b) RESULT(x)
     integeR(kind=c64), dimension(4), intent(in) :: counter
     integeR(kind=c64), dimension(2), intent(in) :: key
      INTEGER(kind=c64), INTENT(IN) :: idx
      REAL(kind=C_DOUBLE), INTENT(IN) :: a, b
      REAL(kind=C_DOUBLE) :: x
      INTEGER(kind=c64) :: word
      ! Precondition: idx >= 1, b >= a
      word = word_from_index(counter, key, idx)
      x = affine_map(a, b, philox_u01_co(word))
   END FUNCTION philox_uniform_co

   PURE FUNCTION philox_uniform_oc(counter, key, idx, a, b) RESULT(x)
     integeR(kind=c64), dimension(4), intent(in) :: counter
     integeR(kind=c64), dimension(2), intent(in) :: key
      INTEGER(kind=c64), INTENT(IN) :: idx
      REAL(kind=C_DOUBLE), INTENT(IN) :: a, b
      REAL(kind=C_DOUBLE) :: x
      INTEGER(kind=c64) :: word
      ! Precondition: idx >= 1, b >= a
      word = word_from_index(counter, key, idx)
      x = affine_map(a, b, philox_u01_oc(word))
   END FUNCTION philox_uniform_oc

   PURE FUNCTION philox_uniform_oo(counter, key, idx, a, b) RESULT(x)
     integer(kind=c64), dimension(4), intent(in) :: counter
     integer(kind=c64), dimension(2), intent(in) :: key
      INTEGER(kind=c64), INTENT(IN) :: idx
      REAL(kind=C_DOUBLE), INTENT(IN) :: a, b
      REAL(kind=C_DOUBLE) :: x
      INTEGER(kind=c64) :: word
      ! Precondition: idx >= 1, b > a
      !
      ! Direct per-word open-interval map, consistent with the block-based fill.
      word = word_from_index(counter, key, idx)
      x = affine_map(a, b, philox_u01_oo(word))
   END FUNCTION philox_uniform_oo

   PURE FUNCTION philox_uniform_cc(counter, key, idx, a, b) RESULT(x)
     integer(kind=c64), dimension(4), intent(in) :: counter
     integer(kind=c64), dimension(2), intent(in) :: key
      INTEGER(kind=c64), INTENT(IN) :: idx
      REAL(kind=C_DOUBLE), INTENT(IN) :: a, b
      REAL(kind=C_DOUBLE) :: x
      INTEGER(kind=c64) :: word
      ! Precondition: idx >= 1, b >= a
      ! If a == b, this returns exactly a.
      word = word_from_index(counter, key, idx)
      x = affine_map(a, b, philox_u01_cc(word))
   END FUNCTION philox_uniform_cc

   ! ===========================================================================
   ! Public vector-fill API
   !
   ! These use philox_fill_words(), which performs block draws internally and
   ! supports arbitrary start_idx and arbitrary SIZE(x).
   ! ===========================================================================

   PURE SUBROUTINE philox_fill_uniform_co(counter, key, start_idx, a, b, x)
     integer(kind=c64), dimension(4), intent(in) :: counter
     integer(kind=c64), dimension(2), intent(in) :: key
      INTEGER(kind=c64), INTENT(IN) :: start_idx
      REAL(kind=C_DOUBLE), INTENT(IN) :: a, b
      REAL(kind=C_DOUBLE), dimension(:), INTENT(OUT) :: x
      INTEGER(kind=c64), dimension(PHILOX_FILL_CHUNK) :: words
      INTEGER :: pos, n_this
      ! Precondition: start_idx >= 1, b >= a
      IF (SIZE(x) == 0) RETURN
      pos = 1
      DO WHILE (pos <= SIZE(x))
         n_this = MIN(PHILOX_FILL_CHUNK, SIZE(x) - pos + 1)
         ! Grabs 4 randoms at a time because that's what philox4x64_10 does
         CALL philox_fill_words(counter, key, &
                                start_idx + INT(pos - 1, C64), &
                                words(:n_this))
         x(pos:pos+n_this-1) = affine_map(a, b, philox_u01_co(words(:n_this)))
         pos = pos + n_this
      END DO
   END SUBROUTINE philox_fill_uniform_co

   PURE SUBROUTINE philox_fill_uniform_oc(counter, key, start_idx, a, b, x)
     integer(kind=c64), dimension(4), intent(in) :: counter
     integer(kind=c64), dimension(2), intent(in) :: key
      INTEGER(kind=c64), INTENT(IN) :: start_idx
      REAL(kind=C_DOUBLE), INTENT(IN) :: a, b
      REAL(kind=C_DOUBLE), dimension(:), INTENT(OUT) :: x
      INTEGER(kind=c64), dimension(PHILOX_FILL_CHUNK) :: words
      INTEGER :: pos, n_this
      ! Precondition: start_idx >= 1, b >= a
      IF (SIZE(x) == 0) RETURN
      pos = 1
      DO WHILE (pos <= SIZE(x))
         n_this = MIN(PHILOX_FILL_CHUNK, SIZE(x) - pos + 1)
         CALL philox_fill_words(counter, key, &
                                start_idx + INT(pos - 1, C64), &
                                words(:n_this))
         x(pos:pos+n_this-1) = affine_map(a, b, philox_u01_oc(words(:n_this)))
         pos = pos + n_this
      END DO
   END SUBROUTINE philox_fill_uniform_oc

   PURE SUBROUTINE philox_fill_uniform_oo(counter, key, start_idx, a, b, x)
     integer(kind=c64), dimension(4), intent(in) :: counter
     integer(kind=c64), dimension(2), intent(in) :: key
      INTEGER(kind=c64), INTENT(IN) :: start_idx
      REAL(kind=C_DOUBLE), INTENT(IN) :: a, b
      REAL(kind=C_DOUBLE), dimension(:), INTENT(OUT) :: x
      INTEGER(kind=c64), dimension(PHILOX_FILL_CHUNK) :: words
      INTEGER :: pos, n_this
      ! Precondition: start_idx >= 1, b > a
      IF (SIZE(x) == 0) RETURN
      pos = 1
      DO WHILE (pos <= SIZE(x))
         n_this = MIN(PHILOX_FILL_CHUNK, SIZE(x) - pos + 1)
         CALL philox_fill_words(counter, key, &
                                start_idx + INT(pos - 1, C64), &
                                words(:n_this))
         x(pos:pos+n_this-1) = affine_map(a, b, philox_u01_oo(words(:n_this)))
         pos = pos + n_this
      END DO
   END SUBROUTINE philox_fill_uniform_oo

   PURE SUBROUTINE philox_fill_uniform_cc(counter, key, start_idx, a, b, x)
     integer(kind=c64), dimension(4), intent(in) :: counter
     integer(kind=c64), dimension(2), intent(in) :: key
      INTEGER(kind=c64), INTENT(IN) :: start_idx
      REAL(kind=C_DOUBLE), INTENT(IN) :: a, b
      REAL(kind=C_DOUBLE), dimension(:), INTENT(OUT) :: x
      INTEGER(kind=c64), dimension(PHILOX_FILL_CHUNK) :: words
      INTEGER :: pos, n_this
      ! Precondition: start_idx >= 1, b >= a
      IF (SIZE(x) == 0) RETURN
      pos = 1
      DO WHILE (pos <= SIZE(x))
         n_this = MIN(PHILOX_FILL_CHUNK, SIZE(x) - pos + 1)
         CALL philox_fill_words(counter, key, &
                                start_idx + INT(pos - 1, C64), &
                                words(:n_this))
         x(pos:pos+n_this-1) = affine_map(a, b, philox_u01_cc(words(:n_this)))
         pos = pos + n_this
      END DO
   END SUBROUTINE philox_fill_uniform_cc

   ! ===========================================================================
   ! Private helper: fill raw consecutive Philox words beginning at start_idx.
   !
   ! Supports:
   !   - ragged first block  (start_idx not aligned)
   !   - ragged last block   (SIZE(words) not multiple of 4)
   ! ===========================================================================

   PURE SUBROUTINE philox_fill_words(counter, key, start_idx, words)
     integer(kind=c64), dimension(4), intent(in) :: counter
     integer(kind=c64), dimension(2), intent(in) :: key
      INTEGER(kind=c64), INTENT(IN)  :: start_idx
      INTEGER(kind=c64), dimension(:), INTENT(OUT) :: words
      INTEGER(kind=c64), dimension(4) :: ctr_i, blk
      INTEGER(kind=c64) :: block_idx, lane
      INTEGER :: pos, l1, l2, n_take, avail
      ! Precondition: start_idx >= 1
      IF (SIZE(words) == 0) RETURN
      block_idx = (start_idx - 1_C64) / 4_C64
      lane      = MODULO(start_idx - 1_C64, 4_C64) + 1_C64
      pos = 1
      DO WHILE (pos <= SIZE(words))
         ctr_i = counter_add_blocks_4x64(counter, block_idx)
         CALL philox4x64_10(ctr_i, key, blk)
         l1     = INT(lane)
         avail  = 5 - l1
         n_take = MIN(SIZE(words) - pos + 1, avail)
         l2     = l1 + n_take - 1
         words(pos:pos+n_take-1) = blk(l1:l2)
         pos       = pos + n_take
         block_idx = block_idx + 1_C64
         lane      = 1_C64
      END DO
   END SUBROUTINE philox_fill_words

   ! ===========================================================================
   ! Private scalar random-access helper
   ! ===========================================================================

   PURE FUNCTION word_from_index(counter, key, idx) RESULT(word)
     integer(kind=c64), dimension(4), intent(in) :: counter
     integer(kind=c64), dimension(2), intent(in) :: key
      INTEGER(kind=c64), INTENT(IN) :: idx
      INTEGER(kind=c64) :: word
      INTEGER(kind=c64), dimension(4) :: ctr_i, blk
      INTEGER(kind=c64) :: block_idx, lane
      block_idx = (idx - 1_C64) / 4_C64
      lane      = MODULO(idx - 1_C64, 4_C64) + 1_C64
      ctr_i = counter_add_blocks_4x64(counter, block_idx)
      CALL philox4x64_10(ctr_i, key, blk)
      word = blk(INT(lane))
   END FUNCTION word_from_index

   ! ===========================================================================
   ! Counter advancement by a number of 4-word blocks
   ! 256-bit little-endian counter represented as 4 x 64-bit words.
   ! ===========================================================================

   PURE FUNCTION counter_add_blocks_4x64(counter, nblocks) RESULT(out)
     integer(kind=c64), dimension(4), intent(in) :: counter
      INTEGER(kind=c64), INTENT(IN) :: nblocks
      INTEGER(kind=c64), dimension(4) :: out
      INTEGER(kind=c64), dimension(0:7) :: limbs
      INTEGER(kind=c64) :: add0, add1, carry
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
      carry    = SHIFTR(limbs(0), 32)
      limbs(0) = IAND(limbs(0), MASK32)

      limbs(1) = limbs(1) + add1 + carry
      carry    = SHIFTR(limbs(1), 32)
      limbs(1) = IAND(limbs(1), MASK32)

      DO k = 2, 7
         limbs(k) = limbs(k) + carry
         carry    = SHIFTR(limbs(k), 32)
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
      INTEGER(kind=c64), INTENT(IN) :: word
      REAL(kind=C_DOUBLE) :: u
      INTEGER(kind=c64) :: m
      ! [0,1)
      m = IAND(SHIFTR(word, 11), TWO53M1)
      u = REAL(m, C_DOUBLE) * INV_TWO53
   END FUNCTION philox_u01_co

   PURE ELEMENTAL FUNCTION philox_u01_oc(word) RESULT(u)
      INTEGER(kind=c64), INTENT(IN) :: word
      REAL(kind=C_DOUBLE) :: u
      INTEGER(kind=c64) :: m
      ! (0,1]
      m = IAND(SHIFTR(word, 11), TWO53M1)
      u = REAL(m + 1_C64, C_DOUBLE) * INV_TWO53
   END FUNCTION philox_u01_oc

   PURE ELEMENTAL FUNCTION philox_u01_oo(word) RESULT(u)
      INTEGER(kind=c64), INTENT(IN) :: word
      REAL(kind=C_DOUBLE) :: u
      INTEGER(kind=c64), PARAMETER :: TWO53P2 = TWO53 + 2_C64
      INTEGER(kind=c64) :: m
      ! (0,1)
      !
      ! Open interval with one output per input word.
      m = IAND(SHIFTR(word, 11), TWO53M1)
      u = REAL(m + 1_C64, C_DOUBLE) / REAL(TWO53P2, C_DOUBLE)
   END FUNCTION philox_u01_oo

   PURE ELEMENTAL FUNCTION philox_u01_cc(word) RESULT(u)
      INTEGER(kind=c64), INTENT(IN) :: word
      REAL(kind=C_DOUBLE) :: u
      INTEGER(kind=c64) :: m
      ! [0,1]
      m = IAND(SHIFTR(word, 11), TWO53M1)
      u = REAL(m, C_DOUBLE) * INV_TWO53M1
   END FUNCTION philox_u01_cc

   ! ===========================================================================
   ! Affine map [0,1] -> [a,b]
   ! ===========================================================================
   PURE ELEMENTAL FUNCTION affine_map(a, b, u) RESULT(x)
      REAL(kind=C_DOUBLE), INTENT(IN) :: a, b, u
      REAL(kind=C_DOUBLE) :: x
      x = a + (b - a) * u
   END FUNCTION affine_map

END MODULE philox_uniform
