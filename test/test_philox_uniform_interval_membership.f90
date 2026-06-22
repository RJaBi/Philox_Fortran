PROGRAM test_philox_uniform_interval_membership
   USE ISO_C_BINDING, ONLY: C_DOUBLE
   USE philox, ONLY: C64, &
                     philox_uniform_cc, philox_uniform_co, &
                     philox_uniform_oc, philox_uniform_oo, &
                     philox_fill_uniform_cc, philox_fill_uniform_co, &
                     philox_fill_uniform_oc, philox_fill_uniform_oo
   IMPLICIT NONE(TYPE, EXTERNAL)

   LOGICAL :: ok
   INTEGER(C64) :: counter0(4), counter1(4)
   INTEGER(C64) :: key0(2), key1(2)

   ok = .TRUE.

   counter0 = 0_C64
   key0 = 0_C64

   counter1 = [ &
              INT(z'0123456789ABCDEF', C64), &
              INT(z'0F1E2D3C4B5A6978', C64), &
              INT(z'1122334455667788', C64), &
              INT(z'99AABBCCDDEEFF00', C64)]
   key1 = [ &
          INT(z'13579BDF2468ACE0', C64), &
          INT(z'FEDCBA9876543210', C64)]

   ! ---------------------------------------------------------------------------
   ! Scalar membership tests
   ! ---------------------------------------------------------------------------
   CALL check_scalar_membership(counter0, key0, 0.0_C_DOUBLE, 1.0_C_DOUBLE, 4096_C64, ok, "scalar stream0 [0,1]")
   CALL check_scalar_membership(counter0, key0, -1.0_C_DOUBLE, 1.0_C_DOUBLE, 4096_C64, ok, "scalar stream0 [-1,1]")
   CALL check_scalar_membership(counter0, key0, 2.5_C_DOUBLE, 7.75_C_DOUBLE, 4096_C64, ok, "scalar stream0 [2.5,7.75]")

   CALL check_scalar_membership(counter1, key1, 0.0_C_DOUBLE, 1.0_C_DOUBLE, 4096_C64, ok, "scalar stream1 [0,1]")
   CALL check_scalar_membership(counter1, key1, -3.0_C_DOUBLE, -0.5_C_DOUBLE, 4096_C64, ok, "scalar stream1 [-3,-0.5]")
   CALL check_scalar_membership(counter1, key1, 10.0_C_DOUBLE, 10.25_C_DOUBLE, 4096_C64, ok, "scalar stream1 [10,10.25]")

   ! Degenerate closed interval: [A,A]
   CALL check_closed_degenerate(counter0, key0, 3.141592653589793_C_DOUBLE, 1024_C64, ok, "scalar stream0 [A,A]")
   CALL check_closed_degenerate(counter1, key1, -2.0_C_DOUBLE, 1024_C64, ok, "scalar stream1 [A,A]")

   ! ---------------------------------------------------------------------------
   ! Array-fill membership + scalar/array consistency tests
   ! ---------------------------------------------------------------------------
   CALL check_fill_membership(counter0, key0, 1_C64, 0.0_C_DOUBLE, 1.0_C_DOUBLE, 257, ok, "fill stream0 [0,1]")
   CALL check_fill_membership(counter0, key0, 100_C64, -1.0_C_DOUBLE, 1.0_C_DOUBLE, 257, ok, "fill stream0 [-1,1]")
   CALL check_fill_membership(counter1, key1, 1_C64, 2.5_C_DOUBLE, 7.75_C_DOUBLE, 257, ok, "fill stream1 [2.5,7.75]")
   CALL check_fill_membership(counter1, key1, 77_C64, -3.0_C_DOUBLE, -0.5_C_DOUBLE, 257, ok, "fill stream1 [-3,-0.5]")

   CALL check_fill_closed_degenerate(counter0, key0, 1_C64, 0.25_C_DOUBLE, 129, ok, "fill stream0 [A,A]")
   CALL check_fill_closed_degenerate(counter1, key1, 99_C64, -7.0_C_DOUBLE, 129, ok, "fill stream1 [A,A]")

   IF (ok) THEN
      PRINT *, "All philox uniform interval-membership tests passed."
   ELSE
      ERROR STOP "philox uniform interval-membership tests failed"
   END IF

CONTAINS

   SUBROUTINE assert_true(cond, msg, ok)
      LOGICAL, INTENT(IN) :: cond
      CHARACTER(*), INTENT(IN) :: msg
      LOGICAL, INTENT(INOUT) :: ok

      IF (cond) THEN
         PRINT '(A,": PASS")', TRIM(msg)
      ELSE
         ok = .FALSE.
         PRINT '(A,": FAIL")', TRIM(msg)
      END IF
   END SUBROUTINE assert_true

   SUBROUTINE check_scalar_membership(counter, key, a, b, n, ok, label)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      REAL(C_DOUBLE), INTENT(IN) :: a, b
      INTEGER(C64), INTENT(IN) :: n
      LOGICAL, INTENT(INOUT) :: ok
      CHARACTER(*), INTENT(IN) :: label

      INTEGER(C64) :: idx
      REAL(C_DOUBLE) :: x
      LOGICAL :: good_cc, good_co, good_oc, good_oo

      good_cc = .TRUE.
      good_co = .TRUE.
      good_oc = .TRUE.
      good_oo = .TRUE.

      DO idx = 1_C64, n
         x = philox_uniform_cc(counter, key, idx, a, b)
         good_cc = good_cc .AND. (x >= a .AND. x <= b)

         x = philox_uniform_co(counter, key, idx, a, b)
         good_co = good_co .AND. (x >= a .AND. x < b)

         x = philox_uniform_oc(counter, key, idx, a, b)
         good_oc = good_oc .AND. (x > a .AND. x <= b)

         IF (b > a) THEN
            x = philox_uniform_oo(counter, key, idx, a, b)
            good_oo = good_oo .AND. (x > a .AND. x < b)
         END IF
      END DO

      CALL assert_true(good_cc, TRIM(label)//" cc membership", ok)
      CALL assert_true(good_co, TRIM(label)//" co membership", ok)
      CALL assert_true(good_oc, TRIM(label)//" oc membership", ok)

      IF (b > a) THEN
         CALL assert_true(good_oo, TRIM(label)//" oo membership", ok)
      END IF
   END SUBROUTINE check_scalar_membership

   SUBROUTINE check_closed_degenerate(counter, key, a, n, ok, label)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      REAL(C_DOUBLE), INTENT(IN) :: a
      INTEGER(C64), INTENT(IN) :: n
      LOGICAL, INTENT(INOUT) :: ok
      CHARACTER(*), INTENT(IN) :: label

      INTEGER(C64) :: idx
      REAL(C_DOUBLE) :: x
      LOGICAL :: good

      good = .TRUE.
      DO idx = 1_C64, n
         x = philox_uniform_cc(counter, key, idx, a, a)
         good = good .AND. (x == a)
      END DO

      CALL assert_true(good, TRIM(label)//" returns exactly A", ok)
   END SUBROUTINE check_closed_degenerate

   SUBROUTINE check_fill_membership(counter, key, start_idx, a, b, n, ok, label)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(IN) :: start_idx
      REAL(C_DOUBLE), INTENT(IN) :: a, b
      INTEGER, INTENT(IN) :: n
      LOGICAL, INTENT(INOUT) :: ok
      CHARACTER(*), INTENT(IN) :: label

      REAL(C_DOUBLE), ALLOCATABLE :: xcc(:), xco(:), xoc(:), xoo(:)
      INTEGER :: i
      LOGICAL :: good_cc, good_co, good_oc, good_oo
      LOGICAL :: same_cc, same_co, same_oc, same_oo

      ALLOCATE (xcc(n), xco(n), xoc(n), xoo(n))

      CALL philox_fill_uniform_cc(counter, key, start_idx, a, b, xcc)
      CALL philox_fill_uniform_co(counter, key, start_idx, a, b, xco)
      CALL philox_fill_uniform_oc(counter, key, start_idx, a, b, xoc)
      IF (b > a) CALL philox_fill_uniform_oo(counter, key, start_idx, a, b, xoo)

      good_cc = ALL(xcc >= a .AND. xcc <= b)
      good_co = ALL(xco >= a .AND. xco < b)
      good_oc = ALL(xoc > a .AND. xoc <= b)

      same_cc = .TRUE.
      same_co = .TRUE.
      same_oc = .TRUE.
      same_oo = .TRUE.

      DO i = 1, n
         same_cc = same_cc .AND. (xcc(i) == philox_uniform_cc(counter, key, start_idx + INT(i - 1, C64), a, b))
         same_co = same_co .AND. (xco(i) == philox_uniform_co(counter, key, start_idx + INT(i - 1, C64), a, b))
         same_oc = same_oc .AND. (xoc(i) == philox_uniform_oc(counter, key, start_idx + INT(i - 1, C64), a, b))
         IF (b > a) THEN
            same_oo = same_oo .AND. (xoo(i) == philox_uniform_oo(counter, key, start_idx + INT(i - 1, C64), a, b))
         END IF
      END DO

      CALL assert_true(good_cc, TRIM(label)//" fill cc membership", ok)
      CALL assert_true(good_co, TRIM(label)//" fill co membership", ok)
      CALL assert_true(good_oc, TRIM(label)//" fill oc membership", ok)

      CALL assert_true(same_cc, TRIM(label)//" fill/scalar cc agreement", ok)
      CALL assert_true(same_co, TRIM(label)//" fill/scalar co agreement", ok)
      CALL assert_true(same_oc, TRIM(label)//" fill/scalar oc agreement", ok)

      IF (b > a) THEN
         good_oo = ALL(xoo > a .AND. xoo < b)
         CALL assert_true(good_oo, TRIM(label)//" fill oo membership", ok)
         CALL assert_true(same_oo, TRIM(label)//" fill/scalar oo agreement", ok)
      END IF

      DEALLOCATE (xcc, xco, xoc, xoo)
   END SUBROUTINE check_fill_membership

   SUBROUTINE check_fill_closed_degenerate(counter, key, start_idx, a, n, ok, label)
      INTEGER(C64), INTENT(IN) :: counter(4), key(2)
      INTEGER(C64), INTENT(IN) :: start_idx
      REAL(C_DOUBLE), INTENT(IN) :: a
      INTEGER, INTENT(IN) :: n
      LOGICAL, INTENT(INOUT) :: ok
      CHARACTER(*), INTENT(IN) :: label

      REAL(C_DOUBLE), ALLOCATABLE :: x(:)
      INTEGER :: i
      LOGICAL :: good, same

      ALLOCATE (x(n))
      CALL philox_fill_uniform_cc(counter, key, start_idx, a, a, x)

      good = ALL(x == a)
      same = .TRUE.
      DO i = 1, n
         same = same .AND. (x(i) == philox_uniform_cc(counter, key, start_idx + INT(i - 1, C64), a, a))
      END DO

      CALL assert_true(good, TRIM(label)//" fill cc returns exactly A", ok)
      CALL assert_true(same, TRIM(label)//" fill/scalar cc agreement for [A,A]", ok)

      DEALLOCATE (x)
   END SUBROUTINE check_fill_closed_degenerate

END PROGRAM test_philox_uniform_interval_membership
