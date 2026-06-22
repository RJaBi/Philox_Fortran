PROGRAM test_philox_uniform_spacing
   USE ISO_C_BINDING, ONLY: C_DOUBLE
   USE philox, ONLY: C64, &
                     philox_u01_cc, philox_u01_co, philox_u01_oc, philox_u01_oo
   IMPLICIT NONE(TYPE, EXTERNAL)

   INTEGER(C64), PARAMETER :: TWO53 = SHIFTL(1_C64, 53)
   INTEGER(C64), PARAMETER :: TWO53M1 = TWO53 - 1_C64

   REAL(C_DOUBLE), PARAMETER :: inv_two53 = 1.0_C_DOUBLE / real(TWO53, C_DOUBLE)
   REAL(C_DOUBLE), PARAMETER :: inv_two53m1 = 1.0_C_DOUBLE / real(TWO53M1, C_DOUBLE)

   LOGICAL :: ok

   ok = .TRUE.

   CALL test_u01_co(ok)
   CALL test_u01_oc(ok)
   CALL test_u01_oo(ok)
   CALL test_u01_cc(ok)

   IF (ok) THEN
      PRINT *, "All philox uniform spacing tests passed."
   ELSE
      ERROR STOP "philox uniform spacing tests failed"
   END IF

CONTAINS

   SUBROUTINE assert_true(cond, msg, ok)
      LOGICAL, INTENT(IN) :: cond
      CHARACTER(*), INTENT(IN) :: msg
      LOGICAL, INTENT(INOUT) :: ok

      IF (.NOT. cond) THEN
         ok = .FALSE.
         PRINT *, "FAIL: ", TRIM(msg)
      ELSE
         PRINT *, "PASS: ", TRIM(msg)
      END IF
   END SUBROUTINE assert_true

   ! ---------------------------------------------------------------------------
   ! [0,1)
   ! ---------------------------------------------------------------------------
   SUBROUTINE test_u01_co(ok)
      LOGICAL, INTENT(INOUT) :: ok
      REAL(C_DOUBLE) :: x0, x1, x2, xmax, dx1, dx2

      ! m = 0, 1, 2, 2^53-1 are produced by choosing word = m << 11
      x0 = philox_u01_co(0_C64)
      x1 = philox_u01_co(SHIFTL(1_C64, 11))
      x2 = philox_u01_co(SHIFTL(2_C64, 11))
      xmax = philox_u01_co(NOT(0_C64))

      dx1 = x1 - x0
      dx2 = x2 - x1

      CALL assert_true(x0 == 0.0_C_DOUBLE, &
                       "u01_co minimum is exactly 0", ok)

      CALL assert_true(xmax == NEAREST(1.0_C_DOUBLE, -1.0_C_DOUBLE), &
                       "u01_co maximum is nearest(1,-1)", ok)

      CALL assert_true(dx1 == inv_two53, &
                       "u01_co spacing at bottom is 2^-53", ok)

      CALL assert_true(dx2 == inv_two53, &
                       "u01_co consecutive spacing is 2^-53", ok)

      CALL assert_true(x0 >= 0.0_C_DOUBLE .AND. xmax < 1.0_C_DOUBLE, &
                       "u01_co stays in [0,1)", ok)
   END SUBROUTINE test_u01_co

   ! ---------------------------------------------------------------------------
   ! (0,1]
   ! ---------------------------------------------------------------------------
   SUBROUTINE test_u01_oc(ok)
      LOGICAL, INTENT(INOUT) :: ok
      REAL(C_DOUBLE) :: xmin, x1, x2, xmax, dx1, dx2

      xmin = philox_u01_oc(0_C64)
      x1 = philox_u01_oc(SHIFTL(1_C64, 11))
      x2 = philox_u01_oc(SHIFTL(2_C64, 11))
      xmax = philox_u01_oc(NOT(0_C64))

      dx1 = x1 - xmin
      dx2 = x2 - x1

      CALL assert_true(xmin == inv_two53, &
                       "u01_oc minimum is 2^-53", ok)

      CALL assert_true(xmax == 1.0_C_DOUBLE, &
                       "u01_oc maximum is exactly 1", ok)

      CALL assert_true(dx1 == inv_two53, &
                       "u01_oc spacing at bottom is 2^-53", ok)

      CALL assert_true(dx2 == inv_two53, &
                       "u01_oc consecutive spacing is 2^-53", ok)

      CALL assert_true(xmin > 0.0_C_DOUBLE .AND. xmax <= 1.0_C_DOUBLE, &
                       "u01_oc stays in (0,1]", ok)
   END SUBROUTINE test_u01_oc

   ! ---------------------------------------------------------------------------
   ! (0,1)
   ! ----------------------------------------------------------------

   SUBROUTINE test_u01_oo(ok)
      LOGICAL, INTENT(INOUT) :: ok
      REAL(C_DOUBLE) :: x0, x1, x2, xmax
      x0 = philox_u01_oo(0_C64)
      x1 = philox_u01_oo(SHIFTL(1_C64, 11))
      x2 = philox_u01_oo(SHIFTL(2_C64, 11))
      xmax = philox_u01_oo(NOT(0_C64))
      CALL assert_true(x0 > 0.0_C_DOUBLE, &
                       "u01_oo minimum is strictly positive", ok)
      CALL assert_true(xmax < 1.0_C_DOUBLE, &
                       "u01_oo maximum is strictly less than 1", ok)
      CALL assert_true(x1 > x0, &
                       "u01_oo is increasing between first two values", ok)
      CALL assert_true(x2 > x1, &
                       "u01_oo is increasing between next two values", ok)
      CALL assert_true(x0 > 0.0_C_DOUBLE .AND. xmax < 1.0_C_DOUBLE, &
                       "u01_oo stays in (0,1)", ok)
   END SUBROUTINE test_u01_oo
   ! ----------------------------------------------------------------
   ! [0,1]
   ! ----------------------------------------------------------------
   SUBROUTINE test_u01_cc(ok)
      LOGICAL, INTENT(INOUT) :: ok
      REAL(C_DOUBLE) :: x0, x1, x2, xmax, dx1, dx2

      x0 = philox_u01_cc(0_C64)
      x1 = philox_u01_cc(SHIFTL(1_C64, 11))
      x2 = philox_u01_cc(SHIFTL(2_C64, 11))
      xmax = philox_u01_cc(NOT(0_C64))

      dx1 = x1 - x0
      dx2 = x2 - x1

      CALL assert_true(x0 == 0.0_C_DOUBLE, &
                       "u01_cc minimum is exactly 0", ok)

      CALL assert_true(xmax == 1.0_C_DOUBLE, &
                       "u01_cc maximum is exactly 1", ok)

      CALL assert_true(dx1 == inv_two53m1, &
                       "u01_cc spacing at bottom is 1/(2^53-1)", ok)

      CALL assert_true(dx2 == inv_two53m1, &
                       "u01_cc consecutive spacing is 1/(2^53-1)", ok)

      CALL assert_true(x0 >= 0.0_C_DOUBLE .AND. xmax <= 1.0_C_DOUBLE, &
                       "u01_cc stays in [0,1]", ok)
   END SUBROUTINE test_u01_cc

END PROGRAM test_philox_uniform_spacing
