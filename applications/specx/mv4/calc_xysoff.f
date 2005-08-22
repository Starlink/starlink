*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------
      SUBROUTINE CALC_XYSOFF (RAOFF,    DECOFF,   POS_ANGLE,
     &                        X_OFFSET, Y_OFFSET, IFAIL)

*  Routine to evaluate given map offset in cells, given map cell sizes
*  and rotation angle (grid assumed rectilinear).

*     Formal parameters:

      IMPLICIT  NONE
      REAL      RAOFF,      DECOFF
      REAL      POS_ANGLE
      REAL      X_OFFSET,   Y_OFFSET
      INTEGER   IFAIL

*     Local variables:

      REAL      ANGLE

*  Ok, go...

      IFAIL = 0
CD    PRINT *, ' -- calc_xysoff --'

      ANGLE = 1.74533D-2 * POS_ANGLE
      X_OFFSET = (RAOFF*COS(ANGLE) - DECOFF*SIN(ANGLE))
      Y_OFFSET = (RAOFF*SIN(ANGLE) + DECOFF*COS(ANGLE))

CD    PRINT *, '    Total X, Y offsets (arcsecs): ',X_OFFSET,Y_OFFSET

      RETURN
      END
