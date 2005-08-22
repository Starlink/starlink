*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE MAP_MAXMIN (MAP, IX, IY, P, Q, BADVAL,
     &                       AMAPMIN, AMAPMAX)

*  Routine to evaluate maximum and minimum values on a map within a
*  box defined by the arrays P,Q

      IMPLICIT  NONE

*  Formal parameters:

      INTEGER*4 IX, IY
      REAL*4    MAP(IX,IY)
      REAL*4    P(2), Q(2)
      REAL*4    BADVAL
      REAL*4    AMAPMIN, AMAPMAX

*  Local variables

      INTEGER*4 IX1, IX2, IY1, IY2
      REAL*4    DZ                  ! Not used in this routine.

      CALL SUBMAP (MAP, IX, IY, P, Q, IX1, IX2, IY1, IY2, DZ)
      CALL MAXMAP (MAP, IX, IY, IX1, IX2, IY1, IY2,
     &             BADVAL,  AMAPMIN, AMAPMAX)

*     call sxgtidle
*     print *,'-- MAP_MAXMIN --'
*     print *,'Minimum and maximum on map:', amapmin, amapmax
*     call sxgttgraph

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE MAXMAP (MAP, IX, IY, IX1, IX2, IY1, IY2,
     &                   BADVAL, MMIN, MMAX)

*  Routine to calculate maximum and minimum in a given area of the map
*  subject to all points being within "good" cells bounded by measured data.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   IX, IY
      REAL      MAP(IX,IY)
      INTEGER   IX1, IX2, IY1, IY2
      REAL      BADVAL
      REAL      MMIN, MMAX

*     Include files

      INCLUDE   'PLOT2D'

*     Local variables

      INTEGER*4 I,J
      REAL*4    DATA

*  Ok, go...

CD    PRINT *, ' -- maxmap --'
CD    PRINT *, '    Map size (x & y) = ', ix, iy
CD    PRINT *, '    Submap area: x   = ', ix1, ix2
CD    PRINT *, '    Submap area: y   = ', iy1, iy2
CD    PRINT *, '    Bad value        = ', badval

      MMIN = +1.e10
      MMAX = -1.e10

      DO I = IX1, IX2
        DO J = IY1, IY2
          DATA = MAP(I,J)
          IF (DATA.NE.BADVAL) THEN
            MMIN = MIN (MMIN, DATA)
            MMAX = MAX (MMAX, DATA)
          END IF
        END DO
      END DO

*     PRINT *, '    Max and min      = ', mmin, mmax

      RETURN
      END

*-----------------------------------------------------------------------

