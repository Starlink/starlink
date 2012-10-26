*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE MAP_INTEG (MAP, IX, IY, P, Q, SIGMA, GOODBOX)

*  Routine to evaluate integrated intensity on a map within the
*  box defined by the arrays P,Q

      IMPLICIT  NONE

*  Formal parameters:

      INTEGER*4 IX, IY
      REAL*4    MAP(IX,IY)
      REAL*4    P(2), Q(2)
      REAL*4    SIGMA
      LOGICAL*4 GOODBOX

*  Local variables

      INTEGER*4 IX1, IX2, IY1, IY2
      REAL*4    MAP_AREA
      REAL*4    DZ

      SIGMA   = 0.0
      GOODBOX = .TRUE.

      CALL SUBMAP (MAP, IX, IY, P, Q, IX1, IX2, IY1, IY2, DZ)
      CALL CHKMAP (MAP, IX, IY, IX1, IX2, IY1, IY2, GOODBOX)
      IF (GOODBOX) THEN
        CALL SUMMAP (MAP, IX, IY, IX1, IX2, IY1, IY2, SIGMA)
      END IF

*  Evaluate area passed to these routines

      MAP_AREA = ABS ((P(2)-P(1))*(Q(2)-Q(1)))

*  Work out integrated intensity (right now SIGMA is mean over points)

      SIGMA    = SIGMA * MAP_AREA * DZ

*  Convert to Jy if possible?

CD    call sxgtidle
CD    print *,'-- MAP_INTEG --'
CD    print *,'Integrated intensity:',SIGMA
CD    call sxgttgraph

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SUBMAP (MAP, IX, IY, P, Q, IX1, IX2, IY1, IY2, DZ)

*  Routine to work out which part of the map MAP(IX,IY) is enclosed
*  in the box whose boundaries are given by P (x) and Q (y).
*  Limits of useful area returned in IX1..IY2

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER*4 IX, IY
      REAL*4    MAP(IX,IY)
      REAL*4    P(2), Q(2)
      INTEGER*4 IX1, IX2, IY1, IY2
      REAL*4    DZ

*     Include files and common blocks

      INCLUDE   'FLAGCOMM'
      INCLUDE   'PLOT2D'
      INCLUDE   'SPECX_PARS'

      LOGICAL*4 INVERT_AXIS
      COMMON /GOOD_PT/ INVERT_AXIS(3)

*     Local variables

      REAL*4    SCALE(LSPMAX)
      INTEGER*4 I,J
      REAL*4    DX,   DY
      REAL*4    XN1,  XN2,  YN1,  YN2
      REAL*4    FX1,  FX2,  FY1,  FY2
      REAL*4    XMIN, XMAX, YMIN, YMAX
      INTEGER*4 IAXX, IAXY
      INTEGER*4 IERR

*  Ok, go...

      IAXX = LINK(1)
      IAXY = LINK(2)

*     Window to within true map boundaries

      XMIN = MIN (PBEG(IAXX),PEND(IAXX))
      XMAX = MAX (PBEG(IAXX),PEND(IAXX))
      YMIN = MIN (PBEG(IAXY),PEND(IAXY))
      YMAX = MAX (PBEG(IAXY),PEND(IAXY))

      P(1) = MAX (MIN (P(1),XMAX),XMIN)
      P(2) = MIN (MAX (P(2),XMIN),XMAX)
      Q(1) = MAX (MIN (Q(1),YMAX),YMIN)
      Q(2) = MIN (MAX (Q(2),YMIN),YMAX)

*  X-axis:

      DX = PFAC(IAXX)/(LXPIX-1)
      IF (INVERT_AXIS(IAXX)) DX = -DX

      DO I = 1, NAXX
        SCALE(I) = PBEG(IAXX) + FLOAT(I-1)*DX
      END DO

      CALL PLIMITS (SCALE, NAXX, DX, P(1), P(2),
     &              IX1, IX2, FX1, FX2, XN1, XN2, IERR)

CD    call sxgtidle
CD    print *,'-- SUBMAP --'
CD    print *,'Error return from x-axis sums: ',ierr

*  Y-axis: Note that since the map was inverted for MONGO we need to
*          measure the scale backwards!

      DY = PFAC(IAXY)/(LYPIX-1)
      IF (INVERT_AXIS(IAXY)) DY = -DY

      DO J = 1, NAXY
        SCALE(J) = PEND(IAXY) - FLOAT(J-1)*DY
      END DO

      CALL PLIMITS (SCALE, NAXY, -DY, Q(1), Q(2),
     &              IY1, IY2, FY1, FY2, YN1, YN2, IERR)

CD    print *,'Error return from y-axis sums: ',ierr

CD    print *,'x-cell size:   ',dx
CD    print *,'input x-values:',P
CD    print *,'x-cell limits:',ix1,ix2
CD    print *,'x-cell values:',xn1,xn2

CD    print *,'y-cell size:   ',dy
CD    print *,'input y-values:',Q
CD    print *,'y-cell limits:',iy1,iy2
CD    print *,'y-cell values:',yn1,yn2

CD    call ttgraph

*  This routine is an appropriate place to work out the appropriate
*  multiplier for the z-axis

      IF (ISUM) THEN
        DZ = 1.0                                 ! Already in K-km/s or whatever
      ELSE
        DZ = ABS (PEND(LINK(3))-PBEG(LINK(3)))   ! Average TA*, multiply by width
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE CHKMAP (MAP, IX, IY, IX1, IX2, IY1, IY2, GOODBOX)

*  Routine to check that points within subarea of map IX1..IX2 and IY1..IY2
*  are all derived from measured data.

      IMPLICIT  NONE

*  Formal parameters:
      INTEGER*4 IX, IY
      REAL*4    MAP(IX,IY)
      INTEGER*4 IX1, IX2, IY1, IY2
      LOGICAL*4 GOODBOX

*  Include files
      INCLUDE   'PLOT2D'
      INCLUDE   'CNF_PAR'

*  Local variables
      INTEGER*4 I,J
      INTEGER*4 I1, I2, J1, J2
      INTEGER*4 JG

*  Functions
      LOGICAL*4 GOODPT


*  First calculate pixel range of "true" (measured) cells enclosed in the box

      I1 = (IX1+LXPIX-3)/(LXPIX-1) + 1
      I2 = (IX2+LXPIX-2)/(LXPIX-1)
      J1 = (IY1+LYPIX-3)/(LYPIX-1) + 1
      J2 = (IY2+LXPIX-2)/(LYPIX-1)

CD    call sxgtidle
CD    print *,'-- CHKMAP --'
CD    print *,'Enclosed data points x-limits:',i1,i2
CD    print *,'Enclosed data points y-limits:',j1,j2
CD    call sxgttgraph

*  If box is not bounded by actual known pixels, then expand check
*  range to include the adjacent measured points (but not outside of actual map
*  boundary).
*
*  This code also takes care of the case of a box within the actual map area but
*  not enclosing ANY measured points (seen by I1,J1 .GT. I2,J2).

      IF ( IX1 .LT. ((I1-1)*(LXPIX-1)+1) ) I1 = I1 - 1
      IF ( IX2 .GT. ((I2-1)*(LXPIX-1)+1) ) I2 = I2 + 1
      IF ( IY1 .LT. ((J1-1)*(LYPIX-1)+1) ) J1 = J1 - 1
      IF ( IY2 .GT. ((J2-1)*(LYPIX-1)+1) ) J2 = J2 + 1

CD    call sxgtidle
CD    print *,'Final data points x-limits:',i1,i2
CD    print *,'Final data points y-limits:',j1,j2
CD    call sxgttgraph

*  For all "true" points within the box, check that data were ACTUALLY
*  measured (the value of INDEX must be .gt. -1; that is, interpolated data
*  with INDEX=0 are OK).

      GOODBOX = .TRUE.
      DO J = J1,J2
        JG = (IY-1)/(LXPIX-1) + 2 - J
        DO I = I1, I2
          GOODBOX = GOODBOX .AND. GOODPT (%VAL(CNF_PVAL(INDEX_PTR)), 
     :                                    I, JG, -1)
        END DO
      END DO

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SUMMAP (MAP, IX, IY, IX1, IX2, IY1, IY2, SIGMA)

*  Routine to integrate area of map defined by IX1..IX2 and IY1..IY2,
*  returning result in sigma

      IMPLICIT  NONE

*  Formal parameters:
      INTEGER*4 IX, IY
      REAL*4    MAP(IX,IY)
      INTEGER*4 IX1, IX2, IY1, IY2
      REAL*4    SIGMA

*  Other variables
      INTEGER*4 I, J
      INTEGER*4 NDAT


      NDAT  = ABS ((IX2-IX1+1) * (IY2-IY1+1))
      SIGMA = 0.0

      DO J = IY1, IY2
        DO I = IX1, IX2
          SIGMA = SIGMA + MAP(I,J)
        END DO
      END DO

CD    call sxgtidle
CD    print *,'-- SUMMAP --'
CD    print *,'Sum of points:',sigma
CD    print *,'  # of points:',ndat
CD    call sxgttgraph

      SIGMA = SIGMA / FLOAT(NDAT)

      RETURN
      END

*-----------------------------------------------------------------------
