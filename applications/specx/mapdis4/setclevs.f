*-----------------------------------------------------------------------

      SUBROUTINE SETCLEVS (ZMIN, ZMAX, ZC, NZ, DZ, NCONT1, NCONT2)

      IMPLICIT  NONE

*     Formal parameters:

      REAL    ZMIN
      REAL    ZMAX
      REAL    ZC(1)
      INTEGER NZ
      REAL    DZ
      INTEGER NCONT1
      INTEGER NCONT2

*     Include files

      INCLUDE 'FLAGCOMM'

*     Local variables:

      INTEGER NC
      REAL    ARRMIN
      REAL    ARRMAX

*  Ok, go...

      ARRMIN = ZMIN
      ARRMAX = ZMAX

      IF (ICAUTO) THEN
        CALL AUTORANGE (ZMIN, ZMAX, NZ)
        NZ = (((NCONT-1)/NZ + 1) * NZ)    ! Tricky line to get more intervals
        DZ = (ZMAX-ZMIN)/FLOAT(NZ)
        DO NC = 1,NZ
          ZC(NC) = ZMIN + NC*DZ
        END DO
      ELSE IF (CLEVELS_SET) THEN
        NZ = NCSET
        DO NC = 1,NCSET
          ZC(NC) = CLEVELS(NC)
        END DO
      ELSE
        NZ = NCONT
        DZ = CONTI
        DO NC = 1,NCONT
          ZC(NC) = CONT0 + (NC-1)*CONTI
        END DO
      END IF

C   Check NZ not negative

      IF (NZ.LE.0) NZ = 1

C   Economize contour levels - assume ordered by size, throw out ones not used

      NCONT1 = 1
      NCONT2 = NZ

*     NCONT2 = N1
*
*     DO NC = 1, NZ
*       IF (ZC(NC) .LT. ARRMIN) NCONT1 = NC+1
*       IF (ZC(NC) .LT. ARRMAX) NCONT2 = NC
*     END DO
*
*     NZ = NCONT2 - NCONT1 + 1
*
*     IF (NZ.LT.1) THEN
*       ZC(1) = 0.5 * (ARRMIN + ARRMAX)
*       NCONT1 = 1
*       NCONT2 = 1
*       NZ     = 1
*       DZ     = 0.0
*     END IF

      RETURN
      END

*-----------------------------------------------------------------------

