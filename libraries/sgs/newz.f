      SUBROUTINE sgs_1NEWZ (IZONID, X1,X2, Y1,Y2)
*+
*   - - - - -
*    N E W Z     (Internal routine)
*   - - - - -
*
*   Finish setting up a new zone.
*
*   The viewport and a default window are stored in the zone table entry
*   for the given zone ID, and the new zone is selected.
*
*   Given:
*      IZONID        i     zone identifier for new zone
*      X1,X2,Y1,Y2   r     zone viewport bounds (NDC)
*
*   Written to COMMON:
*      ZTV           r()   zone table - viewport
*      ZTW           r()   zone table - window
*
*   Externals:
*      sgs_SELZ, sgs_1NORM
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER IZONID
      REAL X1,X2,Y1,Y2

      INCLUDE 'sgscom'


      INTEGER J
      REAL XN,YN



*  Viewport
      ZTV(1,IZONID)=X1
      ZTV(2,IZONID)=X2
      ZTV(3,IZONID)=Y1
      ZTV(4,IZONID)=Y2

*  Default window
      CALL sgs_1NORM(X2-X1,Y2-Y1,XN,YN)
      ZTW(1,IZONID)=0.0
      ZTW(2,IZONID)=1.0/YN
      ZTW(3,IZONID)=0.0
      ZTW(4,IZONID)=1.0/XN

*  Select the zone
      J=0
      CALL sgs_SELZ(IZONID,J)

      END
