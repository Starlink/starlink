      SUBROUTINE sgs_ZSHAP (AR, POS, IZONID, JSTAT)
*+
*   - - - - - -
*    Z S H A P
*   - - - - - -
*
*   Create and select a zone of the specified shape.
*
*   The new zone has a window whose world coordinate extent is (0,0) to
*   (X,Y), where X/Y is the true aspect ratio of the viewport and the
*   smaller of X or Y is unity.
*
*   Given:
*      AR         r      aspect ratio: viewport X/Y
*      POS        c      position code (see below)
*      JSTAT      i      inherited status (if option selected)
*
*   Returned:
*      IZONID     i      zone identifier for new zone
*      JSTAT      i      status (0=OK)
*
*   Position code;  the first two characters determine the position of
*   the new zone within the current zone as follows:
*
*        1st character = B (bottom), C (centre) or T (top).
*        2nd character = L (left),   C (centre) or R (right).
*
*   The new zone can be positioned in one corner of the current zone
*   by specifiying 'BL', 'BR', 'TL' or 'TR'.  The new zone can be
*   positioned centrally against one edge via 'BC', 'CR', 'TC' or 'CL'.
*   'CC' causes the new zone to be concentric with the current one.
*
*   Read from COMMON:
*      ISZID      i      current zone ID
*      ZTV        i()    zone table - viewport
*
*   Externals:
*      sgs_1HSTAT, sgs_1NORM, sgs_1PNZ
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INCLUDE 'sgscom'


      REAL AR
      CHARACTER POS*(*)
      INTEGER IZONID,JSTAT

      REAL ASPR,X1V,Y1V,X2V,Y2V,XV,YV,P,Q,XVN,YVN



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Normalise aspect ratio
      ASPR=MAX(ABS(AR),1E-6)

*  Current viewport
      X1V=ZTV(1,ISZID)
      X2V=ZTV(2,ISZID)
      Y1V=ZTV(3,ISZID)
      Y2V=ZTV(4,ISZID)
      XV=X2V-X1V
      YV=Y2V-Y1V

*  NDC size of new viewport
      CALL sgs_1NORM(XV,ASPR*YV,P,Q)
      XVN=XV*Q
      YVN=YV*P

*  Set up new zone
      CALL sgs_1PNZ(X1V,Y1V,XV,YV,XVN,YVN,POS,IZONID,JSTAT)

*  Exit
 9999 CONTINUE

      END
