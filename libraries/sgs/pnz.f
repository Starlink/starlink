      SUBROUTINE sgs_1PNZ (X0,Y0, XV,YV, XVN,YVN, POS, IZONID, JSTAT)
*+
*   - - - -
*    P N Z     (Internal routine)
*   - - - -
*
*   Create a new zone of given size and position.
*
*   Given:
*        X0         r      current zone viewport corner (x)
*        Y0         r         "     "       "      "    (y)
*        XV         r      current zone viewport size (x)
*        YV         r         "     "       "     "   (y)
*        XVN        r      new zone viewport size (x)
*        YVN        r       "    "     "       "  (y)
*        POS        c*2    position code
*        JSTAT      i      inherited status (if option selected)
*
*   Returned:
*        IZONID     i      zone identifier for new zone
*        JSTAT      i      status (0=OK)
*
*   Read from COMMON:
*        ISZID      i      current zone ID
*        IZTW       i()    zone table - SGS workstation ID
*        ZVT        r()    zone table - viewport
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
*   The new zone has a window whose world coordinate extent is (0,0) to
*   (X,Y), where X/Y is the true aspect ratio of the viewport and the
*   smaller of X or Y is unity.
*
*   Externals:
*      sgs_1HSTAT, sgs_1GETZ, sgs_1ERR, sgs_1UPCAS, sgs_1NEWZ
*
*   Errors:
*      Too many zones
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL X0,Y0,XV,YV,XVN,YVN
      CHARACTER*(*) POS
      INTEGER IZONID,JSTAT

      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'


      REAL DX,DY,X,Y,X1,Y1
      CHARACTER LPOS*2

      CHARACTER RNAME*3
      PARAMETER (RNAME='PNZ')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Allocate a zone table entry
      CALL sgs_1GETZ(ABS(IZTW(ISZID)),IZONID)
      IF (IZONID.EQ.0) THEN
         CALL sgs_1ERR(SGS__ZNZEX,RNAME,'Too many zones',JSTAT)
         GO TO 9999
      END IF

*  Convert alignment string to uppercase
      CALL sgs_1UPCAS(POS(:2),LPOS)

*  Viewport alignment: horizontal
      DX=XV-XVN
      IF (LPOS(2:2).EQ.'R') THEN
         X=X0+DX
      ELSE IF (LPOS(2:2).EQ.'C') THEN
         X=X0+DX/2.0
      ELSE
         X=X0
      END IF

*  Viewport alignment: vertical
      DY=YV-YVN
      IF (LPOS(:1).EQ.'T') THEN
         Y=Y0+DY
      ELSE IF (LPOS(:1).EQ.'C') THEN
         Y=Y0+DY/2.0
      ELSE
         Y=Y0
      END IF

*  Check that new bounds lie within the current zone
      X = MAX(X,ZTV(1,ISZID))
      Y = MAX(Y,ZTV(3,ISZID))
      X1 = MIN(X+XVN,ZTV(2,ISZID))
      Y1 = MIN(Y+YVN,ZTV(4,ISZID))
      
*  Set up zone with appropriate viewport
      CALL sgs_1NEWZ(IZONID,X,X1,Y,Y1)

*  Exit
 9999 CONTINUE

      END
