      SUBROUTINE sgs_WIDEN (CIDENT,WS,CONID,JSTAT)
*+
*   - - - - - -
*    W I D E N
*   - - - - - -
*
*   Convert a character string workstation description to an integer
*   workstation sequence number and connection identifier by calling
*   the GNS library.
*
*   Given:
*      CIDENT     c      the character workstation identifier
*
*   Returned:
*      WS         i      workstation sequence number (if JSTAT=0)
*      CONID      i      connection ID (if JSTAT=0)
*      JSTAT      i      status  = 0 if success
*
*   Externals:
*      gns_TNG
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      CHARACTER CIDENT*(*)
      INTEGER WS,CONID,JSTAT

      CHARACTER*5 RNAME
      PARAMETER(RNAME='WIDEN')



      JSTAT = 0
      
      CALL gns_TNG( CIDENT, WS, CONID, JSTAT)

      END
