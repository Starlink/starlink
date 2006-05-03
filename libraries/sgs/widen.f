      SUBROUTINE sgs_WIDEN (CIDENT,WS,CONID,JSTAT)
*+
*  Name:
*     WIDEN

*  Purpose:
*     Convert a character string workstation description to an integer
*     workstation sequence number and connection identifier by calling
*     the GNS library.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     CIDENT = CHAR (Given)
*         The character workstation identifier
*     WS = INTEGER (Returned)
*         Workstation sequence number (if JSTAT=0)
*     CONID = INTEGER (Returned)
*         Connection ID (if JSTAT=0)
*     JSTAT = INTEGER (Returned)
*         Status  = 0 if success

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     gns_TNG

*-

      IMPLICIT NONE

      CHARACTER CIDENT*(*)
      INTEGER WS,CONID,JSTAT

      CHARACTER*5 RNAME
      PARAMETER(RNAME='WIDEN')



      JSTAT = 0
      
      CALL gns_TNG( CIDENT, WS, CONID, JSTAT)

      END
