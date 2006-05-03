      SUBROUTINE sgs_CLOSE
*+
*  Name:
*     CLOSE

*  Purpose:
*     Flush the buffers, close each open workstation (deactivating if
*     active), and close GKS.

*  Language:
*     Starlink Fortran 77

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     sgs_FLUSH, sgs_1GKSTM, gns_STOP

*-

      IMPLICIT NONE

      INTEGER JSTAT



      JSTAT = 0

*  Flush the buffers
      CALL sgs_FLUSH

* Terminate GKS
      CALL sgs_1GKSTM(JSTAT)

* Shut down the GNS database
      CALL gns_STOP('GKS',JSTAT)

      END
