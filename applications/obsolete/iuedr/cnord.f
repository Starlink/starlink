      SUBROUTINE CNORD
*+
*  Name:
*     SUBROUTINE CNORD

*  Purpose:
*     This routine cancels the current Order contents.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CNORD

*  Method:
*     Propagate cancellation to all lower levels via structured calls.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     21-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     22-OCT-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMEXTP'
      INCLUDE 'CMSPEC'
      INCLUDE 'CMWAV'
      INCLUDE 'CMNET'
      INCLUDE 'CMBKG'
      INCLUDE 'CMDEV'
      INCLUDE 'CMCEN'
      INCLUDE 'CMFLX'
      INCLUDE 'CMCAL'
*.

      ORDER = 0
      NOEXTP = .TRUE.
      NWAV = 0
      NONET = .TRUE.
      NOBKG = .TRUE.
      NODEV = .TRUE.
      NOCEN = .TRUE.
      NOFLX = .TRUE.
      NOCAL = .TRUE.

      END
