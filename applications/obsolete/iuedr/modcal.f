      SUBROUTINE MODCAL
*+
*  Name:
*     SUBROUTINE MODCAL

*  Purpose:
*     If there is a current dataset, mark its calibration part as requiring
*     update.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MODCAL

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     22-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     03-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMFILE'
*.

      IF ( .NOT. NODSN ) THEN
         CACHAN = .TRUE.
      END IF

      END
