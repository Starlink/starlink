      SUBROUTINE MODUEQ
*+
*  Name:
*     SUBROUTINE MODUEQ

*  Purpose:
*     If there is a current dataset, mark its data quality part as requiring
*     update.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MODUEQ

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     22-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     26-APR-95 (MJC):
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
         DQCHAN = .TRUE.
      END IF

      END
