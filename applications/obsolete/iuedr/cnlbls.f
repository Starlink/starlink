      SUBROUTINE CNLBLS
*+
*  Name:
*     SUBROUTINE CNLBLS

*  Purpose:
*     Cancel the current LBLS contents.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CNLBLS

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
*     20-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMLBLS'
*.

      NOLBLS = .TRUE.

      END
