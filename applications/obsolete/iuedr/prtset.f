      SUBROUTINE PRTSET( USR, RED )
*+
*  Name:
*     SUBROUTINE PRTSET

*  Purpose:
*     Initialise global CMPRT variables.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRTSET( USR, RED )

*  Arguments:
*     USR = LOGICAL (Given)
*        Is there an interactive user?
*     RED = LOGICAL (Given)
*        Is output redirected to a file?

*  Description:
*     Set global vars: whether there is an interactive user (USR),
*     and whether output is currently redirected to a file (RED).
*     Sets PARRED, parameter value logging on by default.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-81 (JRG):
*       AT4 version.
*     31-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     13-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     11-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     15-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMPRT'

*  Arguments Given:
      LOGICAL USR     ! Whether user.
      LOGICAL RED     ! Whether output is redirected.
*.

      PRTUSR = USR
      PRTRED = RED
      PARRED = .TRUE.

      END
