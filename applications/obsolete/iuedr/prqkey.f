      SUBROUTINE PRQKEY( STATUS )
*+
*  Name:
*     SUBROUTINE PRQKEY

*  Description:
*     The IUE Spectrum Data Quality Key is printed on the specified
*     file. It does not contain leading or trailing blank lines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRQKEY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     06-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     09-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Status:
      INTEGER STATUS     ! Global status.
*.

      CALL LINE_WCONT('%p Data Quality Key:\\')
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI('%p%2w %2i\\', 1)
      CALL LINE_WCONT(' affected by extrapolated ITF\\')
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI('%p%2w %2i\\', 2)
      CALL LINE_WCONT(' affected by microphonics\\')
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI('%p%2w %2i\\', 3)
      CALL LINE_WCONT(' affected by spike\\')
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI('%p%2w %2i\\', 4)
      CALL LINE_WCONT(' affected by bright point (or user)\\')
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI('%p%2w %2i\\', 5)
      CALL LINE_WCONT(' affected by reseau mark\\')
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI('%p%2w %2i\\', 6)
      CALL LINE_WCONT(' affected by ITF truncation\\')
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI('%p%2w %2i\\', 7)
      CALL LINE_WCONT(' affected by saturation\\')
      CALL PRTBUF( STATUS )
      CALL LINE_WRITS('%p%2w %2s\\', 'U\\')
      CALL LINE_WCONT(' affected by user edit\\')
      CALL PRTBUF( STATUS )
      CALL LINE_WRITS('%p%2w %2s\\', '*\\')
      CALL LINE_WCONT(' value undefined\\')
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT('%p end.\\')
      CALL PRTBUF( STATUS )

      END
