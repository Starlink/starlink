      SUBROUTINE PRTCHR( CHR, STATUS )
*+
*  Name:
*     SUBROUTINE PRTCHR

*  Purpose:
*     Output a line consisting of a Fortran 77 CHARACTER string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRTCHR( CHR, STATUS )

*  Arguments:
*     CHR = CHARACTER*( * ) (Given)
*        The string to be written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Convert to SWT string and use PRTLIN. Trailing blanks are lost.

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
*     15-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Local Constants:
      INTEGER MAXLINE       ! Maximum length of character string.
      PARAMETER ( MAXLINE = 400 )

*  Arguments Given:
      CHARACTER*( * )CHR    ! CHARACTER string.

*  Status:
      INTEGER STATUS        ! Global status.

*  Local Variables:
      BYTE STR( MAXLINE )   ! String to be written.

      INTEGER NCHAR         ! Character count.
*.

      CALL GEN_CTOS( CHR, MAXLINE, STR, NCHAR )
      CALL PRTLIN( STR, STATUS )

      END
