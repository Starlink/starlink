      SUBROUTINE ERRSET
*+
*  Name:
*     SUBROUTINE ERRSET

*  Purpose:
*     Set up ERRLIB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERRSET

*  Method:
*     Set stream for error texts.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-81 (JRG):
*       AT4 version.
*     20-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     23-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Conversion to SGP/16 style.
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

*  Global Constants:
      INTEGER MAXLINE   ! Maximum length of text string.
      PARAMETER ( MAXLINE = 400 )

*  Global variables:
      INCLUDE 'CMERR'
*.

      EPOS = 0
      CALL STR_WCONT( '%p!  \\', MAXLINE, EMESS, EPOS )

      END
