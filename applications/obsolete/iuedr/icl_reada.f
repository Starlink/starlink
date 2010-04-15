      SUBROUTINE ICL_READA( PR1, LEN1, PR2, LEN2, VAL, VALLEN,
     : DEFAULT, DEFLEN )
*+
*  Name:
*     ICL_READA

*  Purpose:
*     To prompt and recieve a reply.
*

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ICL_READA( PR1, LEN1, PR2, LEN2, VAL, VALLEN,
*     DEFAULT, DEFLEN )

*  Description:
*     {routine_description}

*  Arguments:
*     PR1 = CHARACTER*(*) (Given)
*        The initial prompt
*     LEN1 = INTEGER (Given)
*        The used length of PR1
*     PR2 = CHARACTER*(*) (Given)
*        The alternative prompt
*     LEN2 = INTEGER (Given)
*        The used length of PR1
*     VAL = CHARACTER*(*) (Returned)
*        The value given in response to the prompt
*     VALLEN = INTEGER (Returned)
*        The used length of VAL
*     DEFAULT = CHARACTER*(*) (Given)
*        The 'default' value
*     DEFLEN = INTEGER (Given)
*        The used length of DEFAULT

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUL-1991 (AJC):
*        Original version.
*     17-JUN-1992 (AJC):
*        Remove <> from formats
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER*(*) PR1
      INTEGER LEN1
      CHARACTER*(*) PR2
      INTEGER LEN2
      INTEGER VALLEN
      CHARACTER*(*) DEFAULT
      INTEGER DEFLEN

*  Arguments Returned:
      CHARACTER*(*) VAL
*.

      WRITE( *, 10 ) PR1(1:LEN1)
10    FORMAT ( A, $ )
      READ ( *, 20 ) VAL
20    FORMAT ( A )

      END
