      SUBROUTINE TASK_ENC0C ( CVAL, STRING, STATUS )
*+
*  Name:
*     TASK_ENC0C

*  Purpose:
*     Encode a value as a character string

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_ENC0C ( CVAL, STRING, STATUS )

*  Description:
*     Convert the given value of type CHARACTER*(*) into a character
*     string and return it in STRING.
*     A routine exists for each type C, D, L, I, R.

*  Arguments:
*     CVAL=CHARACTER*(*) (given)
*           the value to be encoded
*     STRING=CHARACTER*(*) (returned)
*           the returned character string
*     STATUS=INTEGER

*  Algorithm:
*     Use appropriate CHR routine

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-NOV-1987 (REVAD::BDK):
*        Original
*     29-APR-1989 (AAOEPP::WFL):
*        Make it generic
*     04-OCT-1992 (RLVAD::AJC):
*        Use CHR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
 
*  Arguments Given:
      CHARACTER*(*) CVAL         ! the value to be encoded
 
*  Arguments Returned:
      CHARACTER*(*) STRING  ! the returned character string
 
*  Status:
      INTEGER STATUS
 
*  Local Variables:
      INTEGER NCHAR         ! length of encoded string
*.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      CALL CHR_CTOC( CVAL, STRING, NCHAR )
 
      END
