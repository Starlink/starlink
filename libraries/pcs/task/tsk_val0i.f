      SUBROUTINE TASK_VAL0I ( IVAL, STRING, STATUS )
*+
*  Name:
*     TASK_VAL0I

*  Purpose:
*     Encode a value as a character string

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_VAL0I ( IVAL, STRING, STATUS )

*  Description:
*     Convert the given value of type INTEGER into a character
*     string and return it in STRING.
*     A routine exists for each type C, D, L, I, R.

*  Arguments:
*     IVAL=INTEGER (given)
*           the value to be encoded
*     STRING=CHARACTER*(*) (returned)
*           the returned character string
*     STATUS=INTEGER

*  Algorithm:
*     Call TASK_ENC0I

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-NOV-1987 (REVAD::BDK):
*        Original
*     29-APR-1989 (AAOEPP::WFL):
*        Make it generic (same as TASK_ENC0I)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
 
*  Arguments Given:
      INTEGER IVAL         ! the value to be encoded
 
*  Arguments Returned:
      CHARACTER*(*) STRING  ! the returned character string
 
*  Status:
      INTEGER STATUS
*.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      CALL TASK_ENC0I ( IVAL, STRING, STATUS )
 
      END
 
!*+  TASK_VAL0 - encode a value as a character string
!      SUBROUTINE TASK_VAL0
!*    Description :
!*     Dummy routine to allow MMS to maintain the object library properly
!
!      END
