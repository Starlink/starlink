      LOGICAL FUNCTION MSG1_GKEY( PARAM, KEYSTR, KEYLEN )
*+
*  Name:
*     MSG1_GKEY

*  Purpose:
*     Get the keyword for the specified parameter.

*  Language:
*    Starlink Fortran 77

*  Invocation:
*     RESULT = MSG1_GKEY( PARAM, KEYSTR, KEYLEN)

*  Description:
*     This routine makes an enquiry of the parameter system to
*     get the keyword associated with the specified message parameter.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     KEYSTR = CHARACTER * ( * ) (Returned)
*        The keyword.
*     KEYLEN = INTEGER (Returned)
*        The length of the keyword.

*  Implementation Notes:
*     -  This function is only for use in the ADAM version of MSG_.
*     -  This function makes calls to SUBPAR_FINDPAR and SUBPAR_GETKEY.

*  Algorithm:
*     -  Ask the parameter system for the keyword.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1982 (JRG):
*        Original version.
*     17-AUG-1983 (SLW):
*        New parameter system interface.
*     13-NOV-1984 (BDK):
*        ADAM parameter system.
*     18-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-MAR-1990 (PCTR):
*        Changed function name.
*     22-OCT-1991 (PCTR):
*        Added EMS_MARK and EMS_RLSE to annul any error messages from 
*        SUBPAR on error.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      CHARACTER * ( * ) KEYSTR

      INTEGER KEYLEN

*  External References:
      INTEGER CHR_LEN                   ! Filled string length

*  Local Variables:
      INTEGER NAMCOD                    ! Code number of parameter
      INTEGER STATUS                    ! Local status

*.

*  Initialise the returned value of MSG1_GKEY. 
      MSG1_GKEY = .FALSE.

*  Initialise the returned string.
      KEYSTR = ' '
      KEYLEN = 1

*  Initialise status.
      STATUS = SAI__OK

*  Mark a new error reporting context.
      CALL EMS_MARK

*  Attempt to get the keyword associated with PARAM from the parameter 
*  system.
      CALL SUBPAR_FINDPAR( PARAM, NAMCOD, STATUS )
      CALL SUBPAR_GETKEY( NAMCOD, KEYSTR, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*     Set returned arguments for normal successful completion.
         MSG1_GKEY = .TRUE.
         KEYLEN = CHR_LEN( KEYSTR )
      ELSE

*     Annul the error context and abort.
         CALL EMS_ANNUL( STATUS )
      END IF

*  Release the error reporting context.
      CALL EMS_RLSE

      END
