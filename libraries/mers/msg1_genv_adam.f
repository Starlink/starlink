      LOGICAL FUNCTION MSG1_GENV( PARAM, MSGSTR, MSGLEN )
*+
*  Name:
*    MSG1_GENV

*  Purpose:
*     Get the message text for a specified message parameter from the 
*     interface module. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = MSG1_GENV( PARAM, MSGSTR, MSGLEN )

*  Description:
*     This routine makes an enquiry of the parameter system to get the 
*     message text from the interface module for the specified message 
*     parameter.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The message parameter name.
*     MSGSTR = CHARACTER * ( * ) (Returned)
*        The message text.
*     MSGLEN = INTEGER (Returned)
*        The length of the message text.

*  Implementation Notes:
*     -  This function is only for use in the ADAM version of MSG_.
*     -  This function makes calls to SUBPAR_FINDPAR and SUBPAR_GET0C.

*  Algorithm:
*     -  If the parameter name is not null, then get the message text from
*     parameter system.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright  (UCL)
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1982 (JRG):
*        Original version.
*     17-AUG-1983 (SLW):
*        New parameter system interface.
*     13-NOV-1984 (BDK):
*        ADAM parameter system.
*     16-DEC-1986 (BDK):
*        Call SUBPAR not PAR.
*     18-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-MAR-1990 (PCTR):
*        Changed function name.
*     22-OCT-1991 (PCTR):
*        Added EMS_MARK and EMS_RLSE to annul any error messages
*        from SUBPAR on error.
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
      CHARACTER * ( * ) MSGSTR

      INTEGER MSGLEN

*  External References:
      INTEGER CHR_LEN                   ! String length

*  Local Variables:
      INTEGER PD                        ! Pointer to parameter
      INTEGER STATUS                    ! Local status

*.

*  Initialise returned value of MSG1_GENV. 
      MSG1_GENV = .FALSE.

*  Initialise the returned string.
      MSGSTR = ' '
      MSGLEN = 1

*  Initialise STATUS.
      STATUS = SAI__OK

*  Mark a new error reporting context.
      CALL EMS_MARK

*  Check that PARAM actually contains a parameter name.
      IF ( CHR_LEN( PARAM ) .NE. 0 ) THEN

*     PARAM does contain a parameter name, so attempt to get the
*     message text from the parameter system.
         CALL SUBPAR_FINDPAR( PARAM, PD, STATUS )
         CALL SUBPAR_GET0C( PD, MSGSTR, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN
*        Set returned arguments for normal successful completion.
            MSG1_GENV = .TRUE.
            MSGLEN = CHR_LEN( MSGSTR )

         ELSE
*        Annul the error context and abort.
            CALL EMS_ANNUL( STATUS )
         END IF
      END IF

*  Release the error reporting context.
      CALL EMS_RLSE

      END
