*+  DAT_ERDSC - Report error on HDS object
      SUBROUTINE DAT_ERDSC(LOC,STATUS)
*    Description :
*     This routine reports an error of the form:
*
*        <object name> : status
*
*     where the object name is the name of the object associated with
*     the locator LOC and status is the message text associated with
*     the status value.
*    Invocation :
*     CALL DAT_ERDSC(LOC, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     STATUS=INTEGER
*           Variable holding the status value.
*    Authors :
*     A J Chipperfield (RAL::AJC)
*    History :
*     26-Mar-1987:  original (RAL::AJC)
*     05-Jun-1987:  use EXC_$MSG to obtain text (RAL::AJC)
*     25-JUN-1991:  Changed to use EMS (RFWS)
*    Global constants :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'EMS_PAR'
*    Import
      CHARACTER*(*) LOC		 ! Object locator
      INTEGER STATUS		 ! Status value

*    Local variables :
      CHARACTER*( EMS__SZMSG ) FILE ! Container file name
      CHARACTER*( EMS__SZMSG ) MSG ! Error message
      CHARACTER*( EMS__SZMSG ) NAME ! Object pathname
      INTEGER LMSG               ! Length of error message
      INTEGER LSTAT		 ! Local status value
      INTEGER NLEV               ! No. of levels in pathname

*-

*  Mark the EMS error stack to prevent interaction with any message
*  tokens already defined.
      CALL EMS_MARK

*  Initialise a local status variable and obtain the object name.
      LSTAT = SAI__OK
      CALL HDS_TRACE( LOC, NLEV, NAME, FILE, LSTAT )

*  If the name could not be obtained, then annul the error and
*  substitute an appropriate message.
      IF ( LSTAT .NE. SAI__OK ) THEN
         NAME = '<Unknown object>'
         CALL EMS_ANNUL( LSTAT )
      END IF

*  Obtain a textual translation of the error code.
      CALL DAT_ERMSG( STATUS, LMSG, MSG )

*  Define tokens for the object name and the error message.
      CALL EMS_SETC( 'NAME', NAME )
      CALL EMS_SETC( 'MSG', MSG( : LMSG ) )

*  Report the message and release the error stack.
      CALL EMS_REP( 'HDS_ERROR', '^NAME: ^MSG', STATUS )
      CALL EMS_RLSE

      END
