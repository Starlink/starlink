*+  DAT_ERDSN - Report error on structure component
      SUBROUTINE DAT_ERDSN(LOC,CMP,STATUS)
*    Description :
*     This routine reports an error of the form:
*
*        <structure name>.<component name> : status
*
*     where <structure name> is the name of the object located by LOC.
*           <component name> is CMP           
*           status is the message text associated with the status value.
*    Invocation :
*     CALL DAT_ERDSN(LOC, CMP, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     CMP=CHARACTER*(*)
*           Expression specifying the component name of a primitive object
*           contained in the structure
*     STATUS=INTEGER
*           Variable holding the status value.
*    Authors :
*     A J Chipperfield (RAL::MDL)
*    History :
*     26-Mar-1987:  original (RAL::AJC)
*     05-Jun-1987:  Use EXC_$MSG to get text (RAL::AJC)
*     25-NUN-1991:  Changed to use EMS (RFWS)
*    Global constants :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'EMS_PAR'
*    Import
      CHARACTER*(*) LOC			! structure locator
      CHARACTER*(*) CMP			! Component name
      INTEGER STATUS			! Status value

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
         NAME = '<Unknown structure>'
         CALL EMS_ANNUL( LSTAT )
      END IF

*  Obtain a textual translation of the error code.
      CALL DAT_ERMSG( STATUS, LMSG, MSG )

*  Define tokens for the object name, the component name and the error
*  message.
      CALL EMS_SETC( 'NAME', NAME )
      CALL EMS_SETC( 'CMP', CMP )
      CALL EMS_SETC( 'MSG', MSG( : LMSG ) )

*  Report the message and release the error stack.
      CALL EMS_REP( 'HDS_ERROR', '^NAME.^CMP: ^MSG', STATUS )
      CALL EMS_RLSE

      END
