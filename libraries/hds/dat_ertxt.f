*+  DAT_ERTXT - Report error including text
      SUBROUTINE DAT_ERTXT(TEXT, STATUS)
*    Description :
*     This routine reports an error of the form:
*
*        TEXT : status
*
*     Where TEXT is specified in the subroutine call and status
*     is the message text associated with the status value.
*    Invocation :
*     CALL DAT_ERTXT(TEXT, STATUS)
*    Parameters :
*     TEXT=CHARACTER*(*)
*           Text to be included in the message
*     STATUS=INTEGER
*           Variable holding the status value.
*    Authors :
*     A J Chipperfield (RAL::AJC)
*    History :
*     27-Mar-1987:  original (RAL::AJC)
*     05-Jun-1987:  Use EXC_$MSG to get error text (RAL::AJC)
*     25-jun-1991:  Changed to use EMS (RFWS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'EMS_PAR'

*    Import :
      CHARACTER*(*) TEXT		! Text to be included
      INTEGER STATUS			! Status value

*    Local variables :
      CHARACTER*( EMS__SZMSG ) MSG ! Status message text
      INTEGER LMSG               ! Length of error message

*-

*  Get a textual translation of the error code.
      CALL DAT_ERMSG( STATUS, LMSG, MSG )

*  Mark the EMS error stack to prevent interaction with any message
*  tokens already defined and define a token for the text and the error
*  message.
      CALL EMS_MARK
      CALL EMS_SETC( 'TEXT', TEXT )
      CALL EMS_SETC( 'MSG', MSG( : LMSG ) )

*  Report the message and release the error stack.
      CALL EMS_REP( 'HDS_ERROR', '^TEXT: ^MSG', STATUS )
      CALL EMS_RLSE

      END
