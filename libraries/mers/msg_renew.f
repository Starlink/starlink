      SUBROUTINE MSG_RENEW
*+
*  Name:
*     MSG_RENEW

*  Purpose:
*     Renew any annulled message tokens in the current context.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_RENEW

*  Description:
*     Any message tokens which have been annulled by a call to MSG_OUT,
*     MSG_OUTIF, MSG_LOAD, ERR_REP, ERR_ANNUL or ERR_LOAD are renewed.
*     If any new token value has been defined (using the MSG_SETx and 
*     MSG_FMTx routines) since the previous tokens were annulled, no 
*     action is taken. The intended use of MSG_RENEW is to renew all message
*     tokens immediately after a call MSG_OUT, MSG_OUTIF, MSG_LOAD,
*     ERR_REP, ERR_ANNUL or ERR_LOAD for re-use in a subsequent message.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-JUN-1991 (PCTR):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*.

*  Call EMS_RENEW.
      CALL EMS_RENEW

      END
