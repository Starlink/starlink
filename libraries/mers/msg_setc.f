      SUBROUTINE MSG_SETC( TOKEN, CVALUE )
*+
*  Name:
*     MSG_SETC

*  Purpose:
*     Assign a CHARACTER value to a message token (concise).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_SETC( TOKEN, CVALUE )

*  Description:
*     A given value is encoded using a concise format and the
*     result assigned to the named message token. If the token is
*     already defined, the result is appended to the existing token value.
*     The given value may be one of the following Fortran 77 data types
*     and there is one routine provided for each data type:
*
*        MSG_SETD   DOUBLE PRECISION
*        MSG_SETR   REAL
*        MSG_SETI   INTEGER
*        MSG_SETL   LOGICAL
*        MSG_SETC   CHARACTER
*
*     If this subroutine fails, the token remains unmodified - this will
*     be apparent in any messages which refer to this token.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The message token name. 
*     CVALUE = CHARACTER * ( * ) (Given)
*        The value to be assiged to the message token.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     13-NOV-1984 (BDK):
*        Change name of MSG_STOK.
*     20-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     15-DEC-1989 (PCTR):
*        Converted to call EMS_SETC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      CHARACTER * ( * ) CVALUE

*.

*  Construct the message token string.
      CALL EMS_SETC( TOKEN, CVALUE )

      END
