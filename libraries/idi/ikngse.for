************************************************************************

      SUBROUTINE IKNGSE( DISPID, NEVAL, STRING, SLEN, STATUS )

*+
*  Name:
*     IKNGSE
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIGSE.
*
*  Invocation:
*     CALL IKNGSE( DISPID, NEVAL, STRING, SLEN, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     STRING = CHARACTER * ( * ) (Returned)
*        Input string
*     SLEN = INTEGER (Returned)
*        Length of returned string
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NEVAL

*  Arguments Returned:
      CHARACTER * ( * ) STRING
      INTEGER SLEN

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

