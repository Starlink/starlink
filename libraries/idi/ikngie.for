************************************************************************

      SUBROUTINE IKNGIE( DISPID, NEVAL, IVALUE, STATUS )

*+
*  Name:
*     IKNGIE
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIGIE.
*
*  Invocation:
*     CALL IKNGIE( DISPID, NEVAL, IVALUE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     IVALUE = INTEGER (Returned)
*        Input integer value
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
      INTEGER IVALUE

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

