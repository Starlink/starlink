************************************************************************

      SUBROUTINE IKNGLE( DISPID, NEVAL, LVALUE, STATUS )

*+
*  Name:
*     IKNGLE
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIGLE.
*
*  Invocation:
*     CALL IKNGLE( DISPID, NEVAL, LVALUE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     LVALUE = LOGICAL (Returned)
*        Input logical value
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
      LOGICAL LVALUE

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

