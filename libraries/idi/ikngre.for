************************************************************************

      SUBROUTINE IKNGRE( DISPID, NEVAL, RVALUE, STATUS )

*+
*  Name:
*     IKNGRE
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIGRE.
*
*  Invocation:
*     CALL IKNGRE( DISPID, NEVAL, RVALUE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     RVALUE = REAL (Returned)
*        Input real value
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
      REAL RVALUE

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

