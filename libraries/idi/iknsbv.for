************************************************************************

      SUBROUTINE IKNSBV( DISPID, MEMID, LVIS, STATUS )

*+
*  Name:
*     IKNSBV
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IILSBV.
*
*  Invocation:
*     CALL IKNSBV( DISPID, MEMID, LVIS, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     LVIS = LOGICAL (Given)
*        Visibility
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
      INTEGER MEMID
      LOGICAL LVIS

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

