************************************************************************

      SUBROUTINE IKNSTC( DISPID, NCONF, STATUS )

*+
*  Name:
*     IKNSTC
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDSTC.
*
*  Invocation:
*     CALL IKNSTC( DISPID, NCONF, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Returned)
*        Configuration number
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

*  Arguments Returned:
      INTEGER NCONF

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

