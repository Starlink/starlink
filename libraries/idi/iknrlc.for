************************************************************************

      SUBROUTINE IKNRLC( DISPID, NCONF, STATUS )

*+
*  Name:
*     IKNRLC
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDRLC.
*
*  Invocation:
*     CALL IKNRLC( DISPID, NCONF, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Given)
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
      INTEGER NCONF

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

