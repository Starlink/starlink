************************************************************************

      SUBROUTINE IKNIAG( DISPID, OUTID, STATUS )

*+
*  Name:
*     IKNIAG
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDIAG.
*
*  Invocation:
*     CALL IKNIAG( DISPID, OUTID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     OUTID = INTEGER (Given)
*        Output identifier
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
      INTEGER OUTID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

