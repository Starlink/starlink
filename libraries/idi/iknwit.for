************************************************************************

      SUBROUTINE IKNWIT( DISPID, MEMID, ITTNUM, START, NENT, ITT,
     :                   STATUS )

*+
*  Name:
*     IKNWIT
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IILWIT.
*
*  Invocation:
*     CALL IKNWIT( DISPID, MEMID, ITTNUM, START, NENT, ITT, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     ITTNUM = INTEGER (Given)
*        ITT number
*     START = INTEGER (Given)
*        Start position
*     NENT = INTEGER (Given)
*        Number of entries
*     ITT( 3, NENT ) = REAL (Given)
*        Intensity transformation table
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
      INTEGER ITTNUM
      INTEGER START
      INTEGER NENT
      REAL ITT( 3, NENT )

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

