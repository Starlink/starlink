************************************************************************

      SUBROUTINE IKNSDP( DISPID, MEMID, NMEM, LUTLIS, ITTLIS, STATUS )

*+
*  Name:
*     IKNSDP
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDSDP.
*
*  Invocation:
*     CALL IKNSDP( DISPID, MEMID, NMEM, LUTLIS, ITTLIS, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     LUTLIS( * ) = INTEGER (Given)
*        List of VLUT flags
*     ITTLIS( * ) = INTEGER (Given)
*        List of ITT flags
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
      INTEGER MEMID( * )
      INTEGER NMEM
      INTEGER LUTLIS( * )
      INTEGER ITTLIS( * )

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

