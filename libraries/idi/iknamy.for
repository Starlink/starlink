************************************************************************

      SUBROUTINE IKNAMY( DISPID, XSIZE, YSIZE, MEMDEP, MEMTYP, MEMID,
     :                   STATUS )

*+
*  Name:
*     IKNAMY
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDAMY.
*
*  Invocation:
*     CALL IKNAMY( DISPID, XSIZE, YSIZE, MEMDEP, MEMTYP, MEMID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     XSIZE = INTEGER (Given)
*        Requested X size
*     YSIZE = INTEGER (Given)
*        Requested Y size
*     MEMDEP = INTEGER (Given)
*        Requested memory depth
*     MEMTYP = INTEGER (Given)
*        Requested memory type
*     MEMID = INTEGER (Returned)
*        New memory identifier
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
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER MEMDEP
      INTEGER MEMTYP

*  Arguments Returned:
      INTEGER MEMID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

