************************************************************************

      SUBROUTINE IKNSSS( DISPID, MEMID, XOFF, YOFF, SPLIT, XSPLIT,
     :                   YSPLIT, STATUS )

*+
*  Name:
*     IKNSSS
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDSSS.
*
*  Invocation:
*     CALL IKNSSS( DISPID, MEMID, XOFF, YOFF, SPLIT, XSPLIT,
*    :             YSPLIT, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        Memory identifiers
*     XOFF( * ) = INTEGER (Given)
*        X offsets
*     YOFF( * ) = INTEGER (Given)
*        Y offsets
*     SPLIT = INTEGER (Given)
*        Split flag
*     XSPLIT = INTEGER (Given)
*        Split X location
*     YSPLIT = INTEGER (Given)
*        Split Y location
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
      INTEGER XOFF( * )
      INTEGER YOFF( * )
      INTEGER SPLIT
      INTEGER XSPLIT
      INTEGER YSPLIT

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

