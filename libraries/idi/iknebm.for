************************************************************************

      SUBROUTINE IKNEBM( DISPID, BMDSCR, BMTYPE, XSIZE, YSIZE, STATUS )

*+
*  Name:
*     IKNEBM
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIMEBM.
*
*  Invocation:
*     CALL IKNEBM( DISPID, BMDSCR, BMTYPE, XSIZE, YSIZE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     BMDSCR = CHARACTER * ( * ) (Given)
*        Bitmap descriptor
*     BMTYPE = CHARACTER (Given)
*        Bitmap type
*     XSIZE = INTEGER (Given and Returned)
*        Bitmap X size
*     YSIZE = INTEGER (Given and Returned)
*        Bitmap Y size
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
      CHARACTER * ( * ) BMDSCR
      CHARACTER BMTYPE

*  Arguments Given and Returned:
      INTEGER XSIZE
      INTEGER YSIZE

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

