      SUBROUTINE ARD_INIT( STATUS )
*+
*  Name:
*     ARD_INIT

*  Purpose:
*     Initialises ARD common blocks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD_INIT( STATUS )

*  Description:
*     ARD_INIT initialises the ARD keyword description tables.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-OCT-1991 (PDRAPER):
*        Original version.
*     25-OCT-1991 (PDRAPER):
*        Changed for explicit use within CCDPACK
*     12-MAY-1994 (PDRAPER):
*        Changed ellipse entry to not allow 1 dimensions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_PAR'          ! ARD system buffer sizes

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD Common block.
*        ARD_KWORDS( ARD__MAXKEY ) = CHARACTER * ( ARD__KEYLEN ) (Write)
*           The currently available ARD keywords.
*        ARD_PERDIM( ARD__MAXKEY ) = INTEGER (Write)
*           The number of coordinates expected per dimension.
*           (I.e. if require X,Y then PERDIM is 1,
*            if expect X1,Y1,X2,Y2 then PERDIM is 2). If set to -1 then
*           no limit is set (-1) then no limit should be applied other
*           than an integer of the current dimensionality.
*        ARD_VALDIM( ARD__MAXDIM, ARD__MAXKEY ) = LOGICAL (Write)
*           The dimensions which are valid for this keyword.
*           If PERDIM is -1 and COMPLY is set false then this value
*           should be set true for the dimensions which the values
*           actually apply in.
*        ARD_EXTRA( ARD__MAXKEY ) = INTEGER (Write)
*           The number of extra values which trail a valid sequence of
*           coordinates.
*        ARD_COMPLY( ARD__MAXKEY ) = LOGICAL (Write)
*           Whether or not the coordinates extracted from the keyword
*           liner should be checked for compliance with the current
*           dimensionality. Compliance is determined by checking if the
*           number of returned values is a multiple of the current
*           dimensionality. Keywords with a ARD_PERDIM not equal to -1
*           should be set false, explicit checks will be performed for
*           these cases.
*        ARD_TYPE( ARD__MAXKEY ) = CHARACTER * ( 1 ) (Write)
*           The required type of coordinate. 'A' means that the
*           coordinates can have type of integer or floating (pixel
*           indices - other coordinates). 'P' means pixel indices only.
*           'C' means not pixel indices, so type cannot be integer.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Current array index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IAT = 0

*  Set the keywords and their descriptions.

*  Pixel
      IAT = IAT + 1
      ARD_KWORDS( IAT ) = 'PIXEL'
      ARD_PERDIM( IAT ) = -1           ! Any number of pixels
      DO 1 I = 1, ARD__MAXDIM
         ARD_VALDIM( I, IAT ) =.TRUE.  ! Valid in all dimensions
 1    CONTINUE
      ARD_EXTRA( IAT ) = 0
      ARD_COMPLY( IAT ) = .TRUE.
      ARD_TYPE( IAT ) = 'P'

*  Line
      IAT = IAT + 1
      ARD_KWORDS( IAT ) = 'LINE'
      ARD_PERDIM( IAT ) = 2            ! Two points for a line
      DO 2 I = 1, ARD__MAXDIM
         ARD_VALDIM( I, IAT ) =.TRUE.  ! Valid in all dimensions
 2    CONTINUE
      ARD_EXTRA( IAT ) = 0
      ARD_COMPLY( IAT ) = .FALSE.
      ARD_TYPE( IAT ) = 'A'

*  Row
      IAT = IAT + 1
      ARD_KWORDS( IAT ) = 'ROW'
      ARD_PERDIM( IAT ) = -1
      DO 3 I = 1, ARD__MAXDIM
         ARD_VALDIM( I, IAT ) =.FALSE.
 3    CONTINUE
      ARD_VALDIM( 2, IAT ) = .TRUE.    ! 2nd dimension only
      ARD_EXTRA( IAT ) = 0
      ARD_COMPLY( IAT ) = .FALSE.       ! Only one value per row
      ARD_TYPE( IAT ) = 'P'

*  Column
      IAT = IAT + 1
      ARD_KWORDS( IAT ) = 'COLUMN'
      ARD_PERDIM( IAT ) = -1
      DO 4 I = 1, ARD__MAXDIM
         ARD_VALDIM( I, IAT ) =.FALSE.
 4    CONTINUE
      ARD_VALDIM( 1, IAT ) = .TRUE.    ! 1st dimensional only
      ARD_EXTRA( IAT ) = 0
      ARD_COMPLY( IAT ) = .FALSE.       ! Only one value per row
      ARD_TYPE( IAT ) = 'P'

*  Box
      IAT = IAT + 1
      ARD_KWORDS( IAT ) = 'BOX'
      ARD_PERDIM( IAT ) = 1
      DO 5 I = 1, ARD__MAXDIM
         ARD_VALDIM( I, IAT ) =.FALSE.
 5    CONTINUE
      ARD_VALDIM( 2, IAT ) = .TRUE.    ! Two dimensional only
      ARD_VALDIM( 1, IAT ) = .TRUE.
      ARD_EXTRA( IAT ) = 2             ! Length of sides
      ARD_COMPLY( IAT ) = .FALSE.
      ARD_TYPE( IAT ) = 'A'

*  Polygon
      IAT = IAT + 1
      ARD_KWORDS( IAT ) = 'POLYGON'
      ARD_PERDIM( IAT ) = -1           ! No limit on vertices
      DO 6 I = 1, ARD__MAXDIM
         ARD_VALDIM( I, IAT ) =.FALSE.
 6    CONTINUE
      ARD_VALDIM( 2, IAT ) = .TRUE.    ! Two dimensional only
      ARD_EXTRA( IAT ) = 0
      ARD_COMPLY( IAT ) = .TRUE.
      ARD_TYPE( IAT ) = 'A'

*  Circle
      IAT = IAT + 1
      ARD_KWORDS( IAT ) = 'CIRCLE'
      ARD_PERDIM( IAT ) = 1
      DO 7 I = 1, ARD__MAXDIM
         ARD_VALDIM( I, IAT ) =.FALSE.
 7    CONTINUE
      ARD_VALDIM( 2, IAT ) = .TRUE.    ! Two dimensional only
      ARD_VALDIM( 1, IAT ) = .TRUE.
      ARD_EXTRA( IAT ) = 1             ! Radius
      ARD_COMPLY( IAT ) = .FALSE.
      ARD_TYPE( IAT ) = 'A'

*  Ellipse
      IAT = IAT + 1
      ARD_KWORDS( IAT ) = 'ELLIPSE'
      ARD_PERDIM( IAT ) = 1
      DO 8 I = 1, ARD__MAXDIM
         ARD_VALDIM( I, IAT ) =.FALSE.
 8    CONTINUE
      ARD_VALDIM( 2, IAT ) = .TRUE.    ! Two dimensional only
      ARD_EXTRA( IAT ) = 3             ! Semi major+minor axes, angle
      ARD_COMPLY( IAT ) = .FALSE.
      ARD_TYPE( IAT ) = 'A'

*  NDF
      IAT = IAT + 1
      ARD_KWORDS( IAT ) = 'NDF'
      ARD_PERDIM( IAT ) = 0            ! Whole NDF no trailing values
      DO 9 I = 1, ARD__MAXDIM
         ARD_VALDIM( I, IAT ) =.TRUE.
 9    CONTINUE
      ARD_EXTRA( IAT ) = 0
      ARD_COMPLY( IAT ) = .FALSE.
      ARD_TYPE( IAT ) = 'A'

      END
* $Id$
