      SUBROUTINE IMG_NEW1( PARAM, NX, IP, STATUS )
*+
*  Name:
*     IMG_NEW1

*  Purpose:
*     Creates a new 1-dimensional image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_NEW1( PARAM, NX, IP, STATUS )

*  Description:
*     This routine creates a new 1-dimensional image and returns a
*     pointer to its data, mapped as floating point (REAL) values.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name (case insensitive).
*     NX = INTEGER (Given)
*        Size of the image (in pixels).
*     IP = INTEGER (Returned)
*        Pointer to the image data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine may create more than one new image at a time by
*     using multiple parameter names. Multiple names are specified by
*     supplying a comma separated list (i.e. 'NEW1,NEW2'). A pointer to
*     the data of each image is then returned (in this case the IP
*     argument must be passed as an array of size at least the number
*     of parameter names). An advantage of this method is that multiple
*     images can be made using a single invocation of this routine.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1992 (RFWS):
*        Original version.
*     18-AUG-1994 (PDRAPER):
*        Extended to use multiple parameter names.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ private constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER NX

*  Arguments Returned:
      INTEGER IP( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_OK
      LOGICAL IMG1_OK            ! Test if error status is OK

*  Local Constants:
      INTEGER NDIM               ! Number of NDF dimensions
      PARAMETER ( NDIM = 1 )

*  Local Variables:
      INTEGER DIM( NDIM )        ! NDF dimension sizes

*.

*  Set an initial null value for the first returned pointer.
      IP( 1 ) = IMG__NOPTR

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Assign the NDF dimension.
      DIM( 1 ) = NX

*  Create new NDF(s).
      CALL IMG1_NWNDF( PARAM, '_REAL', NDIM, DIM, IP, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( .NOT. IMG1_OK( STATUS ) ) THEN
         IF ( INDEX( PARAM, ',' ) .NE. 0 ) THEN 
            CALL ERR_REP( 'IMG_NEW1_ERR',
     :           'IMG_NEW1: Error creating new 1-dimensional images.',
     :           STATUS )
         ELSE
            CALL ERR_REP( 'IMG_NEW1_ERR',
     :           'IMG_NEW1: Error creating a new 1-dimensional image.',
     :           STATUS )
         END IF
      END IF

      END
* $Id$
