      SUBROUTINE KPS1_ELPR4( EL, MASK, NBIN, AXCEN, MOUT, STATUS )
*+
*  Name:
*     KPS1_ELPR4

*  Purpose:
*     Creates an output mask for ELPROF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ELPR4( EL, MASK, NBIN, AXCEN, MOUT, STATUS )

*  Description:
*     An output image is created in which each pixel value is
*     determined from the corresponding pixel in the supplied mask
*     image.  If the supplied value is zero (i.e. if the input pixel
*     was not included in any bin), then the output pixel is set bad.
*     Otherwise, the corresponding central bin value is looked up
*     (using AXCEN) and stored in the output pixel.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the two images.
*     MASK( EL ) = INTEGER (Given)
*        The mask created by KPS1_ELPR3.
*     NBIN = INTEGER (Given)
*        The number of bins in the profile.
*     AXCEN( NBIN ) = REAL (Given)
*        The central axis value (radius or azimuthal angle) for each
*        bin.
*     MOUT( EL ) = REAL (Returned)
*        The returned mask.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-FEB-1995 (DSB):
*        Original version.
*     1995 March 28 (MJC):
*        Minor stylistic typographical changes, and used a modern-style
*        variable declaration.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      
*  Arguments Given:
      INTEGER EL
      INTEGER MASK( EL )
      INTEGER NBIN
      REAL AXCEN( NBIN )
      
*  Arguments Returned:
      REAL MOUT( EL )
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Pixel count
      INTEGER IBIN               ! Bin index
      
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      
*  Loop round every pixel in the mask, storing the new value.
      DO I = 1, EL
         IBIN = MASK( I )
         IF ( IBIN .GT. 0 ) THEN
            MOUT( I ) = AXCEN( IBIN )         
         ELSE
            MOUT( I ) = VAL__BADR
         END IF
      END DO
      
      END
