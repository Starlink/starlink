      SUBROUTINE ARD_WCS( IWCS, STATUS )
*+
*  Name:
*     ARD_WCS

*  Purpose:
*     Specify coordinates systems available in the next call to ARD_WORK.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD_WCS( IWCS, STATUS )

*  Description:
*     This routine can be used to supply additional information about
*     coordinate transformations to the ARD_WORK routine. This
*     information enables ARD_WORK to interpret ARD expressions containing
*     positions in any coordinate system implied by the supplied FrameSet
*     (IWCS). Without this extra information, only positions supplied in 
*     "ARD User cordinates" (as defined by the TR argument to ARD_WORK, and 
*     any COEFFS, OFFSET, SCALE, STRETCH or TWIST statements in the ARD 
*     expression) can be interpreted by ARD_WORK.
*
*     If used, this routine should be called prior to calling ARD_WORK.
*     The supplied FrameSet will be used by all subsequent calls to ARD_WORK 
*     until a new FrameSet is specified by calling ARD_WCS again. Once all
*     calls to ARD_WORK have been made, ARD_WCS should be called again,
*     supplying AST__NULL for IWCS. This will free the internal resources
*     used to store the FrameSet.

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST pointer to a FrameSet. This FrameSet should contain a Frame
*        with Domain PIXEL, corresponding to pixel coordinates with the
*        mask array supplied to ARD_WORK. A copy of the supplied FrameSet
*        is taken by this routine, and so any subsequent changes to the
*        FrameSet will not make any changes to ARD_WORK. If AST__NULL is
*        supplied, any FrameSet stored by a previous call to this routine
*        is deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - If IWCS is set to AST__NULL, then this routine attempts to execute 
*     even if an error has already occurred, although no error will be
*     reported if this routine subsequently fails.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUL-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Global Constants:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_AWCS = INTEGER (Write)
*           A pointer to the application FrameSet.

*  Arguments Given:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_INIT         ! Initialise ARD common blocks

*  Local Variables:
      INTEGER IPIX               ! Index of PIXEL Frame
      INTEGER I                  ! Loop counter
      INTEGER FR                 ! Pointer to Frame
*.

*  Annul any existing Application FrameSet stored in common. Do this
*  before checking the global status so that resources are freed even if
*  an error has occurred.
      IF( CMN_AWCS .NE. AST__NULL ) CALL AST_ANNUL( CMN_AWCS, STATUS )

*  Check the inherited global status and also check a FrameSet has been
*  supplied.
      IF ( STATUS .NE. SAI__OK .OR. IWCS .EQ. AST__NULL ) RETURN

*  Take a copy of the supplied FrameSet.
      CMN_AWCS = AST_COPY( IWCS, STATUS )

*  Check each Frame until one is found with Domain PIXEL.
      IPIX = AST__NOFRAME       
      DO I = 1, AST_GETI( IWCS, 'NFRAME', STATUS )
         FR = AST_GETFRAME( IWCS, I, STATUS )

         IF( AST_GETC( FR, 'DOMAIN', STATUS ) .EQ. 'PIXEL' ) THEN
            CALL AST_ANNUL( FR, STATUS )
            IPIX = I
            GO TO 10
         END IF

         CALL AST_ANNUL( FR, STATUS )

      END DO

 10   CONTINUE

*  Report an error if no pixel frame was found.
      IF( IPIX .EQ. AST__NOFRAME .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = ARD__NOPIX 
         CALL ERR_REP( 'ARD_WCS_ERR', 'The supplied FrameSet has '//
     :                 'no PIXEL Frame (possible programming error).',
     :                 STATUS )
      END IF

*  Make the PIXEL Frame the Base Frame.
      CALL AST_SETI( CMN_AWCS, 'BASE', IPIX, STATUS )

*  If an error has occured, give a context message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARD_WCS_ERR3', 'ARD_WCS: Unable to convert '//
     :                 'an ARD description into a pixel mask.', STATUS )
      END IF

      END
