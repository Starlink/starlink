      SUBROUTINE POL1_SNGVN( NNDF, IGRP, ILEVEL, T, PHI, EPS, EL, HW,
     :                       DIM3, STOKES, LBND, UBND, WORK, TVAR, VEST, 
     :                       STATUS )
*+
*  Name:
*     POL1_SNGVN

*  Purpose:
*     Estimate the input variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGVN( NNDF, IGRP, ILEVEL, T, PHI, EPS, EL, HW,
*                      DIM3, STOKES, LBND, UBND, WORK, TVAR, VEST,
*                      STATUS )

*  Description:
*     This routine estimates the input variances. The supplied Stokes
*     vectors represent a least squares fit of a sine wave to the column 
*     of input intensity values at each pixel position. The residuals
*     between this fit and the input intensity values are found, and the
*     mean squared residual at each pixel is then found (i.e. the mean 
*     squared residuals for all intensity values which correspond to the
*     same point on the sky). This image of mean squared residuals is
*     smoothed using a mean box filter with half-width given by HW.
*     The smoothed mean squared residuals image is returned as the
*     variance image estimate.

*  Arguments:
*     NNDF = INTEGER (Given)
*        The number of input NDFs in the supplied group.
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group containing the input NDF names. 
*     ILEVEL = INTEGER (Given)
*        The information level. 
*     T( NNDF ) = REAL (Given)
*        The analyser transmission factor for each input NDF. 
*     PHI( NNDF ) = REAL (Given)
*        The effective analyser angle for each input NDF, in radians. 
*        This is the ACW angle from the output NDF X axis to the
*        effective analyser axis. For a rotating analyser system, this is
*        the same as the actual analyser angle. For a rotating half-wave
*        plate system, this is the orientation of an analyser which gives the
*        same effect as the half-wave plate/fixed analyser combination.
*     EPS( NNDF ) = REAL (Given)
*        The analyser efficiency factor for each input NDF. 
*     EL = INTEGER (Given)
*        Number of pixels in each image.
*     HW = INTEGER (Given)
*        The half size of the box to use when smoothing squared residuals.
*        The full size used is 2*HW + 1.
*     DIM3 = INTEGER (Given)
*        Number of planes in STOKES.
*     STOKES( EL, DIM3 ) = REAL (Given)
*        Current (smoothed) Stokes vectors.
*     LBND( 3 ) = INTEGER (Given)
*        Lower pixel index bounds of output NDF.
*     UBND( 3 ) = INTEGER (Given)
*        Upper pixel index bounds of output NDF.
*     WORK( EL ) = REAL (Given and Returned)
*        A work array.
*     TVAR( NNDF ) = REAL (Given and Returned)
*        Estimate of the mean variance in each NDF. VAL__BADR if no
*        estimate is available.
*     VEST( EL ) = REAL (Returned)
*        Estimate of the variance of the input data at each pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-APR-1999 (DSB):
*        Original version.
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
      INTEGER NNDF
      INTEGER IGRP
      INTEGER ILEVEL
      REAL T( NNDF )
      REAL PHI( NNDF )
      REAL EPS( NNDF )
      INTEGER EL
      INTEGER HW
      INTEGER DIM3
      REAL STOKES( EL, DIM3 )
      INTEGER LBND( 3 )
      INTEGER UBND( 3 )

*  Arguments Given and Returned:
      REAL WORK( EL )
      REAL TVAR( NNDF )

*  Arguments Returned:
      REAL VEST( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Index of current input NDF
      INTEGER INDF               ! NDF identifier for the current input NDF
      INTEGER INDFS              ! NDF identifier for the input section
      INTEGER IPDIN              ! Pointer to input DATA array
      INTEGER IPW1               ! Pointer to work array     
      INTEGER IPW2               ! Pointer to work array     
      INTEGER NEL                ! No. of mapped elements
      INTEGER NX                 ! Dimension of output cube on axis 1
      INTEGER NY                 ! Dimension of output cube on axis 2
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the two running sum images to hold zeros.
      CALL POL1_FILLR( 0.0, EL, WORK, STATUS )
      CALL POL1_FILLR( 0.0, EL, VEST, STATUS )

*  Loop round each NDF.
      DO I = 1, NNDF

*  Begin an NDF context.
         CALL NDF_BEGIN

*  Get the current input NDF identifier.
         CALL NDG_NDFAS( IGRP, I, 'READ', INDF, STATUS )

*  Get a section from it which matches the output NDF.
         CALL NDF_SECT( INDF, 2, LBND, UBND, INDFS, STATUS ) 

*  Map the data array.
         CALL NDF_MAP( INDFS, 'DATA', '_REAL', 'READ', IPDIN, NEL, 
     :                 STATUS )

*  Increment the running sum images to hold this NDF. This call also
*  returns the mean squared residual (i.e. the mean variance) in the 
*  current NDF.
         CALL POL1_SNGVA( EL, %VAL( IPDIN ), T( I ), PHI( I ), EPS( I ), 
     :                    DIM3, STOKES, WORK, VEST, TVAR( I ), STATUS )

*  End the NDF context.
         CALL NDF_END( STATUS )

      END DO

*  If the mean squared residuals are to be smoothed, 
      IF( HW .GT. 0 ) THEN

*  Normalize the VEST values to hold the mean squared residual at each
*  pixel, storing the normalized values in WORK.
         DO I = 1, EL
            IF( WORK( I ) .GT. 0.0 ) THEN
               WORK( I ) = VEST( I )/WORK( I )
            ELSE
               WORK( I ) = VAL__BADR
            END IF
         END DO

*  Store the sizes of the first two dimensions.
         NX = UBND( 1 ) - LBND( 1 ) + 1
         NY = UBND( 2 ) - LBND( 2 ) + 1

*  Get work space needed to do the smoothing in POL1_BLOCR.
         CALL PSX_CALLOC( NX, '_DOUBLE', IPW1, STATUS )
         CALL PSX_CALLOC( NX, '_INTEGER', IPW2, STATUS )

*  Smooth the mean squared residuals in WORK to get the variance 
*  estimates (store them back in VEST).
         CALL POL1_BLOCR( NX, NY, WORK, HW, HW, 1, VEST, %VAL( IPW1 ), 
     :                    %VAL( IPW2 ), STATUS )

*  Free the work space.
         CALL PSX_FREE( IPW1, STATUS )
         CALL PSX_FREE( IPW2, STATUS )

*  If no smoothing is required, normalize the VEST values to hold the 
*  mean squared residual at each pixel, storing the normalized values in 
*  VEST. 
      ELSE

         DO I = 1, EL
            IF( WORK( I ) .GT. 0.0 ) THEN
               VEST( I ) = VEST( I )/WORK( I )
            ELSE
               VEST( I ) = VAL__BADR
            END IF
         END DO

      END IF

      END
