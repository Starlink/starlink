      SUBROUTINE POL1_SNGSV( IGRP1, NNDF, WSCH, OUTVAR, PHI, ANLIND, T, 
     :                       EPS, IGRP2, INDFO, INDFC, NITER, NSIGMA, 
     :                       ILEVEL, STATUS )
*+
*  Name:
*     POL1_SNGSV

*  Purpose:
*     Calulates Stokes vectors from a set of single-beam intensity images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGSV( IGRP1, NNDF, WSCH, OUTVAR, PHI, ANLIND, T, EPS, 
*                      IGRP2, INDFO, INDFC, NITER, NSIGMA, ILEVEL, 
*                      STATUS )

*  Description:
*     This routine calculates Stokes vectors from a set of single-beam 
*     intensity images, and stores them in the supplied output NDF. The
*     method used is described by Sparks & Axon (PASP ????). Once I,Q,U
*     values have been found, the corresponding input data values can be
*     found and the residuals between these and the actual input data values
*     can be used to reject aberrant input data values (i.e. input data
*     values for which the residual is greater than NSIGMA standard
*     deviations, where the standard deviation is either the square root of
*     the supplied input variance value if supplied, or the standard
*     deviation estimated from the residuals). New I,Q,U values are then 
*     found excluding the rejected input values. This rejection process is 
*     repeated NITER times.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the group containing the input NDF names. 
*        These should be aligned pixel-for-pixel.
*     NNDF = INTEGER (Given)
*        The number of input NDFs in the supplied group.
*     WSCH = INTEGER (Given)
*        The scheme to use for selecting the weights for input intensity
*        values:
*
*        1 - Use the reciprocal of the variances supplied with the
*        input images. A check will have been made that these are
*        available for all input images.
*
*        2 - Use the reciprocal of the variances supplied with the input 
*        images. These may or may not be available. If an input image does 
*        not have associated variances then the weights used for that image 
*        are based on an estimate of the variances derived from the spread 
*        of input intensity values. 
*
*        3 - Use the reciprocal of an estimate of the input variance
*        derived from the spread of input intensity values. Any variances 
*        supplied with the input images are ignored.
*
*        4 - Use a constant weight of 1.0 for all input images. Any 
*        variances supplied with the input images are ignored. 

*     OUTVAR = LOGICAL (Given)
*        Should output variances be created?
*     PHI( NNDF ) = REAL (Given)
*        The effective analyser angle for each input NDF, in radians. 
*        This is the ACW angle from the output NDF X axis to the
*        effective analyser axis. For a rotating analyser system, this is
*        the same as the actual analyser angle. For a rotating half-wave
*        plate system, this is the orientation of an analyser which gives the
*        same effect as the half-wave plate/fixed analyser combination.
*     ANLIND( NNDF ) = INTEGER (Given)
*        The analyser index for each input NDF. These are indices into 
*        the IGRP2 group.
*     T( NNDF ) = REAL (Given)
*        The analyser transmission factor for each input NDF. 
*     EPS( NNDF ) = REAL (Given)
*        The analyser efficiency factor for each input NDF. 
*     IGRP2 = INTEGER (Given)
*        A GRP identifier for a group holding the unique analyser identifiers 
*        found in the supplied NDFs. These are text strings which identify 
*        the analysers through which the supplied images were taken. The
*        string "DEFAULT" is used if no analyser identifier is supplied for
*        an NDF. Each unique identifier is included only once in the
*        returned group.
*     INDFO = INTEGER (Given)
*        An NDF identifier for the output NDF in which to store the 
*        I, Q and U values, relative to a reference direction parallel to
*        the output X axis.
*     INDFC = INTEGER (Given)
*        An NDF identifier for the output NDF in which to store the 
*        QU co-variances associated with the Stokes vectors stored in INDFO.
*        The NDF should be 2D with the same bounds as INDFO. This argument is 
*        ignored if VAR is .FALSE. 
*     NITER = INTEGER (Given)
*        The number of rejection iterations to perform. If this is zero
*        then no rejection iterations are performed.
*     NSIGMA = REAL (Given)
*        The number of standard deviations at which input data points are
*        rejected when iterating. Ignored if NITER is zero.
*     ILEVEL = INTEGER (Given)
*        The information level. Zero produces no screen output; 1 gives
*        brief details of each iteration; 2 gives full details of each
*        iteration.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER IGRP1
      INTEGER NNDF
      INTEGER WSCH
      LOGICAL OUTVAR
      REAL PHI( NNDF )
      INTEGER ANLIND( NNDF )
      REAL T( NNDF )
      REAL EPS( NNDF )
      INTEGER IGRP2
      INTEGER INDFO
      INTEGER INDFC
      INTEGER NITER
      REAL NSIGMA
      INTEGER ILEVEL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM1               ! Dimension of output cube on axis 1
      INTEGER DIM2               ! Dimension of output cube on axis 2
      INTEGER DIM3               ! Dimension of output cube on axis 3
      INTEGER EL                 ! No. of elements in a plane of the output NDF
      INTEGER NEL                ! No. of elements in whole output NDF
      INTEGER I                  ! Index of current input NDF
      INTEGER INDF               ! NDF identifier for the current input NDF
      INTEGER INDFS              ! NDF identifier for the input section
      INTEGER IPCM1              ! Pointers to 1st curvature matrix image
      INTEGER IPCM2              ! Pointers to 2nd curvature matrix image
      INTEGER IPCOUT             ! Pointer to output (co-variance) DATA array
      INTEGER IPDIN              ! Pointer to input DATA array
      INTEGER IPDOUT             ! Pointer to output DATA array
      INTEGER IPIE1              ! Pointer to 1st effective intensity image
      INTEGER IPIE2              ! Pointer to 2nd effective intensity image
      INTEGER IPIE3              ! Pointer to 3rd effective intensity image
      INTEGER IPMT11             ! Pointer to column 1 row 1 image
      INTEGER IPMT21             ! Pointer to column 2 row 1 image
      INTEGER IPMT31             ! Pointer to column 3 row 1 image
      INTEGER IPMT22             ! Pointer to column 2 row 2 image
      INTEGER IPMT32             ! Pointer to column 3 row 2 image
      INTEGER IPMT33             ! Pointer to column 3 row 3 image
      INTEGER IPN                ! Pointer to work array holding image counts
      INTEGER IPVIN              ! Pointer to input VARIANCE array
      INTEGER IPVOUT             ! Pointer to output VARIANCE array
      INTEGER ITER               ! Number of rejection iterations completed
      INTEGER LBND( 3 )          ! Lower bounds of output NDF
      INTEGER NDIM               ! No. of axes in output NDF
      INTEGER UBND( 3 )          ! Upper bounds of output NDF
      LOGICAL FRDIN              ! Free the IPDIN pointer?
      LOGICAL FRVIN              ! Free the IPVIN pointer?

      common /ssscom/ iter,dim1,dim2
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the bounds and dimensions of the output NDF.
      CALL NDF_BOUND( INDFO, 3, LBND, UBND, NDIM, STATUS )
      DIM1 = UBND( 1 ) - LBND( 1 ) + 1
      DIM2 = UBND( 2 ) - LBND( 2 ) + 1
      DIM3 = UBND( 3 ) - LBND( 3 ) + 1

*  Store the number of elements in a single plane of the output NDF.
      EL = DIM1*DIM2

*  Map the output DATA array in which to store the IQU values. NEL gets
*  set to the number of elements in the entire 3D output cube.
      CALL NDF_MAP( INDFO, 'DATA', '_REAL', 'WRITE', IPDOUT, NEL, 
     :              STATUS )   

*  If required map the output VARIANCE array in which to store the variances 
*  on I, Q and U, and the DATA array of the co-variance NDF in which to
*  store the co-variances.
      IF( OUTVAR ) THEN
         CALL NDF_MAP( INDFO, 'VARIANCE', '_REAL', 'WRITE', IPVOUT, NEL, 
     :                 STATUS )   
         CALL NDF_MAP( INDFC, 'DATA', '_REAL', 'WRITE', IPCOUT, EL, 
     :                 STATUS )   

*  If no variances are to be stored in the output NDF, allocate a temporary 
*  array to hold output variances and co-variances.
      ELSE
         CALL PSX_CALLOC( NEL, '_REAL', IPVOUT, STATUS )     
         CALL PSX_CALLOC( EL, '_REAL', IPCOUT, STATUS )     
      END IF

*  Allocate work space for the three terms of the vector on the left hand 
*  side of the matrix equation (these are effective measured intensities).
      CALL PSX_CALLOC( EL, '_REAL', IPIE1, STATUS )     
      CALL PSX_CALLOC( EL, '_REAL', IPIE2, STATUS )     
      CALL PSX_CALLOC( EL, '_REAL', IPIE3, STATUS )     

*  We also need work arrays to hold six elements from the matrix on the 
*  right hand side of the equation. The matrix is symetric and so only 
*  six elements are required instead of nine.
      CALL PSX_CALLOC( EL, '_REAL', IPMT11, STATUS )     
      CALL PSX_CALLOC( EL, '_REAL', IPMT21, STATUS )     
      CALL PSX_CALLOC( EL, '_REAL', IPMT31, STATUS )     
      CALL PSX_CALLOC( EL, '_REAL', IPMT22, STATUS )     
      CALL PSX_CALLOC( EL, '_REAL', IPMT32, STATUS )     
      CALL PSX_CALLOC( EL, '_REAL', IPMT33, STATUS )     

*  We also need two extra work arrays to hold the values needed to 
*  calculate the curvature matrix.
      CALL PSX_CALLOC( EL, '_REAL', IPCM1, STATUS )     
      CALL PSX_CALLOC( EL, '_REAL', IPCM2, STATUS )     

*  We also need an extra work array to hold the number of input images
*  contributing to each output pixel.
      CALL PSX_CALLOC( EL, '_REAL', IPN, STATUS )     

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Indicate we have not yet done any rejection iterations.
      ITER = 0

*  Jump back to here when the I,Q,U values have been calculated in order to
*  perform another rejection iteration.
 10   CONTINUE

*  Apply light spatial smoothing to the I,Q,U values calculated on the
*  previous iteration. Each smoothed value is estimated by fitting a least
*  squares quadratic surface to the data within a 5x5 fitting box.
      IF( ITER .GT. 0 ) CALL POL1_SNGSM( ILEVEL, DIM1, DIM2, DIM3, 
     :                                   %VAL( IPVOUT ), %VAL( IPDOUT ), 
     :                                   %VAL( IPCOUT ), %VAL( IPCM1 ),
     :                                   STATUS )

*  Calculate the effective intensities, transmittances, eficiciencies and
*  position angles, etc. See the Sparks and Axon paper for details.
*  ======================================================================

*  Initialise all the work arrays to hold zero.
      CALL POL1_FILLR( 0.0, EL, %VAL( IPIE1 ), STATUS )
      CALL POL1_FILLR( 0.0, EL, %VAL( IPIE2 ), STATUS )
      CALL POL1_FILLR( 0.0, EL, %VAL( IPIE3 ), STATUS )
      CALL POL1_FILLR( 0.0, EL, %VAL( IPMT11 ), STATUS )
      CALL POL1_FILLR( 0.0, EL, %VAL( IPMT21 ), STATUS )
      CALL POL1_FILLR( 0.0, EL, %VAL( IPMT31 ), STATUS )
      CALL POL1_FILLR( 0.0, EL, %VAL( IPMT22 ), STATUS )
      CALL POL1_FILLR( 0.0, EL, %VAL( IPMT32 ), STATUS )
      CALL POL1_FILLR( 0.0, EL, %VAL( IPMT33 ), STATUS )
      CALL POL1_FILLR( 0.0, EL, %VAL( IPCM1 ), STATUS )
      CALL POL1_FILLR( 0.0, EL, %VAL( IPCM2 ), STATUS )
      CALL POL1_FILLR( 0.0, EL, %VAL( IPN ), STATUS )

*  Loop round each NDF.
      DO I = 1, NNDF

*  Begin an NDF context.
         CALL NDF_BEGIN

*  Get the current input NDF identifier.
         CALL NDG_NDFAS( IGRP1, I, 'READ', INDF, STATUS )

*  Get a section from it which matches the output NDF.
         CALL NDF_SECT( INDF, 2, LBND, UBND, INDFS, STATUS ) 

*  Get pointers to the intensity values and associated variance values to
*  use for this NDF. The co-variance array is used as work space here.
         CALL POL1_SNGFL( INDFS, ITER, WSCH, ILEVEL, T( I ), 
     :                    PHI( I ), EPS( I ), DIM1, DIM2, DIM3, 
     :                    %VAL( IPDOUT ), %VAL( IPVOUT ), 
     :                    %VAL( IPCOUT ), NSIGMA, IPDIN, IPVIN, FRDIN, 
     :                    FRVIN, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Update the work arrays to include the effect of the current input NDF.
         CALL POL1_SNGAD( EL, %VAL( IPDIN ), %VAL( IPVIN ), 
     :                   PHI( I ), T( I ), EPS( I ), 
     :                   %VAL( IPIE1 ),  %VAL( IPIE2 ),  %VAL( IPIE3 ), 
     :                   %VAL( IPMT11 ), %VAL( IPMT21 ), %VAL( IPMT31 ), 
     :                                   %VAL( IPMT22 ), %VAL( IPMT32 ),
     :                                                   %VAL( IPMT33 ), 
     :                   %VAL( IPCM1 ), %VAL( IPCM2 ), %VAL( IPN ),
     :                   STATUS )

*  End the NDF context.
         CALL NDF_END( STATUS )

*  Annul the arrays holding the intensity values and variances if required.
         IF( FRDIN ) CALL PSX_FREE( IPDIN, STATUS )
         IF( FRVIN ) CALL PSX_FREE( IPVIN, STATUS )

      END DO

*  Calculate the output Stokes vectors, variances, and co-variances.
*  =================================================================
      CALL POL1_SNGCL( EL, 
     :                 %VAL( IPIE1 ),  %VAL( IPIE2 ),  %VAL( IPIE3 ), 
     :                 %VAL( IPMT11 ), %VAL( IPMT21 ), %VAL( IPMT31 ), 
     :                                 %VAL( IPMT22 ), %VAL( IPMT32 ),
     :                                                   %VAL( IPMT33 ), 
     :                 %VAL( IPCM1 ), %VAL( IPCM2 ), %VAL( IPN ),
     :                 %VAL( IPDOUT ), %VAL( IPVOUT ), %VAL( IPCOUT ), 
     :                 STATUS )

*  Go back to recalculate the output I,Q,U values excluding aberrant input
*  data values unless we have already done the required number of iterations.
      IF( STATUS .EQ. SAI__OK .AND. ITER .LT. NITER ) THEN
         ITER = ITER + 1
         IF( ILEVEL .GT. 1 ) CALL MSG_BLANK( STATUS )
         GO TO 10
      END IF

*  Tidy up
*  =======

 999  CONTINUE

*  Release the work space.
      CALL PSX_FREE( IPIE1, STATUS )
      CALL PSX_FREE( IPIE2, STATUS )
      CALL PSX_FREE( IPIE3, STATUS )
      CALL PSX_FREE( IPMT11, STATUS )
      CALL PSX_FREE( IPMT21, STATUS )
      CALL PSX_FREE( IPMT31, STATUS )
      CALL PSX_FREE( IPMT22, STATUS )
      CALL PSX_FREE( IPMT32, STATUS )
      CALL PSX_FREE( IPMT33, STATUS )
      CALL PSX_FREE( IPCM1, STATUS )     
      CALL PSX_FREE( IPCM2, STATUS )     
      CALL PSX_FREE( IPN, STATUS )     
    
      IF( .NOT. OUTVAR ) THEN
         CALL PSX_FREE( IPVOUT, STATUS )
         CALL PSX_FREE( IPCOUT, STATUS )
      END IF

      END
