      SUBROUTINE KPS1_CLPSR( AXIS, LO, HI, VAR, METH, WLIM, NEL2, 
     :                       NDIM1, LBND1, UBND1, DIN, VIN, NDIM2, 
     :                       LBND2, UBND2, DOUT, VOUT, WORK1, WORK2, 
     :                       STATUS )
*+
*  Name:
*     KPS1_CLPSR

*  Purpose:
*     Collapse one axis of an N-d array using a mean or median.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CLPSR( AXIS, LO, HI, VAR, METH, WLIM, NEL2, NDIM1, 
*                      LBND1, UBND1, DIN, VIN, NDIM2, LBND2, UBND2, 
*                      DOUT, VOUT, WORK1, WORK2, STATUS )

*  Description:
*     This routine collapses the supplied data and variance arrays
*     along the specified axis. See COLLAPSE documentation.

*  Arguments:
*     AXIS = INTEGER (Given)
*        The index of the axis to be collapsed.
*     LO = INTEGER (Given)
*        The low pixel index bound of the range of axis AXIS to be
*        collapsed.
*     HI = INTEGER (Given)
*        The high pixel index bound of the range of axis AXIS to be
*        collapsed.
*     VAR = LOGICAL (Given)
*        Process variances?
*     METH = CHARACTER * ( * ) (Given)
*        Estimator: 'MEAN' or 'MEDIAN'.
*     WLIM = REAL (Given)
*        Fraction of pixels which must be good to create a good output
*        pixel.
*     NEL2 = INTEGER (Given)
*        Total no. of elements in the output array (i.e. the product of
*        the output dimension sizes).
*     NDIM1 = INTEGER (Given)
*        The number of axes in the input array.
*     LBND1 = INTEGER (Given)
*        The lower pixel index bounds of the input array.
*     UBND1 = INTEGER (Given)
*        The upper pixel index bounds of the input array.
*     DIN( * ) = REAL (Given)
*        The input data values.
*     VIN( * ) = REAL (Given)
*        The input variance values. Only accessed if VAR is .TRUE.
*     NDIM2 = INTEGER (Given)
*        The number of axes in the output array.
*     LBND2 = INTEGER (Given)
*        The lower pixel index bounds of the output array.
*     UBND2 = INTEGER (Given)
*        The upper pixel index bounds of the output array.
*     DOUT( * ) = REAL (Returned)
*        The output data values.
*     VOUT( * ) = REAL (Returned)
*        The output variance values. Only accessed if VAR is .TRUE.
*     WORK1( NEL2, * ) = REAL (Returned)
*        Work array. The second axis should have at least (HI-LO+1) elements.
*     WORK2( NEL2, * ) = REAL (Returned)
*        Work array. The second axis should have at least (HI-LO+1) elements.
*        Only accessed if VAR is .TRUE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-AUG-2000 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL constants

*  Arguments Given:
      INTEGER AXIS
      INTEGER LO
      INTEGER HI
      LOGICAL VAR
      CHARACTER METH*(*)
      REAL WLIM
      INTEGER NEL2               
      INTEGER NDIM1
      INTEGER LBND1( NDIM1 )
      INTEGER UBND1( NDIM1 )
      REAL DIN( * )
      REAL VIN( * )
      INTEGER NDIM2
      INTEGER LBND2( NDIM2 )
      INTEGER UBND2( NDIM2 )

*  Arguments Returned:
      REAL DOUT( * )
      REAL VOUT( * )
      REAL WORK1( NEL2, * )
      REAL WORK2( NEL2, * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM1( NDF__MXDIM ) ! Input dimension sizes
      INTEGER DIM2( NDF__MXDIM ) ! Output dimension sizes
      INTEGER IAX1               ! Input axis index
      INTEGER IAX2               ! Output axis index
      INTEGER IEL1               ! Index into input data vectors
      INTEGER IEL2               ! Index into output data vectors
      INTEGER IMETH              ! Combination method
      INTEGER IPCOV              ! Pointer to workspace
      INTEGER IPNCON             ! Pointer to workspace
      INTEGER IPPNT              ! Pointer to workspace
      INTEGER IPPP               ! Pointer to workspace
      INTEGER IPUSED             ! Pointer to workspace
      INTEGER IPVAR              ! Pointer to dummy line variances
      INTEGER IPWRK1             ! Pointer to workspace
      INTEGER IPWRK2             ! Pointer to workspace
      INTEGER J                  ! Pixel index on collapsed axis
      INTEGER K                  ! Work array index
      INTEGER NLIN               ! No. of i/p pixels in each o/p pixel
      INTEGER NMAT               ! Size of workspace 
      INTEGER POS1( NDF__MXDIM ) ! Input pixel indices 
      INTEGER POS2( NDF__MXDIM ) ! Output pixel indices 
      INTEGER STEP1              ! Vector step between collaped pixels
      LOGICAL CARRY              ! Increment next axis?

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied METH value.
      IF( METH .EQ. 'MEAN' ) THEN
         IMETH = 2

      ELSE IF( METH .EQ. 'MEDIAN' ) THEN
         IMETH = 3

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'M', METH )
         CALL ERR_REP( 'KPS1_CLPSR_ERR1', 'KPS1_CLPSR: '//
     :                 'Unsupported combination method ''^M'' '//
     :                 'requested (programming error).', STATUS )    
         GO TO 999
      END IF

*  Find the size of each dimension in the output array. Also initialize 
*  the pixel indices of the current output pixel to be the first pixel.
      DO IAX2 = 1, NDIM2
         DIM2( IAX2 ) = UBND2( IAX2 ) - LBND2( IAX2 ) +1
         POS2( IAX2 ) = LBND2( IAX2 )
      END DO

*  Find the dimensions of the input array.
      DO IAX1 = 1, NDIM1
         DIM1( IAX1 ) = UBND1( IAX1 ) - LBND1( IAX1 ) + 1
      END DO

*  Note the step between adjacent pixels along the collapse axis.
      STEP1 = 1
      DO IAX1 = 1, AXIS - 1
         STEP1 = STEP1*DIM1( IAX1 )
      END DO

*  Step through every pixel in the output arrays.
      DO IEL2 = 1, NEL2      

*  Find the pixel indices of the first input pixel to contribute to
*  the current output pixel.
         DO IAX1 = 1, NDIM1
            IF( IAX1 .LT. AXIS ) THEN 
               POS1( IAX1 ) = POS2( IAX1 )
            ELSE IF( IAX1 .EQ. AXIS ) THEN
               POS1( IAX1 ) = LO
            ELSE
               POS1( IAX1 ) = POS2( IAX1 - 1 )
            END IF
         END DO

*  Find the corresponding vector index into the input arrays at this position.
         IEL1 = 0
         DO IAX1 = NDIM1 - 1, 1, -1
            IEL1 = ( POS1( IAX1 + 1 ) - LBND1( IAX1 + 1 ) + IEL1 )*
     :              DIM1( IAX1 )
         END DO
         IEL1 = IEL1 + POS1( 1 ) - LBND1( 1 ) + 1

*  Loop through the pixels to be collapsed in this "column".
         K = 0
         DO J = LO, HI
            K = K + 1

*  Store this pixel value in a work array.
            WORK1( IEL2, K ) = DIN( IEL1 )
            IF( VAR ) WORK2( IEL2, K ) = VIN( IEL1 )

*  Move on to the next input pixel.
            IEL1 = IEL1 + STEP1

         END DO

*  Update the pixel indices of the current output pixel.
         CARRY = .TRUE.
         IAX2 = 1
         DO WHILE( CARRY )
            POS2( IAX2 ) = POS2( IAX2 ) + 1
            IF( POS2( IAX2 ) .LE. UBND2( IAX2 ) ) THEN
               CARRY = .FALSE.
            ELSE
               POS2( IAX2 ) = LBND2( IAX2 )
               IAX2 = IAX2 + 1
               IF( IAX2 .GT. NDIM2 ) CARRY = .FALSE.
            END IF
         END DO            

      END DO

*  Now we have the input data re-ordered in the work arrays. Combine
*  the values together into the output data and variances. The CCDPACK
*  combination routines used below expect the data to be supplied as a
*  series of lines in a 2D array. Each line comprises one "image" and
*  each column gives the pixels to be combined. The result is a single
*  line of data.
      NLIN = HI - LO + 1
      NMAT = NLIN * ( NLIN + 1 )/ 2

      CALL PSX_CALLOC( NLIN, '_REAL', IPWRK1, STATUS )
      CALL PSX_CALLOC( NLIN, '_REAL', IPWRK2, STATUS )
      CALL PSX_CALLOC( NLIN, '_DOUBLE', IPNCON, STATUS )
      CALL PSX_CALLOC( NLIN, '_INTEGER', IPPNT, STATUS )
      CALL PSX_CALLOC( NLIN, '_LOGICAL', IPUSED, STATUS )

      IF( VAR ) THEN
         CALL PSX_CALLOC( NLIN, '_DOUBLE', IPPP, STATUS )
         NMAT = NLIN*( NLIN + 1 )/2 
         CALL PSX_CALLOC( NLIN*NMAT, '_DOUBLE', IPCOV, STATUS )
  
         CALL CCG1_CM1RR( WORK1, NEL2, NLIN, WORK2, IMETH, 
     :                    MAX( 1, NINT( WLIM*REAL( NLIN ) ) ),
     :                    0, 0.0, 0.0, 0.0, 0.0, DOUT, VOUT, 
     :                    %VAL( IPWRK1 ), %VAL( IPWRK2 ), 
     :                    %VAL( IPPP ), %VAL( IPCOV ), NMAT, 
     :                    %VAL( IPNCON ), %VAL( IPPNT ), 
     :                    %VAL (IPUSED ), STATUS )

         CALL PSX_FREE( IPPP, STATUS )
         CALL PSX_FREE( IPCOV, STATUS )

      ELSE
         CALL PSX_CALLOC( NLIN, '_DOUBLE', IPVAR, STATUS )
         CALL KPG1_FILLD( 1.0D0, NLIN, %VAL( IPVAR ), STATUS )

         CALL CCG1_CM3RR( WORK1, NEL2, NLIN, %VAL( IPVAR ), IMETH,
     :                    MAX( 1, NINT( WLIM*REAL( NLIN ) ) ),
     :                    0, 0.0, 0.0, 0.0, 0.0,
     :                    DOUT, %VAL( IPWRK1 ), %VAL( IPWRK2 ), 
     :                    %VAL( IPNCON ), %VAL( IPPNT ), 
     :                    %VAL (IPUSED ), STATUS )

         CALL PSX_FREE( IPVAR, STATUS )

      END IF

      CALL PSX_FREE( IPWRK1, STATUS )
      CALL PSX_FREE( IPWRK2, STATUS )
      CALL PSX_FREE( IPNCON, STATUS )
      CALL PSX_FREE( IPPNT, STATUS )
      CALL PSX_FREE( IPUSED, STATUS )

 999  CONTINUE

      END
