      SUBROUTINE CONVOLVE( STATUS )
*+
*  Name:
*     CONVOLVE

*  Purpose:
*     Convolves a pair of 1- or 2-dimensional NDFs together

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CONVOLVE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application smooths a 1- or 2-dimensional NDF using a Point-
*     Spread Function given by a second NDF.  The output NDF is
*     normalised to the same mean data value as the input NDF, 
*     and is the same size as the input NDF.

*  Usage:
*     convolve in psf out xcentre ycentre

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF containing the array to be smoothed.
*     OUT = NDF (Write)
*        The output NDF which is to contain the smoothed array.
*     PSF = NDF (Read)
*        An NDF holding the Point-Spread Function (PSF) with which the
*        input array is to be smoothed.  An error is reported if the
*        PSF contains any bad pixels.  The PSF can be centred anywhere
*        within the array (see parameters XCENTRE and YCENTRE).  A
*        constant background is removed from the PSF before use.  This
*        background level is equal to the minimum of the absolute value
*        in the four corner pixel values.  The PSF is assumed to be
*        zero beyond the bounds of the supplied NDF.
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null (!) value means using the
*        title of the input NDF. [!]
*     WLIM = _REAL (Read)
*        If the input array contains bad pixels, then this parameter
*        may be used to determine the number of good pixels which must
*        be present within the smoothing box before a valid output
*        pixel is generated.  It can be used, for example, to prevent
*        output pixels from being generated in regions where there are
*        relatively few good pixels to contribute to the smoothed
*        result.
*
*        By default, a null (!) value is used for WLIM, which causes
*        the pattern of bad pixels to be propagated from the input
*        array to the output array unchanged.  In this case, smoothed
*        output values are only calculated for those pixels which are
*        not bad in the input array.
*
*        If a numerical value is given for WLIM, then it specifies the
*        minimum total weight associated with the good pixels in the
*        smoothing box required to generate a good output pixel
*        (weights for each pixel are defined by the normalised PSF).
*        If this specified minimum weight is not present, then a bad
*        output pixel will result, otherwise a smoothed output value
*        will be calculated.  The value of this parameter should lie
*        between 0.0 and 1.0.  A value of 0.0 will result in a good
*        output pixel being created even if only one good input pixel
*        contributes to it.  A value of 1.0 will result in a good output
*        pixel being created only if all the input pixels which
*        contribute to it are good. [!]
*     XCENTRE = _INTEGER (Read)
*        The x pixel index (column number) of the centre of the PSF
*        within the supplied PSF array.  The suggested default is the
*        centre of the PSF array.  (This is how the PSF command would
*        generate the array.)
*     YCENTRE = _INTEGER (Read)
*        The y pixel index (line number) of the centre of the PSF
*        within the supplied PSF array.  The suggested default is the
*        centre of the PSF array.  (This is how the PSF command would
*        generate the array.)

*  Examples:
*     convolve ccdframe iraspsf ccdlores 50 50
*        The image in the NDF called ccdframe is convolved using the
*        PSF in NDF iraspsf to create the smoothed image ccdlores.  The
*        centre of the PSF image in iraspsf is at pixel indices
*        (50,50).  Any bad pixels in the input image are propagated to
*        the output.
*     convolve ccdframe iraspsf ccdlores 50 50 wlim=1.0
*        As above, but good output values are only created for pixels
*        which have no contributions from bad input pixels.
*     convolve ccdframe iraspsf ccdlores \
*        As in the first example except the centre of the PSF is located
*        at the centre of the PSF array.

*  Notes:
*     -  The algorithm used is based on the multiplication of the
*     Fourier transforms of the input array and PSF array.
*     -  A PSF can be created using the PSF command or MATHS if the
*     PSF is an analytic function.

*  Related Applications:
*     KAPPA: BLOCK, FFCLEAN, GAUSMOOTH, MATHS, MEDIAN, PSF; Figaro:
*     ICONV3, ISMOOTH, IXSMOOTH, MEDFILT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDF and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using double-precision floating point.

*  Authors:
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-MAY-1992 (RAHM):
*        Original version.
*     10-JAN-1995 (DSB):
*        Major changes for inclusion in KAPPA.
*     1995 March 17 (MJC):
*        Improvements to the documentation including remarks about
*        suggested defaults for XCENTRE and YCENTRE, and a related
*        example, and a list of related applications.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL PSFFRA                ! Threshold for finding PSF dimensions
      PARAMETER ( PSFFRA = 0.01 )

*  Local Variables:
      LOGICAL BAD                ! Are there bad pixels in the array?
      INTEGER DIM1( 2 )          ! Dimensions of supplied input NDF
      INTEGER DIM2( 2 )          ! Dimensions of supplied PSF NDF
      INTEGER EL                 ! No. of elements in mapped array
      INTEGER INDF1              ! Identifier for input array NDF
      INTEGER INDF2              ! Identifier for PSF NDF
      INTEGER INDF3              ! Identifier for output array NDF
      INTEGER IP1( 2 )           ! Pointer to mapped input arrays
      INTEGER IP2                ! Pointer to mapped PSF DATA array
      INTEGER IP3( 2 )           ! Pointer to mapped output arrays
      INTEGER IPW1               ! Pointer to mapped work array
      INTEGER IPW2               ! Pointer to mapped work array
      INTEGER IPW3               ! Pointer to mapped work array
      INTEGER IPW4               ! Pointer to mapped work array
      INTEGER IPW5               ! Pointer to mapped work array
      INTEGER NLIN               ! 2nd dimension for internal arrays
      INTEGER NPIX               ! 1st dimension for internal arrays
      INTEGER PSFXSZ             ! Width of PSF on 1st axis
      INTEGER PSFYSZ             ! Width of PSF on 2nd axis
      INTEGER SDIM1( 2 )         ! Indices of used axes for i/p
      INTEGER SDIM2( 2 )         ! Indices of used axes for psf
      INTEGER SLBND1( 2 )        ! Low bounds of used axes of i/p
      INTEGER SLBND2( 2 )        ! Low bounds of used axes of psf
      INTEGER SUBND1( 2 )        ! High bounds of used axes of i/p
      INTEGER SUBND2( 2 )        ! High bounds of used axes of psf
      LOGICAL VAR                ! Does the input NDF have a VARIANCE array?
      INTEGER W1DIM( 2 )         ! Dimensions of work array 1
      REAL WLIM                  ! Fraction of good i/p pixels required
      INTEGER XCEN               ! X index of PSF centre pixel
      INTEGER YCEN               ! Y index of PSF centre pixel

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the input NDF, ensuring that it has no more than two significant
*  dimensions.  Also find the axes to be used and their bounds.
      CALL KPG1_GTNDF( 'IN', 2, .FALSE., 'READ', INDF1, SDIM1, SLBND1, 
     :                 SUBND1, STATUS )

*  Form the dimensions on the axes to be used of the input array.
      DIM1( 1 ) = SUBND1( 1 ) - SLBND1( 1 ) + 1
      DIM1( 2 ) = SUBND1( 2 ) - SLBND1( 2 ) + 1

*  See if the input array has associated variance values.
      CALL NDF_STATE( INDF1, 'VAR', VAR, STATUS )

*  Map the required components of the input array.
      IF ( VAR ) THEN
         CALL NDF_MAP( INDF1, 'DATA,VAR', '_DOUBLE', 'READ', IP1, EL,
     :                 STATUS )
      ELSE
         CALL NDF_MAP( INDF1, 'DATA', '_DOUBLE', 'READ', IP1, EL, 
     :                 STATUS )
      END IF         
      
*  Get the NDF containing the PSF, ensuring that it has no more than
*  two significant dimensions.  Also find the axes to use and their
*  bounds.
      CALL KPG1_GTNDF( 'PSF', 2, .FALSE., 'READ', INDF2, SDIM2, SLBND2,
     :                 SUBND2, STATUS )

*  Form the dimensions on the significant axes of the PSF.
      DIM2( 1 ) = SUBND2( 1 ) - SLBND2( 1 ) + 1
      DIM2( 2 ) = SUBND2( 2 ) - SLBND2( 2 ) + 1
      
*  See if there are any bad pixels in the PSF.  If so, report an error
*  and abort.
      CALL NDF_BAD( INDF2, 'DATA', .TRUE., BAD, STATUS )
      IF ( BAD .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CONVOLVE_ERR1', 'The PSF contains bad or '/
     :     /'missing pixel values.', STATUS )
         GO TO 999
      END IF
         
*  Map the PSF 'DATA' array.
      CALL NDF_MAP( INDF2, 'DATA', '_DOUBLE', 'READ', IP2, EL, STATUS )

*  Get workspace for use by the routine which finds the PSF size.
      W1DIM( 1 ) = MAX( DIM2( 1 ), DIM2( 2 ) )
      W1DIM( 2 ) = 2
      CALL PSX_CALLOC( W1DIM( 1 ) * W1DIM( 2 ), '_DOUBLE', IPW1,
     :                 STATUS )

*  Get the approximate width of the PSF along both array axes.
*  Internal arrays will be padded with a blank margin of this size to
*  reduce edge effects caused by wrap-around in the convolution.
      CALL KPG1_PSFSD( %VAL( IP2 ), DIM2( 1 ), DIM2( 2 ), %VAL( IPW1 ),
     :                 W1DIM( 1 ), W1DIM( 2 ), PSFFRA, 1, PSFXSZ,
     :                 PSFYSZ, STATUS )

*  Add this margin onto the input array dimensions to get the
*  dimensions of the internal arrays used within the convolution
*  process, and then check that this array size can be handled by the
*  FFT routines.  If not, increase the array size until it can.
      CALL FTSIZE( DIM1( 1 ) + 2 * PSFXSZ, NPIX, STATUS )
      CALL FTSIZE( DIM1( 2 ) + 2 * PSFYSZ, NLIN, STATUS )

*  Propagate the output NDF from the input array NDF, copying WCS, UNITS,
*  AXIS and QUALITY components (the default components HISTORY, TITLE
*  and LABEL and all extensions are also copied).
      CALL NDF_PROP( INDF1, 'WCS,UNITS,AXIS,QUALITY', 'OUT', INDF3, 
     :               STATUS )
      
*  Map the required components of the output array.
      IF ( VAR ) THEN
         CALL NDF_MAP( INDF3, 'DATA,VAR', '_DOUBLE', 'WRITE', IP3, EL,
     :                 STATUS )
      ELSE
         CALL NDF_MAP( INDF3, 'DATA', '_DOUBLE', 'WRITE', IP3, EL, 
     :                 STATUS )
      END IF         
      
*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      
*  Obtain the minimum fraction of good pixels which should be used to
*  calculate an output pixel value.  Test if a null value is specified
*  and set WLIM negative, annulling the error.
      CALL PAR_GDR0R( 'WLIM', 0.5, 0.0, 1.0, .FALSE., WLIM, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         WLIM = -1.0 
         CALL ERR_ANNUL( STATUS )

*  If WLIM is exactly 1.0 reduce it slightly (rounding errors, etc. can
*  mean that a pixel weight may be less than 1 even if all input pixels
*  are good).
      ELSE 
         WLIM = MIN( WLIM, 0.9999 )

      END IF

*  Get the co-ordinates of the centre of the PSF within the supplied
*  NDF.  The default is the centre of the NDF.
      CALL PAR_GDR0I( 'XCENTRE', ( SLBND2( 1 ) + SUBND2( 1 ) ) / 2,
     :                SLBND2( 1 ), SUBND2( 1 ), .FALSE., XCEN, STATUS )
      CALL PAR_GDR0I( 'YCENTRE', ( SLBND2( 2 ) + SUBND2( 2 ) ) / 2,
     :                SLBND2( 2 ), SUBND2( 2 ), .FALSE., YCEN, STATUS )

*  Get the required workspace.
      CALL PSX_CALLOC( NPIX * NLIN, '_DOUBLE', IPW2, STATUS )
      CALL PSX_CALLOC( NPIX * NLIN, '_DOUBLE', IPW3, STATUS )
      CALL PSX_CALLOC( NPIX * NLIN, '_DOUBLE', IPW4, STATUS )
      CALL PSX_CALLOC( 3 * MAX( NPIX, NLIN ) + 15, '_DOUBLE', IPW5, 
     :                 STATUS )

*  Call a lower level routine to do the work.
      CALL KPS1_CNVLV( VAR, DIM1( 1 ), DIM1( 2 ), %VAL( IP1( 1 ) ), 
     :                 %VAL( IP1( 2 ) ), DIM2( 1 ), DIM2( 2 ), 
     :                 %VAL( IP2 ), XCEN - SLBND2( 1 ) + 1, 
     :                 YCEN - SLBND2( 2 ) + 1, NPIX, NLIN, WLIM, 
     :                 %VAL( IP3( 1 ) ), %VAL( IP3( 2 ) ), BAD,
     :                 %VAL( IPW2 ), %VAL( IPW3 ), %VAL( IPW4 ),
     :                 %VAL( IPW5 ), STATUS )

*  Set the bad pixel flags for the output NDF components.
      CALL NDF_SBAD( BAD, INDF3, 'DATA', STATUS )
      IF ( VAR ) CALL NDF_SBAD( BAD, INDF3, 'VAR', STATUS )

*  Obtain a new title for the output NDF, with the default value
*  being the input array title.
      CALL KPG1_CCPRO( 'TITLE', 'Title', INDF1, INDF3, STATUS )
      
*  Jump to here if an error has occurred.
 999  CONTINUE

*  Release work space.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )
      CALL PSX_FREE( IPW3, STATUS )
      CALL PSX_FREE( IPW4, STATUS )
      CALL PSX_FREE( IPW5, STATUS )
      
*  If an error occurred, delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF3, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CONVOLVE_ERR2', 'CONVOLVE: Unable to '/
     :                 /'convolve two NDFs.', STATUS )
      END IF
      
      END
