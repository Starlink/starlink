      SUBROUTINE POLVEC( STATUS )
*+
*  Name:
*     POLVEC

*  Purpose:
*     Calculates polarisation vectors from supplied Stokes parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLVEC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine calculates values of percentage polarisation, polarisation 
*     angle, total intensity and polarised intensity, based on Stokes 
*     parameters in the supplied input cube (which will normally have been 
*     created by POLCAL). These calculated values may be stored either in
*     a series of 2-dimensional NDFs, or in a single table which may be
*     examined and manipulated using the CURSA package (see SUN/190).
*
*     The Stokes parameters may be binned before calculating the output
*     values.

*  Usage:
*     polvec in cat [p] [theta] [i] [ip]

*  ADAM Parameters:
*     BOX( 2 ) = _INTEGER (Read)
*        The x and y sizes (in pixels) of the bins to be used when
*        binning the supplied Stokes parameters prior to estimating the
*        polarisation vectors. If only a single value is given,
*        then it will be duplicated so that a square bin is used. A value
*        of 1 produces no binning. If a null (!) value is supplied, then a 
*        default is used which gives about 30 square bins along the longest 
*        axis. [!]
*     CAT = LITERAL (Read)
*        The name of a catalogue to create, holding the calculated
*        polarisation paremeters tabulated at each point for which Stokes
*        parameters are available. If a null (!) value is supplied, then
*        no catalogue is created. The catalogue will contain the following
*        columns (all stored as single precision _REAL values):
*
*           X     : The pixel X coordinate at the tabulated point.
*           Y     : The pixel Y coordinate at the tabulated point.
*           I     : The total intensity.
*           Q     : The Stokes Q parameter.
*           U     : The Stokes U parameter.
*           P     : The percentage polarisation.
*           THETA : The polarisation angle (in degrees).
*           PI    : The polarised intensity.
*
*        If VAR is TRUE, then the catalogue will also contain 
*        additional columns giving the standard deviation on each of the 
*        tabulated values (excluding the X and Y columns). The names of 
*        these columns will be formed by prepending the letter D to the
*        start of the column names listed above.
*
*        When measuring circular polarisation, the columns describing Q
*        and U will be replaced by equivalent columns describing V.
*
*        The coordinates contained in columns X and Y refer to pixel
*        coordinates after binning (i.e. to the pixel coordinate frames
*        of the output NDFs associated with parameters P, THETA, I, and 
*        IP). Information describing the mappings between this coordinate 
*        Frame and any other known coordinate Frames will be stored in the 
*        catalogue in textual form, as an AST FrameSet (see SUN/210). 
*
*        The storage format of the catalogue is determined by the "file
*        type" specified with the file name. If no file type is supplied,
*        the catalogue will be stored in the form of a FITS binary table
*        with file extension ".FIT". Other possibilities are described in
*        SUN/190. 
*     DEBIAS = _LOGICAL (Read)
*        TRUE if a correction for statistical bias is to be made to
*        percentage polarisation and polarised intensity. This correction
*        subtracts the variance of the percentage polarisation from the
*        squared percentage polarisation, and uses the square root of this
*        as the corrected percentage polarisation.  The corresponding
*        polarised intensity is then found by multiplying the corrected
*        percentage polarisation by the total intensity.  The returned 
*        variance values are unchanged. This correction only applies to
*        calculations of plane polarisation, and cannot be used if the 
*        input NDF does not contain variance values, or if you supply a 
*        FALSE value for parameter VARIANCE. If a null value (!) is
*        supplied, then the correction is applied if output variances
*        are being created, and not otherwise.           [!]
*     I = NDF (Write)
*        An output NDF holding the total intensity. A null value can be
*        supplied if this output image is not required. [!]
*     IN = NDF (Read)
*        The 3-d NDF holding the Stokes parameters. This should have been
*        created by POLCAL.
*     IP = NDF (Write)
*        An output NDF holding the polarised intensity. A null value can be
*        supplied if this output image is not required. [!]
*     METHOD = LITERAL (Read)
*        The method to be used when binning Stokes parameters. This may be 
*        set to any unique abbreviation of the following:
*           -  MEAN      -- Mean of the input data values
*           -  MEDIAN    -- Median of the input data values
*           -  SIGMA     -- A sigma clipped mean
*        [MEDIAN]
*     P = NDF (Write)
*        An output NDF holding percentage polarisation. A null value can be
*        supplied if this output image is not required. [!]
*     SIGMAS = _REAL (Read)
*        Number of standard deviations to reject data at. Only used if
*        METHOD is set to "SIGMA". [4.0]
*     THETA = NDF (Write)
*        An output NDF holding the polarisation angle in degrees. In the
*        the case of circular polarisation, a value of zero is stored 
*        if the normalised Stokes parameter V is positive, and a value
*        of 90 is stored otherwise. A null value can be supplied if this 
*        output image is not required.
*     VARIANCE = _LOGICAL (Read)
*        TRUE if output variances are to be calculated.  This parameter
*        is only accessed if all input NDFs contain variances, otherwise
*        no variances are generated.  [TRUE]
*     WLIM = _REAL (Read)
*        If the input cube contains bad pixels, then this parameter
*        may be used to determine the number of good Stokes parameters 
*        which must be present within each bin before a valid output vector 
*        is generated.  It can be used, for example, to prevent output 
*        vectors from being generated in regions where there are relatively 
*        few good Stokes parameters to contribute to the bin.
*
*        The value given for WLIM specifies the minimum fraction of 
*        good pixels which must be present in each bin in order to 
*        generate a good output vector. If this specified minimum fraction 
*        of good input pixels is not present, then a bad output vector 
*        will result. The value of this parameter should lie between 0.0 
*        and 1.0 (the actual number used will be rounded up if necessary 
*        to correspond to at least 1 pixel). [0.0]

*  Notes:
*     -  The output NDFs are deleted if there is an error during the
*     formation of the polarisation parameters.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER METH*6           ! Binning method
      CHARACTER STOKES*(NDF__MXDIM) ! Identifiers for each plane of input
      CHARACTER UNITS*40         ! Units from input Stokes cube
      INTEGER BOX( 2 )           ! Bin size
      INTEGER CI                 ! CAT identifier for output catalogue
      INTEGER DIM( 3 )           ! Dimensions of input Stokes cube
      INTEGER EL                 ! No. of elements in mapped arrays
      INTEGER I                  ! Loop count
      INTEGER INDF1              ! Identifier for input Stokes cube
      INTEGER INDFI              ! Identifier for total intensity output
      INTEGER INDFIP             ! Identifier for polarised int. output
      INTEGER INDFP              ! Identifier for % polarisation output
      INTEGER INDFT              ! Identifier for angle output
      INTEGER IPDBIN             ! Pointers to binned input DATA arrays
      INTEGER IPDIN              ! Pointers to input DATA arrays
      INTEGER IPI                ! Pointer to total intensity output
      INTEGER IPIA               ! Pointer to total int. (A) output
      INTEGER IPIAV              ! Pointer to total int. (A) variance
      INTEGER IPIB               ! Pointer to total int. (B) output
      INTEGER IPIBV              ! Pointer to total int. (B) variance
      INTEGER IPIP               ! Pointer to polarised int. output
      INTEGER IPIPV              ! Pointer to polarised int. variance
      INTEGER IPIV               ! Pointer to total intensity variance
      INTEGER IPP                ! Pointer to % polarisation output
      INTEGER IPPV               ! Pointer to % polarisation variance
      INTEGER IPQ                ! Pointer to Q output
      INTEGER IPQV               ! Pointer to Q variance
      INTEGER IPT                ! Pointer to angle output
      INTEGER IPTV               ! Pointer to angle variance
      INTEGER IPU                ! Pointer to U output
      INTEGER IPUV               ! Pointer to U variance
      INTEGER IPVBIN             ! Pointers to binned input VARIANCE arrays
      INTEGER IPVIN              ! Pointers to input VARIANCE arrays
      INTEGER IWCS               ! AST FrameSet holding o/p NDFs WCS component
      INTEGER LBND( 3 )          ! Lower bounds of input NDF
      INTEGER MINPIX             ! Min. no. of good input pixels per bin
      INTEGER NDFQ               ! Identifier for Q output
      INTEGER NDFU               ! Identifier for U output
      INTEGER NDIM               ! No. of dimensions in input Stokes cube
      INTEGER NVAL               ! No. of values obtained
      INTEGER NXBIN              ! No. of bins along X axis
      INTEGER NYBIN              ! No. of bins along Y axis
      INTEGER UBND( 3 )          ! Upper bounds of input NDF
      INTEGER WKBNSZ             ! Size of workspace for binned data
      LOGICAL DEBIAS             ! Statistical de-biassing required?
      LOGICAL MAKECT             ! Catalogue output required?
      LOGICAL MAKEI              ! Total intensity output required?
      LOGICAL MAKEIP             ! Polarised int. output required?
      LOGICAL MAKEP              ! % polarisation output required?
      LOGICAL MAKET              ! Angle output required?
      LOGICAL VAR                ! Output variances required?
      REAL NSIGMA                ! No. of sigmas to clip at
      REAL WLIM                  ! Min. fraction of good input pixels per bin
      DOUBLE PRECISION TR( 4 )   ! Coeffs. of linear mapping produced by binning
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
*  =====================

*  Get the input NDF holding the Stokes parameters.
      CALL NDF_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Get its bounds and dimensions.
      CALL NDF_BOUND( INDF1, 3, LBND, UBND, NDIM, STATUS ) 
      DIM( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      DIM( 2 ) = UBND( 2 ) - LBND( 2 ) + 1
      DIM( 3 ) = UBND( 3 ) - LBND( 3 ) + 1

*  Get the value of the STOKES component in the POLPACK extension. 
*  This is a string in which each character identifies the corresponding
*  plane in the DATA array.
      STOKES = ' '
      CALL NDF_XGT0C( INDF1, 'POLPACK', 'STOKES', STOKES, STATUS ) 
      IF( ( NDIM .NE. 3 .OR. STOKES .EQ. ' ' ) .AND. 
     :     STATUS .EQ. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF1 )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLVEC_ERR1', '''^NDF'' does not contain '//
     :                 'Stokes parameter values.', STATUS )
         GO TO 999
      END IF

*  See if the VARIANCE component is defined.
      CALL NDF_STATE( INDF1, 'VARIANCE', VAR, STATUS )

*  See if output NDFs are to have VARIANCE components, provided the
*  input NDF has variance arrays.
      IF ( VAR ) CALL PAR_GET0L( 'VARIANCE', VAR, STATUS )

*  Map the data array of the input NDF, and also the VARIANCE component
*  if required.
      CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'READ', IPDIN, EL, STATUS )
      IPVIN = IPDIN
      IF ( VAR ) CALL NDF_MAP( INDF1, 'VARIANCE', '_REAL', 'READ', 
     :                         IPVIN, EL, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Bin the Stokes parameters if required.
*  ======================================

*  Obtain the sizes of each bin.
      CALL PAR_GDRVI( 'BOX', 2, 1, VAL__MAXI, BOX, NVAL, STATUS )

*  Duplicate the value if only a single value was given.  
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NVAL .LT. 2 ) BOX( 2 ) = BOX( 1 )

*  If a null value was given, annull the error and use a default which 
*  results in about 30 vectors along the longest axis.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

         BOX( 1 ) = MAX( DIM( 1 ), DIM( 2 ) )/30
         BOX( 2 ) = BOX( 1 )         

      END IF

*  Obtain the dimensions and bounds of the binned arrays.
      NXBIN = DIM( 1 )/BOX( 1 )
      NYBIN = DIM( 2 )/BOX( 2 )
      LBND( 1 ) = 1
      LBND( 2 ) = 1
      UBND( 1 ) = NXBIN
      UBND( 2 ) = NYBIN

*  If the bin size is one on each axis, then just use the supplied DATA
*  and VARIANCE arrays.
      IF( BOX( 1 ) .EQ. 1 .AND. BOX( 2 ) .EQ. 1 ) THEN
         WKBNSZ = 0
         IPDBIN = IPDIN
         IPVBIN = IPVIN

*  If we are binning, allocate memory to hold the binned Stokes parameters, 
*  and their variances.
      ELSE
         WKBNSZ = NXBIN*NYBIN*DIM( 3 )
         CALL PSX_CALLOC( WKBNSZ, '_REAL', IPDBIN, STATUS )         
         IPVBIN = IPDBIN 
         IF( VAR ) CALL PSX_CALLOC( WKBNSZ, '_REAL', IPVBIN, STATUS )         

*  Get the binning method to use.
         CALL PAR_CHOIC( 'METHOD', ' ', 'MEDIAN,MEAN,SIGMA', .FALSE.,
     :                   METH, STATUS )

*  If using sigma clipping, get the number of sigmas to clip at.
         IF ( METH .EQ. 'SIGMA' ) THEN
            CALL PAR_GDR0R( 'SIGMAS', 4.0, 0.1, 100.0, .FALSE., NSIGMA, 
     :                      STATUS )
         END IF

*  Get the fraction of good input pixels required to create a good output 
*  pixel, and convert this to an absolute number of pixels.
         CALL PAR_GDR0R( 'WLIM', 0.0, 0.0, 1.0, .FALSE., WLIM, STATUS )
         MINPIX = NINT( REAL( BOX( 1 )*BOX( 2 ) )*WLIM )
         MINPIX = MAX( 1, MIN( BOX( 1 )*BOX( 2 ), MINPIX ) )

*  Do the binning.
         CALL POL1_STBIN( DIM( 1 ), DIM( 2 ), DIM( 3 ), %VAL( IPDIN ),
     :                    VAR, %VAL( IPVIN ), BOX, METH, MINPIX, NSIGMA,
     :                    NXBIN, NYBIN, %VAL( IPDBIN ), %VAL( IPVBIN ), 
     :                    TR, STATUS )

      END IF

*  Get a FrameSet which represents the WCS information of one of the binned
*  2D arrays.
      CALL POL1_GTWCS( INDF1, TR, IWCS, STATUS )

*  Obtain the total-intensity NDF.
*  ===============================

*  Attempt to get an output NDF to hold total intensity.
      CALL NDF_CREAT( 'I', '_REAL', 2, LBND, UBND, INDFI, STATUS ) 

*  If successful, set a flag indicating that a total-intensity NDF is to
*  be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEI = .TRUE.

*  Set up the WCS information 
         CALL NDF_PTWCS( IWCS, INDFI, STATUS )

*  Set the LABEL and TITLE in the output NDF to 'Total Intensity'.
         CALL NDF_CPUT( 'Total Intensity', INDFI, 'TITLE', STATUS )
         CALL NDF_CPUT( 'Total Intensity', INDFI, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( INDFI, 'DATA', '_REAL', 'WRITE', IPI, EL, 
     :                 STATUS )
         IPIV = IPI
         IF ( VAR ) CALL NDF_MAP( INDFI, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPIV, EL, STATUS )

*  If no total-intensity NDF was obtained, annul the error and set a
*  flag to indicate that no total intensity NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEI = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for percentage polarisation.
*  ===========================================

*  Attempt to get an output NDF to hold percentage polarisation.
      CALL NDF_CREAT( 'P', '_REAL', 2, LBND, UBND, INDFP, STATUS ) 

*  If successful, set a flag indicating that a percent-polarisation NDF
*  is to be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEP = .TRUE.

*  Set up the WCS information 
         CALL NDF_PTWCS( IWCS, INDFI, STATUS )

*  Set the LABEL and TITLE in the output NDF to 'Percentage polarisation'.
         CALL NDF_CPUT( 'Percentage Polarisation', INDFP, 'LABEL',
     :                  STATUS )
         CALL NDF_CPUT( 'Percentage Polarisation', INDFP, 'TITLE',
     :                  STATUS )

*  Store "%" in the output NDF UNITS component.
         CALL NDF_CPUT( '%', INDFP, 'UNITS', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( INDFP, 'DATA', '_REAL', 'WRITE', IPP, EL, 
     :                 STATUS )
         IPPV = IPP
         IF ( VAR ) CALL NDF_MAP( INDFP, 'VARIANCE', '_REAL', 'WRITE',
     :                           IPPV, EL, STATUS )

*  If no percent-polarisation NDF was obtained, annul the error and set
*  a flag to indicate that no percent polarisation NDF need be
*  produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEP = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for polarisation angle.
*  ======================================

*  Attempt to get an output NDF to hold polarisation angle.
      CALL NDF_CREAT( 'THETA', '_REAL', 2, LBND, UBND, INDFT, STATUS ) 

*  If successful, set a flag indicating that an angle NDF is to
*  be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKET = .TRUE.

*  Set up the WCS information 
         CALL NDF_PTWCS( IWCS, INDFI, STATUS )

*  Set the LABEL and TITLE in the output NDF to 'Polarisation Angle'.
         CALL NDF_CPUT( 'Polarisation Angle', INDFT, 'LABEL', STATUS )
         CALL NDF_CPUT( 'Polarisation Angle', INDFT, 'TITLE', STATUS )

*  Store "Degrees" in the output NDFs UNITS component.
         CALL NDF_CPUT( 'Degrees', INDFT, 'UNITS', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( INDFT, 'DATA', '_REAL', 'WRITE', IPT, EL, 
     :                 STATUS )
         IPTV = IPT
         IF ( VAR ) CALL NDF_MAP( INDFT, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPTV, EL, STATUS )

*  If no angle NDF was obtained, annul the error and set a flag to
*  indicate that no angle NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKET = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for polarised intensity.
*  =======================================

*  Attempt to get an output NDF to hold polarised intensity.
      CALL NDF_CREAT( 'IP', '_REAL', 2, LBND, UBND, INDFIP, STATUS ) 

*  If successful, set a flag indicating that a polarised-intensity NDF
*  is to be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEIP = .TRUE.

*  Set up the WCS information 
         CALL NDF_PTWCS( IWCS, INDFI, STATUS )

*  Set the LABEL and TITLE in the output NDF to 'Polarised Intensity'.
         CALL NDF_CPUT( 'Polarised Intensity', INDFIP, 'LABEL', STATUS )
         CALL NDF_CPUT( 'Polarised Intensity', INDFIP, 'TITLE', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( INDFIP, 'DATA', '_REAL', 'WRITE', IPIP, EL,
     :                 STATUS )
         IPIPV = IPIP
         IF ( VAR ) CALL NDF_MAP( INDFIP, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPIPV, EL, STATUS )

*  If no polarised-intensity NDF was obtained, annul the error and set a
*  flag to indicate that no polarised intensity NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEIP = .FALSE.
      END IF

*  Obtain the CAT catalogue to hold everything.
*  ============================================

*  Get the units string from the input cube.
      UNITS = ' '
      CALL NDF_CGET( INDF1, 'UNITS', UNITS, STATUS ) 
 
*  Create the catalogue.
      CALL POL1_MKCAT( 'CAT', ( INDEX( STOKES, 'V') .NE. 0 ), UNITS, 
     :                 VAR, CI, STATUS )

*  If successful, set a flag indicating that a catalogue is to be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKECT = .TRUE.

*  If no catalogue was obtained, annul the error and set a flag to indicate 
*  that no catalogue need be produced.
      ELSE 
         MAKECT = .FALSE.
         CALL CAT_TRLSE( CI, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check that there is an output NDF.
*  ==================================

*  Abort if no output images are required.
      IF ( .NOT. ( MAKEI .OR. MAKEIP .OR. MAKEP .OR. MAKET .OR. 
     :             MAKECT ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLVEC_ERR2', 'No output NDFs requested',
     :                 STATUS )
         GO TO 999
      END IF         

*  Decide whether or not a bias correction is needed and possible.
*  ===============================================================

*  See if a correction is to be made to the percentage polarisation to
*  correct for bias introduced as a result of the noise distribution not
*  being symmetric.
      CALL PAR_GET0L( 'DEBIAS', DEBIAS, STATUS )

*  If a null value is supplied, annull the error, and debias if the
*  varianes required are available.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         DEBIAS = VAR

*  Otherwise issue a warning if the user wants to debias the results and there 
*  are no variances available.
      ELSE IF ( DEBIAS .AND. ( .NOT. VAR ) .AND. 
     :          STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_OUT( 'POLVEC_ERR3', 'Vectors will not be '/
     :                 /'corrected for statistical bias because no '/
     :                 /'variance values are available.', STATUS )
         CALL ERR_FLUSH( STATUS )
         DEBIAS = .FALSE.
      END IF

*  Calculate the output arrays.
*  ============================

*  Call the routine to do the work.
      CALL POL1_PLVEC( LBND, NXBIN, NYBIN, DIM( 3 ), %VAL( IPDBIN ), 
     :                 %VAL( IPVBIN ), STOKES, DEBIAS, VAR, MAKEI, 
     :                 MAKEP, MAKET, MAKEIP, MAKECT, CI, %VAL( IPI ), 
     :                 %VAL( IPP ), %VAL( IPT ), %VAL( IPIP ), 
     :                 %VAL( IPIV ), %VAL( IPPV ), %VAL( IPTV ), 
     :                 %VAL( IPIPV ), STATUS )

*  Closedown.
*  ==========

*  Arrive here if an error occurs.
 999  CONTINUE

*  If a catalogue was created, store the WCS information with it as "textual
*  information", and release it.
      IF( MAKECT ) CALL POL1_CLCAT( IWCS, CI, STATUS )

*  Release any work space used to hold the binned Stokes parameters.
      IF( WKBNSZ .GT. 0 ) THEN
         CALL PSX_FREE( IPDBIN, STATUS )
         IF( VAR ) CALL PSX_FREE( IPVBIN, STATUS )
      END IF

*  If an error has occurred, delete the output NDFs.
      IF ( STATUS .NE. SAI__OK ) THEN 
         IF ( MAKEI ) CALL NDF_DELET( INDFI, STATUS )
         IF ( MAKEP ) CALL NDF_DELET( INDFP, STATUS )
         IF ( MAKET ) CALL NDF_DELET( INDFT, STATUS )
         IF ( MAKEIP ) CALL NDF_DELET( INDFIP, STATUS )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLVEC_ERR4', 'POLVEC: Error producing '//
     :                 'polarisation vectors.', STATUS )
      END IF

      END
