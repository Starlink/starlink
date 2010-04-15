      SUBROUTINE COLTEMP( STATUS )
*+
*  Name:
*     COLTEMP

*  Purpose:
*     Calculate colour temperature and optical depth maps.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COLTEMP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine takes two aligned images from different wavebands as
*     input, and creates two corresponding output images holding colour
*     temperature and optical depth. An optically thin greybody source
*     spectrum is assumed. The optical depth is assumed to vary as
*     frequency to the power BETA, where BETA is a constant supplied by
*     the user.  Temperatures outside the range 30 K to 10000 K cannot
*     be handled and cause corresponding bad values to be introduced
*     into the output NDFs. Negative values in either of the two input
*     images also cause bad output values to be created.
*
*     The input NDFs should be aligned pixel-for-pixel. If the bounds
*     of the two NDFs do not match, the output images cover just the
*     overlap area. Any QUALITY component present in the shorter
*     waveband NDF is propagated to the output NDFs. All extensions are
*     propagated from the short waveband NDF, but some information
*     describing the origin of the input image is deleted from the
*     IRAS extension.
*
*     The calculation of colour temperature and optical depth is based
*     on the use of the detector spectral response curves described in
*     the IRAS Catalogs and Atlases Explanatory Supplement, table
*     II.C.5. The uncertainty in these curves is not well known, but a
*     description of the likely ranges is given on page VI-28
*     (paragraph C3). The change in the calculated temperature and
*     optical depth values caused by varying the spectral response
*     curves slightly can be investigated using the parameters LERR and
*     RERR.
*
*     Variances for the calculated temperatures and optical depths can
*     be created if both input NDFs have VARIANCE components (see
*     parameter VAROUT). These variances do not take into account the
*     uncertainties in the detector spectral response curves, but just
*     describe the uncertainty in output data caused by the uncertainty
*     in the input data.

*  Usage:
*     COLTEMP IN1 IN2 BETA TEMP TAU WAVEL

*  ADAM Parameters:
*     BETA = _REAL (Read)
*        The emissivity spectral index. A value of zero causes a
*        blackbody source spectrum to be used.
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output NDF. See help on "History_in_IRAS90" for more
*        information on history.
*                                              [current history setting]
*     IN1 = NDF (Read)
*        The first input image.
*     IN2 = NDF (Read)
*        The second input image.
*     LERR = _REAL (Read)
*        Specifies a shift in wavelength to apply to the published
*        detector spectral response curves before using them. The
*        wavelength of each tabulated point is increased by the value
*        supplied. The value should be given in microns, and can be
*        positive or negative.                                     [0.0]
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     RERR = _REAL (Read)
*        Specifies a factor by which to multiply the published detector
*        spectral response values before using them.               [1.0]
*     TAU = NDF (Write)
*        An output image containing the optical depth at the wavelength
*        given by parameter WAVEL. The values stored in this image are
*        in units of 1.0E-16. Thus an NDF value of 9678.2 represents an
*        optical depth of 9678.2E-16. If a null value is supplied for
*        this parameter, no optical depth image is created.
*     TEMP = NDF (Write)
*        The temperature output image. Temperatures are stored in
*        Kelvin.
*     VAROUT = _LOGICAL (Read)
*        Specifies if VARIANCE components should be created in the
*        output NDFs. A warning is given if output variances are
*        requested but cannot be calculated. The run time default is
*        YES if both input NDFs have VARIANCE components, and NO
*        otherwise.                                                   []
*     WAVEL = _REAL (Read)
*        The wavelength at which the optical depth is required, in
*        microns. It can take any positive value (not just 12, 25, 60
*        and 100). This parameter is not used if parameter TAU is given
*        a null value.

*  Examples:
*     COLTEMP RING_B1 RING_B2 1.0 TEMP TAU 30
*        This evaluates colour temperature and optical depth maps from
*        the two images RING_B1 and RING_B2. The optical depth is
*        assumed to vary linearly with frequency (BETA=1.0), and the
*        optical depth is calculated at a wavelength of 30 microns.
*     COLTEMP RING_B1 RING_B2 1.0 TEMP TAU 30 LERR=0.3
*        This example is the same as the previous example except that
*        the published detector spectral response curves are shifted by
*        0.3 micron before being used. The change in the resulting
*        temperatures and optical depths gives some idea of the
*        uncertainty in their values.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-MAY-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'I90_DAT'          ! IRAS90 data
      INCLUDE 'IRI_PAR'          ! IRI_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :      LABEL*40,            ! Label for output NDF.
     :      XLOC*(DAT__SZLOC)    ! Locator to IRAS extension.

      DOUBLE PRECISION
     :      CC,                  ! Constant term in optical depth
                                 ! calculation.
     :      RATHI,               ! Limits of the domain of the
     :      RATLO                ! cubic spline fit to the model
                                 ! observed flux ratios.

      INTEGER
     :      BAND,                ! Temporary waveband index.
     :      BAND1,               ! Waveband index from first input.
     :      BAND2,               ! Waveband index from first input.
     :      EL,                  ! No. of elements in a mapped array.
     :      INDF,                ! Temporary identifier
     :      INDF1,               ! Identifier for first input NDF.
     :      INDF2,               ! Identifier for second input NDF.
     :      INDF3,               ! Identifier for output temp. NDF.
     :      INDF4,               ! Identifier for output opt. depth NDF.
     :      IPCKF1,              ! Pointer to workspace holding
                                 ! BAND1 flux cubic spline coefficients.
     :      IPCKR,               ! Pointer to workspace holding
                                 ! flux ratio cubic spline coefficients.
     :      IPIN1,               ! Pointer to mapped data array from
                                 ! first input NDF.
     :      IPIN2                ! Pointer to mapped data array from
                                 ! second input NDF.
      INTEGER
     :      IPLKF1,              ! Pointer to workspace holding
                                 ! BAND1 flux cubic spline knots.
     :      IPLKR,               ! Pointer to workspace holding
                                 ! flux ratio cubic spline knots.
     :      IPOD,                ! Pointer to mapped data array from
                                 ! output optical depth NDF.
     :      IPT,                 ! Pointer to mapped data array from
                                 ! output temperature NDF.
     :      IPVIN1,              ! Pointer to mapped variance array from
                                 ! first input NDF.
     :      IPVIN2,              ! Pointer to mapped variance array from
                                 ! second input NDF.
     :      IPVOD,               ! Pointer to mapped variance array from
                                 ! output optical depth NDF.
     :      IPVT,                ! Pointer to mapped variance array from
                                 ! output temperature NDF.
     :      LCK,                 ! No. of knots in in cubic spline.
     :      LLAB                 ! Used length of LABEL.

      LOGICAL
     :      BAD,                 ! True if there are any bad values in
                                 ! the output.
     :      VAR,                 ! True if output variances are to be
                                 ! created.
     :      VAR1,                ! True if INDF1 has variances.
     :      VAR2                 ! True if INDF2 has variances.

      REAL
     :      BETA,                ! Emisivity spectral index.
     :      LERR,                ! Wavelength shift for spectral
                                 ! response curves, in microns.
     :      RERR,                ! Gain factor for spectral response
                                 ! curves.
     :      SCALE,               ! Temporary factor
     :      SCALE1,              ! Factors which converts data values
     :      SCALE2,              ! from the input NDFs into pico-Watts
                                 ! per steradian.
     :      WAVEL                ! Wavelength at which optical depth is
                                 ! required, in microns.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the filter level for conditional message output.
      CALL MSG_IFGET( STATUS )

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Get the first surface brightness map.
      CALL NDF_ASSOC( 'IN1', 'READ', INDF1, STATUS )

*  Get a factor which converts the input units to units of pico-Watts
*  per square metre per steradian.
      CALL IRM_UNTIM( INDF1, IRI__FPS, SCALE1, BAND1, STATUS )

*  Now do the same for the second input image.
      CALL NDF_ASSOC( 'IN2', 'READ', INDF2, STATUS )
      CALL IRM_UNTIM( INDF2, IRI__FPS, SCALE2, BAND2, STATUS )

*  Abort if the two wavebands are equal.
      IF( BAND1 .EQ. BAND2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'W', I90__WAVEL( BAND 1 ) )
         CALL ERR_REP( 'COLTEMP_ERR1',
     : 'COLTEMP: Both input images are from the ^W um wave band',
     :                 STATUS )
         GO TO 999
      END IF

*  Ensure that the NDF1 refers to the shorter of the two wavebands.
      IF( BAND1 .GT. BAND2 ) THEN
         INDF  = INDF1
         SCALE = SCALE1
         BAND = BAND1

         INDF1  = INDF2
         SCALE1 = SCALE2
         BAND1 = BAND2

         INDF2  = INDF
         SCALE2 = SCALE
         BAND2 = BAND
      END IF

*  Now create sections from the two input images which cover the region
*  of overlap between the two images. This ensure that the two data
*  arrays from which temperatures are derived have the same shape and
*  size.
      CALL NDF_MBND( 'TRIM', INDF1, INDF2, STATUS )

*  Set flags if the input NDFs have defined variances.
      CALL NDF_STATE( INDF1, 'VAR', VAR1, STATUS )
      CALL NDF_STATE( INDF2, 'VAR', VAR2, STATUS )

*  See if the user wants the output NDFs to have VARIANCE components.
      CALL PAR_DEF0L( 'VAROUT', VAR1.AND.VAR2, STATUS )
      CALL PAR_GET0L( 'VAROUT', VAR, STATUS )

*  Warn the user if VARIANCE components were requested but no input
*  variances are available.
      IF( VAR .AND. .NOT. ( VAR1 .AND. VAR2 ) ) THEN
         CALL MSG_OUTIF( MSG__QUIET, 'COLTEMP_MSG1',
     :   'WARNING: Output variances cannot be produced because at '//
     :   'least one of the input NDFs have no variances', STATUS )
         VAR = .FALSE.
      END IF

*  Map the DATA components of both input NDFs.
      CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'READ', IPIN1, EL, STATUS )
      CALL NDF_MAP( INDF2, 'DATA', '_REAL', 'READ', IPIN2, EL, STATUS )

*  If required, map the VARIANCE components of both input NDFs.
      IF( VAR ) THEN
         CALL NDF_MAP( INDF1, 'VAR', '_REAL', 'READ', IPVIN1, EL,
     :                 STATUS )
         CALL NDF_MAP( INDF2, 'VAR', '_REAL', 'READ', IPVIN2, EL,
     :                 STATUS )
      END IF

*  Get the dust emissivity spectral index.
      CALL PAR_GET0R( 'BETA', BETA, STATUS )

*  Get the wavelength shift to apply to the published spectral response
*  curves.
      CALL PAR_GET0R( 'LERR', LERR, STATUS )

*  Get the GAIN FACTOR apply to the published spectral response
*  curves.
      CALL PAR_GET0R( 'RERR', RERR, STATUS )

*  Create a function which converts flux ratios into colour
*  temperatures. The function is returned in the form of a cubic
*  spline.
      CALL CTEMZ0( BAND1, BAND2, DBLE( BETA ), LERR, RERR, LCK, IPLKR,
     :             IPCKR, IPLKF1, IPCKF1, RATLO, RATHI, STATUS )

*  Create the temperature output by propagation from the shorter
*  wavelength  input.  The QUALITY component is the only component
*  propagated (together with all extensions).
      CALL NDF_PROP( INDF1, 'NOHISTORY,NOLABEL,NOTITLE,QUALITY', 'TEMP',
     :               INDF3, STATUS )

*  Delete the IMAGE_INFO structure from the IRAS extension in the
*  termperature map.
      CALL NDF_XLOC( INDF3, 'IRAS', 'UPDATE', XLOC, STATUS )
      CALL DAT_ERASE( XLOC, 'IMAGE_INFO', STATUS )
      CALL DAT_ANNUL( XLOC, STATUS )

*  Store label, title and units in the output temperature NDF.
      CALL NDF_CPUT( 'Colour Temperature', INDF3, 'LABEL', STATUS )
      CALL NDF_CPUT( 'Output from IRAS90:COLTEMP', INDF3, 'TITLE',
     :               STATUS )
      CALL NDF_CPUT( 'K', INDF3, 'UNITS', STATUS )

*  Map the DATA component of the output temperature map.
      CALL NDF_MAP( INDF3, 'DATA', '_REAL', 'WRITE', IPT, EL, STATUS )

*  If required, map the VARIANCE component of the output temperature
*  map.
      IF( VAR ) CALL NDF_MAP( INDF3, 'VAR', '_REAL', 'WRITE', IPVT, EL,
     :                        STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Create the optical depth output by propagation from the shorter
*  wavelength input. The QUALITY component is the only component
*  propagated (together with all extensions).
      CALL NDF_PROP( INDF1, 'NOHISTORY,NOLABEL,NOTITLE,QUALITY',
     :               'TAU', INDF4, STATUS )

*  If a null value was supplied, annul the error and set the constant
*  needed to evaluate optical depths to a nagative value to indicate
*  that no optical depth output is required.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CC = -1.0D0
         WAVEL = -1.0D0

*  If an output NDF was supplied...
      ELSE

*  Get the wavelength (in microns) at which the optical depth is to be
*  calculated for the output image.
         CALL PAR_GET0R( 'WAVEL', WAVEL, STATUS )

*  Check the wavelength is positive.
         IF( WAVEL .LE. 0.0 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETR( 'W', WAVEL )
            CALL ERR_REP( 'COLTEMP_ERR2',
     :                 'COLTEMP: Bad wavelength value (^W um) supplied',
     :                    STATUS )
         END IF

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set up the constant needed to evaluate optical depths.
         CC = (299.79D0/DBLE( WAVEL ) )**DBLE( BETA )

*  Delete the IMAGE_INFO structure from the IRAS xtension of the output
*  optical depth map.
         CALL NDF_XLOC( INDF4, 'IRAS', 'UPDATE', XLOC, STATUS )
         CALL DAT_ERASE( XLOC, 'IMAGE_INFO', STATUS )

*  Add a component to the IRAS extension giving the wavelength at
*  which the optical depth is calculated.
         CALL NDF_XPT0R( WAVEL, INDF4, 'IRAS', 'TAU_WAVELEN', STATUS )

*  Add a component to the IRAS extension giving the emissivity
*  spectral index.
         CALL NDF_XPT0R( BETA, INDF4, 'IRAS', 'TAU_BETA', STATUS )

*  Annul the locator to the IRAS extension.
         CALL DAT_ANNUL( XLOC, STATUS )

*  Store label and title in the output optical depth NDF.
         CALL MSG_SETR( 'L', WAVEL )
         CALL MSG_LOAD( ' ', 'Optical depth at ^L microns (x 1.0E16)',
     :                  LABEL, LLAB, STATUS )
         CALL NDF_CPUT( LABEL( : LLAB ), INDF4, 'LABEL', STATUS )
         CALL NDF_CPUT( 'Output from IRAS90:COLTEMP', INDF4, 'TITLE',
     :                  STATUS )

*  Map the data array
         CALL NDF_MAP( INDF4, 'DATA', '_REAL', 'WRITE', IPOD, EL,
     :                 STATUS )

*  If required, map the VARIANCE component.
         IF( VAR ) CALL NDF_MAP( INDF4, 'VAR', '_REAL', 'WRITE', IPVOD,
     :                           EL, STATUS )

      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Store the temperature values in the output DATA array. There are
*  separate routines to handle the two cases of output variances being
*  required or not required.
      IF( VAR ) THEN
         CALL CTEMZ4( EL, %VAL( IPIN1 ), %VAL( IPIN2 ), %VAL( IPVIN1 ),
     :                %VAL( IPVIN2 ), SCALE1, SCALE2, LCK,
     :                %VAL( IPCKR ), %VAL( IPLKR ), %VAL( IPCKF1 ),
     :                %VAL( IPLKF1 ), RATLO, RATHI, CC, %VAL( IPT ),
     :                %VAL( IPOD ), %VAL( IPVT ), %VAL( IPVOD ), BAD,
     :                STATUS )
      ELSE
         CALL CTEMZ2( EL, %VAL( IPIN1 ), %VAL( IPIN2 ), SCALE1, SCALE2,
     :                LCK, %VAL( IPCKR ), %VAL( IPLKR ), %VAL( IPCKF1 ),
     :                %VAL( IPLKF1 ), RATLO, RATHI, CC, %VAL( IPT ),
     :                %VAL( IPOD ), BAD, STATUS )
      END IF

*  Set the bad value flag for the output NDFs.
      CALL NDF_SBAD( BAD, INDF3, 'DATA', STATUS )
      IF( CC .GT. 0.0D0 ) CALL NDF_SBAD( BAD, INDF4, 'DATA', STATUS )

      IF( VAR ) THEN
         CALL NDF_SBAD( BAD, INDF3, 'VAR', STATUS )
         IF( CC .GT. 0.0D0 ) CALL NDF_SBAD( BAD, INDF4, 'VAR', STATUS )
      END IF

*  Store history in the outputs.
      CALL CTEMZ3( 'HISTORY', INDF1, INDF2, INDF3, INDF4, BAND1, BAND2,
     :             BETA, LERR, RERR, WAVEL, STATUS )

*  Release the workspace used to hold the spline knots and coefficients.
 999  CONTINUE
      CALL PSX_FREE( IPLKR, STATUS )
      CALL PSX_FREE( IPCKR, STATUS )
      CALL PSX_FREE( IPLKF1, STATUS )
      CALL PSX_FREE( IPCKF1, STATUS )

*  If an error has occurred, attempt to delete the output NDFs.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_DELET( INDF3, STATUS )
         CALL NDF_DELET( INDF4, STATUS )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If a parameter null or abort error exists, annul it.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COLTEMP_ERR3',
     :   'COLTEMP: Error generating a colour temperature map.',
     :   STATUS )
      END IF

      END
