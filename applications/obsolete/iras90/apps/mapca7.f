      SUBROUTINE MAPCA7( IDC, IDA, PIXSOL, BAND, NDETS, DETIND, BADIN,
     :                   WEIGHT, VAROUT, ACEN, BCEN, UNITS, OUNITS,
     :                   SAMPLO, DINDLO, SAMPHI, DINDHI, XLO,
     :                   YLO, XHI, YHI, C1, NZSECT, NYSECT, IPSECT,
     :                   IPINS, IPDOUT, IPVOUT, IPTEMP, PWGSZX, PWGSZY,
     :                   NX, NY, IPPWG, STATUS )
*+
*  Name:
*     MAPCA7

*  Purpose:
*     Add the data from a single CRDD file into the running-sum images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCA7( IDC, IDA, PIXSOL, BAND, NDETS, DETIND, BADIN,
*                  WEIGHT, VAROUT, ACEN, BCEN, UNITS, OUNITS, SAMPLO,
*                  DINDLO, SAMPHI, DINDHI, XLO, YLO, XHI, YHI, C1,
*                  NZSECT, NYSECT, IPSECT, IPINS, IPDOUT, IPVOUT,
*                  IPTEMP, PWGSZX, PWGSZY, NX, NY, IPPWG, STATUS )

*  Description:
*     The data from the input CRDD file is used to increment various
*     "running-sum" images. The output DATA array is used to store the
*     sum of weighted input surface brightness values at each pixel.
*     The temporary work array is used to store the sum of the weights
*     at each pixel.  The output VARIANCE array is used to store the
*     sum of the weighted variances at each pixel (if VAROUT=EXTERNAL)
*     or the sum of the weighted squared input surface brightness
*     values (if VAROUT=INTERNAL). If required, input variances can be
*     used to modify the weight of each input data sample to give
*     greater weight to the more accurate samples.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the input CRDD file.
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information defining the
*        position on the sky of each pixel in the output image.
*     PIXSOL = DOUBLE PRECISION (Given)
*        The nominal solid angle of output map pixels, in steradians.
*     BAND = INTEGER (Given)
*        IRAS waveband (1-4) of the CRDD.
*     NDETS = INTEGER (Given)
*        The number of detectors from which data is to be included in
*        the output map.
*     DETIND( NDETS ) = INTEGER (Given)
*        The detector indices of the detectors from which data is to be
*        included in the output map.
*     BADIN = LOGICAL (Given)
*        True if the input DATA or VARIANCE component may contain any
*        bad values.
*     WEIGHT = LOGICAL (Given)
*        True if the input variance values are to be used to modify the
*        weight of each CRDD sample to give more weight to the more
*        accurate samples.
*     VAROUT = CHARACTER * ( * ) (Given)
*        The sort of output variances required: EXTERNAL causes output
*        variances to be calculated on the basis of the input variance
*        values, INTERNAL causes output variances to be calculated on
*        the basis of the spread in input data values at each output
*        pixel, NONE causes no output variance values to be produced.
*     ACEN = DOUBLE PRECISION (Given)
*        The RA of the image centre, in radians.
*     BCEN = DOUBLE PRECISION (Given)
*        The DEC of the image centre, in radians.
*     UNITS = CHARACTER * ( * ) (Given)
*        The value of NDF component UNITS, from the current CRDD file.
*     OUNITS = CHARACTER * ( * ) (Given)
*        The value of NDF component UNITS, for the output image.
*     SAMPLO = INTEGER (Given)
*        The lower bound on sample number.
*     DINDLO = INTEGER (Given)
*        The lower bound on detector index.
*     SAMPHI = INTEGER (Given)
*        The upper bound on sample number.
*     DINDHI = INTEGER (Given)
*        The upper bound on detector index.
*     XLO = INTEGER (Given)
*        The lower pixel index bound on the first (X) axis of the
*        output NDF.
*     YLO = INTEGER (Given)
*        The lower pixel index bound on the second (Y) axis of the
*        output NDF.
*     XHI = INTEGER (Given)
*        The upper pixel index bound on the first (X) axis of the
*        output NDF.
*     YHI = INTEGER (Given)
*        The upper pixel index bound on the second (Y) axis of the
*        output NDF.
*     C1( 6 ) = REAL (Given)
*        The coefficients of a linear transformation between sector
*        indices and focal plane offsets from the centre of a full size
*        detector.
*     NZSECT = INTEGER (Given)
*        The number of sector in the cross-scan direction across a full
*        size detector.
*     NYSECT = INTEGER (Given)
*        The number of sector in the in-scan direction across a full
*        size detector.
*     IPSECT = INTEGER (Given)
*        A pointer to the mapped array of sector weights.
*     IPINS( 2 ) = INTEGER (Given)
*        Pointers to the mapped DATA and (if required) VARIANCE
*        components of the input CRDD file.
*     IPDOUT = INTEGER (Given)
*        A pointer to the mapped DATA component of the output NDF.
*     IPVOUT = INTEGER (Given)
*        A pointer to the mapped VARIANCE component of the output NDF
*        (if required).
*     IPTEMP = INTEGER (Given)
*        A pointer to a temporary work array, the same shape as the
*        output NDF.
*     PWGSZX  = INTEGER (Given)
*        The total no. of pixels per row in each pixel weight grid.
*     PWGSZY  = INTEGER (Given)
*        The total no. of rows in each pixel weight grid.
*     NX = INTEGER (Given)
*        The no. of evenly spaced offsets in X between detector centre
*        and pixel centre for which weight grids are to be created.
*     NY = INTEGER (Given)
*        The no. of evenly spaced offsets in Y between detector centre
*        and pixel centre for which weight grids are to be created.
*     IPPWG( NX, NY ) = INTEGER (Given and Returned)
*        Pointers to workspace holding the pixel weight grids. These
*        arrays will be expanded to encompass a full size detector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-NOV-1991 (DSB):
*        Original version.
*     18-MAR-1992 (DSB):
*        Output units "per pixel" added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'I90_DAT'          ! IRAS constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRI_PAR'          ! IRI constants.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER IDC
      INTEGER IDA
      DOUBLE PRECISION PIXSOL
      INTEGER BAND
      INTEGER NDETS
      INTEGER DETIND( NDETS )
      LOGICAL BADIN
      LOGICAL WEIGHT
      CHARACTER VAROUT*(*)
      DOUBLE PRECISION ACEN
      DOUBLE PRECISION BCEN
      CHARACTER UNITS*(*)
      CHARACTER OUNITS*(*)
      INTEGER SAMPLO
      INTEGER DINDLO
      INTEGER SAMPHI
      INTEGER DINDHI
      INTEGER XLO
      INTEGER YLO
      INTEGER XHI
      INTEGER YHI
      REAL C1( 6 )
      INTEGER NZSECT
      INTEGER NYSECT
      INTEGER IPSECT
      INTEGER IPINS( 2 )
      INTEGER IPDOUT
      INTEGER IPVOUT
      INTEGER IPTEMP
      INTEGER PWGSZX
      INTEGER PWGSZY
      INTEGER NX
      INTEGER NY

*  Arguments Given and Returned:
      INTEGER IPPWG( NX, NY )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRC_DETNO          ! Convert detector index to detector
                                 ! number.

*  Local Variables:
      REAL    C2( 6 )            ! Coefficients of linear transformation
                                 ! from sector indices to focal plane
                                 ! offsets (relative to current detector
                                 ! centre).
      INTEGER DET                ! Current detector index.
      INTEGER DETNO              ! Current detector number.
      LOGICAL DIVSOL             ! True if solid angle factor needs to
                                 ! be included when converting stored
                                 ! CRDD values to requested output units
      REAL    END( I90__MAXDT )  ! Last sample in common section.
      INTEGER IDET               ! Current index into list of detector
                                 ! indices to be used.
      REAL    MY                 ! Relative in-scan size of the
                                 ! current detector compared to a
                                 ! full-size detector.
      REAL    MZ                 ! Relative cross-scan size of the
                                 ! current detector compared to a
                                 ! full-size detector.
      LOGICAL NEW                ! True when mapping the first sample.
      REAL    SAMP0              ! Closest detector sample to the image
                                 ! centre.
      REAL    SFACT              ! Factor for converting flux values to
                                 ! surface brightness values.
      REAL    SFACT0             ! Factor for converting flux values to
                                 ! surface brightness values, excluding
                                 ! any solid angle factor.
      REAL    START( I90__MAXDT )! First sample in common section.
      REAL    VFACT              ! Factor for converting flux variances
                                 ! to surface brightness variances.
      REAL    WFACT              ! Factor to cancel the higher weight
                                 ! per pixel which smaller detectors
                                 ! would otherwise receive as a result
                                 ! of shrinking the sector array.
      REAL    ZFP                ! Focal plane Z coordinate of image
                                 ! centre at closest approach.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the scaling factor  (excluding solid angle factor) for
*  converting stored CRDD values to units of MJy/sr. Also determine if
*  the solid angle factor needs to be included.
      IF( UNITS .EQ. IRC__F ) THEN                ! pW/(M**2)
         SFACT0 = I90__JY( BAND )*10.0
         DIVSOL = .TRUE.

      ELSE IF( UNITS .EQ. IRC__FPS ) THEN        ! (pW/M**2)/sr
         SFACT0 = I90__JY( BAND )*1.0E-6
         DIVSOL = .FALSE.

      ELSE IF( UNITS .EQ. IRC__J ) THEN           ! Jy
         SFACT0 = 10.0
         DIVSOL = .TRUE.

      ELSE IF( UNITS .EQ. IRC__JPS ) THEN        ! Jy/sr
         SFACT0 = 1.0E-6
         DIVSOL = .FALSE.

      ELSE IF( UNITS .EQ. IRC__MJPS) THEN       ! MJy/sr
         SFACT0 = 1.0
         DIVSOL = .FALSE.

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'U', UNITS )
         CALL ERR_REP( 'MAPCA7_ERR1',
     :                 'MAPCA7: CRDD units "^U" not yet supported',
     :                 STATUS )
         GO TO 999

      END IF

*  Modify the flux scaling factor to produce the requested output units,
*  instead of MJy/sr.
      IF( OUNITS .EQ. IRI__JPS ) THEN                  ! Jy/sr
         SFACT0 = SFACT0*1.0E6

      ELSE IF( OUNITS .EQ. IRI__FPS ) THEN             ! (pW/M**2)/sr
         SFACT0 = SFACT0*1.0E6/I90__JY( BAND )

      ELSE IF( OUNITS .EQ. IRI__JPP ) THEN             ! Jy/PIXEL
         SFACT0 = SFACT0*1.0E6*PIXSOL

      ELSE IF( OUNITS .EQ. IRI__FPP ) THEN             ! (pW/M**2)/PIXEL
         SFACT0 = SFACT0*1.0E6*PIXSOL/I90__JY( BAND )

      ELSE IF( OUNITS .NE. IRI__MJPS ) THEN            ! MJy/sr
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'U', OUNITS )
         CALL ERR_REP( 'MAPCA7_ERR2',
     :                 'MAPCA7: Image units "^U" not yet supported',
     :                 STATUS )
         GO TO 999

      END IF

*  Find the limits of the scan section excluding the section at each end
*  which is not covered by all detectors.
      CALL IRC_TRUNC( IDC, NDETS, DETIND, START, END, STATUS )

*  Indicate that a new CRDD file is being pasted.
      NEW = .TRUE.

*  Loop round each detector index to be used.
      DO IDET = 1, NDETS
         DET = DETIND( IDET )

*  Get the corresponding detector number.
         DETNO = IRC_DETNO( IDC, DET, STATUS )

*  Find the sample number at which the detector track reaches its
*  closest approach to the image centre.
         CALL IRC_DCLAP( IDC, DET, ACEN, BCEN, SAMP0, ZFP, STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Store factors for converting sample flux and variance values from
*  the stored values to surface brightness values in the required
*  output units. Checks have already been done to ensure that the solid
*  angle is not zero.
         IF( DIVSOL ) THEN
            SFACT = SFACT0/I90__SOLAN( DETNO )
         ELSE
            SFACT = SFACT0
         END IF
         VFACT = SFACT**2

*  Calculate the factors by which to shrink the array of sector weights
*  so that it just covers the detector mask area.
         MZ = I90__DETDZ( DETNO )/I90__DZ( BAND )
         MY = I90__DETDY( DETNO )/I90__DY( BAND )

*  Create coefficients for a transformation from sector indices to
*  focal plane offsets which incorporate the above shrinkage.
         C2( 1 ) = C1( 1 )*MZ
         C2( 2 ) = C1( 2 )*MZ
         C2( 3 ) = C1( 3 )*MZ

         C2( 4 ) = C1( 4 )*MY
         C2( 5 ) = C1( 5 )*MY
         C2( 6 ) = C1( 6 )*MY

*  Calculate the factor by which the sector weights should be multiplied
*  to cancel the effect of smaller detectors having a higher weight per
*  pixel because of the shrinkage of the sector array. This means that
*  smaller samples have less total weight than large samples (which is
*  reasonable since they effect fewer pixels).
         WFACT = MZ*MY

*  The following block deals with cases where the input CRDD file may
*  contain bad pixels...
         IF( BADIN ) THEN

*  ... and where output variance values are required, calculated on the
*  basis of the input variance values...
            IF( VAROUT .EQ. 'EXTERNAL' ) THEN

*  ... and where input variance values are to be used to modify the
*  weight of each input data value.
               IF( WEIGHT ) THEN

                  CALL MAPCB0( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         VFACT, NINT( SAMP0 ),
     :                         NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         %VAL(CNF_PVAL(IPINS( 2 ))),
     :                         PWGSZX, PWGSZY, NX, NY, IPPWG,
     :                         NEW, %VAL( CNF_PVAL( IPDOUT ) ), 
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         %VAL( CNF_PVAL( IPTEMP ) ), STATUS )

*  If input variance values are NOT to be used to modify the weight of
*  each input data value...
               ELSE

                  CALL MAPCB1( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         VFACT, NINT( SAMP0 ),
     :                         NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         %VAL(CNF_PVAL(IPINS( 2 ))),
     :                         PWGSZX, PWGSZY, NX, NY, IPPWG,
     :                         NEW, %VAL( CNF_PVAL( IPDOUT ) ), 
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         %VAL( CNF_PVAL( IPTEMP ) ), STATUS )

               END IF

*  If output variance values are to be calculated on the basis of the
*  spread in input data values...
            ELSE IF( VAROUT .EQ. 'INTERNAL' ) THEN

*  ... and if input variance values are to be used to modify the
*  weight of each input data value.
               IF( WEIGHT ) THEN

                  CALL MAPCD0( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         VFACT, NINT( SAMP0 ),
     :                         NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         %VAL(CNF_PVAL(IPINS( 2 ))),
     :                         PWGSZX, PWGSZY, NX, NY, IPPWG,
     :                         NEW, %VAL( CNF_PVAL( IPDOUT ) ), 
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         %VAL( CNF_PVAL( IPTEMP ) ), STATUS )

*  If input variance values are NOT to be used to modify the weight of
*  each input data value...
               ELSE

                  CALL MAPCD1( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         NINT( SAMP0 ), NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         PWGSZX, PWGSZY, NX,
     :                         NY, IPPWG, NEW, 
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ), 
     :                         %VAL( CNF_PVAL( IPTEMP ) ), STATUS )

               END IF

*  The following block deals with cases where output variance values
*  are not required
            ELSE

*  ...and where input variance values are to be used to modify the
*  weight of each input data value.
               IF( WEIGHT ) THEN

                  CALL MAPCB2( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         VFACT, NINT( SAMP0 ),
     :                         NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         %VAL(CNF_PVAL(IPINS( 2 ))),
     :                         PWGSZX, PWGSZY, NX, NY, IPPWG,
     :                         NEW, %VAL( CNF_PVAL( IPDOUT ) ), 
     :                         %VAL( CNF_PVAL( IPTEMP ) ),
     :                         STATUS )

*  If input variance values are NOT to be used to modify the weight of
*  each input data value...
               ELSE

                  CALL MAPCB3( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         NINT( SAMP0 ), NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         PWGSZX, PWGSZY, NX, NY,
     :                         IPPWG, NEW, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPTEMP ) ), STATUS )


               END IF

            END IF

*  The following block deals with cases where the input CRDD file does
*  not contain any bad pixels...
         ELSE

*  ... and where output variance values are required, calculated on the
*  basis of the input variance values...
            IF( VAROUT .EQ. 'EXTERNAL' ) THEN

*  ... and where input variance values are to be used to modify the
*  weight of each input data value.
               IF( WEIGHT ) THEN

                  CALL MAPCB4( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         VFACT, NINT( SAMP0 ),
     :                         NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         %VAL(CNF_PVAL(IPINS( 2 ))),
     :                         PWGSZX, PWGSZY, NX, NY, IPPWG,
     :                         NEW, %VAL( CNF_PVAL( IPDOUT ) ), 
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         %VAL( CNF_PVAL( IPTEMP ) ), STATUS )

*  If input variance values are NOT to be used to modify the weight of
*  each input data value.
               ELSE

                  CALL MAPCB5( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         VFACT, NINT( SAMP0 ),
     :                         NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         %VAL(CNF_PVAL(IPINS( 2 ))),
     :                         PWGSZX, PWGSZY, NX, NY, IPPWG,
     :                         NEW, %VAL( CNF_PVAL( IPDOUT ) ), 
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         %VAL( CNF_PVAL( IPTEMP ) ), STATUS )

               END IF

*  If output variance values are to be calculated on the basis of the
*  spread of input data values...
            ELSE IF( VAROUT .EQ. 'INTERNAL' ) THEN

*  ... and where input variance values are to be used to modify the
*  weight of each input data value.
               IF( WEIGHT ) THEN

                  CALL MAPCD2( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         VFACT, NINT( SAMP0 ),
     :                         NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         %VAL(CNF_PVAL(IPINS( 2 ))),
     :                         PWGSZX, PWGSZY, NX, NY, IPPWG,
     :                         NEW, %VAL( CNF_PVAL( IPDOUT ) ), 
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         %VAL( CNF_PVAL( IPTEMP ) ), STATUS )

*  If input variance values are NOT to be used to modify the weight of
*  each input data value.
               ELSE

                  CALL MAPCD3( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         NINT( SAMP0 ), NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         PWGSZX, PWGSZY, NX,
     :                         NY, IPPWG, NEW, 
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ), 
     :                         %VAL( CNF_PVAL( IPTEMP ) ), STATUS )

               END IF

*  The following block deals with cases where output variance values
*  are not required.
            ELSE

*  .. and where input variance values are to be used to modify the
*  weight of each input data values.
               IF( WEIGHT ) THEN

                  CALL MAPCB6( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         VFACT, NINT( SAMP0 ),
     :                         NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         %VAL(CNF_PVAL(IPINS( 2 ))),
     :                         PWGSZX, PWGSZY, NX, NY, IPPWG,
     :                         NEW, %VAL( CNF_PVAL( IPDOUT ) ), 
     :                         %VAL( CNF_PVAL( IPTEMP ) ),
     :                         STATUS )

*  If input variance values are NOT to be used to modify the weight of
*  each input data value.
               ELSE

                  CALL MAPCB7( IDC, IDA, WFACT, DET, DETNO, SFACT,
     :                         NINT( SAMP0 ), NINT( START( IDET ) ),
     :                         NINT( END( IDET ) ), SAMPLO, DINDLO,
     :                         SAMPHI, DINDHI, XLO, YLO, XHI, YHI,
     :                         C2, NZSECT, NYSECT, 
     :                         %VAL( CNF_PVAL( IPSECT ) ),
     :                         %VAL(CNF_PVAL(IPINS( 1 ))), 
     :                         PWGSZX, PWGSZY, NX, NY,
     :                         IPPWG, NEW, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPTEMP ) ), STATUS )

               END IF

            END IF

         END IF

      END DO

 999  CONTINUE

      END
