      SUBROUTINE MAPCB1( IDC, IDA, WFACT, DET, DETNO, SFACT, VFACT,
     :                   SAMP0, START, END, SAMPLO, DINDLO, SAMPHI,
     :                   DINDHI, XLO, YLO, XHI, YHI, C2, NZSECT,
     :                   NYSECT, SECWGT, DATIN, VARIN, PWGSZX, PWGSZY,
     :                   NX, NY, IPPWG, NEW, DATOUT, VAROUT, WGTOUT,
     :                   STATUS )
*+
*  Name:
*     MAPCB1

*  Purpose:
*     Paste a detector data stream, assuming BAD pixels, no sample
*     weighting, and EXTERNAL output variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCB1( IDC, IDA, WFACT, DET, DETNO, SFACT, VFACT,
*                  SAMP0, START, END, SAMPLO, DINDLO, SAMPHI, DINDHI,
*                  XLO, YLO, XHI, YHI, C2, NZSECT, NYSECT, SECWGT,
*                  DATIN, VARIN, PWGSZX, PWGSZY, NX, NY, IPPWG, NEW,
*                  DATOUT, VAROUT, WGTOUT, STATUS )

*  Description:
*     This routine adds the data stored in a single detector data
*     stream to the "running-sum" images, DATOUT, VAROUT and WGTOUT.
*     Checks for BAD values are performed on the input data and
*     variance values.  Any sample for which either value is BAD is
*     excluded from the output image. Each input data sample is given
*     a total weight of unity (excluding the sector shrinkage factor,
*     WFACT). Output variances are created based on the input variance
*     values.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the input CRDD file.
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information defining the
*        position on the sky of each pixel in the output image.
*     WFACT = REAL (Given)
*        A factor by which to reduce the sector weights to ensure that
*        small detectors don't have greater weight per pixel than
*        full size detectors.
*     DET = INTEGER (Given)
*        The current detector index.
*     DETNO = INTEGER (Given)
*        The current detector number.
*     SFACT = REAL (Given)
*        The factor which converts the supplied flux values to the
*        required surface brightness values in the required output
*        units.
*     VFACT = REAL (Given)
*        The factor which scales the supplied variance values to take
*        account of the change in flux values caused by SFACT.
*     SAMP0 = INTEGER (Given)
*        The sample number at which the scan makes its closest approach
*        to the image centre.
*     START = INTEGER (Given)
*        The lowest sample number at which the scan no longer has
*        cross-scan coverage by all detectors.
*     END = INTEGER (Given)
*        The highest sample number at which the scan no longer has
*        cross-scan coverage by all detectors.
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
*     C2( 6 ) = REAL (Given)
*        The coefficients of a linear transformation between sector
*        indices and focal plane offsets from the centre of a full size
*        detector, incorporating shrinkage if the detector is less than
*        full size.
*     NZSECT = INTEGER (Given)
*        The number of sector in the cross-scan direction across a full
*        size detector.
*     NYSECT = INTEGER (Given)
*        The number of sector in the in-scan direction across a full
*        size detector.
*     SECWGT( NZSECT, NYSECT ) = REAL (Given)
*        The array of sector weights, normalized to a sum of unity.
*     DATIN( SAMPLO:SAMPHI, DINDLO:DINDHI ) = REAL (Given)
*        The input CRDD flux values.
*     VARIN( SAMPLO:SAMPHI, DINDLO:DINDHI ) = REAL (Given)
*        The variance in each input CRDD flux values.
*     PWGSZX  = INTEGER (Given)
*        The initial suggestion for the no. of pixels per row in each
*        pixel weight grid. This is increased if necessary as the
*        mapping proceeds.
*     PWGSZY  = INTEGER (Given)
*        The initial suggestion for the no. of rows in each pixel
*        weight grid. This is increased if necessary as the mapping
*        proceeds.
*     NX = INTEGER (Given)
*        The no. of evenly spaced offsets in X between detector centre
*        and pixel centre for which weight grids are to be created.
*     NY = INTEGER (Given)
*        The no. of evenly spaced offsets in Y between detector centre
*        and pixel centre for which weight grids are to be created.
*     IPPWG( NX, NY ) = INTEGER (Given and Returned)
*        Pointers to the pixel weight grids.
*     NEW = LOGICAL (Given and Returned)
*        True if a new CRDD file is being process. Returned false.
*     DATOUT( XLO:XHI, YLO:YHI ) = REAL (Given and Returned)
*        An image holding the sum of the weighted CRDD surface
*        brightness values. The input CRDD is added into this
*        array by this routine.
*     VAROUT( XLO:XHI, YLO:YHI ) = REAL (Given and Returned)
*        An image holding the sum of the weighted surface brightness
*        variances. The input weighted input variances are added into
*        this array by this routine.
*     WGTOUT( XLO:XHI, YLO:YHI ) = REAL (Given and Returned)
*        An image holding the sum of the weights used at each pixel in
*        DATOUT. The weights used for mapping the input CRDD are added
*        into this array by this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-NOV-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      INTEGER IDC
      INTEGER IDA
      REAL WFACT
      INTEGER DET
      INTEGER DETNO
      REAL SFACT
      REAL VFACT
      INTEGER SAMP0
      INTEGER START
      INTEGER END
      INTEGER SAMPLO
      INTEGER DINDLO
      INTEGER SAMPHI
      INTEGER DINDHI
      INTEGER XLO
      INTEGER YLO
      INTEGER XHI
      INTEGER YHI
      REAL C2( 6 )
      INTEGER NZSECT
      INTEGER NYSECT
      REAL SECWGT( NZSECT, NYSECT )
      REAL DATIN( SAMPLO:SAMPHI, DINDLO:DINDHI )
      REAL VARIN( SAMPLO:SAMPHI, DINDLO:DINDHI )
      INTEGER PWGSZX
      INTEGER PWGSZY
      INTEGER NX
      INTEGER NY

*  Arguments Given and Returned:
      INTEGER IPPWG( NX, NY )
      LOGICAL NEW
      REAL DATOUT( XLO:XHI, YLO:YHI )
      REAL VAROUT( XLO:XHI, YLO:YHI )
      REAL WGTOUT( XLO:XHI, YLO:YHI )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL    C3( 6 )            ! Coefficients of linear transformation
                                 ! from focal plane offsets (relative to
                                 ! current detector centre), to pixel
                                 ! coordinates.
      REAL    C3SAVE( 6 )        ! Coefficients of linear
                                 ! transformation from sector indices,
                                 ! to pixel coordinates used to create
                                 ! the last set of pixel weight grids.
      LOGICAL CHANGE             ! True if new pixel weight grids
                                 ! required.
      REAL    DATVAL             ! Input sample data value.
      INTEGER DCXIND             ! The X pixel index within the output
                                 ! map of the pixel containing the
                                 ! detector centre.
      INTEGER DCYIND             ! The Y  pixel index within the output
                                 ! map of the pixel containing the
                                 ! detector centre.
      INTEGER FIRST              ! The first sample number to be mapped.
      INTEGER GCXIND             ! The X pixel index within the weight
                                 ! grids of the pixel containing the
                                 ! detector centre.
      INTEGER GCYIND             ! The Y pixel index within the weight
                                 ! grids of the pixel containing the
                                 ! detector centre.
      INTEGER GINDX              ! The X index identifying the weight
                                 ! grid to be used.
      INTEGER GINDY              ! The Y index identifying the weight
                                 ! grid to be used.
      REAL    GOFFX              ! The offset from the left of the pixel
                                 ! holding the detector centre, to the
                                 ! detector centre, as a fraction of the
                                 ! pixel width.
      REAL    GOFFY              ! The offset from the bottom of the
                                 ! pixel holding the detector centre,
                                 ! to the detector centre, as a
                                 ! fraction of the pixel height.
      INTEGER INCR               ! Sample increment.
      LOGICAL INSIDE             ! True if the sample overlaps the image
                                 ! area.
      INTEGER LAST               ! The extreme sample number to be
                                 ! mapped.
      INTEGER MAPPED             ! Total no. of samples mapped.
      INTEGER NBAD               ! No. of bad samples mapped.
      INTEGER RHI                ! Upper row bound of the used region of
                                 ! the pixel weight grids.
      INTEGER RLO                ! Lower row bound of the used region of
                                 ! the pixel weight grids.
      INTEGER PHI                ! Upper pixel bound of the used region
                                 ! of the pixel weight grids.
      INTEGER PLO                ! Lower pixel bound of the used region
                                 ! of the pixel weight grids.
      INTEGER POFFX              ! The shift in X beteen weight grid and
                                 ! output map.
      INTEGER POFFY              ! The shift in Y beteen weight grid and
                                 ! output map.
      INTEGER SAMP               ! Current sample number.
      REAL    VARVAL             ! Input sample variance value.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the coefficients of the linear transformation which maps focal
*  plane offsets from the centre of sample SAMP0, to pixel coordinates.
      CALL IRC_IMCO( IDC, REAL( SAMP0 ), DET, IDA, NEW, C3SAVE, STATUS )

*  Indicate that the next call to IRC_IMCO will be for the same CRDD
*  file as the previous call. This prevents IRC_IMCO from
*  re-initialising its internal data.
      NEW = .FALSE.

*  Create a set of pixel grids holding the weights to use for each map
*  pixel influenced by sample SAMP0. There are several grids to cater
*  for different relative positionings of the sample centre within the
*  closest grid pixel. The sum of the weights in each grid is unity. The
*  grids are expended as necessary so that the entire sector array will
*  fit into them.
      RLO = 1
      RHI = PWGSZY
      PLO = 1
      PHI = PWGSZX

      CALL MAPCC5( C2, NZSECT, NYSECT, SECWGT, NX, NY, C3SAVE, PWGSZX,
     :             PWGSZY, RLO, RHI, PLO, PHI, IPPWG, GCXIND, GCYIND,
     :             STATUS )

*  Initialise the total number of samples, and the number of bad
*  samples, mapped from this detector.
      MAPPED = 0
      NBAD = 0

*  Set up the first and last sample numbers and the sample increment to
*  map samples obtained prior to the closest approach to the image
*  centre.
      FIRST = SAMP0
      LAST = START
      INCR = -1

 10   CONTINUE

*  Set a flag to indicate that the edge of the image has not yet been
*  reached.
      INSIDE = .TRUE.

*  Loop round each sample from the current detector until the edge of
*  the image is reached.
      DO SAMP = FIRST, LAST, INCR

*  Only proceed if the edge of the image has not yet been reached.
         IF( INSIDE ) THEN

*  Find the coefficients of the linear transformation which maps
*  focal plane offsets from the sample centre to pixel coordinates.
            CALL IRC_IMCO( IDC, REAL( SAMP ), DET, IDA, NEW, C3,
     :                     STATUS )

*  Find the indices of the map pixel in which the detector centre falls.
            DCXIND = NINT( C3( 1 ) + 0.5 )
            DCYIND = NINT( C3( 4 ) + 0.5 )

*  Use the position of the detector centre within the surrounding map
*  pixel to identify the best pixel weight grid to use.
            GOFFX = C3( 1 ) - REAL( DCXIND ) + 1.0
            GINDX = MIN( NX, INT( REAL( NX )*GOFFX ) + 1 )

            GOFFY = C3( 4 ) - REAL( DCYIND ) + 1.0
            GINDY = MIN( NY, INT( REAL( NY )*GOFFY ) + 1 )

*  See if any of the transformation coefficients have changed by more
*  than 2% (except for the coefficients giving the position of the
*  sample centre) since the last set of pixel weight grids were
*  calculated.
            CHANGE = .FALSE.

            IF( ABS( C3( 2 ) - C3SAVE( 2 ) )
     :          .GT. 0.02*ABS( C3SAVE( 2 ) ) ) THEN
               CHANGE = .TRUE.

            ELSE IF( ABS( C3( 3 ) - C3SAVE( 3 ) )
     :               .GT. 0.02*ABS( C3SAVE( 3 ) ) ) THEN
               CHANGE = .TRUE.

            ELSE IF( ABS( C3( 5 ) - C3SAVE( 5 ) )
     :               .GT. 0.02*ABS( C3SAVE( 5 ) ) ) THEN
               CHANGE = .TRUE.

            ELSE IF( ABS( C3( 6 ) - C3SAVE( 6 ) )
     :               .GT. 0.02*ABS( C3SAVE( 6 ) ) ) THEN
               CHANGE = .TRUE.

            END IF

*  If the transformation has changed, save the current transformation
*  coefficients, and calculate a new set of pixel weight grids.
            IF( CHANGE ) THEN

               C3SAVE( 2 ) = C3( 2 )
               C3SAVE( 3 ) = C3( 3 )
               C3SAVE( 5 ) = C3( 5 )
               C3SAVE( 6 ) = C3( 6 )

               CALL MSG_SETI( 'S', SAMP )
               CALL MSG_SETI( 'D', DET )
               CALL MSG_OUTIF( MSG__VERB, 'MAPCB1_MSG1',
     :'  Calculating new pixel weights at sample ^S, detector index ^D',
     :                         STATUS )

               CALL MAPCC5( C2, NZSECT, NYSECT, SECWGT, NX, NY, C3,
     :                      PWGSZX, PWGSZY, RLO, RHI, PLO, PHI,
     :                      IPPWG, GCXIND, GCYIND, STATUS )

            END IF

*  Find the offsets between pixel indices in the output map and the
*  weights grid.
            POFFX = DCXIND - GCXIND
            POFFY = DCYIND - GCYIND

*  Store the current sample data and variance values.
            DATVAL = DATIN( SAMP, DET )
            VARVAL = VARIN( SAMP, DET )

*  If the sample value or variance value is bad, pass on to the next
*  sample.
            IF( DATVAL .NE. VAL__BADR .AND.
     :          VARVAL .NE. VAL__BADR ) THEN

*  Convert the data value from flux to surface brightness in the
*  required output units. Modify the variance value accordingly.
               DATVAL = DATVAL*SFACT
               VARVAL = VARVAL*VFACT

*  Include the current sample in the running sum arrays.
               CALL MAPCC6( XLO, YLO, XHI, YHI, RLO, RHI, PLO, PHI,
     :                      POFFX, POFFY, WFACT, PWGSZX, PWGSZY,
     :                      %VAL( IPPWG( GINDX, GINDY ) ), DATVAL,
     :                      VARVAL, DATOUT, VAROUT, WGTOUT, INSIDE,
     :                      STATUS )


*  If the sample is bad...
            ELSE

*  ...see if the sample falls within the output map area.
               CALL MAPCC7( XLO, YLO, XHI, YHI, RLO, RHI, PLO, PHI,
     :                      POFFX, POFFY, PWGSZX, PWGSZY,
     :                      %VAL( IPPWG( GINDX, GINDY ) ), INSIDE,
     :                      STATUS )

*  If it does, increment the number of bad samples.
               IF( INSIDE ) NBAD = NBAD + 1

            END IF

*  If the edge of the image has been reached, increment the number of
*  samples mapped from this detector.
            IF( .NOT. INSIDE ) MAPPED = MAPPED + ABS( SAMP - FIRST )

         END IF

      END DO

*  If all samples were contained within the image area, then the
*  number of mapped samples will not yet have been incremented. Do so
*  now.
      IF( INSIDE ) MAPPED = MAPPED + ABS( LAST - FIRST ) + 1

*  Now loop round and map the samples taken after the closest approach
*  to the image centre.
      IF( INCR .EQ. -1 ) THEN
         FIRST = SAMP0 + 1
         LAST = END
         INCR = 1
         GO TO 10
      END IF

*  Conditionally display the number of samples mapped from this
*  detector.
      CALL MSG_SETI( 'D', DETNO )
      CALL MSG_SETI( 'N', MAPPED )
      CALL MSG_SETI( 'B',  NBAD )
      CALL MSG_OUTIF( MSG__VERB, 'MAPCB1_MSG2',
     :'    ^N samples mapped from detector #^D, of which ^B were bad.',
     :                STATUS )

      END
