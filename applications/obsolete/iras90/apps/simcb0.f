      SUBROUTINE SIMCB0( IDC, IDA, DET, DETNO, SAMP0, SAMPLO, DINDLO,
     :                   SAMPHI, DINDHI, XLO, YLO, XHI, YHI, C2, PXLO,
     :                   PXHI, PYLO, PYHI, PSF, SKY, PWGSZX, PWGSZY,
     :                   SCALES, SCALEC, NX, NY, IPPWG, IPPWG2, NEW,
     :                   DATOUT, STATUS )
*+
*  Name:
*     SIMCB0

*  Purpose:
*     Create simulated CRDD for a single detector.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SIMCB0( IDC, IDA, DET, DETNO, SAMP0, SAMPLO, DINDLO, SAMPHI,
*                  DINDHI, XLO, YLO, XHI, YHI, C2, PXLO, PXHI, PYLO,
*                  PYHI, PSF, SKY, PWGSZX, PWGSZY, SCALES, SCALEC, NX,
*                  NY, IPPWG, IPPWG2, NEW, DATOUT, STATUS )

*  Description:
*      The PSF values for the detector are binned into several "pixel
*      weight grids". These grids consists of sets of pixels which are
*      the same shape and size as the pixels in the sky image. The
*      binning is done with the PSF centre located within the central
*      pixel of the weight grid, the exact position varies from grid to
*      grid. Each PSF value is binned by finding the closest pixel in
*      the weight grid and incrementing its value by the value of the
*      PSF value.
*
*      The idea is that when simulating a CRDD sample, the weight grid
*      is found which most nearly corresponds to the relative position
*      of the sample centre within the closest sky pixel. The values in
*      this grid are then multiplied by the corresponding sky values to
*      get the sample value.
*
*      These weight grids need are recalculated if the orientation of
*      the focal plane within sky images changes significantly.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information stored in the
*        sky image.
*     DET = INTEGER (Given)
*        The current detector index.
*     DETNO = INTEGER (Given)
*        The current detector number.
*     SAMP0 = INTEGER (Given)
*        The sample number at which the scan makes its closest approach
*        to the sky image centre.
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
*        sky image.
*     YLO = INTEGER (Given)
*        The lower pixel index bound on the second (Y) axis of the
*        sky image.
*     XHI = INTEGER (Given)
*        The upper pixel index bound on the first (X) axis of the
*        sky image.
*     YHI = INTEGER (Given)
*        The upper pixel index bound on the second (Y) axis of the
*        sky image.
*     C2( 6 ) = REAL (Given)
*        The coefficients of a linear transformation between PSF
*        indices and focal plane offsets from the centre of the
*        detector.
*     PXLO = INTEGER (Given)
*        The lower bound of PSF column numbers.
*     PXHI = INTEGER (Given)
*        The upper bound of PSF column numbers.
*     PYLO = INTEGER (Given)
*        The lower bound of PSF row numbers.
*     PYHI = INTEGER (Given)
*        The upper bound of PSF row numbers.
*     PSF( PXLO:PXHI, PYLO:PYHI ) = REAL (Given)
*        The array of psf values.
*     SKY( XLO:XHI, YLO:YHI ) = REAL (Given)
*        The sky image.
*     PWGSZX  = INTEGER (Given)
*        The initial suggestion for the no. of pixels per row in each
*        pixel weight grid. This is increased if necessary as the
*        mapping proceeds.
*     PWGSZY  = INTEGER (Given)
*        The initial suggestion for the no. of rows in each pixel
*        weight grid. This is increased if necessary as the mapping
*        proceeds.
*     SCALES = REAL (Given)
*        The factor which converts input sky values to units of Jy/sr.
*     SCALEC = REAL (Given)
*        Factor for converting values in Jy to the required output CRDD
*        units.
*     NX = INTEGER (Given)
*        The no. of evenly spaced offsets in X between detector centre
*        and pixel centre for which weight grids are to be created.
*     NY = INTEGER (Given)
*        The no. of evenly spaced offsets in Y between detector centre
*        and pixel centre for which weight grids are to be created.
*     IPPWG( NX, NY ) = INTEGER (Given and Returned)
*        Pointers to the pixel weight grids.
*     IPPWG2 = INTEGER (Given and Returned)
*        Pointer to a real work array of the same size as the pixel
*        weight grids.
*     NEW = LOGICAL (Given and Returned)
*        True if a new CRDD file is being process. Returned false.
*     DATOUT( SAMPLO:SAMPHI, DINDLO:DINDHI ) = REAL (Returned)
*        The simulated CRDD values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1993 (DSB):
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
      INCLUDE 'PRM_PAR'          ! Starlink data constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'I90_DAT'          ! IRAS90 data
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER IDC
      INTEGER IDA
      INTEGER DET
      INTEGER DETNO
      INTEGER SAMP0
      INTEGER SAMPLO
      INTEGER DINDLO
      INTEGER SAMPHI
      INTEGER DINDHI
      INTEGER XLO
      INTEGER YLO
      INTEGER XHI
      INTEGER YHI
      REAL C2( 6 )
      INTEGER PXLO
      INTEGER PXHI
      INTEGER PYLO
      INTEGER PYHI
      REAL PSF( PXLO:PXHI, PYLO:PYHI )
      REAL SKY( XLO:XHI, YLO:YHI )
      INTEGER PWGSZX
      INTEGER PWGSZY
      REAL SCALES
      REAL SCALEC
      INTEGER NX
      INTEGER NY

*  Arguments Given and Returned:
      INTEGER IPPWG( NX, NY )
      INTEGER IPPWG2
      LOGICAL NEW
      REAL DATOUT( SAMPLO:SAMPHI, DINDLO:DINDHI )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
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
      INTEGER INCR               ! Sample increment.
      INTEGER LAST               ! The extreme sample number to be
                                 ! mapped.
      INTEGER SIMED              ! Total no. of samples simulated.
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


      LOGICAL CHANGE             ! True if new pixel weight grids
                                 ! required.
      LOGICAL INSIDE             ! True if the sample is completely
                                 ! within the sky image.


      REAL C3( 6 )               ! Coefficients of linear transformation
                                 ! from focal plane offsets (relative to
                                 ! current detector centre), to pixel
                                 ! coordinates.
      REAL C3SAVE( 6 )           ! Coefficients of linear
                                 ! transformation from psf indices,
                                 ! to pixel coordinates used to create
                                 ! the last set of pixel weight grids.
      REAL FACTOR                ! Converts sky values to CRDD values.
      REAL GOFFX                 ! The offset from the left of the pixel
                                 ! holding the detector centre, to the
                                 ! detector centre, as a fraction of the
                                 ! pixel width.
      REAL GOFFY                 ! The offset from the bottom of the
                                 ! pixel holding the detector centre,
                                 ! to the detector centre, as a
                                 ! fraction of the pixel height.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the coefficients of the linear transformation which maps focal
*  plane offsets from the centre of sample SAMP0, to pixel coordinates
*  in the sky image.
      CALL IRC_IMCO( IDC, REAL( SAMP0 ), DET, IDA, NEW, C3SAVE, STATUS )

*  Indicate that the next call to IRC_IMCO will be for the same CRDD
*  file as the previous call. This prevents IRC_IMCO from
*  re-initialising its internal data.
      NEW = .FALSE.

*  Create a set of pixel grids holding the weights to use for each sky
*  pixel which influences sample SAMP0. There are several grids to cater
*  for different relative positionings of the sample centre within the
*  closest sky pixel. The sum of the weights in each grid is unity. The
*  grids are expanded as necessary so that the entire PSF will fit into
*  them.
      RLO = 1
      RHI = PWGSZY
      PLO = 1
      PHI = PWGSZX

      CALL SIMCC5( C2, PXLO, PXHI, PYLO, PYHI, PSF, NX, NY, C3SAVE,
     :             PWGSZX, PWGSZY, RLO, RHI, PLO, PHI, IPPWG, IPPWG2,
     :             GCXIND, GCYIND, STATUS )

*  Initialise the total number of samples, and the number of bad
*  samples, simulated from this detector.
      SIMED = 0
      NBAD = 0

*  Store a factor which converts units of input sky values to units of
*  output CRDD values. This incorporates the solid angle of the
*  detector.
      FACTOR = I90__SOLAN( DETNO )*1.0E-7*SCALES*SCALEC

*  Set up the first and last sample numbers and the sample increment, in
*  order to simulate samples obtained prior to the closest approach to
*  the image centre.
      FIRST = SAMP0
      LAST = SAMPLO
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
*  focal plane offsets from the sample centre to pixel coordinates in
*  the sky image.
            CALL IRC_IMCO( IDC, REAL( SAMP ), DET, IDA, NEW, C3,
     :                     STATUS )

*  Find the indices of the sky pixel in which the detector centre falls.
            DCXIND = NINT( C3( 1 ) + 0.5 )
            DCYIND = NINT( C3( 4 ) + 0.5 )

*  Use the position of the detector centre within the surrounding sky
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
               CALL MSG_OUTIF( MSG__VERB, 'SIMCB0_MSG1',
     :'  Calculating new pixel weights at sample ^S, detector index ^D',
     :                         STATUS )

               CALL SIMCC5( C2, PXLO, PXHI, PYLO, PYHI, PSF, NX, NY, C3,
     :                      PWGSZX, PWGSZY, RLO, RHI, PLO, PHI,
     :                      IPPWG, IPPWG2, GCXIND, GCYIND, STATUS )

            END IF

*  Find the offsets between pixel indices in the sky image and the
*  weights grid.
            POFFX = DCXIND - GCXIND
            POFFY = DCYIND - GCYIND

*  Generate the current simulated sample value.
            CALL SIMCC6( XLO, YLO, XHI, YHI, RLO, RHI, PLO, PHI,
     :                   POFFX, POFFY, PWGSZX, PWGSZY, FACTOR,
     :                   %VAL( CNF_PVAL( IPPWG( GINDX, GINDY ) ) ), 
     :                   SKY, NBAD,
     :                   DATOUT( SAMP, DET) , INSIDE, STATUS )

*  If the edge of the image has been reached, increment the number of
*  simulated samples generated for this detector.
            IF( .NOT. INSIDE ) SIMED = SIMED + ABS( SAMP - FIRST )

         END IF

      END DO

*  If all samples were contained within the image area, then the
*  number of simulated samples will not yet have been incremented. Do so
*  now.
      IF( INSIDE ) SIMED = SIMED + ABS( LAST - FIRST ) + 1

*  Now loop round and simulate the samples taken after the closest
*  approach to the image centre.
      IF( INCR .EQ. -1 ) THEN
         FIRST = SAMP0 + 1
         LAST = SAMPHI
         INCR = 1
         GO TO 10
      END IF

*  Conditionally display the number of samples simulated for this
*  detector.
      CALL MSG_SETI( 'D', DETNO )
      CALL MSG_SETI( 'N', SIMED )
      CALL MSG_SETI( 'B',  NBAD )
      CALL MSG_OUTIF( MSG__VERB, 'SIMCB0_MSG2',
     :      '    ^N samples simulated for detector #^D, of which ^B  '//
     :      'were bad.', STATUS )

      END
