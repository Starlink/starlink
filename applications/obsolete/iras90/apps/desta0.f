      SUBROUTINE DESTA0( IDC, DHIGH, DLOW, SHIGH, SLOW, DATIN, DATTMP, 
     :                   HBOX, NITER, NSIGMA, UNITS, WSLOW, WSHIGH, OK,
     :                   QNAME, LOCS, DATOUT, DETNO, DETOFF, BSB, NSUM,
     :                   BAD, WORK1, WORK2, WORK3, STATUS )
*+
*  Name:
*     DESTA0

*  Purpose:
*     Destripe a single CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DESTA0( IDC, DHIGH, DLOW, SHIGH, SLOW, DATIN, DATTMP, HBOX,
*                  NITER, NSIGMA, UNITS, WSLOW, WSHIGH, OK, QNAME, LOCS,
*                  DATOUT, DETNO, DETOFF, BSB, NSUM, BAD, WORK1, WORK2,
*                  WORK3, STATUS )

*  Description:
*      Each detector data stream is cleaned by removing bright sources
*      using an iterative comparison between the original data and a
*      smoothed version of the data. The median surface brightness of
*      the remaining data is found. Once this has been done for all
*      detectors, the mean of the median values is found. This is the
*      estimate of the mean background surface brightness in the CRDD
*      file. This value is subtracted from the median surface brightness
*      for each detector, to give the offsets to subtract from the
*      detectors. These offsets are converted to the units of the input
*      data before being used. This method results in the mean
*      background surface brightness in the output data being the same
*      as that in the input data.

*  Arguments:
*     IDC = INTEGER (Given)
*        IRC identifier for the input CRDD file.
*     DHIGH = INTEGER (Given)
*        The upper bound on detector index.
*     DLOW = INTEGER (Given)
*        The lower bound on detector index.
*     SHIGH = INTEGER (Given)
*        The upper bound on sample number.
*     SLOW = INTEGER (Given)
*        The lower bound on sample number.
*     DATIN( SLOW:SHIGH, DLOW:DHIGH ) = REAL (Given)
*        The input data.
*     DATTMP( SLOW:SHIGH, DLOW:DHIGH ) = REAL (Given)
*        A copy of the input data from which have been removed (by being
*        set bad) any samples which do not satisfy the users quality 
*        expression.
*     HBOX = INTEGER (Given)
*        Half the width of the smoothing box. The full width used is
*        2*HBOX+1.
*     NITER = INTEGER (Given)
*        The number of cleaning iterations to perform.
*     NSIGMA = REAL (Given)
*        The no. of standard deviations at which data should be rejected
*        during the cleaning algorithm.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units of the input CRDD file.
*     WSLOW = INTEGER (Given)
*        The lower bound on sample number in the work arrays. This
*        should be SLOW - HBOX - 1.
*     WSHIGH = INTEGER (Given)
*        The upper bound on sample number in the work arrays. This
*        should be SHIGH + HBOX.
*     OK = LOGICAL (Given)
*        True if the locators in LOCS are valid. If OK is supplied
*        false, then no quality is assigned to un-used samples.
*     QNAME = CHARACTER * ( * ) (Given)
*        A quality name to be assigned to all samples which do not
*        participate in the calculation of detector offsets. These
*        include samples which are bad in the input data, samples which
*        do not satisfy the given quality expression, and samples which
*        are rejected by the cleaning algorithm.
*     LOCS( 5 ) = CHARACTER * ( * ) (Given)
*        An array of locators which relate to the quality information
*        stored in the output NDF. Not used if OK is false.
*     DATOUT( SLOW:SHIGH, DLOW:DHIGH ) = REAL (Returned)
*        The output destriped data.
*     DETNO( DLOW:DHIGH ) = INTEGER (Returned)
*        The detector number of each detector in the CRDD file.
*     DETOFF( DLOW:DHIGH ) = REAL (Returned)
*        The offset subtracted from each detector.
*     BSB = REAL (Returned)
*        The mean background surface brightnesses, taken over all the 
*        detector data streams (in MJy/sr).
*     NSUM = INTEGER (Returned)
*        The number of values which were averaged together to form the
*        value returned in BSB.
*     BAD = LOGICAL (Returned)
*        True if the output contains any bad values.
*     WORK1( WSLOW:WSHIGH ) = REAL (Returned)
*        Work space for use by the cleaning algorithm.
*     WORK2( WSLOW:WSHIGH ) = REAL (Returned)
*        Work space for use by the cleaning algorithm.
*     WORK3( WSLOW:WSHIGH ) = INTEGER (Returned)
*        Work space for use by the cleaning algorithm.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'MSG_PAR'          ! MSG constants.
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRC_PAR'          ! IRC constants.

*  Arguments Given:
      INTEGER IDC
      INTEGER DHIGH
      INTEGER DLOW
      INTEGER SHIGH
      INTEGER SLOW
      REAL DATIN( SLOW:SHIGH, DLOW:DHIGH )
      REAL DATTMP( SLOW:SHIGH, DLOW:DHIGH )
      INTEGER HBOX
      INTEGER NITER
      REAL NSIGMA
      CHARACTER UNITS*(*)
      INTEGER WSLOW
      INTEGER WSHIGH
      LOGICAL OK
      CHARACTER QNAME*(*)
      CHARACTER LOCS(5)*(*)
      
*  Arguments Returned:
      REAL DATOUT( SLOW:SHIGH, DLOW:DHIGH )
      INTEGER DETNO( DLOW:DHIGH )
      REAL DETOFF( DLOW:DHIGH )
      REAL BSB
      INTEGER NSUM
      LOGICAL BAD
      REAL WORK1( WSLOW:WSHIGH )
      REAL WORK2( WSLOW:WSHIGH )
      INTEGER WORK3( WSLOW:WSHIGH )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRC_DETNO          ! Detector number corresponding to a 
                                 ! given detector index.

*  Local Variables:
      INTEGER DET                ! Current detector index.
      INTEGER DN                 ! Detector number corresponding to DN.
      INTEGER NGOOD              ! No. of good samples remaining.
      INTEGER NSAMP              ! Total no. of samples per detector.
      INTEGER SAMP               ! Current sample number.
      INTEGER SET                ! No. of samples with given quality.


      REAL INVAL                 ! Input data value.
      REAL OFFSET                ! The destriping offset for the
                                 ! current detector.
      REAL RMS                   ! Noise level in the detector data stream.
      REAL SBSUM                 ! Sum of median surface brightnesses.
      REAL SCALE                 ! Scale factor for converting sample
                                 ! values into different units.
      REAL TOTMED                ! The flux which corresponds to the
                                 ! mean median surface brightness.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark the error stack
      CALL ERR_MARK

*  Save the total number of samples per detector.
      NSAMP = SHIGH - SLOW + 1

*  Initialise the sum of the median surface brightnesses and the number
*  of values summed.
      SBSUM = 0.0
      NSUM = 0

*  Loop round all the detectors, finding the median of each detector
*  data stream after rejection of sources.
      DO DET = DLOW, DHIGH

*  Find the detector number stored at this detector index, and save it.
         DN = IRC_DETNO( IDC, DET, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

         DETNO( DET ) = DN

*  Clean the input data by removing sources which are (roughly
*  speaking) smaller than the box size. Removed samples are set bad.
         CALL IRM_CLEAN( SLOW, SHIGH, DATTMP( SLOW, DET ), WSLOW, 
     :                   WSHIGH, HBOX, NITER, NSIGMA, 
     :                   DATOUT( SLOW, DET ), RMS, WORK1, WORK2, WORK3, 
     :                   STATUS )

*  If there were no good values in the input data, annul the error and
*  set the detector offset to the bad value.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            DETOFF( DET ) = VAL__BADR

*  Tell the user that this detector contains no good data.
            CALL MSG_SETI( 'DN', DETNO( DET ) )
            CALL MSG_OUTIF( MSG__VERB, 'DESTA0_MSG1',
     :                      '    Detector #^DN contains no good data', 
     :                      STATUS )

*  Otherwise, find the median of the remaining good data.
         ELSE
            CALL IRM_MEDN( NSAMP, DATOUT( SLOW, DET ), DETOFF( DET ),
     :                     NGOOD, STATUS )

*  Tell the user the background value in this detector.
            CALL MSG_SETI( 'DN', DETNO( DET ) )
            CALL MSG_SETC( 'U', UNITS )
            CALL MSG_SETR( 'OFF', DETOFF( DET ) )
            CALL MSG_OUTIF( MSG__VERB, 'DESTA0_MSG2',
     :           '    Detector #^DN has a background value of ^OFF ^U', 
     :                      STATUS )

*  Calculate the scale factor for converting the median data value to
*  units of MJy/sr. The solid angle of the current detector is used.
            CALL IRM_UNTCV( UNITS, IRC__MJPS, 1, DN, SCALE, STATUS )

*  Increment the sum of the median surface brightnesses unless this is
*  a dead detector. Each value is given a weight equal to the number of
*  good samples remaining in the detector data stream after cleaning.
            IF( SCALE .NE. VAL__BADR ) THEN

               SBSUM = SBSUM + NGOOD*DETOFF( DET )*SCALE
               NSUM = NSUM + NGOOD

            ELSE
               DETOFF( DET ) = VAL__BADR

            END IF

         END IF

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Do the next detector.
      END DO

*  If required, assign a quality to the samples which were not included
*  in the estimation of the detector median value. DATOUT now holds a
*  copy of the input data in which all un-usable samples (i.e. samples
*  which are not to be used in the estimation of the destripe constants)
*  have been set bad.
      IF( OK ) CALL IRQ_SETQM( LOCS, .TRUE., QNAME,
     :                         NSAMP*( DHIGH - DLOW + 1 ), DATOUT, SET,
     :                         STATUS )

*  Display the total number of samples excluded from the estimation of
*  the destripe constants.
      CALL MSG_BLANKIF( MSG__VERB, STATUS )
      CALL MSG_SETI( 'NEXCL', NSAMP*( DHIGH - DLOW + 1 ) - NSUM )
      CALL MSG_OUTIF( MSG__VERB, 'DESTA0_MSG3',
     :          '    ^NEXCL samples excluded from the calculation of '//
     :          'detector offsets', STATUS )

*  Store the mean of the median surface brightness values. This is 
*  taken to be the background surface brightness.
      IF( NSUM .GT. 0 ) THEN
         BSB = SBSUM/NSUM

*  If no valid median values found, report an error and abort.
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'DESTA0_ERR1',
     :           'DESTA0: No usable data found in the input CRDD file.',
     :                 STATUS )
         GO TO 999
      END IF

*  Display the median surface brightness value.
      CALL MSG_SETR( 'B', BSB )
      CALL MSG_SETC( 'U', IRC__MJPS )
      CALL MSG_OUTIF( MSG__VERB, 'DESTA0_MSG4',
     :      '    Mean background surface brightness of all detectors '//
     :      'is ^B ^U', STATUS )

*  Loop round each detector, storing the destriped data.
      DO DET = DLOW, DHIGH

*  If no median is defined for this detector, store bad values in the
*  output.
         IF( DETOFF( DET ) .EQ. VAL__BADR ) THEN

            DO SAMP = SLOW, SHIGH
               DATOUT( SAMP, DET ) = VAL__BADR
            END DO

            BAD = .TRUE.

*  Otherwise, convert the mean of the median surface brightness values
*  to the same units as the input CRDD file, using this detectors solid
*  angle value.
         ELSE
            CALL IRM_UNTCV( IRC__MJPS, UNITS, 1, DETNO( DET ), SCALE,
     :                      STATUS )
            TOTMED = BSB*SCALE

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Evaluate and store the destriping offset for this detector, which
*  results in the mean surface brightness of the data within the CRDD
*  file remaining constant.
            OFFSET = DETOFF( DET ) - TOTMED
            DETOFF( DET ) = OFFSET

*  Subtract the offset from the input data values, and store in the
*  output data array.
            DO SAMP = SLOW, SHIGH         
               INVAL = DATIN( SAMP, DET )
               IF( INVAL .NE. VAL__BADR ) THEN
                  DATOUT( SAMP, DET ) = INVAL - OFFSET
               ELSE
                  DATOUT( SAMP, DET ) = VAL__BADR
                  BAD = .TRUE.
               END IF
            END DO

         END IF

*  Do the next detector.
      END DO

 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DESTA0_ERR2',
     :                 'DESTA0: Unable to destripe a CRDD file.',
     :                 STATUS )
      END IF

*  Release the error stack
      CALL ERR_RLSE

      END
