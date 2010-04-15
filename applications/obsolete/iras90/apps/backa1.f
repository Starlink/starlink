      SUBROUTINE BACKA1( IDC, DHIGH, DLOW, SHIGH, SLOW, DATIN, DATTMP,
     :                   NSIGMA, UNITS, OUTBAC, OUTTYP, OK, QNAME, LOCS,
     :                   DATOUT, BAD, GRAD, OFFSET, DNLOW, WORK1, WORK2,
     :                   WORK3, WORK4, WORK5, STATUS )
*+
*  Name:
*     BACKA1

*  Purpose:
*     Remove a linear background from a single CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL BACKA1( IDC, DHIGH, DLOW, SHIGH, SLOW, DATIN, DATTMP, NSIGMA,
*                  UNITS, OUTBAC, OUTTYP, OK, QNAME, LOCS, DATOUT, BAD,
*                  GRAD, OFFSET, DNLOW, WORK1, WORK2, WORK3, WORK4,
*                  WORK5, STATUS )

*  Description:
*     The 2-dimensional input CRDD is collapsed to a single dimension
*     "cross scan data array" by finding the median cross-scan value at
*     each in-scan position.  Since the detectors have different
*     positions within the focal plane, a given sample number from two
*     different detectors will not in general come from the same
*     in-scan position on the sky. This in-scan shift between detectors
*     is taken into account when finding the median cross-scan values.
*     The sample values used in the median calculation are converted
*     from the input CRDD units to surface brightness in units of
*     Mega-Janskys per steradian.
*
*     A linear fit is then made to the cross scan data array. This fit
*     gives surface brightness against in-scan distance from the first
*     sample (in units of radians).
*
*     An attempt is then made to reduce the influence of bright sources
*     on this fit by rejecting any values in the cross scan data array
*     which are further than a specified limit from the linear fit. The
*     limit is specified as a multiple of the RMS deviation of the cross
*     scan data array from the linear fit. The multiple is given by
*     argument NSIGMA. This process is repeated until the RMS residual
*     reduces by less than 0.1% between iterations. The requested
*     output surface brightness after background subtraction
*     is subtracted from the fit. This final fit is then either
*     subtracted from the input data, or stored directly in the output
*     data,depending on the value of OUTTYP.
*
*     The parameters of the final linear background are returned in
*     arguments GRAD and OFFSET. The background (in Mega-Janskys per
*     steradian) is equal to ( OFFSET + GRAD*DIST ) where DIST is the
*     in-scan distance from sample SLOW (in the detector with index
*     DLOW) to the required point, measured positive in the same
*     direction as the focal plane Y axis, and given in units of
*     radians.

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
*        A copy of the input data from which has been removed (by being
*        set bad) any samples which do not satisfy the users quality
*        expression.
*     NSIGMA = REAL (Given)
*        The no. of standard deviations from the median at which data
*        should be rejected.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units of the input CRDD file.
*     OUTBAC = REAL (Given)
*        The required background surface brightness in the output data.
*        Given in units of Mega-Janskys per steradian.
*     OUTTYP = CHARACTER * ( * ) (Given)
*        The type of output required; DATA if the background subtracted
*        data is to be stored in DATOUT; BACKGROUND if the background
*        itself is to be stored in DATOUT.
*     OK = LOGICAL (Given)
*        True if the locators in LOCS are valid. If OK is supplied
*        false, then no quality is assigned to un-used samples.
*     QNAME = CHARACTER * ( * ) (Given)
*        A quality name to be assigned to all samples which do not
*        participate in the calculation of scan background. These
*        include samples which are bad in the input data, samples which
*        do not satisfy the given quality expression, and samples which
*        are rejected by the cleaning algorithm.
*     LOCS( 5 ) = CHARACTER * ( * ) (Given)
*        An array of locators which relate to the quality information
*        stored in the output NDF. Not used if OK is false.
*     DATOUT( SLOW:SHIGH, DLOW:DHIGH ) = REAL (Returned)
*        The output data.
*     BAD = LOGICAL (Returned)
*        True if the output contains any bad values.
*     GRAD = REAL (Returned)
*        The gradient of the background, in units of Mega-Janskys per
*        steradian, per radian.
*     OFFSET = REAL (Returned)
*        The background, in units of Mega-Janskys per steradian, at
*        sample SLOW from detector with index DLOW.
*     DNLOW = INTEGER (Returned)
*        The detector number corresponding to index DLOW.
*     WORK1( SLOW:SHIGH ) = REAL (Returned)
*        Work space to hold the cross scan data array (i.e the median
*        cross-scan value at each in-scan position, in Mega-Janskys per
*        steradian).
*     WORK2( DLOW:DHIGH ) = REAL (Returned)
*        Work space to hold the scaling factors for converting data
*        from each detector in the input units, to units of
*        Mega-Janskys per steradian.
*     WORK3( DLOW:DHIGH ) = REAL (Returned)
*        Work space to hold the cross scan values which contribute to
*        each value in WORK1, in Mega-Janskys per steradian.
*     WORK4( SLOW:SHIGH ) = REAL (Returned)
*        Work space to hold a copy of the cross scan data array (WORK1)
*        in which points far from the current linear fit are set bad.
*     WORK5( SLOW:SHIGH ) = REAL (Returned)
*        Work space to hold the in-scan displacement (in the positive
*        focal plane Y direction) from sample SLOW of the detector with
*        index DLOW, to each of the in-scan positions for which WORK1
*        holds a value. In units of radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'MSG_PAR'          ! MSG constants.
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRA_PAR'          ! IRC constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.

*  Arguments Given:
      INTEGER IDC
      INTEGER DHIGH
      INTEGER DLOW
      INTEGER SHIGH
      INTEGER SLOW
      REAL DATIN( SLOW:SHIGH, DLOW:DHIGH )
      REAL DATTMP( SLOW:SHIGH, DLOW:DHIGH )
      REAL NSIGMA
      CHARACTER UNITS*(*)
      REAL OUTBAC
      CHARACTER OUTTYP*(*)
      LOGICAL OK
      CHARACTER QNAME*(*)
      CHARACTER LOCS(5)*(*)

*  Arguments Returned:
      REAL DATOUT( SLOW:SHIGH, DLOW:DHIGH )
      LOGICAL BAD
      REAL GRAD
      REAL OFFSET
      INTEGER DNLOW
      REAL WORK1( SLOW:SHIGH )
      REAL WORK2( DLOW:DHIGH )
      REAL WORK3( DLOW:DHIGH )
      REAL WORK4( SLOW:SHIGH )
      REAL WORK5( SLOW:SHIGH )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRC_DETNO          ! Detector number corresponding to a
                                 ! given detector index.

*  Local Constants:
      INTEGER MAXIT              ! Max. no. of iterations.
      PARAMETER( MAXIT = 20 )

      REAL TARGET                ! Fractional change in RMS between
      PARAMETER( TARGET = 0.001 )! iterations for convergence.

*  Local Variables:
      INTEGER DET                ! Current detector index.
      INTEGER DN                 ! Detector number corresponding to DN.
      INTEGER ISAMP              ! Integer sample number.
      INTEGER ITER               ! Current iteration.
      INTEGER NDET               ! The number of detectors in the input.
      INTEGER NGOOD1             ! No. of good samples in WORK1.
      INTEGER NGOOD3             ! No. of good samples in WORK3.
      INTEGER NGOOD4             ! No. of good samples in WORK4.
      INTEGER SAMP               ! Current sample number.
      INTEGER SAMP2              ! Sample from detector DET which is
                                 ! nearest to sample SAMP from detector
                                 ! DLOW.
      INTEGER SET                ! No. of samples with given quality.


      REAL BHIGH                 ! Background at end of scan.
      REAL BLOW                  ! Background at start of scan.
      REAL DATVAL                ! Data value.
      REAL DIST                  ! The in-scan distance from sample SLOW
                                 ! of detector DLOW, to the current
                                 ! sample. In radians, positive in same
                                 ! sense as the focal plane Y axis.
      REAL FITVAL                ! Current fit value.
      REAL FSAMP                 ! Sample no. from detector DET which
                                 ! is at the same in-scan position as
                                 ! sample SAMP from detector DLOW.
      REAL GRAD2                 ! The gradient of the background in the
                                 ! input units, per radian.
      REAL LRMS                  ! The value of RMS on the previous
                                 ! iterations.
      REAL OFF2                  ! The background at the start of the
                                 ! scan in the input units.
      REAL RMS                   ! The RMS residual between the cross
                                 ! scan data array and the current fit.
      REAL THRESH                ! Maximum acceptable surface
                                 ! brightness residual.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the scaling factors in WORK2 which convert data for each
*  detector from the input units to units of Mega-Janskys per
*  steradian.
      DO DET = DLOW, DHIGH
         DN = IRC_DETNO( IDC, DET, STATUS )
         CALL IRM_UNTCV( UNITS, IRC__MJPS, 1, DN, WORK2( DET ), STATUS )
      END DO

*  Store the number of detectors represented in the CRDD file.
      NDET = DHIGH - DLOW + 1

*  Initalise the count of good values in WORK1, which holds the median
*  cross-scan data values.
      NGOOD1 = 0

*  Loop round each sample, producing a one dimensional array holding
*  the median cross-scan value at each sample. The detector data stream
*  are shifted to remove the time difference between different data
*  streams. The detector with index DLOW is used as the reference
*  detector to which the others are aligned.
      DO SAMP = SLOW, SHIGH

*  Loop round each detector to obtain a set of data values (one for
*  each detector) which have the same in-scan position as sample number
*  SAMP from the detector with index DLOW.
         DO DET = DLOW, DHIGH

*  Copy the input data (excluding samples which do not satisfy the
*  supplied quality expression) to the output.
            DATOUT( SAMP, DET ) = DATTMP( SAMP, DET )

*  Find the sample number in the current detector which is at the same
*  in-scan position as sample SAMP in the first detector.
            CALL IRC_OFFST( IDC, REAL( SAMP ), DLOW, DET, 0.0, FSAMP,
     :                      STATUS )
            SAMP2 = NINT( FSAMP )

*  Store the data value at this sample from the current detector. The
*  data is converted into units of Mega-Janskys per steradian using the
*  scaling factors stored in WORK2.
            IF( SAMP2 .GE. SLOW .AND. SAMP2 .LE. SHIGH ) THEN
               DATVAL = DATTMP( SAMP2, DET )

               IF( DATVAL .NE. VAL__BADR ) THEN
                  WORK3( DET ) = DATVAL*WORK2( DET )
               ELSE
                  WORK3( DET ) = VAL__BADR
               END IF

            ELSE
               WORK3( DET ) = VAL__BADR
            END IF

         END DO

*  Find the median of these values, and store it in the one dimensional
*  "cross scan data array", WORK1.
         CALL IRM_MEDN( NDET, WORK3, WORK1( SAMP ), NGOOD3, STATUS )

*  Increment the number of good values in WORK1.
         IF( NGOOD3 .GT. 0 ) NGOOD1 = NGOOD1 + 1

*  Find the in-scan distance between this sample and the first sample
*  from the detector with index DLOW, and store in WORK5. The value is
*  in radians and is positive in the anti-scan direction.
         CALL IRC_DIST( IDC, REAL( SLOW ), DLOW, REAL( SAMP ), DLOW,
     :                  WORK5( SAMP ), STATUS )

      END DO

*  Abort if there are no good values in WORK1.
      IF( NGOOD1 .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'BACKA1_ERR1',
     :                 'BACKA1: This CRDD file contains no good data',
     :                 STATUS )
         GO TO 999
      END IF

*  Fit a straight line giving the cross scan data value in terms of the
*  in-scan position in radians. The RMS residual between the data and
*  the fit is returned, together with the gradient and zero-point
*  offset of the fit.
      CALL IRM_FIT2( SLOW, SHIGH, WORK1, WORK5, GRAD, OFFSET, RMS,
     :               STATUS )

*  Display the original RMS residual.
      CALL MSG_SETR( 'RMS', RMS )
      CALL MSG_OUTIF( MSG__VERB, 'BACKA1_MSG1',
     :                '    Iteration 0: RMS residual is ^RMS',
     :                STATUS )

*  Loop round rejecting outlying samples from the straight line fit,
*  until the RMS changes by less than 0.1% between succesive iterations.
      LRMS = 0.0
      ITER = 0

      DO WHILE( ABS( ( RMS - LRMS )/( RMS + LRMS ) ) .GT. 0.5*TARGET
     :         .AND. ITER .LE. MAXIT .AND. STATUS .EQ. SAI__OK )
         ITER = ITER + 1

*  Create a copy (in WORK4) of the cross-scan data array in which
*  samples which have an unacceptably large deviation from the fitted
*  line are set bad. At the same time, make a count of the number of
*  good values remaining in WORK4.
         NGOOD4 = 0
         THRESH = NSIGMA*RMS
         DO SAMP = SLOW, SHIGH

            DATVAL = WORK1( SAMP )
            IF( DATVAL. NE. VAL__BADR ) THEN
               FITVAL = GRAD*WORK5( SAMP ) + OFFSET

               IF( ABS( DATVAL - FITVAL ) .LT. THRESH ) THEN
                  WORK4( SAMP ) = DATVAL
                  NGOOD4 = NGOOD4 + 1
               ELSE
                  WORK4( SAMP ) = VAL__BADR
               ENDIF

            ELSE
               WORK4( SAMP ) = VAL__BADR
            END IF

         END DO

*  Abort if no good data remains in WORK4.
         IF( NGOOD4 .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'BACKA1_ERR2',
     :         'BACKA1: All data has been rejected from this CRDD file',
     :                    STATUS )
            GO TO 999
         END IF

*  Save the old RMS value.
         LRMS = RMS

*  Fit a straight line to the remaining data.
         CALL IRM_FIT2( SLOW, SHIGH, WORK4, WORK5, GRAD, OFFSET, RMS,
     :                  STATUS )

*  Display the current RMS residual.
         CALL MSG_SETI( 'I', ITER )
         CALL MSG_SETI( 'REJ', NGOOD1 - NGOOD4 )
         CALL MSG_SETR( 'RMS', RMS )
         CALL MSG_OUTIF( MSG__VERB, 'BACKA1_MSG2',
     : '    Iteration ^I: RMS residual is ^RMS (^REJ values rejected)',
     :                   STATUS )

      END DO

*  Find the detector number for detector index DLOW.
      DNLOW = IRC_DETNO( IDC, DLOW, STATUS )

*  Store the background at the start and end of the scan.
      BLOW = GRAD*WORK5( SLOW ) + OFFSET
      BHIGH = GRAD*WORK5( SHIGH ) + OFFSET

*  Display the background at the start of the scan, and the amount by
*  which it changes along the length of the scan.
      CALL MSG_BLANKIF( MSG__VERB, STATUS )

      CALL MSG_SETR( 'B', BLOW )
      CALL MSG_SETC( 'U', IRC__MJPS )
      CALL MSG_OUTIF( MSG__NORM, 'BACKA1_MSG3',
     :        '    Background at the start of the scan is ^B ^U',
     :                STATUS )

      IF( BHIGH .GT. BLOW ) THEN
         CALL MSG_SETR( 'B', BHIGH - BLOW )
         CALL MSG_SETC( 'U', IRC__MJPS )
         CALL MSG_SETR( 'L', REAL( ABS( WORK5( SHIGH ) - WORK5( SLOW ) )
     :                  *IRA__R2AM ) )
         CALL MSG_OUTIF( MSG__NORM, 'BACKA1_MSG4',
     :    '    Background increases by ^B ^U along the length of the '//
     :    'scan (^L arc-mins).', STATUS )

      ELSE
         CALL MSG_SETR( 'B', BLOW - BHIGH )
         CALL MSG_SETC( 'U', IRC__MJPS )
         CALL MSG_SETR( 'L', REAL( ABS( WORK5( SHIGH ) - WORK5( SLOW ) )
     :                  *IRA__R2AM ) )
         CALL MSG_OUTIF( MSG__NORM, 'BACKA1_MSG5',
     :    '    Background decreases by ^B ^U along the length of the '//
     :    'scan (^L arc-mins).', STATUS )

      END IF

*  DATOUT contains a copy of the input data with sample which do not
*  satisfy the supplied quality expression set bad. In addition, set bad
*  those samples which correspond to bad values in the cross scan data
*  array. Loop round each index in the cross scan data array.
      IF( OK ) THEN
         DO SAMP = SLOW, SHIGH

*  If this cross scan data value is BAD, identify the samples from each
*  detector which contributed to it.
            IF( WORK4( SAMP ) .EQ. VAL__BADR ) THEN
               DO DET = DLOW, DHIGH

*  Find the sample number from the current detector which contributed to
*  the current cross scan data value.
                  CALL IRC_OFFST( IDC, REAL( SLOW ), DLOW, DET,
     :                            WORK5( SAMP ), FSAMP, STATUS )
                  ISAMP = NINT( FSAMP )

*  If this sample is within the bounds of the NDF, flag the sample
*  with a bad value in the output.
                  IF( ISAMP .GE. SLOW .AND. ISAMP .LE. SHIGH ) THEN
                     DATOUT( ISAMP, DET ) = VAL__BADR
                  END IF

               END DO

            END IF

         END DO

*  Assign a quality to the samples which were not included in the
*  estimation of the background.
         CALL IRQ_SETQM( LOCS, .TRUE., QNAME, NDET*( SHIGH - SLOW + 1 ),
     :                   DATOUT, SET, STATUS )

*  Tell the user how many samples were excluded from the background
*  estimation.
         CALL MSG_SETI( 'NEXCL', SET )
         CALL MSG_OUTIF( MSG__VERB, 'BACKA1_MSG6',
     :          '    ^NEXCL samples excluded from the calculation of '//
     :          'background', STATUS )

      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Modify the offset of the fit so that the background surface
*  brightness after background subtraction has the requested value.
      OFFSET = OFFSET - OUTBAC

*  Initalise the flag used to indicate if there are any bad values in
*  the output array.
      BAD = .FALSE.

*  If required, subtract the background from every input value and
*  store in the output array.
      IF( OUTTYP .EQ. 'DATA' ) THEN

*  Do every detector in turn.
         DO DET = DLOW, DHIGH

*  Convert gradient and offset to give values in the same units as the
*  input CRDD file.
            GRAD2 = GRAD/WORK2( DET )
            OFF2 = OFFSET/WORK2( DET )

*  Loop round every good sample from this detector.
            DO SAMP = SLOW, SHIGH
               DATVAL = DATIN( SAMP, DET )
               IF( DATVAL .NE. VAL__BADR ) THEN

*  Find the in-scan distance between sample SLOW from detector DLOW and
*  the current detector sample.
                  CALL IRC_DIST( IDC, REAL( SLOW ), DLOW, REAL( SAMP ),
     :                           DET, DIST, STATUS )

*  Subtract the fit value from the input value.
                  DATOUT( SAMP, DET ) = DATVAL - ( GRAD2*DIST + OFF2 )

*  Store a bad value in the output if the input is bad.
               ELSE
                  DATOUT( SAMP, DET ) = VAL__BADR
                  BAD = .TRUE.
               END IF

            END DO

         END DO

*  Otherwise, fill the output array with the background value (excluding
*  samples which are bad in the input).
      ELSE
         DO DET = DLOW, DHIGH
            GRAD2 = GRAD/WORK2( DET )
            OFF2 = OFFSET/WORK2( DET )

            DO SAMP = SLOW, SHIGH
               DATVAL = DATIN( SAMP, DET )
               IF( DATVAL .NE. VAL__BADR ) THEN

                  CALL IRC_DIST( IDC, REAL( SLOW ), DLOW, REAL( SAMP ),
     :                           DET, DIST, STATUS )
                  DATOUT( SAMP, DET ) = GRAD2*DIST + OFF2

               ELSE
                  DATOUT( SAMP, DET ) = VAL__BADR
                  BAD = .TRUE.
               END IF

            END DO

         END DO

      END IF

 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'BACKA1_ERR3',
     :   'BACKA1: Unable to remove a linear background from a CRDD '//
     :   'file.', STATUS )
      END IF

      END
