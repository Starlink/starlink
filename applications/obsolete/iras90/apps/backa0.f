      SUBROUTINE BACKA0( IDC, DHIGH, DLOW, SHIGH, SLOW, DATIN, DATTMP,
     :                   NSIGMA, UNITS, OUTBAC, OUTTYP, OK, QNAME, LOCS,
     :                   DATOUT, BAD, REMOVE, WORK1, WORK2, STATUS )
*+
*  Name:
*     BACKA0

*  Purpose:
*     Remove a constant background from a single CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL BACKA0( IDC, DHIGH, DLOW, SHIGH, SLOW, DATIN, DATTMP, NSIGMA,
*                  UNITS, OUTBAC, OUTTYP, OK, QNAME, LOCS, DATOUT, BAD,
*                  REMOVE, WORK1, WORK2, STATUS )

*  Description:
*     The background surface brightness in the input data is estimated
*     by a "robust" method in which successive median values are found,
*     excluding points which lay a long way from the previous median
*     value. If OUTTYP is DATA, this background is then subtracted from
*     the input data resulting in it having zero background, and the
*     requested output surface brightness is then added back on. If
*     OUTTYP is BACKGROUND, the output array is filled with the
*     background value.

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
*        set bad) any samples not satisying the users quality
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
*        participate in the calculation of the scan background. These
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
*     REMOVE = REAL (Returned)
*        The background surface brightness value for the CRDD, in
*        Mega-Janskys per steradian.
*     WORK1( SLOW:SHIGH, DLOW:DHIGH ) = REAL (Returned)
*        Work space.
*     WORK2( DLOW:DHIGH ) = REAL (Returned)
*        Work space.
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
      REAL REMOVE
      REAL WORK1( SLOW:SHIGH, DLOW:DHIGH )
      REAL WORK2( DLOW:DHIGH )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRC_DETNO          ! Detector number corresponding to a
                                 ! given detector index.

*  Local Constants:
      INTEGER MAXIT              ! Max. no. of iterations.
      PARAMETER( MAXIT = 20 )

      REAL TARGET                ! Fractional change in median between
      PARAMETER( TARGET = 0.001 )! iterations for convergence.

*  Local Variables:
      INTEGER DET                ! Current detector index.
      INTEGER DN                 ! Detector number corresponding to DN.
      INTEGER ITER               ! Iteration number.
      INTEGER NEL                ! Total no. of samples in the input.
      INTEGER NGOOD              ! No. of good samples remaining.
      INTEGER NGOOD0             ! No. of good samples in the input.
      INTEGER SAMP               ! Current sample number.


      REAL BACK                  ! The offset for the current detector.
      REAL DATVAL                ! Data value.
      REAL LRMS                  ! The RMS residual on the previous
                                 ! iteration.
      REAL MEDIAN                ! The median surface brightness.
      REAL RMS                   ! An estimate of the noise in the
                                 ! current detector.
      REAL SCALE                 ! Scale factor for converting sample
                                 ! values into different units.
      INTEGER SET                ! No. of samples with given quality.
      REAL SUMSQ                 ! Sum of the squared residuals.
      REAL THRESH                ! Maximum acceptable surface
                                 ! brightness residual.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each detector, producing a copy of the input data in
*  units of Mega-Janskys per steradian. The scaled data is stored in
*  WORK1 and the scale factors are stored in WORK2.
      DO DET = DLOW, DHIGH

*  Get the detector number stored at this index.
         DN = IRC_DETNO( IDC, DET, STATUS )

*  Get the scale factor for converting from the input units to units of
*  Mega-Janskys per steradian. This takes into account the effective
*  solid angle of the current detector (if necessary).
         CALL IRM_UNTCV( UNITS, IRC__MJPS, 1, DN, SCALE, STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Store the scale factor for later use.
         WORK2( DET ) = SCALE

*  Loop round each sample from this detector, applying the scaling
*  factor to all valid samples.
         DO SAMP = SLOW, SHIGH

            DATVAL = DATTMP( SAMP, DET )
            IF( DATVAL .NE. VAL__BADR ) THEN
               WORK1( SAMP, DET ) = DATVAL*SCALE

            ELSE
               WORK1( SAMP, DET ) = VAL__BADR

            END IF

         END DO

      END DO

*  Find the median surface brightness in the input data. Report an
*  error if no good data is found.
      NEL = ( SHIGH - SLOW + 1 )*( DHIGH - DLOW + 1 )
      CALL IRM_MEDN( NEL, WORK1, MEDIAN, NGOOD0, STATUS )

      IF( NGOOD0 .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'BACKA0_ERR1',
     :                 'BACKA0: This CRDD file contains no good data',
     :                 STATUS )
         GO TO 999
      END IF

*  Form the RMS residual between the median value and the input data.
      SUMSQ = 0.0

      DO DET = DLOW, DHIGH
         DO SAMP = SLOW, SHIGH

            DATVAL = WORK1( SAMP, DET )
            IF( DATVAL. NE. VAL__BADR ) THEN
               SUMSQ = SUMSQ + ( MEDIAN - DATVAL )**2
            END IF

         END DO
      END DO

      RMS = SQRT( SUMSQ/NGOOD0 )

*  Display the original RMS residual.
      CALL MSG_SETR( 'RMS', RMS )
      CALL MSG_OUTIF( MSG__VERB, 'BACKA0_MSG1',
     :                '    Iteration 0: RMS residual is ^RMS',
     :                STATUS )

*  Loop round rejecting outlying samples from the median estimate,
*  until two succesive median estimates agree to withiin 0.1%
      ITER = 0
      LRMS = 0.0

      DO WHILE( ABS( ( RMS - LRMS )/( RMS + LRMS ) ) .GT. 0.5*TARGET
     :          .AND. ITER .LE. MAXIT .AND. STATUS .EQ. SAI__OK )
         ITER = ITER + 1

*  Store a copy of the input data at every point at which the residual
*  between the input data and the median value is within the acceptable
*  range. Store a bad value at all other points.
         THRESH = NSIGMA*RMS

         DO DET = DLOW, DHIGH
            DO SAMP = SLOW, SHIGH

               DATVAL = WORK1( SAMP, DET )
               IF( DATVAL. NE. VAL__BADR ) THEN

                  IF( ABS( DATVAL - MEDIAN ) .LT. THRESH ) THEN
                     DATOUT( SAMP, DET ) = DATVAL
                  ELSE
                     DATOUT( SAMP, DET ) = VAL__BADR
                  ENDIF

               ELSE
                  DATOUT( SAMP, DET ) = VAL__BADR
               END IF

            END DO
         END DO

*  Find the median of the remaining good data. Report an error if no
*  good data is found.
         CALL IRM_MEDN( NEL, DATOUT, MEDIAN, NGOOD, STATUS )

         IF( NGOOD .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'BACKA0_ERR2',
     :         'BACKA0: All data has been rejected from this CRDD file',
     :                    STATUS )
            GO TO 999
         END IF

*  Save the previous RMS value.
         LRMS = RMS

*  Form the RMS residual between the new median value and the input
*  data.
         SUMSQ = 0.0

         DO DET = DLOW, DHIGH
            DO SAMP = SLOW, SHIGH

               DATVAL = WORK1( SAMP, DET )
               IF( DATVAL. NE. VAL__BADR ) THEN
                  SUMSQ = SUMSQ + ( MEDIAN - DATVAL )**2
               END IF

            END DO
         END DO

         RMS = SQRT( SUMSQ/NGOOD0 )

*  Display the current RMS residual.
         CALL MSG_SETI( 'I', ITER )
         CALL MSG_SETI( 'REJ', NGOOD0 - NGOOD )
         CALL MSG_SETR( 'RMS', RMS )
         CALL MSG_OUTIF( MSG__VERB, 'BACKA0_MSG2',
     : '    Iteration ^I: RMS residual is ^RMS (^REJ values rejected)',
     :                   STATUS )

      END DO

*  If required, assign a quality to the samples which were not included
*  in the estimation of the background value. DATOUT now holds a copy
*  of the input data in which all un-usable samples (i.e. samples which
*  are not to be used in the estimation of the background) have been
*  set bad.
      IF( OK ) CALL IRQ_SETQM( LOCS, .TRUE., QNAME, NEL, DATOUT, SET,
     :                         STATUS )

*  Tell the user how many samples have been rejected.
      CALL MSG_SETI( 'NEXCL', NEL - NGOOD )
      CALL MSG_OUTIF( MSG__VERB, 'BACKA0_MSG3',
     :          '    ^NEXCL samples excluded from the calculation of '//
     :          'background value', STATUS )

*  Display the background surface brightness.
      CALL MSG_BLANKIF( MSG__VERB, STATUS )
      CALL MSG_SETR( 'SB', MEDIAN )
      CALL MSG_SETC( 'U', IRC__MJPS )
      CALL MSG_OUTIF( MSG__NORM, 'BACKA0_MSG4',
     :                '    Background surface brightness is ^SB ^U',
     :                STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Store the background to be removed which results in the background
*  surface brightness having the requested value.
      REMOVE = MEDIAN - OUTBAC

*  Initalise the flag used to indicate if there are any bad values in
*  the output array.
      BAD = .FALSE.

*  If required, subtract a constant from every sample which
*  constant is converted from Mega-Janskys per steradian to the units
*  of the input data.
      IF( OUTTYP .EQ. 'DATA' ) THEN
         DO DET = DLOW, DHIGH
            BACK = REMOVE/WORK2( DET )

            DO SAMP = SLOW, SHIGH

               DATVAL = DATIN( SAMP, DET )
               IF( DATVAL .NE. VAL__BADR ) THEN
                  DATOUT( SAMP, DET ) = DATVAL - BACK
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
            BACK = REMOVE/WORK2( DET )

            DO SAMP = SLOW, SHIGH

               DATVAL = DATIN( SAMP, DET )
               IF( DATVAL .NE. VAL__BADR ) THEN
                  DATOUT( SAMP, DET ) = BACK
               ELSE
                  DATOUT( SAMP, DET ) = VAL__BADR
                  BAD = .TRUE.
               END IF

            END DO

         END DO

      END IF

 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'BACKA0_ERR3',
     :   'BACKA0: Unable to remove a constant background from a CRDD '//
     :   'file.', STATUS )
      END IF

      END
