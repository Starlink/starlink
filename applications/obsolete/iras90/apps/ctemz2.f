      SUBROUTINE CTEMZ2( EL, IN1, IN2, SCALE1, SCALE2, LCK, CKR, LKR,
     :                   CKF1, LKF1, RATLO, RATHI, CC, TEMP, OD, BAD,
     :                   STATUS )
*+
*  Name:
*     CTEMZ2

*  Purpose:
*     Generate output temperature and optical depth values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTEMZ2( EL, IN1, IN2, SCALE1, SCALE2, LCK, CKR, LKR, CKF1,
*                  LKF1, RATLO, RATHI, CC, TEMP, OD, BAD, STATUS )

*  Description:
*     A temperature and optical depth is found for each pair of good,
*     positive input pixel values. Bad output values are generated for
*     any positions at which either input is bad or negative. Bad
*     values are also created if the ratio of flux per steradin in BAND1
*     to that in BAND2 is outside the range specified by RATLO and
*     RATHI. For all other pixels, the temperature corresponding to the
*     ratio value is obtained using the cubic spline fit found by
*     routine CTEMZ0. The observed model flux per steradian at this
*     temperature is then found using the second cubic spline found by
*     CTEMZ0. The optical depth is proportional to the ratio of the
*     actual observed flux to the model observed flux.

*  Arguments:
*     EL = INTEGER (Given)
*        No. of pixels in each input image.
*     IN1( EL ) = REAL (Given)
*        Input image for the lower waveband.
*     IN2( EL ) = REAL (Given)
*        Input image for the higher waveband.
*     SCALE1 = REAL (Given)
*        A factor which converts the values supplied in IN1 into units
*        of pico-Watts per square metre per steradian.
*     SCALE2 = REAL (Given)
*        A factor which converts the values supplied in IN2 into units
*        of pico-Watts per square metre per steradian.
*     LCK = INTEGER (Given)
*        The number of knots in the cubic splines.
*     LKR = INTEGER (Given)
*        The positions of the knots for the cubic spline which gives
*        temperature as a function of flux ratio.
*     LKR = INTEGER (Given)
*        The coefficients for the cubic spline which gives temperature
*        as a function of flux ratio.
*     LKF1 = INTEGER (Given)
*        The positions of the knots for the cubic spline which gives
*        observed model flux in BAND1 as a function of temperature.
*     LKF1 = INTEGER (Given)
*        The coefficients for the cubic spline which gives observed
*        model flux in BAND1 as a function of temperature.
*     RATLO = DOUBLE PRECISION (Given)
*        The lowest usable flux ratio value.
*     RATHI = DOUBLE PRECISION (Given)
*        The highest usable flux ratio value.
*     CC = DOUBLE PRECISION (Given)
*        The constant of proportionality between the optical depth, and
*        the ratio of actual to model observed flux. If this is zero or
*        negative, no optical depth values are created.
*     TEMP( EL ) = REAL (Returned)
*        The colour temperatures.
*     OD( EL ) = REAL (Returned)
*        The optial depth values. Not used if CC is less than or equal
*        to zero.
*     BAD = LOGICAL (Returned)
*        True if any bad output values have been created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
      INCLUDE 'PRM_PAR'          ! VAL_ constants.
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'IRC_ERR'          ! NAG error

*  Arguments Given:
      INTEGER EL
      REAL IN1( EL )
      REAL IN2( EL )
      REAL SCALE1
      REAL SCALE2
      INTEGER LCK
      DOUBLE PRECISION CKR( LCK )
      DOUBLE PRECISION LKR( LCK )
      DOUBLE PRECISION CKF1( LCK )
      DOUBLE PRECISION LKF1( LCK )
      DOUBLE PRECISION RATLO
      DOUBLE PRECISION RATHI
      DOUBLE PRECISION CC

*  Arguments Returned:
      REAL TEMP( EL )
      REAL OD( EL )
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :      RATIO,               ! Ratio of input fluxes.
     :      SB1,                 ! Surface brightness from first BAND.
     :      T                    ! Colour temperature value.

      INTEGER
     :      I,                   ! Loop count.
     :      IFAIL                ! NAG status.

      REAL
     :      DATA1,               ! BAND1 data value
     :      DATA2                ! BAND2 data value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Tell the user what is happening.
      CALL MSG_OUTIF( MSG__VERB, 'CTEMZ2_MSG1',
     : '  Using flux ratio table to calculate temperatures and '//
     : 'optical depths...', STATUS )

*  Initialise a flag to indicate that no bad output values have yet been
*  generated.
      BAD = .FALSE.

*  Loop round each element of the output arrays.
      DO I = 1, EL

*  Get the two data values.
         DATA1 = IN1( I )
         DATA2 = IN2( I )

*  Check that both data values are positive and not bad.
         IF( DATA1 .NE. VAL__BADR .AND. DATA1 .GT. 0.0 .AND.
     :       DATA2 .NE. VAL__BADR .AND. DATA2 .GT. 0.0 ) THEN

*  Convert the data values to flux per unit solid angle and form the
*  ratio of the two resulting values.
            RATIO = ( DBLE( DATA1 )*DBLE( SCALE1 ) )/
     :              ( DBLE( DATA2 )*DBLE( SCALE2 ) )

*  Check that this ratio is within the domain of the cubic spline.
            IF( RATIO .GT. RATLO .AND. RATIO .LT. RATHI ) THEN

*  Call a NAG routine to evaluate the cubic spline value (i.e. the
*  temperature) corresponding to this ratio value.
               IFAIL = -1
*               CALL E02BBF( LCK, LKR, CKR, RATIO, T, IFAIL )

               STATUS = IRC__NAGER
               CALL ERR_REP('CTEMZ2_ERR0',
     :              'NAG not compiled into this version of IRAS90.',
     :              STATUS)
               GO TO 999

*  Report an error if anything went wrong in the NAG routine.
               IF( IFAIL .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'CTEMZ2_ERR1',
     :    'CTEMZ2: Unable to evaluate the temperature from the cubic '//
     :    'spline fit.', STATUS )
                  GO TO 999
               END IF

*  Store the temperature in single precision.
               TEMP( I ) = REAL( T )

*  If an optical depth image is required, evaluate the observed
*  surface brightness in BAND1 at this temperature.
               IF( CC .GT. 0.0D0 ) THEN
                  IFAIL = -1
                  CALL E02BBF( LCK, LKF1, CKF1, T, SB1, IFAIL )

*  Report an error if anything went wrong in the NAG routine.
                  IF( IFAIL .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETR( 'T', REAL( T ) )
                     CALL ERR_REP( 'CTEMZ2_ERR2',
     :    'CTEMZ2: Unable to evaluate ^T K surface brightness from '//
     :    'the cubic spline fit.', STATUS )
                     GO TO 999
                  END IF

*  Evaluate the optical depth at the required wavelength.
                  OD( I ) = REAL( CC*DBLE( DATA1 )*DBLE( SCALE1 ) /
     :                      SB1 )
               END IF

*  If no temperature can be calculated for this ratio value, store a bad
*  value in the output.
            ELSE
               TEMP( I ) = VAL__BADR
               IF( CC .GT. 0.0D0 ) OD( I ) = VAL__BADR
               BAD = .TRUE.
            END IF

*  If the input data values are bad or non-positive, store a bad
*  value in the output.
         ELSE
            TEMP( I ) = VAL__BADR
            IF( CC .GT. 0.0D0 ) OD( I ) = VAL__BADR
            BAD = .TRUE.
         END IF

      END DO

 999  CONTINUE

      END
