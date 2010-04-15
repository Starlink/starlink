      SUBROUTINE CTEMZ4( EL, IN1, IN2, VIN1, VIN2, SCALE1, SCALE2,
     :                   LCK, CKR, LKR, CKF1, LKF1, RATLO, RATHI, CC,
     :                   TEMP, OD, VTEMP, VOD, BAD, STATUS )
*+
*  Name:
*     CTEMZ4

*  Purpose:
*     Generate output temperature and optical depth values and
*     variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTEMZ4( EL, IN1, IN2, VIN1, VIN2, SCALE1, SCALE2, LCK, CKR,
*                  LKR, CKF1, LKF1, RATLO, RATHI, CC, TEMP, OD, VTEMP,
*                  VOD, BAD, STATUS )

*  Description:
*     A temperature, optical depth, and associated variances are
*     found for each pair of good, positive input pixel values. Bad
*     output values are generated for any positions at which either
*     input is bad or negative. Bad values are also created if the
*     ratio of flux per steradin in BAND1 to that in BAND2 is outside
*     the range specified by RATLO and RATHI. For all other pixels, the
*     temperature corresponding to the ratio value is obtained using
*     the cubic spline fit found by routine CTEMZ0. The observed model
*     flux per steradian at this temperature is then found using the
*     second cubic spline found by CTEMZ0. The optical depth is
*     proportional to the ratio of the actual observed flux to the
*     model observed flux.
*
*     The input variances are used to calculate the variance of the
*     flux ratio. This is then converted into a variance on temperature
*     using the slope of the temperature against flux ratio cubic
*     spline. The variance in temperature is then used to find the
*     variance in the model observed flux from BAND1, and finally this
*     is combined with the variance from the actual BAND1 flux to get
*     the  variance on the optical depth.

*  Arguments:
*     EL = INTEGER (Given)
*        No. of pixels in each input image.
*     IN1( EL ) = REAL (Given)
*        Input image for the lower waveband.
*     IN2( EL ) = REAL (Given)
*        Input image for the higher waveband.
*     VIN1( EL ) = REAL (Given)
*        Input image variances for the lower waveband.
*     VIN2( EL ) = REAL (Given)
*        Input image variances for the higher waveband.
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
*     VTEMP( EL ) = REAL (Returned)
*        The colour temperature variances.
*     VOD( EL ) = REAL (Returned)
*        The optial depth variancess. Not used if CC is less than or
*        equal to zero.
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
      REAL VIN1( EL )
      REAL VIN2( EL )
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
      REAL VTEMP( EL )
      REAL VOD( EL )
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :      RATIO,               ! Ratio of input fluxes.
     :      SB1( 4 ),            ! Surface brightness from first BAND
                                 ! and first three derivatives.
     :      T( 4 ),              ! Colour temperature value and first
                                 ! three derivatives.
     :      VRAT,                ! Variance on RATIO value.
     :      VSB1,                ! Variance on SB1( 1 ) value.
     :      VT                   ! Variance on T( 1 ) value.

      INTEGER
     :      I,                   ! Loop count.
     :      IFAIL                ! NAG status.

      REAL
     :      DATA1,               ! BAND1 data value
     :      DATA2,               ! BAND2 data value
     :      VAR1,                ! Variance on DATA1.
     :      VAR2                 ! Variance on DATA2.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Tell the user what is happening.
      CALL MSG_OUTIF( MSG__VERB, 'CTEMZ4_MSG1',
     : '  Using flux ratio table to calculate temperatures and '//
     : 'optical depths (and variances)...', STATUS )

*  Initialise a flag to indicate that no bad output values have yet been
*  generated.
      BAD = .FALSE.

*  Loop round each element of the output arrays.
      DO I = 1, EL

*  Get the two data values.
         DATA1 = IN1( I )
         DATA2 = IN2( I )

*  Get the two variance values.
         VAR1 = VIN1( I )
         VAR2 = VIN2( I )

*  Check that all values are positive and not bad.
         IF( DATA1 .NE. VAL__BADR .AND. DATA1 .GT. 0.0 .AND.
     :       DATA2 .NE. VAL__BADR .AND. DATA2 .GT. 0.0 .AND.
     :       VAR1 .NE. VAL__BADR .AND. VAR1 .GT. 0.0 .AND.
     :       VAR2 .NE. VAL__BADR .AND. VAR2 .GT. 0.0 ) THEN

*  Convert the input values into units of pico-Watts per square metre
*  per steradian.
            DATA1 = DATA1*SCALE1
            DATA2 = DATA2*SCALE2
            VAR1 = VAR1*(SCALE1**2)
            VAR2 = VAR2*(SCALE2**2)

*  Form the ratio of the two flux values.
            RATIO = DBLE( DATA1 )/DBLE( DATA2 )

*  Check that this ratio is within the domain of the cubic spline.
            IF( RATIO .GT. RATLO .AND. RATIO .LT. RATHI ) THEN

*  Call a NAG routine to evaluate the cubic spline value (i.e. the
*  temperature) corresponding to this ratio value. The NAG routine also
*  returns the first three derivatives in T(2),T(3) and T(4).
               IFAIL = -1
*               CALL E02BCF( LCK, LKR, CKR, RATIO, .TRUE., T, IFAIL )

               STATUS = IRC__NAGER
               CALL ERR_REP('CTEMZ4_ERR0',
     :              'NAG not compiled into this version of IRAS90.',
     :              STATUS)
               GO TO 999

*  Report an error if anything went wrong in the NAG routine.
               IF( IFAIL .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'CTEMZ4_ERR1',
     :    'CTEMZ4: Unable to evaluate the temperature from the cubic '//
     :    'spline fit.', STATUS )
                  GO TO 999
               END IF

*  Store the temperature in single precision.
               TEMP( I ) = REAL( T( 1 ) )

*  Evaluate the variance of the ratio value based on the input
*  variances.
               VRAT = (RATIO**2)*( VAR1/(DATA1**2) + VAR2/(DATA2**2) )

*  T(2) contains the rate of change of temperature with flux ratio
*  value. Use it to convert the uncertainty in ratio into the
*  uncertainty in temperature.
               VT = VRAT*T( 2 )**2
               VTEMP( I ) = REAL( VT )

*  If an optical depth image is required, evaluate the observed
*  surface brightness in BAND1 at this temperature. The NAG routine also
*  returns the first three derivatives with temperature.
               IF( CC .GT. 0.0D0 ) THEN
                  IFAIL = -1
                  CALL E02BCF( LCK, LKF1, CKF1, T( 1 ), .TRUE., SB1,
     :                         IFAIL )

*  Report an error if anything went wrong in the NAG routine.
                  IF( IFAIL .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETR( 'T', REAL( T( 1 ) ) )
                     CALL ERR_REP( 'CTEMZ4_ERR2',
     :    'CTEMZ4: Unable to evaluate ^T K surface brightness from '//
     :    'the cubic spline fit.', STATUS )
                     GO TO 999
                  END IF

*  Evaluate the optical depth at the required wavelength.
                  OD( I ) = REAL( CC*DBLE( DATA1 ) / SB1( 1 ) )

*  Evaluate the uncertainty in SB1( 1 ) given the uncertainty in the
*  temperature.
                  VSB1 = VT*( SB1( 2 )**2 )

*  Calculate the uncertainty in the optical depth.
                  VOD( I ) = REAL( ( ( CC*OD( I ) )**2 )*
     :                             ( ( VAR1/(DATA1**2) ) +
     :                               ( VSB1/(SB1( 1 )**2) ) ) )

               END IF

*  If no temperature can be calculated for this ratio value, store a bad
*  value in the output.
            ELSE
               TEMP( I ) = VAL__BADR
               VTEMP( I ) = VAL__BADR

               IF( CC .GT. 0.0D0 ) THEN
                  OD( I ) = VAL__BADR
                  VOD( I ) = VAL__BADR
               END IF

               BAD = .TRUE.
            END IF

*  If the input data values are bad or non-positive, store a bad
*  value in the output.
         ELSE
            TEMP( I ) = VAL__BADR
            VTEMP( I ) = VAL__BADR

            IF( CC .GT. 0.0D0 ) THEN
               OD( I ) = VAL__BADR
               VOD( I ) = VAL__BADR
            END IF

            BAD = .TRUE.
         END IF

      END DO

 999  CONTINUE

      END
