      SUBROUTINE SURFLIB_REMOVE_IP( ELEVATION, NUM_CHAN, NUM_ADC,
     :     N_BOLS, WPLATE_ANG, BOL_CHAN, BOL_ADC,
     :     BOL_IP_DATA, BOL_DATA, BOL_VAR, STATUS)
*+
*  Name:
*     SURFLIB_REMOVE_IP

*  Purpose:
*     Remove instrumental polarisation

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_REMOVE_IP( ELEVATION, NUM_CHAN, NUM_ADC,
*    :     N_BOLS, WPLATE_ANG, BOL_CHAN, BOL_ADC,
*    :     BOL_IP_DATA, BOL_DATA, STATUS)

*  Description:
*     Remove the instrumental polarisation signal from the data
*     The following formula is used:
*
*     New flux = Measured flux * (1 - fractional IP )
*
*     IP = fractional polarisation * (1 + cos (4*waveplate - 2* IP angle))
*
*     where fractional polarisation and IP angle vary linearly with
*     elevation.
*
*     This is an approximation of
*
*       Actual flux = measured flux - mean flux * frac IP
*
*     where
*
*        IP = P * (1 + cos (4 WP - 2 THETA ) ),
*
*     and we have assumed that the instrumental polarisation is small
*     such that the measured flux and the mean fluxed are
*     approximately equal. This is approximation is required since
*     the mean flux can not be calculated trivially since the
*     bolometers are jiggling on and off the source.
*
*     This routine corrects the data point for each bolometer
*     by looking up the IP data in the supplied array. Only one
*     set of bolometers can be processed at any one time (since the
*     elevation of the array changes during the observation). The
*     routine should be called repeatedly for each time/elevation.
*


*  Arguments:
*     ELEVATION = DOUBLE (Given)
*        Elevation of the source (radians)
*     NUM_CHAN = INTEGER (Given)
*        Number of channels per A/D card
*     NUM_ADC = INTEGER (Given)
*        the number of A/D cards
*     N_BOLS = INTEGER (Given)
*        the actual number of bolometers
*     WPLATE_ANG = REAL (Given)
*        nasmyth angle of waveplate for these data (degrees)
*     BOL_CHAN (N_BOLS) = INTEGER (Given)
*        channel numbers of bolometers
*     BOL_ADC (N_BOLS) = INTEGER (Given)
*        ADC numbers of bolometers
*     BOL_IP_DATA( 8, NUM_CHAN, NUM_ADC ) = REAL (Given)
*        The IP data. This array contains the ip data for each
*        bolometer (specified by a combination of CHAN and ADC).
*        The 4 slices represent: P0, Pslope, Theta0 and ThetaSlope
*        The last 4 slices represent variance of each data point
*        All angles are in radians.
*     BOL_DATA (N_BOLS) = REAL (Given and returned)
*        bolometer data
*     BOL_VAR ( N_BOLS ) = REAL (Given and returned)
*        variance of bolometer data
*     STATUS = INTEGER (Given and returned)
*        global status



*  Notes:
*     - The elevation of each bolometer is assumed to be the elevation
*       of the central bolometer.
*     - Output variance is calculated
*     - degrees are used throughout since this simplifies the calculations
*       when the gradients and ThetaZero are specified in degrees
*       (or inverse degrees for the slope) from the input IP file.

*  Authors:
*     Tim Jenness (JACH)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.5  2003/04/07 23:48:35  timj
*     Change %age to 'frac' in documentation header
*
*     Revision 1.4  2003/04/03 03:19:09  timj
*     Calculation now fixed to S(meas)*(1-P(1+cos[])
*
*     Revision 1.3  1999/08/03 19:32:53  timj
*     Add copyright message to header.
*
*     Revision 1.2  1999/07/15 01:45:52  timj
*     Check for division by zero in IP variance calculation
*
*     Revision 1.1  1999/02/27 04:34:19  timj
*     First version
*

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      DOUBLE PRECISION ELEVATION
      INTEGER NUM_CHAN
      INTEGER NUM_ADC
      INTEGER N_BOLS
      REAL    WPLATE_ANG
      INTEGER BOL_CHAN( N_BOLS )
      INTEGER BOL_ADC ( N_BOLS )
      REAL    BOL_IP_DATA ( 8, NUM_CHAN, NUM_ADC )

*  Arguments Given & Returned:
      REAL    BOL_DATA ( N_BOLS )
      REAL    BOL_VAR  ( N_BOLS )

*  Global Status:
      INTEGER STATUS

*  Local Constants:
      DOUBLE PRECISION PI       ! double precision pi
      PARAMETER (PI = 3.14159265359D0)

*  Local Variables:
      INTEGER ADC               ! Current A-to-D card
      REAL    ANG_BIT           ! cosd part of IP
      REAL    ANG_VAR           ! variance off ANG_BIT (in radians)
      INTEGER BOL               ! Loop counter (bolometer number)
      REAL    BEFORE            ! Bolometer data before correction
      INTEGER CHAN              ! Current channel
      REAL    EL                ! Elevation in degrees
      REAL    IP                ! change in flux due to IP
      REAL    IP_VAR            ! varince on IP
      REAL    P_IP              ! %age polarization at current elevation
      REAL    P_IP_VAR          ! variance on P_IP
      REAL    PSLOPE            ! Gradient in %age polarization (p/deg)
      REAL    PSLOPE_VAR        ! variance on PSLOPE
      REAL    PZERO             ! %age polarization at 0 elevation
      REAL    PZERO_VAR         ! Variance of PZERO
      REAL    SD2R              ! Degrees to radians (single precision)
      REAL    THETA_IP          ! angle of IP at current elevation
      REAL    THETA_IP_VAR      ! variance on theta_ip
      REAL    THETA             ! Angle of IP at 0 elevation (degrees)
      REAL    THETA_VAR         ! Variance on theta 0
      REAL    THETASLOPE        ! gradient in angle (deg theta/deg elevation)
      REAL    THETASLOPE_VAR    ! error on gradient in theta
      REAL    WPANG             ! Waveplate angle in radians

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Precalculate degrees to radians conversion
      SD2R = SNGL(PI) / 180.0

*     Convert the elevation to degrees since the IP_DATA
*     specifies the slope in X/degrees

      EL = SNGL(ELEVATION)

*     Check that Waveplate angle is valid

      IF (WPLATE_ANG .EQ. VAL__BADR) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','SURFLIB_REMOVE_IP: Waveplate angle invalid',
     :        STATUS)
      END IF

*     Convert waveplate angle to radians
      WPANG = WPLATE_ANG * SD2R

*     Loop over each bolometer

      DO BOL = 1, N_BOLS

*     Find the Channel and ADC for this bolometer

         ADC = BOL_ADC (BOL)
         CHAN= BOL_CHAN(BOL)

*     Now calculate the percentage polarization and angle
*     for this elevation. Need to check that the BOL_IP_DATA
*     array contains valid (non-bad) values.

*     Now find the IP parameters from the BOL_IP_DATA array
*     and check that they are non-bad

         PZERO = BOL_IP_DATA(1, CHAN, ADC)
         PSLOPE= BOL_IP_DATA(2, CHAN, ADC)
         THETA = BOL_IP_DATA(3, CHAN, ADC)
         THETASLOPE = BOL_IP_DATA(4, CHAN, ADC)

         PZERO_VAR = BOL_IP_DATA(5, CHAN, ADC)
         PSLOPE_VAR= BOL_IP_DATA(6, CHAN, ADC)
         THETA_VAR = BOL_IP_DATA(7, CHAN, ADC)
         THETASLOPE_VAR = BOL_IP_DATA(8, CHAN, ADC)

*     Proceed if STATUS is good
         IF (STATUS .EQ. SAI__OK) THEN

            IF ((PZERO .EQ. VAL__BADR) .OR. (PSLOPE .EQ. VAL__BADR)
     :           .OR. (THETA .EQ. VAL__BADR)
     :           .OR. (THETASLOPE .EQ. VAL__BADR)) THEN

*     Something wrong since the selected bolometer does not have
*     valid IP information

               STATUS = SAI__ERROR
               CALL MSG_SETI('C',CHAN)
               CALL MSG_SETI('A',ADC)
               CALL ERR_REP(' ','SURFLIB_REMOVE_IP: Invalid IP data'//
     :              ' for selected bolometer (chan=^C, adc=^A)',
     :              STATUS)


            ELSE
*     Everything okay - proceed to calculate the percentage polarization
*     and angle at this elevation [p is a percentage]

               P_IP     = ( PZERO + (PSLOPE * EL) ) / 100
               THETA_IP = THETA + (THETASLOPE * EL)

*     Variance of P_IP and THETA_IP
*     Assume that the variances are good if the data IP values
*     were good

               P_IP_VAR = (PZERO_VAR + (EL ** 2.0) * PSLOPE_VAR ) /
     :              (100 ** 2)
               THETA_IP_VAR = THETA_VAR + (EL ** 2.0) * THETASLOPE_VAR

*     Now calculate the IP correction

               ANG_BIT = COS((4.0*WPANG) - (2.0*THETA_IP))
               IP = P_IP * ( 1 + ANG_BIT )

*     Now calculate the error in the IP calculation
*     First the variance in the cosine 'bit' [note that the angles
*     have to be in radians for this to work]

               ANG_VAR = ((2.0 * SIN((4.0*WPANG)-(2.0*THETA_IP)))
     :              ** 2.0) * THETA_IP_VAR

*     check for division by zero problems
               IF (P_IP .EQ. 0.0 .OR. ANG_BIT .EQ. 0) THEN
                  IP_VAR = VAL__BADR
               ELSE
                  IP_VAR = (IP**2)*( (P_IP_VAR/(P_IP**2)) +
     :                 (ANG_VAR/(ANG_BIT**2)) )
               END IF

*     ...and modify the input data

               IF (BOL_DATA(BOL) .NE. VAL__BADR) THEN
                  BEFORE = BOL_DATA(BOL) ! Needed for variance calc
                  BOL_DATA(BOL) = BOL_DATA(BOL) * (1 - IP)

*     Division by zero protection
                  IF (BOL_VAR(BOL) .NE. VAL__BADR
     :                 .AND. BEFORE .NE. 0.0
     :                 .AND. IP_VAR .NE. VAL__BADR ) THEN
                     BOL_VAR(BOL) = (BOL_DATA(BOL)**2) *
     :                    ( (BOL_VAR(BOL) / (BEFORE**2) ) +
     :                    ( IP_VAR / (1 +IP)**2 ) )
                  ELSE
                     BOL_VAR(BOL) = VAL__BADR
                  END IF

               ELSE
                  BOL_VAR(BOL) = VAL__BADR
               END IF

*               IF (CHAN .EQ. 7 .AND. ADC .EQ. 8) THEN
*                  print *,EL,IP,IP_VAR,P_IP,P_IP_VAR,WPLATE_ANG
*                  print *,'--',PZERO,PSLOPE,THETA,THETASLOPE
*                  print *,'---',P_IP_VAR,THETA_IP_VAR,ANG_BIT,ANG_VAR
*               END IF

            END IF

         END IF

      END DO


      END

