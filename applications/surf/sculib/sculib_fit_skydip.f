      SUBROUTINE SCULIB_FIT_SKYDIP (CVAR, N_MEASUREMENTS, AIRMASS,
     :     J_MEASURED, J_VARIANCE, SUB_WAVELENGTH, SUB_INSTRUMENT,
     :     SUB_FILTER, T_TEL, T_AMB, ETA_TEL_IN, B_IN, ETA_TEL_FIT,
     :     B_FIT, TAUZ_FIT, REXISQ, TAU_ERROR, ETA_ERROR,
     :     B_ERROR, RESIDUAL, SIGMA, STATUS)
*+
*  Name:
*     SCULIB_FIT_SKYDIP

*  Purpose:
*     Fit the skydip data

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_FIT_SKYDIP (CVAR, N_MEASUREMENTS, AIRMASS, J_MEASURED,
*    :  J_VARIANCE, SUB_WAVELENGTH, SUB_INSTRUMENT, SUB_FILTER, T_TEL,
*    :  T_AMB, ETA_TEL_IN, B_IN, ETA_TEL_FIT, B_FIT, TAUZ_FIT,
*    :  REXISQ, TAU_ERROR, ETA_ERROR, B_ERROR, RESIDUAL, SIGMA, STATUS)

*  Description:
*     This routine fits a sub-instrument's measurements of the sky
*     brightness at a range of airmasses to obtain the sky opacity, ETAtel
*     and `b' parameters. The fit is to the function:-
*
*        Jmeas = (1-ETAtel) * Jtel + ETAtel * Jatm * (1 - b * exp (-tau * A))
*
*     Jtel is known and Jatm is related to Jamb by:-
*
*         Jatm = integral (from h=0 to infinity) of
*              {Jamb * (1-h/h1) * exp (k * A * h2 * (exp (-h/h2) - 1)) *
*              k * A * exp (-h/h2) * dh
*
*     where h2 = scale height of absorption ( = 2km)
*           h1 = coefficient to give 6.5K/km temperature drop in absorber
*            A = airmass
*            k = extinction
*
*     which relation has been fudged by Bill Duncan to:-
*
*         Jatm = Jamb * Xg * (1 - exp(-tau * A))
*
*     with
*
*           Xg = 1 + h2 * h1 * exp (- A * t / Xgconst)
*                    -------
*                     Jamb
*
*           h1 = 2
*           h2 = -6.5 (note this h2 is defined differently to that in the
*                      previous equation)
*      Xgconst = 3.669383
*
*     See `Calibration of mm and sub-mm Photometers by Skydipping', W.D.Duncan
*     preprint and `Inversion of Sky Dips', SCU/WDD/31.1/1093 for further
*     details.
*
*        The fit can be made with ETAtel and/or b either fixed or
*     varying. To allow one of these parameters to vary it should be
*     input to the routine with a value below zero. If the input
*     value is greater than zero then the routine will fix it at
*     that for the fit.
*

*  Arguments:
*     CVAR                      = LOGICAL (Given)
*              flag to govern whether to use a fixed variance (true)
*              or the actual variance. The fixed variance is the mean
*              of the actual variances.
*     N_MEASUREMENTS            = INTEGER (Given)
*              the number of SKYDIP measurements
*     AIRMASS (N_MEASUREMENTS)  = REAL (Given)
*              the airmasses at which the measurements were made
*     DATA (N_MEASUREMENTS)     = REAL (Given)
*              the measured sky brightness temperatures
*     VARIANCE (N_MEASUREMENTS) = REAL (Given)
*              the variance on DATA
*     SUB_WAVELENGTH            = REAL (Given)
*              the wavelength of the measurements in microns
*     SUB_INSTRUMENT            = CHARACTER*(*) (Given)
*              the name of the sub-instrument used
*     SUB_FILTER                = CHARACTER*(*) (Given)
*              the name of the filter used
*     T_TEL                     = REAL (Given)
*              the telescope temperature (K)
*     T_AMB                     = REAL (Given)
*              the ambient temperature (K)
*     ETA_TEL_IN                = REAL (Given)
*              if >= 0 then this will the ETAtel assumed in the fit.
*              if < 0 then ETAtel will be allowed to vary in the fit.
*     B_IN                      = REAL (Given)
*              if >=0 then this value of b will be assumed in the fit.
*              if < 0 then b will be allowed to vary in the fit.
*     ETA_TEL_FIT               = REAL (Returned)
*              the result for ETAtel
*     B_FIT                     = REAL (Returned)
*              the result for b
*     TAUZ_FIT                  = REAL (Returned)
*              the fitted result for tauz
*     REXISQ                    = REAL (Returned)
*              the reduced chi square of the fit
*     TAU_ERROR                 = REAL (Returned)
*              error in the tau
*     ETA_ERROR                 = REAL (Returned)
*              error in eta_tel
*     B_ERROR                   = REAL (Returned)
*              error in B
*     RESIDUAL                  = DOUBLE (Returned)
*              Absolute difference between the model and the fit.
*     SIGMA                     = DOUBLE (Returned)
*              standard deviation of the difference between the
*              model and the fit.
*     STATUS                    = INTEGER (Given and returned)
*              Global status

*  Authors:
*     T.Jenness (timj@jach.hawaii.edu)
*     J.Lightfoot (REVAD::JFL)
*     Nick Tothill (N.F.H.Tothill@qmw.ac.uk)

*  Method:

*  Deficiencies:

*  Bugs:


*  Copyright:
*     Copyright (C) 1995-2000 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     $Log$
*     Revision 1.21  2002/09/16 05:16:27  timj
*     Make sure we initialise XIOLD to something
*
*     Revision 1.20  2000/07/10 21:09:30  timj
*     Documentation tweaks for V1.6
*
*     Revision 1.19  2000/05/11 20:00:27  timj
*     Handle RESIDUAL
*
*     Revision 1.18  1999/08/19 03:37:11  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.17  1999/08/03 19:35:00  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.16  1999/03/24 00:28:03  timj
*     Increase the common-block for fitting to 500
*
*     Revision 1.15  1998/12/03 19:39:04  timj
*     Check that the number of degrees of freedom is positive.
*
*     Revision 1.14  1998/10/02 00:49:38  timj
*     Do not try to report the errors if the fit has failed
*
*     Revision 1.13  1998/06/06 03:24:02  timj
*     Add errors (Thanks to Nick Tothill for adding them).
*     Tidy up -- return errors to calling routine.
*

*     1998/05/18 Revised to give reasonably realistic errors on fitted
*     quantities; degrees-of-freedom handling changed; reduced chi-sq
*     quoted with respect to different variance - NFHT

*     Revision 1.12  1998/01/15 19:36:41  timj
*     Return immediately if N_MEASUREMENTS is 0
*
*     Revision 1.11  1998/01/12 20:32:12  timj
*     Return the reduced chi square
*
*     Revision 1.10  1998/01/07 00:28:40  timj
*     Use the CVAR variable to govern whether or not a constant variance is used.
*
*     Revision 1.9  1998/01/06 01:55:39  timj
*     Use constant (mean) variance for all points for fit.
*
*     Revision 1.8  1998/01/06 00:34:16  timj
*     Modernise header
*
*     11-MAR-1997 (JFL):
*        try again, using Bevington LSQ fit routine (JFL).
*     10-MAR-1997 (TIMJ):
*        new version, using LSQ_FIT rather than NAG routine (TIMJ).
*     23-JUL-1996 (JFL):
*        renamed SCULIB_ from SCUDR_ and made to fit the correct
*        function (JFL).
*      7-FEB-1996 (JFL):
*        split off from SCUDR_SKYDIP_SWITCH.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'              ! for VAL__NBx
      INCLUDE 'MSG_PAR'              ! For MSG__

*  Global Constants (for COMMON):
      INTEGER MAX_FIT_DATA           ! max number of measurements
      PARAMETER (MAX_FIT_DATA = 500)


*  Arguments Given:
      LOGICAL CVAR
      INTEGER N_MEASUREMENTS
      REAL    AIRMASS (N_MEASUREMENTS)
      REAL    J_MEASURED (N_MEASUREMENTS)
      REAL    J_VARIANCE (N_MEASUREMENTS)
      REAL    SUB_WAVELENGTH
      CHARACTER*(*) SUB_INSTRUMENT
      CHARACTER*(*) SUB_FILTER
      REAL    T_TEL
      REAL    T_AMB
      REAL    ETA_TEL_IN
      REAL    B_IN

*  Arguments Returned:
      REAL    B_FIT
      REAL    TAUZ_FIT
      REAL    ETA_TEL_FIT
      DOUBLE PRECISION RESIDUAL      ! Residual of fit
      REAL             REXISQ        ! Reduced chi square
      DOUBLE PRECISION SIGMA
      REAL    B_ERROR
      REAL    ETA_ERROR
      REAL    TAU_ERROR

*  Status:
      INTEGER STATUS

*  External references:
      REAL     SCULIB_JNU            ! brightness temperature function
      EXTERNAL SCULIB_SKYDIP_XISQ    ! Skydip chi-squared function

*  Global variables:
      DOUBLE PRECISION C_AIRMASS (MAX_FIT_DATA)
      DOUBLE PRECISION C_B_HI
      DOUBLE PRECISION C_B_LO
      DOUBLE PRECISION C_ETA_TEL_HI
      DOUBLE PRECISION C_ETA_TEL_LO
      DOUBLE PRECISION C_J_AMB
      DOUBLE PRECISION C_J_MEASURED (MAX_FIT_DATA)
      INTEGER          C_J_QUALITY (MAX_FIT_DATA)
      DOUBLE PRECISION C_J_TEL
      DOUBLE PRECISION C_J_VARIANCE (MAX_FIT_DATA)
      INTEGER          C_M
      COMMON /SCULIB_SKYDIP_FIT_DATA_I/ C_J_QUALITY,
     :                                  C_M
      COMMON /SCULIB_SKYDIP_FIT_DATA_D/ C_AIRMASS,
     :                                  C_B_HI,
     :                                  C_B_LO,
     :                                  C_ETA_TEL_HI,
     :                                  C_ETA_TEL_LO,
     :                                  C_J_AMB,
     :                                  C_J_MEASURED,
     :                                  C_J_TEL,
     :                                  C_J_VARIANCE

*  Local Constants:
      REAL             LIGHT         ! velocity of light
      PARAMETER (LIGHT = 2.997929E8)
      DOUBLE PRECISION MIN_VAR       ! Minimum variance of a measurement
      PARAMETER (MIN_VAR = 1.0E-3)

*  Local variables:
      DOUBLE PRECISION ALPHA (3,3)   ! scratch used by SCULIB_FIT_FUNCTION
      DOUBLE PRECISION BETA (3)
      CHARACTER*100    BUFFER        ! buffer to hold results of fit
      CHARACTER*80     SIGBUFFER     ! buffer to hold scatter about fit
      INTEGER          COUNT         ! Counter
      DOUBLE PRECISION DA (6)
      DOUBLE PRECISION FIT (3)       ! the fitted parameters
      INTEGER          I             ! DO loop variable
      INTEGER          IK (3)
      INTEGER          ITERATION     ! fit iteration
      INTEGER          JK (3)
      REAL             J_AMB         ! brightness temperature of ambient air
      REAL             J_TEL         ! brightness temperature of telescope
      DOUBLE PRECISION LAMBDA
      LOGICAL          LOOPING
      DOUBLE PRECISION MEAN_VAR      ! Mean of the input variances
      INTEGER          NDEG          ! Number of degrees of freedom
      REAL             NU            ! frequency
      INTEGER          QUALITY       ! quality of fit
      DOUBLE PRECISION SUM           ! Sum of variances
      DOUBLE PRECISION XICUT         ! when an iteration produces an
                                     ! improvement in chi-squared below
                                     ! this limit no further iterations
                                     ! will be performed
      DOUBLE PRECISION XIOLD         ! chi-squared of fit
      DOUBLE PRECISION XISQ          ! chi-squared of fit

      DOUBLE PRECISION CHISQ         ! chi-squared of fit (not reduced)
      DOUBLE PRECISION REDCHISQ      ! chi-squared of fit (reduced)
                                     ! both with respect to updated variance
      DOUBLE PRECISION VAR           ! variance of data around fit
      DOUBLE PRECISION OLDVAR (MAX_FIT_DATA) !storage array for original
                                             !variance values
*.

      IF (STATUS .NE. SAI__OK) RETURN

* Check number of data points for fit

      IF (N_MEASUREMENTS .GT. MAX_FIT_DATA) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ','SCULIB_FIT_SKYDIP: Too many data points.',
     :        STATUS)
         RETURN
      ELSE IF (N_MEASUREMENTS .LE. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','SCULIB_FIT_SKYDIP: There are no points '//
     :        'available for fitting', STATUS)
         RETURN
      END IF

* Calculate J_TEL and J_AMB

      NU = LIGHT / (SUB_WAVELENGTH * 1.0E-6)
      J_TEL = SCULIB_JNU (NU, T_TEL, STATUS)
      J_AMB = SCULIB_JNU (NU, T_AMB, STATUS)

*     Calculate the mean of the input VARIANCE if CVAR is true.
*     Try to take care of negative variances (ignore them) and
*     bad values.

      MEAN_VAR = MIN_VAR
      IF (CVAR) THEN

         SUM = 0.0D0
         COUNT = 0

         DO I = 1, N_MEASUREMENTS
            IF (J_VARIANCE(I) .NE. VAL__BADR .AND.
     :           J_VARIANCE(I) .GE. 0.0D0) THEN
               SUM = SUM + DBLE(J_VARIANCE(I))
               COUNT = COUNT + 1
            END IF
         END DO

*     Calculate the MEAN
         IF (COUNT .GT. 0) THEN
            MEAN_VAR = SUM / DBLE(COUNT)
         ELSE
            MEAN_VAR = MIN_VAR  ! No good data anyway
         END IF

      END IF

* Put data into common

      C_M = N_MEASUREMENTS
      C_J_AMB = DBLE (J_AMB)
      C_J_TEL = DBLE (J_TEL)

      DO I = 1, N_MEASUREMENTS
         C_AIRMASS (I) = DBLE (AIRMASS(I))
         C_J_MEASURED (I) = DBLE (J_MEASURED(I))
         C_J_QUALITY (I) = 0

         IF (CVAR) THEN
            C_J_VARIANCE (I) = MAX (MEAN_VAR, MIN_VAR)
         ELSE
            C_J_VARIANCE (I) = DBLE(MAX (J_VARIANCE(I), SNGL(MIN_VAR)))
         END IF

      END DO

*     In general, the number of d.f. is
*     given by the number of observations _minus_ the number
*     of free parameters (see Bevington)
*     The - 1 is only used when there is an implicit additive constant
*     in the fitting function, eg in most polynomial fits.
*     d.f. = n_obs - 1 (since tau is always free - eta & b
*     may or may not be free parameters) - NFHT

      NDEG = N_MEASUREMENTS - 1

* Now try to fit this

      IF (STATUS .EQ. SAI__OK) THEN

* Set initial guess and limits

* Again, change the calculation of degrees of freedom - NFHT

         IF (B_IN .LT. 0.0) THEN
            C_B_HI = 0.9999D0
            C_B_LO = 0.0001D0
            FIT (2) = 0.7D0

            NDEG = NDEG - 1
         ELSE
            C_B_HI = DBLE (B_IN)
            C_B_LO = DBLE (B_IN)
            FIT (2) = DBLE (B_IN)
         END IF

         IF (ETA_TEL_IN .LT. 0.0) THEN
            C_ETA_TEL_HI = 0.9999D0
            C_ETA_TEL_LO = 0.0001D0
            FIT (1) = 0.7D0

            NDEG = NDEG - 1
         ELSE
            C_ETA_TEL_HI = DBLE (ETA_TEL_IN)
            C_ETA_TEL_LO = DBLE (ETA_TEL_IN)
            FIT (1) = DBLE (ETA_TEL_IN)
         END IF

         FIT (3) = 0.5D0

*     If the number of degrees of freedom is less than 1 then we
*     have a problem (eg too few input points)
         IF (NDEG .LE. 1 .AND. STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI('NDEG', NDEG)
            CALL ERR_REP(' ','FIT_SKYDIP: Too few degrees of freedom '//
     :           '(^NDEG)', STATUS)
         END IF

* some initial settings

         LAMBDA = 0.001
         CALL SCULIB_SKYDIP_XISQ (XISQ, 3, FIT, STATUS)
         XICUT = MAX (0.00001D0 * XISQ, 0.01D0)

* now iterate

         QUALITY = 0
         LOOPING = .TRUE.
         ITERATION = 0

*     We need to make sure that our XIOLD is much different to XISQ
*     on entry to the loop otherwise we will end up aborting incorrectly.
*     Easiest thing is simply to set XIOLD to -XISQ. This will still
*     allow us to abort if we fit the data perfectly on entry but is
*     difficult to fake (and it does not really matter if XIOLD-XISQ
*     is so small that we meet XICUT)
         XIOLD = - XISQ

         DO WHILE (LOOPING)
            CALL SCULIB_FIT_FUNCTION (SCULIB_SKYDIP_XISQ, XICUT,
     :        3, FIT, LAMBDA, ALPHA, BETA, IK, JK, DA, STATUS)

            ITERATION = ITERATION + 1

* check for last integration

            IF (STATUS .NE. SAI__OK) THEN
               LOOPING = .FALSE.
               QUALITY = 1
            ELSE IF (ABS(XIOLD-XISQ) .LT. XICUT) THEN
               LOOPING = .FALSE.
            ELSE IF (ITERATION .GT. 40) THEN
               STATUS = SAI__WARN
               CALL ERR_OUT (' ', 'SCULIB_FIT_SKYDIP: '//
     :           'SKYDIP fit has failed to converge after 40 '//
     :           'iterations', STATUS)
               LOOPING = .FALSE.
               QUALITY = 1
            END IF

            XIOLD = XISQ
            CALL SCULIB_SKYDIP_XISQ (XISQ, 3, FIT, STATUS)

* report the iteration result

            CALL MSG_SETI ('ITER', ITERATION)
            CALL MSG_SETR ('CHISQ', REAL(XISQ))
*            CALL MSG_OUT (' ', 'iter=^ITER chisq=^CHISQ', STATUS)
         END DO

         ETA_TEL_FIT = REAL (FIT(1))
         B_FIT = REAL (FIT(2))
         TAUZ_FIT = REAL (FIT(3))


******************** - Nick Tothill - Adding error determination

*     First make sure that we have a good fit before trying this

         IF (QUALITY .NE. 0) THEN

            B_ERROR = VAL__BADR
            TAU_ERROR = VAL__BADR
            ETA_ERROR = VAL__BADR
            SIGMA = VAL__BADD
            RESIDUAL = VAL__BADD

         ELSE

* Now we have a fit, we can calculate the error on the
* tau estimate -NFHT

* First we need a more realistic estimate of the data variance

            CALL SCULIB_SKYDIP_VAR(RESIDUAL, VAR, N_MEASUREMENTS,
     :           FIT, NDEG, STATUS)

*  Calculate the std deviation of the scatter about the fit

            SIGMA = SQRT(VAR)

*  Dump the original variance values from the common block data array
*  into a storage array. Replace them with the newly calculated variance

            DO I=1,N_MEASUREMENTS

               OLDVAR(I) = C_J_VARIANCE(I)
               C_J_VARIANCE(I) = VAR

            END DO

*  Call subroutine to produce error matrix, and to find chi-squared
*  wrt new variance. Otherwise, error values are derived from last
*  returned inverted-alpha matrix

            LAMBDA = 0

            CALL SCULIB_FIT_FUNCTION (SCULIB_SKYDIP_XISQ, XICUT,
     :           3, FIT, LAMBDA, ALPHA, BETA, IK, JK, DA, STATUS)

            CALL SCULIB_SKYDIP_XISQ (CHISQ, 3, FIT, STATUS)

            ETA_ERROR = SQRT(ABS(ALPHA(1,1)))
            B_ERROR = SQRT(ABS(ALPHA(2,2)))
            TAU_ERROR = SQRT(ABS(ALPHA(3,3)))

            IF (ALPHA(1,1) .LT. 0) THEN
               CALL MSG_OUTIF (MSG__QUIET,' ',
     :              'WARNING: The variance of eta_tel '//
     :              'is negative. Caution is advised.', STATUS)
            END IF

            IF (ALPHA(2,2) .LT. 0) THEN
               CALL MSG_OUTIF (MSG__QUIET,' ',
     :              'WARNING: The variance of b '//
     :              'is negative. Caution is advised.', STATUS)
            END IF

            IF (ALPHA(3,3) .LT. 0) THEN
               CALL MSG_OUTIF (MSG__QUIET, ' ',
     :              'WARNING: The variance of tau '//
     :              'is negative. Caution is advised.', STATUS)
            END IF

            REDCHISQ = CHISQ / REAL(NDEG)

*     Restore the common block data array to its original form -
*     replace the variances with the old variance values out of the
*     storage array

            DO I=1,N_MEASUREMENTS

               C_J_VARIANCE(I) = OLDVAR(I)

            END DO

         END IF

*************** End error determination *******************

*  output results
*  include errors, and output reduced chi-sq with respect to
*  updated variance - NFHT

*         REXISQ = REAL(XISQ) / REAL(NDEG) ! Reduce chi sq

         REXISQ = REDCHISQ

         IF (QUALITY .NE. 0) THEN
            CALL MSG_SETC ('SUB', SUB_INSTRUMENT)
            CALL MSG_SETC ('FILT', SUB_FILTER)

            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_FIT_SKYDIP: fit failed '//
     :        'for filter = ^FILT and sub-instrument ^SUB', STATUS)
            CALL ERR_REP (' ', ' - last fit values were:-', STATUS)

*     Since the fit has failed -- there is no point printing
*     errors (they are bad values)

            WRITE (BUFFER, 15) ETA_TEL_FIT, B_FIT, TAUZ_FIT,
     :           REXISQ, ITERATION
 15         FORMAT ('eta = ', F6.2, '          b = ', F6.2,
     :           '  tau = ', F7.3, '  X= ', F7.1, '  N= ', I4)


            CALL MSG_SETC ('BUFFER', BUFFER)
            CALL ERR_REP (' ', ' ^BUFFER', STATUS)
         ELSE

            CALL MSG_SETC ('FILT', SUB_FILTER)
            CALL MSG_SETC ('SUB', SUB_INSTRUMENT)

            CALL MSG_OUTIF (MSG__NORM,' ', 'SCULIB: fit for filter '//
     :           '^FILT and sub-instrument ^SUB', STATUS)

* modified to output errors, and

            WRITE (BUFFER, 20) ETA_TEL_FIT, ETA_ERROR, B_FIT,
     :           B_ERROR, TAUZ_FIT, TAU_ERROR
 20         FORMAT ('eta = ', F5.2, ' +/- ', F5.2,
     :           '  b = ', F5.2, ' +/- ', F5.2,
     :           '  tau = ', F7.3, ' +/- ', F5.3)

            WRITE (SIGBUFFER, 30) SIGMA, REDCHISQ, ITERATION
 30         FORMAT ('Standard Deviation of fit residual = ',
     :           F6.2, ' K (X= ',F7.1, ' N= ',I4,')')

            CALL MSG_SETC ('BUFFER', BUFFER)
            CALL MSG_OUTIF (MSG__NORM,' ', ' ^BUFFER', STATUS)

            CALL MSG_SETC ('SIGBUFFER', SIGBUFFER)
            CALL MSG_OUTIF (MSG__NORM,' ', ' ^SIGBUFFER', STATUS)

*     Write out the residual
            CALL MSG_SETR('RES', REAL(RESIDUAL))
            CALL MSG_OUTIF(MSG__NORM, ' ', ' Residual of fit: ^RES K',
     :           STATUS)

         END IF
      END IF

      END

