      SUBROUTINE SCULIB_FIT_SKYDIP (N_MEASUREMENTS, AIRMASS, J_MEASURED,
     :  J_VARIANCE, SUB_WAVELENGTH, SUB_INSTRUMENT, SUB_FILTER, T_TEL,
     :  T_AMB, ETA_TEL_IN, B_IN, ETA_TEL_FIT, B_FIT, TAUZ_FIT, STATUS)
*+
*  Name:
*     SCULIB_FIT_SKYDIP

*  Purpose:
*     Fit the skydip data

*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL SCULIB_FIT_SKYDIP (N_MEASUREMENTS, AIRMASS, J_MEASURED,
*    :  J_VARIANCE, SUB_WAVELENGTH, SUB_INSTRUMENT, SUB_FILTER, T_TEL,
*    :  T_AMB, ETA_TEL_IN, B_IN, ETA_TEL_FIT, B_FIT, TAUZ_FIT, STATUS)

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
*     N_MEASUREMENTS            = INTEGER (Given)
*              the number of SKYDIP measurements
*     AIRMASS (N_MEASUREMENTS)  = REAL (Given)
*              the airmasses at which the measurements were made
*     DATA (N_MEASUREMENTS)     = REAL (Given)
*              the measured sky brightness temperatures
*     VARIANCE (N_MEASUREMENTS) = REAL (Given)
*              the variance on DATA
*     SUB_WAVELENGTH            = REAL (Given)
*              the wavelength of the measurements
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
*     STATUS                    = INTEGER (Given and returned)
*              Global status

*  Method:

*  Deficiencies:

*  Bugs:

*  Notes:
*     Uses the Mean input VARIANCE as the VARIANCE for every point.

*  Authors:
*     T.Jenness (timj@jach.hawaii.edu)
*     J.Lightfoot (REVAD::JFL)

*  History:
*     $Id$
*     $Log$
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

*  Global Constants (for COMMON):
      INTEGER MAX_FIT_DATA           ! max number of measurements
      PARAMETER (MAX_FIT_DATA = 100)


*  Arguments Given:
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
      CHARACTER*80     BUFFER        ! buffer to hold results of fit
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
      REAL             REXISQ        ! Reduced chi square
      DOUBLE PRECISION SUM           ! Sum of variances
      DOUBLE PRECISION XICUT         ! when an iteration produces an
                                     ! improvement in chi-squared below
                                     ! this limit no further iterations
                                     ! will be performed
      DOUBLE PRECISION XIOLD         ! chi-squared of fit
      DOUBLE PRECISION XISQ          ! chi-squared of fit

*.

      IF (STATUS .NE. SAI__OK) RETURN

* Check number of data points for fit

      IF (N_MEASUREMENTS .GT. MAX_FIT_DATA) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ','SCULIB_FIT_SKYDIP: Too many data points.',
     :        STATUS)
         RETURN
      END IF

* Calculate J_TEL and J_AMB

      NU = LIGHT / (SUB_WAVELENGTH * 1.0E-6)
      J_TEL = SCULIB_JNU (NU, T_TEL, STATUS)
      J_AMB = SCULIB_JNU (NU, T_AMB, STATUS)

*     Calculate the mean of the input VARIANCE
*     Try to take care of negative variances (ignore them) and
*     bad values.

      SUM = 0.0D0
      COUNT = 0

      DO I = 1, N_MEASUREMENTS
         IF (J_VARIANCE(I) .NE. VAL__BADR .AND. 
     :        J_VARIANCE(I) .GE. 0.0D0) THEN
            SUM = SUM + DBLE(J_VARIANCE(I))
            COUNT = COUNT + 1
         END IF
      END DO

*     Calculate the MEAN
      IF (COUNT .GT. 0) THEN
         MEAN_VAR = SUM / DBLE(COUNT)
      ELSE
         MEAN_VAR = MIN_VAR   ! No good data anyway
      END IF

* Put data into common

      C_M = N_MEASUREMENTS
      C_J_AMB = DBLE (J_AMB)
      C_J_TEL = DBLE (J_TEL)

      DO I = 1, N_MEASUREMENTS
         C_AIRMASS (I) = DBLE (AIRMASS(I))
         C_J_MEASURED (I) = DBLE (J_MEASURED(I))
         C_J_VARIANCE (I) = MAX (MEAN_VAR, MIN_VAR)
         C_J_QUALITY (I) = 0
      END DO

*     Work out the number of degrees of freedom
*     This is number of observations plus number of free parameters - 1
*     (yes I know that the observations are free parameters)
*     + 1 since tau is always free

      NDEG = N_MEASUREMENTS - 1 + 1

* Now try to fit this

      IF (STATUS .EQ. SAI__OK) THEN

* Set initial guess and limits

         IF (B_IN .LT. 0.0) THEN
            C_B_HI = 0.9999D0
            C_B_LO = 0.0001D0
            FIT (2) = 0.7D0

            NDEG = NDEG + 1
         ELSE
            C_B_HI = DBLE (B_IN)
            C_B_LO = DBLE (B_IN)
            FIT (2) = DBLE (B_IN)
         END IF
         
         IF (ETA_TEL_IN .LT. 0.0) THEN
            C_ETA_TEL_HI = 0.9999D0
            C_ETA_TEL_LO = 0.0001D0
            FIT (1) = 0.7D0

            NDEG = NDEG + 1
         ELSE
            C_ETA_TEL_HI = DBLE (ETA_TEL_IN)
            C_ETA_TEL_LO = DBLE (ETA_TEL_IN)
            FIT (1) = DBLE (ETA_TEL_IN)
         END IF

         FIT (3) = 0.5D0

* some initial settings

         LAMBDA = 0.001
         CALL SCULIB_SKYDIP_XISQ (XISQ, 3, FIT, STATUS)
         XICUT = MAX (0.00001D0 * XISQ, 0.01D0)

* now iterate

         QUALITY = 0
         LOOPING = .TRUE.
         ITERATION = 0

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

*  output results

         REXISQ = REAL(XISQ) / REAL(NDEG) ! Reduce chi sq

         IF (QUALITY .NE. 0) THEN
            CALL MSG_SETC ('SUB', SUB_INSTRUMENT)
            CALL MSG_SETC ('FILT', SUB_FILTER)

            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_FIT_SKYDIP: fit failed '//
     :        'for filter = ^FILT and sub-instrument ^SUB', STATUS)
            CALL ERR_REP (' ', ' - last fit values were:-', STATUS)

            WRITE (BUFFER, 20) ETA_TEL_FIT, B_FIT, TAUZ_FIT, 
     :           REXISQ, ITERATION

            CALL MSG_SETC ('BUFFER', BUFFER)
            CALL ERR_REP (' ', ' ^BUFFER', STATUS)
         ELSE
            CALL MSG_SETC ('FILT', SUB_FILTER)
            CALL MSG_SETC ('SUB', SUB_INSTRUMENT)

            CALL MSG_OUT (' ', 'SCULIB: fit for filter '//
     :           '^FILT and sub-instrument ^SUB', STATUS)

            WRITE (BUFFER, 20) ETA_TEL_FIT, B_FIT, TAUZ_FIT, 
     :           REXISQ, ITERATION
 20         FORMAT ('eta = ', F6.2, '          b = ', F6.2,
     :           '  tau = ', F7.3, '  X= ', F7.1, '  N= ', I4)

            CALL MSG_SETC ('BUFFER', BUFFER)
            CALL MSG_OUT (' ', ' ^BUFFER', STATUS)
         END IF
      END IF

      END

