*+  SCULIB_FIT_SKYDIP - fit the SKYDIP data
      SUBROUTINE SCULIB_FIT_SKYDIP (N_MEASUREMENTS, AIRMASS, J_MEASURED,
     :  VARIANCE, SUB_WAVELENGTH, SUB_INSTRUMENT, SUB_FILTER, T_TEL,
     :  T_AMB, SUB_ETA_TEL, B_IN, ETA_TEL_FIT, B_FIT, TAUZ_FIT, STATUS)
*    Description :
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
*        Two fits are made; the first with ETAtel fixed and varying only b 
*     and tau, the second allowing all 3 to vary. The first fit will give the 
*     measurement of tau to be used and a value for b, the second will give 
*     values for ETA_tel, tau and b to be used for long-term sky statistics.
*        The fitting is done using the NAG routine E04UPF, which needs several
*     work arrays whose size depends on the number of measurements taken.
*
*     The routine proceeds as follows:-
*
*        If status is good on entry, virtual memory is obtained for the E04UPF
*        work arrays.
*
*        The NAG routine E04URF is called to turn off internal output from 
*        E04UPF.
*
*        SCULIB_SET_USER is called to fill with appropriate information the 
*        USER array which communicates it to routines called by E04UPF.
*
*        For fit 1 -
*
*           An initial guess for the solution is set; eta_tel to the 
*           current value used, b = 0.9 and tau = 0.1
*
*           Constraints on the values are set such that -
*
*              eta_tel  = fixed
*               0.9999 >= b   >= 0.0001
*              15      >= tau >= 0.0001
*               0.9999 >= eta_tel * b >= 0.0001
*
*           E04UPF is called to perform the fit. If no satisfactory fit can
*           be achieved a warning will be issued but the routine will
*           complete with good status. Otherwise, the fitted results and 
*           the chi-squared achieved will be reported. 
*
*        Fit 2 is done in exactly the same way but eta_tel is allowed to
*        vary subject to the constraint -
*
*               0.9999 >= eta_tel >= 0.0001
*
*        Lastly, the virtual memory used as work arrays by E04UPF is freed.
*
*    Invocation :
*     CALL SCULIB_FIT_SKYDIP (N_MEASUREMENTS, AIRMASS, DATA,
*    :  VARIANCE, SUB_WAVELENGTH, SUB_INSTRUMENT, SUB_FILTER, T_TEL,
*    :  T_AMB, SUB_ETA_TEL, B_FIT1, TAUZ_FIT, STATUS)
*    Parameters :
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
*     SUB_ETA_TEL               = REAL (Given)
*              current value of eta_tel for this sub-instrument/filter
*     B_FIT                     = REAL (Returned)
*              fitted value of b with eta_tel fixed at SUB_ETA_TEL
*     TAUZ_FIT                 = REAL (Returned)
*              fitted value of tauz with eta_tel fixed at SUB_ETA_TEL
*     STATUS                    = INTEGER (Given and returned)
*              Global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*      7-FEB-1996: split off from SCUDR_SKYDIP_SWITCH.
*     23-JUL-1996: renamed SCULIB_ from SCUDR_ and made to fit the correct
*                  function (JFL).
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'              ! for VAL__NBx
*    Import :
      INTEGER N_MEASUREMENTS
      REAL    AIRMASS (N_MEASUREMENTS)
      REAL    J_MEASURED (N_MEASUREMENTS)
      REAL    VARIANCE (N_MEASUREMENTS)
      REAL    SUB_WAVELENGTH
      CHARACTER*(*) SUB_INSTRUMENT
      CHARACTER*(*) SUB_FILTER
      REAL    T_TEL
      REAL    T_AMB
      REAL    SUB_ETA_TEL
      REAL    B_IN
*    Import-Export :
*    Export :
      REAL    B_FIT
      REAL    TAUZ_FIT
      REAL    ETA_TEL_FIT
*    Status :
      INTEGER STATUS
*    External references :
      REAL     SCULIB_JNU            ! brightness temperature function
      EXTERNAL SCULIB_SKYFUNC        ! Skydip function
      EXTERNAL SCULIB_SKYFUNCD       ! Partial derivatives of skydip fn
*    Global variables :
*    Local Constants :
      REAL             LAB           ! Mixing parameter of fit
      PARAMETER (LAB = 0.01)
      REAL             LIGHT         ! velocity of light
      PARAMETER (LIGHT = 2.997929E8)
      INTEGER          MAX_ITER      ! Maximum number of allowed iterations
      PARAMETER (MAX_ITER = 1000)
      INTEGER          MAX_PTS       ! Maximum number of data points for fit
      PARAMETER (MAX_PTS = 100)
      REAL             MIN_VAR       ! Minimum variance for weighting
      PARAMETER (MIN_VAR = 1.0E-5)
      INTEGER          NUM_PARS      ! Number of parameters
      PARAMETER (NUM_PARS = 5)   
      REAL             TOL           ! Tolerance of fit
      PARAMETER (TOL = 1.0E-5)
*    Local variables :
      CHARACTER*80     BUFFER        ! buffer to hold results of fit
      REAL             CHISQ         ! the reduced chi-squared of the fit
      REAL             ERR_PARS(NUM_PARS) ! error in parameters
      INTEGER          I             ! Loop variable
      REAL             J_AMB         ! brightness temperature of ambient air
      REAL             J_TEL         ! brightness temperature of telescope 
      REAL             LOWER(NUM_PARS)! Lower limit of parameters
      INTEGER          NRT           ! Number of iterations for fit (-ve=err)
      REAL             NU            ! frequency
      INTEGER          NUM_DOF       ! Number of degrees of freedom for CHISQ
      INTEGER          MASK(NUM_PARS)! Parameter mask (free or fixed)
      REAL             PARS(NUM_PARS)! Initial value of each parameter
      REAL             UPPER(NUM_PARS)! Upper limit of parameters
      REAL             VALUE         ! Value of skydip function
      REAL             WEIGHTS(MAX_PTS)! Weight of each data point

*    Internal References :
*    Local data :
      DATA ERR_PARS/0.,0.,0.,0.,0./  ! No errors on input parameters
*-

      IF (STATUS .NE. SAI__OK) RETURN

* Check number of data points for fit

      IF (N_MEASUREMENTS .GT. MAX_PTS) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP('','SCULIB_FIT_SKYDIP: Too many data points',
     :        STATUS)
         RETURN
      END IF

* Calculate J_TEL and J_AMB

      NU = LIGHT / (SUB_WAVELENGTH * 1.0E-6)
      J_TEL = SCULIB_JNU (NU, T_TEL, STATUS)
      J_AMB = SCULIB_JNU (NU, T_AMB, STATUS)

* Calculate weight

      DO I = 1, N_MEASUREMENTS
         IF (VARIANCE(I) .LT. 0.0) THEN 
            WEIGHTS(I) = 0.0
         ELSE IF (VARIANCE(I) .GT. MIN_VAR) THEN
            WEIGHTS(I) = 1.0 / SQRT(VARIANCE(I))
         ELSE
            WEIGHTS(I) = 1.0 / SQRT(MIN_VAR)
         ENDIF
      END DO

*  set arrays holding details of each parameter:-
*
*      index 1 = ETA_TEL 
*      index 2 = B
*      index 3 = TAU
*            4 = J_AMB
*            5 = J_TEL
*       J_AMB and J_TEL are always fixed. Use parameters to avoid COMMON
*
* Need to set initial guess for parameter (PARS) and
* whether the parameter is fixed(0) or free(1) via MASK
* upper and lower bounds are set via the UPPER and LOWER arrays

*  Deal with ETA_TEL
      IF (SUB_ETA_TEL .LT. 0.0) THEN
         MASK(1) = 1
         PARS(1) = 0.99
         NUM_DOF = NUM_DOF + 1
      ELSE
         MASK(1) = 0
         PARS(1) = SUB_ETA_TEL
      ENDIF

      UPPER(1) = 0.9999
      LOWER(1) = 0.0001

*  The second parameter is B
      IF (B_IN .LT. 0.0) THEN
         MASK(2) = 1
         PARS(2) = 0.99
         NUM_DOF = NUM_DOF + 1
      ELSE
         MASK(2) = 0
         PARS(2) = B_IN
      ENDIF

      UPPER(2) = 0.9999
      LOWER(2) = 0.0001

* TAU
      MASK(3) = 1      ! Free parameter
      PARS(3) = 0.5    ! Initial guess
      NUM_DOF = 1
      UPPER(3) = 15.00
      LOWER(3) = 0.0001

* J_AMB
      MASK(4) = 0
      PARS(4) = J_AMB

* J_TEL
      MASK(5) = 0
      PARS(5) = J_TEL
      
* Now try to fit this
      IF (STATUS .EQ. SAI__OK) THEN

         CALL LSQ_FIT (AIRMASS, 1, J_MEASURED, WEIGHTS,
     :        N_MEASUREMENTS, PARS, ERR_PARS, NUM_PARS, TOL,
     :        MAX_ITER, LAB, MASK, .TRUE., LOWER, UPPER, NRT,
     :        SCULIB_SKYFUNC, SCULIB_SKYFUNCD)

         ETA_TEL_FIT = PARS(1)
         B_FIT = PARS(2)
         TAUZ_FIT = PARS(3)

         IF (NRT .LT. 0) THEN
            CALL MSG_SETI ('IFAIL', NRT)
            CALL MSG_SETC ('SUB', SUB_INSTRUMENT)
            CALL MSG_SETC ('FILT', SUB_FILTER)
            STATUS = SAI__WARN
*     I didnt flush ERR before returning...
            CALL ERR_OUT (' ', 'SCULIB_FIT_SKYDIP: '//
     :           'LSQFIT has failed to fit ETA_TEL, B and '//
     :           'TAUZ for filter = ^FILT and sub-instrument '//
     :           '^SUB with IFAIL = ^IFAIL', STATUS)

            STATUS = SAI__WARN
*     Report the FIT error message
            IF (NRT .EQ. -4) THEN
               CALL ERR_REP(' ','Determinant of the coefficient '//
     :              'matrix is zero', STATUS)
            ELSE IF (NRT .EQ. -3) THEN
               CALL ERR_REP(' ','Diagonal of matrix contains '//
     :              'elements which are zero or almost zero', STATUS)
            ELSE IF (NRT .EQ. -2) THEN
               CALL ERR_REP(' ','Maximum number of iterations '//
     :              'too small to obtain a solution which satisfies '//
     :              'the required tolerance', STATUS)
            ENDIF

         ELSE

*     Calculate CHISQ by hand
            CHISQ = 0.0
            DO I = 1, N_MEASUREMENTS
               IF (VARIANCE(I) .GT. 0.0) THEN
                  CALL SCULIB_SKYFUNC(VALUE, AIRMASS(I), PARS, 5)
                  CHISQ = CHISQ + (J_MEASURED(I) - VALUE)**2 
     :                 / VARIANCE(I)
                  NUM_DOF = NUM_DOF + 1
               ENDIF
            END DO
* Reduced Chi Sq - num of free parameters
            CHISQ = CHISQ / REAL(NUM_DOF - 1)


*  Print out answer
            CALL MSG_SETC ('FILT', SUB_FILTER)
            CALL MSG_SETC ('SUB', SUB_INSTRUMENT)
            CALL MSG_OUT (' ', 'SCULIB: fit for filter '//
     :           '^FILT and sub-instrument ^SUB with ETA '//
     :           'varying', STATUS)
            
            WRITE (BUFFER, 20) PARS(1), PARS(2), PARS(3), 
     :           CHISQ, NRT
 20         FORMAT ('eta = ', F6.2, '          b = ', F6.2,
     :           '  tau = ', F6.2, '  X = ', F7.1, '  N = ', I4)

            CALL MSG_SETC ('BUFFER', BUFFER)
            CALL MSG_OUT (' ', ' ^BUFFER', STATUS)
         END IF
      END IF

      END

