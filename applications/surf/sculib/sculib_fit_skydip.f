*+  SCULIB_FIT_SKYDIP - fit the SKYDIP data
      SUBROUTINE SCULIB_FIT_SKYDIP (N_MEASUREMENTS, AIRMASS, DATA,
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
*        The fit allows B and ETA_TEL to vary - Free is negative, else fixed.
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
*        The constraints are:
*               0.9999 >= b   >= 0.0001
*              15      >= tau >= 0.0001
*               0.9999 >= eta_tel >= 0.0001
*
*           E04UPF is called to perform the fit. If no satisfactory fit can
*           be achieved a warning will be issued and the routine will
*           complete with bad status. Otherwise, the fitted results and 
*           the chi-squared achieved will be reported. 
*
*        Lastly, the virtual memory used as work arrays by E04UPF is freed.
*
*    Invocation :
*     CALL SCULIB_FIT_SKYDIP (N_MEASUREMENTS, AIRMASS, DATA,
*    :  VARIANCE, SUB_WAVELENGTH, SUB_INSTRUMENT, SUB_FILTER, T_TEL,
*    :  T_AMB, SUB_ETA_TEL, B_FIT1, TAUZ_FIT1, ETA_TEL_FIT2, B_FIT2,
*    :  TAUZ_FIT2, STATUS)
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
*     B_IN                      = REAL (Returned)
*              current value of B to be used for fit
*     ETA_TEL_FIT               = REAL (Returned)
*              fitted value of eta_tel
*     B_FIT                     = REAL (Returned)
*              fitted value of b with eta_tel
*     TAUZ_FIT                  = REAL (Returned)
*              fitted value of tauz with eta_tel
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
*     $Log$
*     Revision 1.2  1996/08/16 15:36:00  timj
*     Now only does one fit (for off-line SKYDIP). Accepts B and ETA_TEL as free
*     or fixed paramters. Returns bad status if fit fails.
*
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'              ! for VAL__NBx
*    Import :
      INTEGER N_MEASUREMENTS
      REAL    AIRMASS (N_MEASUREMENTS)
      REAL    DATA (N_MEASUREMENTS)
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
      EXTERNAL SCULIB_SKYCON_1       ! CONFUN routine for E04UPF
      EXTERNAL SCULIB_SKYFUNC_1      ! OBJFUN routine for E04UPF
      REAL SCULIB_JNU                ! brightness temperature function
*    Global variables :
*    Local Constants :
      REAL LIGHT                     ! velocity of light
      PARAMETER (LIGHT = 2.997929E8)
*    Local variables :
      DOUBLE PRECISION BL (4)        ! BL array in E04UPF, holds lower limits
                                     ! of constraints on fitted parameters
      DOUBLE PRECISION BU (4)        ! BU array in E04UPF, holds upper limits
                                     ! of constraints on fitted parameters
      CHARACTER*80     BUFFER        ! buffer to hold results of fit
      DOUBLE PRECISION C (1)         ! C array in E04UPF
      DOUBLE PRECISION CHISQ         ! OBJF in E04UPF (the chi-squared of the
                                     ! fit
      DOUBLE PRECISION CJAC (1,3)    ! CJAC array in E04UPF
      DOUBLE PRECISION CLAMBDA (4)   ! CLAMBDA array in E04UPF
      DOUBLE PRECISION DIGNORE       ! unused parameter of E04UPF
      INTEGER          IFAIL         ! NAG IFAIL parameter
      INTEGER          ISTATE (4)    ! ISTATE array in E04UPF
      INTEGER          ITER          ! number of iterations performed by E04UPF
      INTEGER          IUSER (1)     ! IUSER array in E04UPF
      REAL             J_AMB         ! brightness temperature of ambient air
      REAL             J_TEL         ! brightness temperature of telescope 
      INTEGER          LIWORK        ! dimension of IWORK array
      INTEGER          LWORK         ! dimension of WORK array
      INTEGER          L_STATUS      ! local status
      INTEGER          NAG_FJAC_END  ! pointer to end of FJAC array in E04UPF
      INTEGER          NAG_FJAC_PTR  ! pointer to FJAC array in E04UPF
      INTEGER          NAG_F_END     ! pointer to end of array F in E04UPF 
      INTEGER          NAG_F_PTR     ! pointer to array F in E04UPF 
      INTEGER          NAG_IWORK_END ! pointer to end of IWORK array in E04UPF
      INTEGER          NAG_IWORK_PTR ! pointer to IWORK array in E04UPF
      INTEGER          NAG_USER_END  ! pointer to end of USER array in E04UPF
      INTEGER          NAG_USER_PTR  ! pointer to USER array in E04UPF
      INTEGER          NAG_WORK_END  ! pointer to end of WORK array in E04UPF
      INTEGER          NAG_WORK_PTR  ! pointer to WORK array in E04UPF
      INTEGER          NCLIN         ! the number of linear constraints to the
                                     ! sky-dip fit in E04UPF
      INTEGER          NCNLN         ! the number of non-linear constraints
                                     ! to the sky-dip fit in E04UPF (see NAG
                                     ! manual)
      REAL             NU            ! frequency
      DOUBLE PRECISION R (3,3)       ! R array in E04UPF
      DOUBLE PRECISION X (3)         ! X array in E04UPF, the initial guess on
                                     ! input and fitted result on output of the
                                     ! fitted parameters
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

*  get VM for work arrays

      CALL SCULIB_MALLOC (N_MEASUREMENTS * VAL__NBD, NAG_F_PTR, 
     :  NAG_F_END, STATUS)
      CALL SCULIB_MALLOC (N_MEASUREMENTS * 3 * VAL__NBD, NAG_FJAC_PTR, 
     :  NAG_FJAC_END, STATUS)
      LIWORK = 2 + 9 + N_MEASUREMENTS * 6 
      CALL SCULIB_MALLOC (LIWORK * VAL__NBI, NAG_IWORK_PTR, 
     :  NAG_IWORK_END, STATUS)
      LWORK = 18 + 6 + 60 + 21 + N_MEASUREMENTS * 6
      CALL SCULIB_MALLOC (LWORK * VAL__NBD, NAG_WORK_PTR, NAG_WORK_END, 
     :  STATUS)
      CALL SCULIB_MALLOC ((N_MEASUREMENTS * 3 + 3) * VAL__NBD,
     :  NAG_USER_PTR, NAG_USER_END, STATUS)

*  turn off printing inside E04UPF and set tolerance of fit

      IF (STATUS .EQ. SAI__OK) THEN
         CALL E04URF ('Nolist')
         CALL E04URF ('Major print level    0')
         CALL E04URF ('Minor print level    0')
         CALL E04URF ('Optimality tolerance 1.0E-3')
         CALL E04URF ('Derivative level   3')

*  fill USER array with appropriate numbers

         NU = LIGHT / (SUB_WAVELENGTH * 1.0E-6)
         J_TEL = SCULIB_JNU (NU, T_TEL, STATUS)
         J_AMB = SCULIB_JNU (NU, T_AMB, STATUS)

         CALL SCULIB_SET_USER (J_TEL, J_AMB, N_MEASUREMENTS,
     :     AIRMASS, DATA, VARIANCE, %val(NAG_USER_PTR))

         NCLIN = 0
         NCNLN = 1
   
*  Set initial guess on tau

         X (3) = 0.1D0

*  set arrays holding limits and constraints for fitted values:-
*
*      index 1 = ETA_TEL
*      index 2 = B
*      index 3 = TAU
*      index 4 = non-linear constraint  - ETA_TEL * B

*  lower and upper limits

         BL (3) = 0.0001D0
         BL (4) = 0.0001D0

         BU (3) = 15.0D0
         BU (4) = 0.9999D0


         IF (SUB_ETA_TEL .LT. 0.0) THEN
            X(1)  = 0.99D0
            BL(1) = 0.0001D0
            BU(1) = 0.9999D0
         ELSE
            X(1)  = DBLE (SUB_ETA_TEL)
            BL(1) = DBLE (SUB_ETA_TEL)
            BU(1) = DBLE (SUB_ETA_TEL)
         ENDIF

         IF (B_IN .LT. 0.0) THEN
            X(2)  = 0.99D0
            BL(2) = 0.0001D0
            BU(2) = 0.9999D0
         ELSE
            X(2)  = DBLE (B_IN)
            BL(2) = DBLE (B_IN)
            BU(2) = DBLE (B_IN)
         ENDIF


* Now try to fit this

         IF (STATUS .EQ. SAI__OK) THEN
            IFAIL = 1

            CALL E04UPF (N_MEASUREMENTS, 3, 0, 1, 1, 1, 
     :        N_MEASUREMENTS, 3, DIGNORE, BL, BU, 
     :        SCULIB_SKYCON_1, SCULIB_SKYFUNC_1, ITER, 
     :        ISTATE, C, CJAC, %val(NAG_F_PTR), 
     :        %val(NAG_FJAC_PTR), CLAMBDA, CHISQ, R, X, 
     :        %val(NAG_IWORK_PTR), LIWORK, 
     :        %val(NAG_WORK_PTR), LWORK, IUSER, 
     :        %val(NAG_USER_PTR), IFAIL)

            IF (IFAIL .NE. 0) THEN
               CALL MSG_SETI ('IFAIL', IFAIL)
               CALL MSG_SETC ('SUB', SUB_INSTRUMENT)
               CALL MSG_SETC ('FILT', SUB_FILTER)
               STATUS = SAI__WARN
*     I didnt flush ERR before returning...
               CALL ERR_OUT (' ', 'SCULIB_FIT_SKYDIP: '//
     :           'E04UPF has failed to fit ETA_TEL, B and '//
     :           'TAUZ for filter = ^FILT and sub-instrument '//
     :           '^SUB with IFAIL = ^IFAIL', STATUS)

               STATUS = SAI__WARN
               ETA_TEL_FIT = REAL (X(1))
               B_FIT = REAL (X(2))
               TAUZ_FIT = REAL (X(3))
            ELSE
               CALL MSG_SETC ('FILT', SUB_FILTER)
               CALL MSG_SETC ('SUB', SUB_INSTRUMENT)
               CALL MSG_OUT (' ', 'SCULIB: fit for filter '//
     :           '^FILT and sub-instrument ^SUB with ETA '//
     :           'varying', STATUS)

               ETA_TEL_FIT = REAL (X(1))
               B_FIT = REAL (X(2))
               TAUZ_FIT = REAL (X(3))

               WRITE (BUFFER, 20) ETA_TEL_FIT, B_FIT, TAUZ_FIT, 
     :           REAL(CHISQ), ITER
 20            FORMAT ('eta = ', F6.2, '          b = ', F6.2,
     :           '  tau = ', F6.2, '  X = ', F7.1, '  N = ', I4)
 
               CALL MSG_SETC ('BUFFER', BUFFER)
               CALL MSG_OUT (' ', ' ^BUFFER', STATUS)
            END IF
         END IF
      END IF

*  free VM for work arrays

      L_STATUS = SAI__OK
      CALL SCULIB_FREE ('NAG_F', NAG_F_PTR, NAG_F_END, L_STATUS)
      IF (L_STATUS .NE. SAI__OK) THEN
         CALL ERR_FLUSH (L_STATUS)
      END IF
      CALL SCULIB_FREE ('NAG_FJAC', NAG_FJAC_PTR, NAG_FJAC_END, 
     :  L_STATUS)
      IF (L_STATUS .NE. SAI__OK) THEN
         CALL ERR_FLUSH (L_STATUS)
      END IF
      CALL SCULIB_FREE ('NAG_IWORK', NAG_IWORK_PTR, NAG_IWORK_END, 
     :  L_STATUS)
      IF (L_STATUS .NE. SAI__OK) THEN
         CALL ERR_FLUSH (L_STATUS)
      END IF
      CALL SCULIB_FREE ('NAG_WORK', NAG_WORK_PTR, NAG_WORK_END, 
     :  L_STATUS)
      IF (L_STATUS .NE. SAI__OK) THEN
         CALL ERR_FLUSH (L_STATUS)
      END IF
      CALL SCULIB_FREE ('NAG_USER', NAG_USER_PTR, NAG_USER_END, 
     :  L_STATUS)
      IF (L_STATUS .NE. SAI__OK) THEN
         CALL ERR_FLUSH (L_STATUS)
      END IF

      END
