*+  SPEC_BR_EMISS - Gould's bremmstrahlung energy spectrum
      DOUBLE PRECISION FUNCTION SPEC_BR_EMISS( ENERGY, KT, STATUS )
*    Description :
*     Returns bremss emissivity in keV/(cm**3*s*keV) from a hot (1E7-1E9 K)
*     optically thin plasma with cosmic element abundances.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Andy Pollock (BHVAD::AMTP)
*    History :
*
*     20 Aug 85: Original
*      3 Mar 89: Amended to give emissivity rather than flux (TJP)
*     12 Jan 93: Returns double precision value (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      REAL ENERGY                 ! energy in keV
      REAL KT                     ! plasma temperature in keV
*    Status :
      INTEGER STATUS
*    Local constants( in cgs units)
      REAL PI
      PARAMETER (PI=3.1415927)
      REAL C                            ! velocity of light
      PARAMETER (C=2.9979248E10)        !
      REAL E                            ! electronic charge
      PARAMETER (E=4.80325E-10)         ! esu
      REAL EC                           ! electronic charge
      PARAMETER (EC=1.60219E-19)        ! Coulomb
      REAL M                            ! electron rest mass
      PARAMETER (M=9.10956E-28)         !
      REAL ERM                          ! electron rest mass
      PARAMETER (ERM=511.0034)          ! keV
      REAL H                            ! Planck's constant
      PARAMETER (H=6.62620E-27)         !
      REAL ECW                          ! electron Compton wavelength
      PARAMETER (ECW=H/(2.*PI*M*C))     !
      REAL ALPHA                        ! fine structure constant
      PARAMETER (ALPHA=E*E*2.*PI/(H*C)) !
      REAL SZ2                          ! sum Z*Z*N(Z)/N(e)
      PARAMETER (SZ2=1.258)             ! from Trevor Ponman (BHVAD::TJP)
      REAL SZ3                          ! sum Z*Z*Z*N(Z)/N(e)
      PARAMETER (SZ3=2.767)             ! from Trevor Ponman (BHVAD::TJP)
      REAL RY                           ! 1 Rydberg in keV
      PARAMETER (RY=ALPHA*ALPHA*M*C*C*1E-10/(2.*EC))
      REAL C0
      PARAMETER (C0=2.8284271)
      REAL C1
      PARAMETER (C1=C0/5.)
      REAL NORM0                        ! spectrum norm for unit emissn measure
      PARAMETER (NORM0=4.2553843*ALPHA*ALPHA*ALPHA*ECW*ECW*C*SZ2)
*
*    Function declarations :
*
      DOUBLE PRECISION BR_DEXP		! Simple exponential
      DOUBLE PRECISION BR_K0		! modified Bessel function
      DOUBLE PRECISION BR_F		! electron-ion correction
      DOUBLE PRECISION BR_PSI		! electron-electron correction
      DOUBLE PRECISION BR_PHI		! Born approximation correction
*
*    Local variables
*
      DOUBLE PRECISION S                !
      DOUBLE PRECISION A                ! ENERGY/2KT
      REAL T                            ! KT/ERM
*
*    Persistent variables :
*
      REAL SQ_1_T,SQ_RY_KT		! sqrt(1/t) , sqrt(ry,kt)
      REAL LAST_KT
*
*    Local data :
*
      DATA LAST_KT/-1.0/
*
*    Saved variables :
*
      SAVE SQ_1_T,SQ_RY_KT,LAST_KT
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      A=DBLE(ENERGY/(2.0*KT))
      T=KT/ERM

      IF ( KT .NE. LAST_KT ) THEN
        SQ_1_T = SQRT(1.0/T)
        SQ_RY_KT = SQRT(RY/KT)
        LAST_KT = KT
      END IF

      S = 1.0D0 +
     :    BR_F(A,STATUS)/4.0*ENERGY/ERM +
     :    BR_PSI(A,STATUS)*C1/SZ2*ENERGY/ERM +
     :    BR_PHI(A,STATUS)*C0/SZ2*SZ3*SQ_RY_KT
      S = S*NORM0 * SQ_1_T * BR_DEXP(-A)*BR_K0(A,STATUS)

      IF ( STATUS .EQ. SAI__OK ) THEN
        SPEC_BR_EMISS = S
      ELSE
        SPEC_BR_EMISS = 0.0D0
        CALL ERR_REP( ' ', 'from SPEC_BR_EMISS', STATUS )
      END IF

      END
