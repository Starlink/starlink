*+ SPEC_ST - Computes flux from Sunyaev-Titarchuk Comptonization spectrum
      SUBROUTINE SPEC_ST(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Returns the flux in each energy channel delimited by bounds ELBOUND and
*     EUBOUND from a Sunyaev-Titarchuk Comptonization model:
*     See Astr. Astrphys. 86, 121
*      where
*       norm = PARAM(1)
*       temperature = PARAM(2)
*       optical depth = PARAM(3)
*
*    Method :
*     Quadratic integral approximation used for each bin.
*     Note: Contiguous energy bins are assumed (for efficiency savings).
*    Deficiencies :
*    Bugs :
*     Setting the temperature to less than about 0.5 keV will very likely
*     cause this routine to crash, regardless of the values of the rest of
*     parameters. Setting the optical depth to less than about 2.5 will
*     also cause problems if the temperature is a few keV or less. It is
*     recommended that the default lower limits are NOT changed.
*    Authors : BHVAD::CGH
*    History :
*     22 Aug 88 : Original
*     1 Sept 88 : Attempts to stop floating overflow errors (BHVAD::CGH)
*     24 Jan 89 : Minor mods (BHVAD::CGH)
*      3 Mar 89 : ASTERIX88 version, varoius name changes (TJP)
*     25 Feb 93 : Error handling corrected (DJA)
*
*    Type definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'USER_ERR'
*    Import :
        INTEGER NEN                     ! No of energy channels
        REAL ELBOUND(NEN)               ! Lower bin bounds (keV)
        REAL EUBOUND(NEN)               ! Upper bin bounds (keV)
        REAL PARAM(3)                   ! Model parameters
*    Import-Export :
*    Export :
        REAL FLUX(NEN)                  ! Photon flux (photon/cm**2/s)
*    Status :
        INTEGER STATUS
*    Function declarations :
        REAL*8 ST_FUNCT                  ! Does most of the work
        REAL*8 ST_GAMMA                   ! Checks the input parameter for S14AAF
        REAL*8 S14AAF                   ! NAG routine to evaluate Gamma function
*    Local variables :
        INTEGER I,J
        INTEGER IFAIL                   ! NAG error parameter
        REAL COM(3)                     ! Unnormalized fluxes at bottom, middle
                                        ! and top of current channel
        REAL COMREF                     ! Reference flux
        REAL DE                         ! Width of the current channel
        REAL F                          ! Unnormalized flux integral
        REAL*8 A                        ! S-T's parameter alpha
        REAL*8 DTEMP                    ! Temperature
        REAL*8 DTAU                     ! Optical depth
        REAL*8 E(3)                     ! Bottom, middle and top of current
                                        ! channel
        REAL*8 EREF                     ! Reference energy (set to 1 keV)
        REAL*8 GAMPAR                   ! The input argument for calls to S14AAF
        REAL*8 G1                       ! First Gamma function
        REAL*8 G2                       ! Second Gamma function
        REAL*8 G3                       ! Third Gamma function
        REAL*8 G4                       ! Fourth Gamma function
        REAL*8 PI                       ! Pi
        REAL*8 RME                      ! Rest mass of electron (* pi**2 / 3)
                                        ! (in keV)
        REAL*8 STF1                     ! First evaluation of ST_FUNCT function
        REAL*8 STF2                     ! Second evaluation of ST_FUNCT function
        REAL*8 X                        ! E/kT
*    Local data :
        DATA EREF/1.0D0/,PI/3.141592654D0/,RME/1681.133714D0/
*
*-

* Status check
        IF ( STATUS .NE. SAI__OK ) RETURN

* Check for spot value
        IF ( ELBOUND(1) .EQ. EUBOUND(1) ) THEN
          STATUS=SAI__ERROR
          CALL ERR_REP('SPOT','Spot values not supported by SPEC_ ',
     :    'routines',STATUS)
          GO TO 9000
        END IF
*
* Parameters
        DTEMP=DBLE(PARAM(2))
        DTAU=DBLE(PARAM(3))
*
* Check if temperature and optical depth are OK
*       IF ( DTEMP .LT. 0.5 .OR. DTAU .LT. 2.5 ) THEN
*         DO I=1,NEN
*           FLUX(I)=0.0
*         ENDDO
*         STATUS=USER__001
*         GO TO 9000
*       END IF
*
* Temperature and optical depth OK so ...
        A=-1.5D0+DSQRT(2.25D0+RME/DTEMP/(DTAU+0.66667D0)/
     :  (DTAU+0.66667D0))
*
        X=EREF/DTEMP
*
* Do the preliminary flux calculations for the reference energy
        STF1=ST_FUNCT(A,2.0D0*A+4.0D0,X)
        STF2=ST_FUNCT(-3.0D0-A,-2.0D0*A-2.0D0,X)
*
* Calculate Gamma functions
        GAMPAR=ST_GAMMA(A)
        IFAIL=1
        G1=S14AAF(GAMPAR,IFAIL)
        IF (IFAIL .NE. 0) THEN
          PRINT *, ' IFAIL = ',IFAIL,' in S14AAF - parameter = G1 '
          STATUS=USER__001
        END IF
*
        GAMPAR=ST_GAMMA(-3.0D0-A)
        IFAIL=1
        G2=S14AAF(GAMPAR,IFAIL)
        IF (IFAIL .NE. 0) THEN
          PRINT *, ' IFAIL = ',IFAIL,' in S14AAF - parameter = G2 '
          STATUS=USER__001
        END IF
*
        GAMPAR=ST_GAMMA(2.0D0*A+4.0D0)
        IFAIL=1
        G3=S14AAF(GAMPAR,IFAIL)
        IF (IFAIL .NE. 0) THEN
          PRINT *, ' IFAIL = ',IFAIL,' in S14AAF - parameter = G3 '
          STATUS=USER__001
        END IF
*
        GAMPAR=ST_GAMMA(-2.0D0*A-2.0D0)
        IFAIL=1
        G4=S14AAF(GAMPAR,IFAIL)
        IF (IFAIL .NE. 0) THEN
          PRINT *, ' IFAIL = ',IFAIL,' in S14AAF - parameter = G4 '
          STATUS=USER__001
        END IF
*
* Finish the flux calculation for the reference energy
        COMREF=SNGL(X**(2.0D0+A)*DEXP(-X)*G1*PI/DSIN(2.0D0*PI*A)*
     :  (STF1/G2/G3-X**(-2.0D0*A-3.0D0)*STF2/G1/G4))
*
        IF (COMREF .EQ. 0.0) THEN
          DO I=1,NEN
            FLUX(I)=0.0
          END DO
          RETURN
        END IF
*
* Calculate the unnormalized flux at at the botton of the first channel
        E(1)=DBLE(ELBOUND(1))
        X=E(1)/DTEMP
        STF1=ST_FUNCT(A,2.0D0*A+4.0D0,X)
        STF2=ST_FUNCT(-3.0D0-A,-2.0D0*A-2.0D0,X)
        COM(1)=SNGL(X**(2.0D0+A)*DEXP(-X)*G1*PI/DSIN(2.0D0*PI*A)*
     :  (STF1/G2/G3-X**(-2.0D0*A-3.D0)*STF2/G1/G4))
*
* Integrate the flux over each energy channel and normalize it
        DO I=1,NEN
*
          E(3)=DBLE(EUBOUND(I))
          E(2)=DBLE(E(1)+E(3))/2.0D0
          DE=SNGL(E(3)-E(1))
*
          DO J=2,3
            X=E(J)/DTEMP
*
            STF1=ST_FUNCT(A,2.0D0*A+4.0D0,X)
            STF2=ST_FUNCT(-3.0D0-A,-2.0D0*A-2.0D0,X)
            COM(J)=SNGL(X**(2.0D0+A)*DEXP(-X)*G1*PI/DSIN(2.0D0*PI*A)*
     :      (STF1/G2/G3-X**(-2.0D0*A-3.D0)*STF2/G1/G4))
 100      END DO
*
          CALL MATH_INTEGRT(DE,COM(1),COM(2),COM(3),F)
*
          FLUX(I)=PARAM(1)*F/COMREF
*
          IF (FLUX(I) .LT. 0.0) FLUX(I)=0.0
*
          COM(1)=COM(3)
          E(1)=E(3)
*
        END DO

* Exit
 9000   IF (STATUS .NE. SAI__OK .AND. STATUS .NE. USER__001)
     :  CALL ERR_REP('EXERR','from SPEC_ST',STATUS)
*
        END
