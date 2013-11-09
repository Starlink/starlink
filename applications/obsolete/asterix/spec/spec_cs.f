*+  SPEC_CS - Computes flux from Chapline & Stevens Comptonised brems spectrum
      SUBROUTINE SPEC_CS(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*
*   Description :
*
*    See Chapline, G. Jr. & Stevens, J., 1973. Astrophys. J., 184, 1041
*    and Lamb, P. & Sanford, P.W., 1979. Mon. Not. R. astr. Soc. 188, 555.
*    The thermal bremsstrahlung emission is evaluated by Gould's thermal
*    bremsstrahlung formula (SPEC_BR_EMISS).
*    Note: this model is only valid if: optical depth < [(m.c**2)/(2.kT)]**0.5
*     where
*      norm = PARAM(1)
*      temperure = PARAM(2)
*      optical depth = PARAM(3)
*
*    Method :
*
*     Quadratic integral approximation used for each bin.
*     Note: Contiguous energy bins are assumed (for efficiency savings).
*
*    Deficiencies :
*    Bugs :
*    Authors : BHVAD::CGH
*    History :
*     25 Aug 1988 - original
*     24 Jan 1989 - modified to handle invalid region of parameter space
*                                                            (BHVAD::CGH)
*      3 Mar 89: ASTERIX88 version, EM10 introduced (TJP)
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
        DOUBLE PRECISION SPEC_BR_EMISS  ! Function to evaluate Gould's thermal
                                        ! Bremsstrahlung formula
        REAL EM10                       ! Emission measure of 1E60 at 10 kpc
        PARAMETER (EM10=8.35774E13)
*    Local variables :
        INTEGER I
        REAL ATAU2                      ! alpha*tau*tau
        REAL EATAU2                     ! exp(-alpha*tau*tau)
        REAL DE                         ! Current channel width
        REAL E0                         ! Bottom of current channel
        REAL E1                         ! Centre of current channel
        REAL E2                         ! Top of current channel
        REAL F                          ! Unnormalised integrated photon flux
        REAL FAC1                       ! C & S correction factor at centre of
                                        ! current channel
        REAL FAC2                       ! C & S correction factor at top of
                                        ! current channel
        REAL P                          ! C & S factor P(k) divided by ATAU2
        REAL Q                          ! C & S factor Q(k) divided by ATAU2
        REAL TEMP                       ! Temperature
        REAL TAU                        ! Optical depth
        REAL X                          ! E/kT
        REAL X2                         ! X*X
        REAL X3                         ! X*X*X
        REAL Y0                         ! Unnormalized flux at bottom of
                                        ! current channel
        REAL Y1                         ! Unnormalized flux at centre of
                                        ! current channel
        REAL Y2                         ! Unnormalized flux at top of current
                                        ! channel
*-

* Status check
        IF ( STATUS .NE. SAI__OK ) RETURN
*
* Check for spot value
        IF ( ELBOUND(1) .EQ. EUBOUND(1) ) THEN
          CALL ERR_REP('SPOT','Spot values not supported by SPEC_',
     :     ' routines',STATUS)
          STATUS=SAI__ERROR
          GO TO 9000
        END IF
*
* Parameters
        TEMP=PARAM(2)
        TAU=PARAM(3)
*
        ATAU2=2.0*TEMP/511.0*TAU*TAU
        EATAU2=EXP(-ATAU2)
*
* Calculate the unnormalized flux at the bottom of the first channel and
* the Chapline & Stevens correction factor at this point
        E2=ELBOUND(1)
        Y2=SPEC_BR_EMISS(E2,TEMP,STATUS)/E2
        X=E2/TEMP
        X2=X*X
        X3=X2*X
        P=1.5*(X3-X2-X)/ATAU2
        Q=(1.5*X2-0.75*X3)/ATAU2
        FAC2=(1.0+0.375*X3*ATAU2-P*(EATAU2-1.0+ATAU2)+
     :   Q*(EATAU2*(ATAU2+1.0)-1.0))
        Y2=FAC2*Y2
*
* Integrate the flux over each energy channel and normalize it
        IF ( STATUS .EQ. SAI__OK ) THEN
          DO I=1,NEN
            E0=E2
            Y0=Y2
*
            E2=EUBOUND(I)
            DE=E2-E0
            E1=E0+DE/2.0
*
            Y1=SPEC_BR_EMISS(E1,TEMP,STATUS)/E1
            X=E1/TEMP
            X2=X*X
            X3=X2*X
            P=1.5*(X3-X2-X)/ATAU2
            Q=(1.5*X2-0.75*X3)/ATAU2
            FAC1=(1.0+0.375*X3*ATAU2-P*(EATAU2-1.0+ATAU2)+
     :       Q*(EATAU2*(ATAU2+1.0)-1.0))
            Y1=FAC1*Y1
*
            Y2=SPEC_BR_EMISS(E2,TEMP,STATUS)/E2
            X=E2/TEMP
            X2=X*X
            X3=X2*X
            P=1.5*(X3-X2-X)/ATAU2
            Q=(1.5*X2-0.75*X3)/ATAU2
            FAC2=(1.0+0.375*X3*ATAU2-P*(EATAU2-1.0+ATAU2)+
     :       Q*(EATAU2*(ATAU2+1.0)-1.0))
            Y2=FAC2*Y2
            CALL MATH_INTEGRT(DE,Y0,Y1,Y2,F)
            FLUX(I)=PARAM(1)*EM10*F
          END DO
        END IF
*
* Check that optical depth < [(m.c**2)/(2.kT)]**0.5
        IF ( ATAU2 .GE. 1.0 ) THEN
          STATUS=USER__001
        END IF
*
* Exit
 9000   IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. USER__001 ) THEN
          CALL AST_REXIT( 'SPEC_CS', STATUS )
        END IF

        END
