*+  SPEC_BR - Gould's thermal bremsstrahlung formulation
      SUBROUTINE SPEC_BR(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Returns the photon fluxes in the energy channels delimited by bounds
*     for a thermal bremsstrahlung model, including various corrections
*     important at the ~10% level for hot (~1E8 K) plasmas.
*     Returned fluxes are  photons/(cm**2*s*keV).
*     Emission measure/distance**2 (parameter 1) is Ne**2*V/d**2 expressed
*     in units of 1E60 cm**3/10kpc**2.
*    Method :
*     Taken from R.J.Gould,Astrophys.J.,238,1026,1980 although an extra factor
*     of 'a' has been added to the denominator of the third term of his equation
*     (31) to make the numbers computed from this exact formula agree with the
*     values in his table 1 which themselves are approximated by his equation
*     (46). Quadratic integral approximation is used.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Deficiencies :
*     Gould's approximations are valid only for a hot (1E7-1E9 K) plasma, and
*     should be treated cautiously for cooler plasmas.
*    Bugs :
*     MJC has found that the integrated flux from this routine is significantly
*     (>20%) larger than the result obtained from the formula for integrated
*     flux given by Gould. However the good agreement obtained with SPEC_BH
*     suggests that it is the integrated flux expression which is inaccurate.
*    Authors :
*     Andy Pollock (BHVAD::AMTP)
*    History :
*     20 August 1985 : original
*     26 Apr 88: Lower and upper energy bounds passed in separate arrays (TJP)
*     14 Jul 88 : Amended to cope with high values of E/kT (BHVAD::RDJ)
*     12 Jan 1989 : Photon fluxes now calculated using a quadratic integral
*      approximation (BHVAD::CGH)
*      3 Mar 89: Tidied up for ASTERIX88 version, EM10 included in SPEC_BR (TJP)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      INTEGER NEN                   ! no of energy channels
      REAL ELBOUND(NEN)		    ! lower bin bounds (keV)
      REAL EUBOUND(NEN)		    ! upper bin bounds (keV)
      REAL PARAM(2)                 ! model parameters
*    Export :
      REAL FLUX(NEN)                ! computes photon fluxes
*    Status :
      INTEGER STATUS
*    Function declarations :
      DOUBLE PRECISION SPEC_BR_EMISS ! bremsstrahlung energy spectrum
*    Local constants :
      REAL EM10                         ! emission measure of 1e60 at 10 kpc
      PARAMETER (EM10=8.35774E13)
*    Local variables :
      REAL             E0,E1,E2		! energy channel lower, middle and upper
                                    ! bounds
      DOUBLE PRECISION Y0,Y1,Y2                 ! corresponding bremsstrahlung photon
                                    ! intensity
      DOUBLE PRECISION DE                       ! channel width
      DOUBLE PRECISION F                        ! unnormalized integrated photon flux
      DOUBLE PRECISION NORM                     ! spectrum normalisation
      REAL KT               		! plasma temperature in keV
      INTEGER I
*-

* Status check
      IF(STATUS.NE.SAI__OK)RETURN

* Check for spot value
	IF(ELBOUND(1).EQ.EUBOUND(1))THEN
	  CALL ERR_REP('SPOT','Spot values not supported by SPEC_ routines',
     :    STATUS)
	  STATUS=SAI__ERROR
	  GO TO 9000
	ENDIF

* Parameters
      NORM=PARAM(1)*EM10
      KT=PARAM(2)

* Calculate 3 values per energy bin and perform quadratic integration
      E2=ELBOUND(1)
      Y2=SPEC_BR_EMISS(E2,KT,STATUS)/E2		! /E gives PHOTON emissivity
      IF(STATUS.EQ.SAI__OK)THEN
         DO I=1,NEN
            E0=E2
            Y0=Y2
            E2=EUBOUND(I)
            DE=E2-E0
            E1=E0+DE/2.0
*
            Y1=SPEC_BR_EMISS(E1,KT,STATUS)/E1
            Y2=SPEC_BR_EMISS(E2,KT,STATUS)/E2
*
            CALL MATH_INTEGRTD(DE,Y0,Y1,Y2,F)
            FLUX(I)=NORM*F
         END DO
      ENDIF

* Exit
 9000 IF(STATUS.NE.SAI__OK)CALL ERR_REP('EXERR','from SPEC_BR',STATUS)
      END
