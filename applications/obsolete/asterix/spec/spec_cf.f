*+ SPEC_CF - Constant pressure cooling flow model
      SUBROUTINE SPEC_CF(NDS,NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*
*    Description :
*
*     Integrated emission from a hot plasma cooling steadily at constant
*     pressure.
*     Spectral luminosity in these circumstances is
*          L(E)=(5*k*Mdot)/(2*m)*Int_0^Tmax{(X(E,t)/X(T))*dT} erg/(s keV)
*     where
*       Mdot is mass cooling rate, m is mean particle mass (~0.5 amu),
*       X(E,T) is spectral emissivity at temp T [~erg/(cm^3 s keV)]
*       X(T)=Int_0^inf{X(E,T)*dE} is total emissivity at T [~erg/(cm^3 s)]
*     Observed photon flux is therefore
*            F(E)=L(E)/(4*pi*d^2*Eerg) photon/(cm^2 s keV),
*     where d is the source distance in cm and Eerg is the photon energy
*     in ergs.
*     This is a function of just three parameters: Mdot/d^2 , Tmax & Z.
*
*     The model parameters used are:
*	A    = PARAM(1)   Mdot/d^2 in units of 10 Msol p.a. at 100 Mpc
*       TMAX = PARAM(2)   Temperature from which gas is cooling
*       Z    = PARAM(3)   Metallicity in cosmic units
*
*    Method :
*
*     Spectrum is sythesized from a superposition of Raymond and Smith
*     spectra each derived by interpolation (over temperature and if
*     necessary energy) from a precalculated grid. See model RZ for details.
*     The temperature bins are distributed as T^2 as a compromise between
*     linear and logarithmic spacing.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*
*    Deficiencies :
*
*     Note that the assumption of "constant pressure" requires, in the
*     context of cooling flows, that the gravitational potential change
*     through the flow is negligible (compared to the thermal energy of
*     the gas). If this is not so then (for a given Mdot) extra luminosity
*     will be fed into the temperature phases where the potential gradient
*     is important.
*     If pressure really is constant, then the spectrum is applicable even
*     in the case where the flow is "multiphase".
*
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*
*    History :
*
*     15 May 91 : Original (TJP)
*     25 Feb 93 : Error handling corrected (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
	INTEGER NDS			! Dataset number
	INTEGER NEN			! No of energy channels
	REAL ELBOUND(NEN)		! Lower bin bounds (keV)
	REAL EUBOUND(NEN)		! Upper bin bounds (keV)
	REAL PARAM(3)			! Model parameters
*    Import-Export :
*    Export :
	REAL FLUX(NEN)			! Photon flux (phot/cm**2/s)
*    Status :
	INTEGER STATUS
*    Local constants :
	INTEGER NT			! Number of temp. bands used
	PARAMETER (NT=100)
	REAL TMIN			! Minimum gas temperature contributing
	PARAMETER (TMIN=0.088)		!  to observed spectrum (in keV)
	REAL NORM			! 10 Msol/p.a. at 100 Mpc converted
	PARAMETER (NORM=1.328E-3)	!  to keV/(cm^2*s*keV)
*    Local variables :
	INTEGER I			! Energy index
	INTEGER J			! Temperature index
	INTEGER EMSPTR			! Pointer to emissivity array
	REAL A				! Normalisation (Mdot/distance^2)
	REAL TMAX			! Max temperature of cooling gas
	REAL Z				! Metallicity (cosmic units)
	REAL DT				! Temperature step (parabolic)
	REAL TLO,THI			! Temp bounds
	REAL TAV			! Mean temp in band
	REAL EMTOT			! Total (energy integrated) emissivity
	REAL EMS			! Single spectral emissivity value
	REAL FAC			! Scale factor (for use in loop)
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Check for spot value
	IF(ELBOUND(1).EQ.EUBOUND(1))THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('SPOT','Spot values not supported by SPEC_ routines',
     :    STATUS)
	  GO TO 9000
	ENDIF

* Parameters
	A=PARAM(1)
	TMAX=PARAM(2)
	Z=PARAM(3)

* Set up temperature step (for parabolic model TN=TMIN+DT*(N-1)^2, N=0 TO N-1)
	DT=(TMAX-TMIN)/(NT-1)**2

* Initialise flux array and set up dynamic storage for emissivity array
	DO I=1,NEN
	  FLUX(I)=0.0
	ENDDO
	CALL DYN_MAPR(1,NEN,EMSPTR,STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000

* Loop through temperature bands
	DO J=1,NT-1
	  TLO=TMIN+DT*(J-1)**2
	  THI=TMIN+DT*J**2
	  TAV=(TLO+THI)/2.0
D	  print *,'tlo,thi,tav:',tlo,thi,tav

*   Get spectral and bolometric emissivity
	  CALL SPEC_RZ_EMISS(NDS,TAV,Z,NEN,ELBOUND,EUBOUND,PARAM,
     :	  %VAL(EMSPTR),STATUS)
	  CALL SPEC_RZ_BOL(NDS,TAV,Z,EMTOT,STATUS)
	  IF(STATUS.NE.SAI__OK) GO TO 9000

*   Add contributions from this temp band to FLUX array
	  FAC=(THI-TLO)/EMTOT
	  DO I=1,NEN
	    CALL ARR_ELEM1R(EMSPTR,NEN,I,EMS,STATUS)
	    FLUX(I)=FLUX(I)+EMS*FAC
D	    print *,'emean,ems,emtot:',(elbound(i)+eubound(2))/2,ems,emtot
	  ENDDO
	ENDDO					! Next temp.

*    Normalise and divide by photon energy to give photon/(cm^2*s)
      DO I=1,NEN
	FLUX(I)=A*NORM*FLUX(I)
C.........Note, no need to divide by EMEAN, since EMS was in photons and EMTOT
C         in keV. i.e. it's already been done effectively.
      END DO

*    Exit
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SPEC_CF', STATUS )
      END IF

      END
