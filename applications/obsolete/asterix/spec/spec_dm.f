*+ SPEC_DM - Computes spectra from a differential emission measure model
	SUBROUTINE SPEC_DM(NDS,NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)

* Description:
*  Returns the flux in each energy channel from a differential emission
*  measure model defined by DEM=cT^a between two cut off temperatures,
*  where DEM=d(EM)/dT rather than DEM=T.d(EM)/dT.
*
*  Returned flux has units of photons/(cm**2*s*keV).
*  Model parameters are:
*   A=PARAM(1)       Normalisation/emission measure in units of
*                    1E60cm**-3/(10kpc)**2 (see later)
*   SLOPE=PARAM(2)   Slope of the DEM power law
*   TMIN=PARAM(3)    Low temperature cut off in keV
*   TMAX=PARAM(4)    High temperature cut off in keV
*   METALS=PARAM(5)  Metal abundance
*  Note that definition of emission measure is Ne**2*V
*
* Note also: A RS grid matching the instrument response MUST be used,
* this can be created by RS_REBIN in OBELIX.
*
* Method:
*  A grid is calculated for a range of parameter values. For each set
*  of values the differential emission model is integrated in 21
*  sections and the R-S models at each temperature summed in a weighted
*  fashion. The range of the grid is obtained from the dataset.
*  As required, for any particular set of parameter values, the six model
*  spectra which bracket these values are extracted, and a linear
*  interpolation performed.
*  The emissivity grid is multiplied by Ne*NH*V to obtain radiated power.
*
*    Authors:
*
*     Rob Jeffries (BHVAD::RDJ)
*     Martin Watt (BHVAD::MPW)
*
*    History:
*
*      3 Jun 89 : Original
*     10 Sep 90 : PARAM(1) changed to be the actual DEM for a slope of -1
*     14 Feb 91 : Include abundance as free parameter (MPW)
*     28 Feb 91 : ASTERIX88 version (MPW)
*      1 Mar 91 : Read components from RS grid to determine temperatures
*                 in grid (MPW)
*     15 Mar 91 : Implement automatic searching of RS directory for grids to
*                 match the energy bounds in the data (see SPEC_RM header for
*                 details) (MPW)
*      7 May 91 : Tidied for release (MPW)
*      7 Oct 92 : ARR_INITR call changed to ARR_INIT1R (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
	INTEGER NDS			! dataset number
	INTEGER NEN			! No of energy channels
	REAL ELBOUND(NEN)		! Lower bin bounds (keV)
	REAL EUBOUND(NEN)		! Upper bin bounds (keV)
	REAL PARAM(5)			! Model parameters

*    Export :
	REAL FLUX(NEN)			! Photon flux (phot/cm**2/s)

*    Status :
	INTEGER STATUS

*    Local constants :
	REAL EM10			! EM of 1E60 cm**-3 at 10 kpc in
	PARAMETER (EM10=8.35774E13)	! units of cm**-6*cm**3/cm**2

	INTEGER MAXRESP			! Maximum number of energy bands
	PARAMETER (MAXRESP=3000)

*    Local variables :
	INTEGER I,J

	REAL A				! Normalisation (e.m./distance**2)
        REAL SLOPE              	! =PARAM(2)
        REAL TMIN               	! =PARAM(3)
        REAL TMAX               	! =PARAM(4)
	REAL METALS			! [Metal/H] relative to cosmic

	REAL BRPARAM(3)			! model parameters for SPEC_BR, if required

	REAL FACTOR			! multiplication factor

	REAL TEMPFLUX(MAXRESP)		! temporary array for flux in each temperature band

	REAL DT
	REAL KT				! temperature of lower bound of bins in keV
	REAL TEMP			! temperature of bin centres in kev
	REAL NORMO
	REAL M1,M2
	REAL DS

	REAL T		 		! temporary temperature for swapping TMIN & TMAX

	LOGICAL MINUSONE		! DEM has a slope of exactly -1

*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Check for spot value
	IF(ELBOUND(1).EQ.EUBOUND(1))THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('SPOT','Spot values not supported by SPEC_ '//
     :     'routines',STATUS)
	  GO TO 9000
	ENDIF

* Model parameters
	A=PARAM(1)
	SLOPE=PARAM(2)
	TMIN=PARAM(3)
	TMAX=PARAM(4)
	METALS=PARAM(5)

* Convert emission measure from Ne**2*V to Ne*NH*V (i.e. *NH/Ne)
*   (assumes full ionisation)
	A=A/(1.17+0.011*METALS)

* if overlap between TMAX and TMIN, swap parameters
	IF(TMAX.LT.TMIN) THEN	! is this sensible for error bounds?
	  TEMP=TMIN
	  TMIN=TMAX
	  TMAX=TEMP
*      if equality between TMAX and TMIN, slightly increment TMAX
	ELSEIF(TMAX.EQ.TMIN) THEN
	  TMAX=TMIN+1E-3
	ENDIF

*      if TMIN=0, would get an undefined exponentiation. It will be
*      reset anyway by SPEC_RZ_EMISS, and the normalisation will then
*      be wrong - but nobody should use the result if the points lie
*      outside the RS grid anyway!
	IF(TMIN.EQ.0.0) TMIN=1E-4

	MINUSONE=.FALSE.

* clear output array
	CALL ARR_INIT1R(0.0,NEN,FLUX,STATUS)

* number of temperature bins over which DEM is sampled
	DT=(TMAX-TMIN)/20.0

* special case of slope=-1, need to use logs
	IF(SLOPE.GE.-1.0001.AND.SLOPE.LE.-0.9999) THEN
	  NORMO=LOG(TMAX)-LOG(TMIN)
	  MINUSONE=.TRUE.
	ELSE
	  M1=TMIN**(SLOPE+1.)
	  M2=TMAX**(SLOPE+1.)
	  NORMO=ABS((M2-M1)/(SLOPE+1.))
	ENDIF

* calculate spectrum  - sample more finely?
	DO I=1,20

*         temperature at bottom and midpoint of bin in keV
	   KT=TMIN+FLOAT(I-1)*DT
	   T=(KT+DT/2)

*         normalisation factor
	   IF(MINUSONE) THEN
	     DS=(LOG(KT+DT)-LOG(KT))/NORMO
	   ELSE
	     DS=(((KT+DT)**(SLOPE+1.)-KT**(SLOPE+1.))/(SLOPE+1.))/NORMO
	   ENDIF

*        set up BR paramaters for SPEC_BR, if required
	  BRPARAM(1)=A
	  BRPARAM(2)=T
	  BRPARAM(3)=METALS

*        Compute interpolated emissivity (in units of 1E-23 photons*cm**3/s)
          CALL SPEC_RZ_EMISS(NDS,T,METALS,NEN,ELBOUND,EUBOUND,BRPARAM,
     :                                                 TEMPFLUX,STATUS)
          IF(STATUS.NE.SAI__OK) GO TO 9000

*        normalise spectrum
          DO J=1,NEN
	    FLUX(J)=FLUX(J)+TEMPFLUX(J)*DS
	  ENDDO


	ENDDO

* Multiply by 1E-23*EM/4*pi*R**2 to get spec photon flux in photons/(cm**2*s)
*     This line changed for norm!!!!
	FACTOR=A*1.0E-23*EM10
	DO I=1,NEN
	  FLUX(I)=FLUX(I)*FACTOR
	ENDDO

* Exit
9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('E','from SPEC_DM',STATUS)
	END
