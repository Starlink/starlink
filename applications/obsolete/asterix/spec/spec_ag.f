*+ SPEC_AG - Absorption cutoff due to ISM in our galaxy
      SUBROUTINE SPEC_AG(NEN,ELBOUND,EUBOUND,PARAM,FAC,STATUS)
*    Description :
*     Returns array of energy dependent attenuation factors due to the
*     interstellar absorption model of Morrison & McCammon (without grains)
*     plus the Compton scattering cross-section, which is significant in
*     comparison at E>5 keV.
*				FAC=exp(-SIG*NH)
*     where NH is the hydrogen column density in units of 1.0E21 /cm**2.
*		NH = PARAM(1)
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Method :
*     The quadratic coefficients given by Morrison & McCammon (Ap.J.270,p119)
*     are used to evaluate the cross-section at every energy bin boundary and on
*     either side of each absorption edge. The Thomson scattering cross-section
*     due to H+He(10% abundance) is added to this. It is quite significant
*     even at fairly low energies (e.g. 4.5% at 3 keV) even though it is
*     commonly ignored.
*     (NOTE that scattering is a loss process for galactic absorption,
*      but not for circumsource absorption.)
*     The resulting absorption factors are superposed linearly to obtain a
*     mean absorption within each energy bin.
*     In the case where no edge falls within an energy bin, a
*     quadratic integral approximation is used at low energies (<20 keV) to
*     give the absorption factor more accurately (at higher energies sigma is
*     essentially constant at the Thomson value).
*     If a source redshift has been applied to the energy bound arrays
*     then this has to be undone by the subroutine (since galactic absorption
*     is obviously not subject to the source redshift). This is achieved by
*     retrieving the Z value from the parameter system.
*    Deficiencies :
*     Below 0.03 keV sigma is assumed proportional to E**-3, this is only
*     an approximation.
*     At E>100 keV the Klein-Nishina scattering cross-section should be used.
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*      2 Apr 85 : Original (SPEC_AB)
*     20 Apr 88 : Lower and upper energy bounds passed in separate arrays (TJP)
*      8 Jul 88 : Proofed against floating underflow at high energies (TJP)
*     14 Nov 89 : SPEC_AG original. Undoes any redshifting (TJP)
*     25 Feb 93 : Error handling corrected (DJA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
	INTEGER NEN			! No of energy bins
	REAL ELBOUND(NEN)		! Lower bin bounds (keV)
	REAL EUBOUND(NEN)		! Upper bin bounds (keV)
	REAL PARAM(1)			! Model parameter (NH)
*    Import-Export :
*    Export :
	REAL FAC(NEN)			! Attenuation factors
*    Status :
	INTEGER STATUS
*    Function declarations :
	REAL SPEC_AG_SIGMA
*    Local variables :
	INTEGER I
	INTEGER N			! Edge number

	REAL NH				! H column (1.0E21 cm**-2)
	REAL E				! Current energy (keV)
	REAL Z				! Redshift
	REAL RESCALE			! Energy rescale factor (deredshifts)
	REAL C0,C1,C2			! Morrison & McCammon coefficients
	REAL SIG			! Cross-section (1.0E-21 cm**2)
	REAL ABS			! Absorption factor
	REAL OLDAB			! Old absorption factor
	REAL ABMID			! Abs. factor at centre of bin
	REAL ELAST			! Last energy value (lower bound)
	REAL EUPP			! Top of current energy bin
	REAL DE				! Energy bin width
	REAL AFAC			! Accumulated integrated absorption
	REAL ECUT(16)			! Energy cut values (mostly edges)
	REAL C0V(16),C1V(16),C2V(16)	! Arrays of coefficients
*    Local data :
	DATA ECUT/0.03,0.10,0.284,0.40,0.532,0.707,0.867,1.303,1.840,
     :  2.471,3.210,4.048,7.111,8.331,10.0,1.0E20/
	DATA C0V/0.03361,0.0173,0.0346,0.0781,0.0714,0.0955,0.3089,0.1206,
     :  0.1413,0.2027,0.3427,0.3522,0.4339,0.629,0.7012,0.953/
	DATA C1V/0.0,0.6081,0.2679,0.0188,0.0668,0.1458,-0.3806,0.1693,
     :  0.1468,0.1047,0.0187,0.0187,-0.0024,0.0309,0.0252,0.0/
	DATA C2V/0.0,-2.150,-0.4761,0.0043,-0.0514,-0.0611,0.294,-0.0477,
     :  -0.0315,-0.017,0.0,0.0,0.00075,0.0,0.0,0.0/
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

* Check for non-zero redshift
	CALL USI_GET0R('REDSHIFT',Z,STATUS)
	RESCALE=1.0/(1.0+Z)

* Set up base absorption
	NH=PARAM(1)
	E=ELBOUND(1)*RESCALE

* Find first edge above base - edge no. N
	N=1
	DO I=1,15
	  IF(ECUT(I).LE.E) N=N+1
	ENDDO

	C0=C0V(N)			!
	C1=C1V(N)			! Coefficients applying below Nth edge
	C2=C2V(N)			!
	SIG=SPEC_AG_SIGMA(C0,C1,C2,E)
	OLDAB=EXP(-SIG*NH)
	ELAST=E
	DO I=1,NEN
	  EUPP=EUBOUND(I)*RESCALE
	  DE=EUPP-ELAST

* Step through any edges within bin I
	  IF(ECUT(N).GE.EUPP)THEN		! Next edge >= upper bin bound

* No edges within energy bin - perform quadratic integral over absorption
	    E=(ELAST+EUPP)/2			! Mean bin energy
	    SIG=SPEC_AG_SIGMA(C0,C1,C2,E)
	    ABMID=EXP(-SIG*NH)
	    SIG=SPEC_AG_SIGMA(C0,C1,C2,EUPP)
	    ABS=EXP(-SIG*NH)
	    IF(E.LT.20.0)THEN
	      CALL MATH_INTEGRT(DE,OLDAB,ABMID,ABS,AFAC)
	      FAC(I)=AFAC/DE			! Divide integral by width
	    ELSE
	      FAC(I)=ABMID
	    ENDIF
	    OLDAB=ABS				! Assumes contiguous bins
	    ELAST=EUPP				! Ditto
	  ELSE

* Make linear superposition of absorption factors at edges and bin bounds
	    AFAC=0.0
	    E=ECUT(N)
	    SIG=SPEC_AG_SIGMA(C0,C1,C2,E)
	    ABS=EXP(-SIG*NH)
	    AFAC=AFAC+(OLDAB+ABS)*(E-ELAST)	! Mean absorption contribution
						! up to first edge

 50	    N=N+1				! Next ECUT segment
	    C0=C0V(N)
	    C1=C1V(N)
	    C2=C2V(N)				! New coefficients
	    SIG=SPEC_AG_SIGMA(C0,C1,C2,E)
	    OLDAB=EXP(-SIG*NH)			! Absorption at TOP of last
	    ELAST=E				! edge

	    IF(ECUT(N).LT.EUPP)THEN

* Add in absorption between last and next edges
	      E=ECUT(N)
	      SIG=SPEC_AG_SIGMA(C0,C1,C2,E)
	      ABS=EXP(-SIG*NH)
	      AFAC=AFAC+(OLDAB+ABS)*(E-ELAST)
	      GO TO 50
	    ELSE

* No further edges in this bin
	      SIG=SPEC_AG_SIGMA(C0,C1,C2,EUPP)
	      ABS=EXP(-SIG*NH)
	      AFAC=AFAC+(OLDAB+ABS)*(EUPP-ELAST)
	      FAC(I)=AFAC/(2*DE)
	      ELAST=EUPP			! Assumes contiguous bins
	      OLDAB=ABS				! Ditto
	    ENDIF
	  ENDIF

* Check for bin boundary falling on an edge - if so then move to top
* 				of edge, ready for next bin
	  IF(EUPP.EQ.ECUT(N))THEN
	    N=N+1
	    C0=C0V(N)
	    C1=C1V(N)
	    C2=C2V(N)
	    SIG=SPEC_AG_SIGMA(C0,C1,C2,E)
	    OLDAB=EXP(-SIG*NH)
	  ENDIF
	ENDDO

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_AG',STATUS)
	END


*+  SPEC_AG_SIGMA - Interstellar absorption cross-section
	REAL FUNCTION SPEC_AG_SIGMA(C0,C1,C2,E)
*    Description :
*	Calculates absorption + scattering cross section per H atom in
*	units of 1.0E-21 cm**2 from the coefficients C0,C1 & C2 of
*	Morrison & McCammon.
*    Result :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
	REAL C0,C1,C2		! Morrison and McCammon coefficients
	REAL E			! Energy in keV for evaluation
*    Global variables :
*    Local constants :
*    Local variables :
*-----------------------------------------------------------------------

	SPEC_AG_SIGMA=8.0E-4				! Includes 10% He
	IF(E.LT.1.0E4)	SPEC_AG_SIGMA=SPEC_AG_SIGMA+(C0/E+C1+C2*E)/E**2
	END
