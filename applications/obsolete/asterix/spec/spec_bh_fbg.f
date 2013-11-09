*+  SPEC_BH_FBG - Karzas and Latter Gaunt factor
      REAL FUNCTION SPEC_BH_FBG(E,Q)
*    Description :
*     THIS FUNCTION COMPUTES THERMAL BREMSSTRAHLUNG WITH GAUNT FACTOR
*     A SERIES OF POLYNOMIAL APPROXIMATIONS TO KARZAS AND LATTERS
*     (1961) TABULATED VALUES IS USED TO DETERMINE THE FREE-FREE GAUNT
*     FACTOR FOR A WIDE RANGE OF TEMPERATURES AND PHOTON ENERGIES
*     COMPILED BY R.BALDWIN APRIL 9,1974
*     COPIED FROM KELLOG,BALDWIN AND KOCH  AP.J 199 L299 1975
*     N.B. THIS GIVES THE GAUNT FACTOR FOR A HYDROGEN PLASMA.
*    Parameters :
*    Result :
*    Method :
*    Deficiencies :
*     As copied from Kellog et al the routine actually contains an inaccessible
*     line of code (commented out in the following)!
*    Bugs :
*    Authors :
*     J.R.Baldwin (SAO)
*     Trevor Ponman (BHVAD::TJP)
*     Martin Watt (BHVAD::MPW)
*    History :
*      9 Apr 74: Original (J.R.Baldwin)
*     11 Aug 88: Logic tidied up and heading modified (MPW/TJP)
*    Type Definitions :
*    Global constants :
*    Global variables :
*    Import :
      REAL E				! Energy (keV)
      REAL Q				! kT (keV)
*    Local constants :
*    Local variables :
      DIMENSION A(6,7,3),GAM2(6),GAM3(6)
*    Local data :
      DATA GAM2/.7783,1.2217,2.6234,4.3766,20.,70./
      DATA GAM3/1.,1.7783,3.,5.6234,10.,30./
      DATA A/1.001,1.004,1.017,1.036,1.056,1.121,1.001,1.005,1.017,
     1 1.046,1.073,1.115,.9991,1.005,1.03,1.055,1.102,1.176,0.997,1.005,
     2 1.035,1.069,1.134,1.186,.9962,1.004,1.042,1.1,1.193,1.306,.9874,
     3 .9962,1.047,1.156,1.327,1.485,.9681,.9755,.8363,1.208,1.525,
     4 1.965,.3029,.1616,.04757,.013,.0049,-.0032,.4905,.2155,.08357,
     5 .02041,.00739,.00029,.654,.2833,.08057,.03257,.00759,-.00151,
     6 1.029,.391,.1266,.05149,.01274,.00324,.9569,.4891,.1764,.05914,
     7 .01407,-.00024,1.236,.7579,.326,.1077,.028,.00548,1.327,1.017,
     8 1.398,.205,.0605,.00187,-1.323,-.254,-.01571,-.001,-.000184,
     9 .00008,-4.762,-.3386,-.03571,-.001786,-.0003,.00001,-6.349,
     1 -.4206,-.02571,-.003429,-.000234,.00005,-13.231,-.59,-.04571,
     1 -.005714,-.000445,-.00004,-7.672,-.6852,-.0643,-.005857,-.00042,
     2 .00004,-7.143,-.9947,-.12,-.01007,-.000851,-.00004,-3.175,
     3 -1.116,-.8414,-.01821,-.001729,.00023/
*-
C
C CONVERT E AND KT TO KARZAS AND LATTER (1961) UNITS
C
	GAM=0.01358/Q
	GAM1=GAM*1000.
	U=E/Q
	U2=U**2
C
C COMPUTE BORN APPROXIMATION GAUNT FACTOR
C
	U1=U/2.
	T=U1/3.75

	IF (U1.LE.2.) THEN
          AI=1. + 3.5156229*T**2 + 3.0899424*T**4 + 1.2067492*T**6
     :       + .2659732*T**8 + .0360768*T**10 + .0045813*T**12
          AK= -ALOG(U1/2.)*AI - .57721566 + .4227842*(U1/2.)**2
     :       + .23069756*(U1/2.)**4 + .0348859*(U1/2.)**6
     :       + .00262698*(U1/2.)**8 + .0001075*(U1/2.)**10
     :       + .0000074*(U1/2.)**12
          BORN= .5513*EXP(U1)*AK
	ELSE
          AK= 1.25331414 - .07832358*(2./U1) + .02189568*(2./U1)**2
     :      -.01062446*(2./U1)**3 + .00587872*(2./U1)**4
     :      - .0025254*(2./U1)**5 + .00053208*(2./U1)**6
            BORN=0.5513*AK/SQRT(U1)
	END IF
C
C COMPUTE POLYNOMIAL FACTOR TO MULTIPLY BORN APPROXIMATION
C

*    If statements ordered so most probable inequalities are at top
	IF (U.LE.5.0  .AND. U.GT.1.0)  THEN
	  N=4
	ELSEIF (U.LE.1.0  .AND. U.GT.0.30) THEN
	  N=3
	ELSEIF (U.LE.0.30 .AND. U.GT.0.03) THEN
	  N=2
	ELSEIF (U.LE.15.0 .AND. U.GT.5.0)  THEN
	  N=5
	ELSEIF (U.LE.0.03 .AND. U.GT.0.003)THEN
	  N=1
	ELSEIF (U.GT.15.0) THEN
	  N=6
        ELSEIF (U.LE.0.003) THEN
	  SPEC_BH_FBG=BORN
	  RETURN
	END IF

*    IF statements ordered so most probable inequalities are at top
	IF (GAM1 .LE. 3.0 .AND. GAM1 .GT. 1.7783) THEN
	  M=2
	ELSEIF (GAM1.LE.1.7783 .AND. GAM1.GT.1.0) THEN
	  M=1
	ELSEIF (GAM1.LE.5.6234 .AND. GAM1.GT.3.0) THEN
	  M=3
	ELSEIF (GAM1.LE.10.0 .AND. GAM1.GT.5.6234) THEN
	  M=4
	ELSEIF (GAM1.LE.30.0 .AND. GAM1.GT.10.0) THEN
	  M=5
	ELSEIF (GAM1.LE.100.0 .AND. GAM1.GT.30.0) THEN
	  M=6
	ELSEIF (GAM1.GT.100.0) THEN
          SPEC_BH_FBG=1.0
	  RETURN
        ELSEIF (GAM1.LE.1.0) THEN
	  SPEC_BH_FBG=BORN
	  RETURN
	END IF

	M1=M+1

	G1 = (A(N,M,1) + A(N,M,2)*U + A(N,M,3)*U2)*BORN
	G2 = (A(N,M1,1) + A(N,M1,2)*U + A(N,M1,3)*U2)*BORN
	P= (GAM1 - GAM3(M))/GAM2(M)
	G = (1. - P)*G1 + P*G2
	SPEC_BH_FBG=G


*         601   G = 0.8*(Q/E)**0.4    ! This line is inaccessible in original

	END
