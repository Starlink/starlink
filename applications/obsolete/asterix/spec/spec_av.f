*+ SPEC_AV - Instrinsic absorption with variable abundances
      SUBROUTINE SPEC_AV (NEN,ELBOUND,EUBOUND,PARAM,FAC,STATUS)
*    Description :
*       The Balucinska-Church and McCammon formalism for
*	evaluation of the photoelectric absorption for desired
*	elemental abundances (17 elements).
*	Balucinska-Church M. and McCammon D., Ap. J. 400,pp,(1992)
*
*    Method:
*       Polynomial fit coefficients have been obtained for the energy dependence
*	of the photoelectric absorption cross sections of 17 astrophysically
*	important elements. The aim of this work is to provide convenient fits
*	to the photoelectric absorption cross sections for each of 17 elements
*	separately, so that spectral modelling can be performed with an
*	absorption term containing the abundances of some or all of the
*	elements as adjustable parameters.  The fits to the individual
*	elements can also be used independently for calculating window
*	transmissions, gas stopping efficiency, etc.
*
*	Polynomial fits have been made to the atomic absorption cross
*	sections in the energy range of 0.03 -- 10 keV for seventeen elements:
*	hydrogen, helium, carbon, nitrogen, oxygen, neon, sodium, magnesium,
*	aluminium, silicon, sulphur, chlorine, argon, calcium, chromium, iron
*	and nickel. In the case of elements with only the K-edge in this energy
*	range, polynomial fits were made each side of the edge; with the L-edge
*	also present three fits were made.  Polynomials of up to degree 8 were
*	required. The functions fit Henke's data points with a typical error of
*	2% and a maximum error of 7%, except for points below ~40 eV for argon,
*	calcium and sodium, where the errors are larger. The effective cross
*	section per hydrogen atom for a particular set of elemental abundances
*	may be simply calculated from the individual cross sections.

*	The file XSECT.FOR contains seventeen REAL functions that will return
*	the photoelectric cross sections for H, He, C, N, O, Ne, Na,Mg, Al,
*	Si, S, Cl, Ar, Ca, Cr, Fe, and Ni in cm**2/g, given the photon energy
*	in eV. The file TOTLXS.FOR contains a single function that calls the
*	XSECT.FOR subroutines and returns the effective cross section in
*	cm**2/H atom, given the photon energy in eV and the logarithmic
*	(log10) relative abundances for the 17 elements. All of these routines
*	are valid for the photon energy range 30 eV to 10,000 eV.
*
*     	The total cross section is evaluated at every energy bin boundary and
*	on either side of each absorption edge. If no edge falls within a bin,
*	a quadratic integral approximation is used to give the mean absorption
*	factor of the bin. If one or more edges falls within the bin, the
*	function each side of each edge is approximated as linear.
*
*    Deficiencies : Below 30 eV accuracy of cross section decreases.
*    Bugs :
*          none known - please report to the authors if found
*    Authors :
*            Monika Balucinska-Church (BHVAD::MBC)
*            Dan Mccammon (WISP::MCCAMMON)
*    History :
*            original :  Aug 3rd., 1992
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
	INTEGER NEN			! No of energy bins
	REAL ELBOUND(NEN)		! Lower bin bounds (keV)
	REAL EUBOUND(NEN)		! Upper bin bounds (keV)
	REAL PARAM(18)			! Model parameters (NH +
                                        ! abundances of 17 elements)
*    Import-Export :
*    Export :
	REAL FAC(NEN)			! Attenuation factors
*    Status :
	INTEGER STATUS
*    Function declarations :
	REAL TOTLXS
*    Local variables :
	INTEGER I
	INTEGER N			! Edge number

	REAL NH				! H column (1.0E21 cm**-2)
        REAL AB(17)                     ! abundances of 17 elements
	REAL E				! Energy (keV)
	REAL ECUT(28)			! Edge energies
	REAL SIG			! Cross-section
	REAL ABSORB			! Absorption factor
	REAL OLDAB			! Last factor: bin or part-bin
	REAL ABMID			! Abs. factor at centre of bin
	REAL ELAST			! Last energy value
	REAL DE				! Energy bin width
	REAL AFAC			! Accumulated integrated absorption
*    Local data :
	DATA ECUT/0.03, 0.04945, 0.07278, 0.1006, 0.165, 0.202,
     :  0.245, 0.284, 0.34931, 0.401, 0.5317, 0.598, 0.691, 0.7074,
     :  0.8536, 0.867, 1.0717, 1.3034, 1.5599, 1.84, 2.4705,
     :  2.8196, 3.2029, 4.0381, 5.9888, 7.1112, 8.3316, 10.0/
*--------------------------------------------------------------------------

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Check for spot value
	IF(EUBOUND(1)-ELBOUND(1).LT.0.1E-5)THEN
	   CALL ERR_REP('SPOT','Spot values not supported by SPEC_ routines',
     :     STATUS)
	   STATUS=SAI__ERROR
	   GO TO 9000
	ENDIF

* Set up base absorption
	NH=PARAM(1)*1.E21
        DO I=1,17
           AB(I)=PARAM(I+1)
        ENDDO
	E=ELBOUND(1)

        IF(E.LT.ECUT(1))THEN
           PRINT *,'Warning - energy < 30 eV, accuracy reduced'
        ENDIF

* Find first edge above ELBOUND(1):
	N=2
	DO I=2,26
	   IF(ECUT(I).LE.E) N=N+1
	ENDDO

* Get absorption for lower bound of 1st bin:
	SIG=TOTLXS(E,AB,STATUS)
	OLDAB=EXP(-SIG*NH)
	ELAST=E

* Loop through energy bins:
	DO I=1,NEN
           DE=EUBOUND(I)-ELAST

* 	   Next edge within bin I ?
	   IF(ECUT(N).GE.EUBOUND(I))THEN
* 	      No edges within energy bin - perform quadratic integral
	      E=(ELAST+EUBOUND(I))/2
	      SIG=TOTLXS(E,AB,STATUS)
	      ABMID=EXP(-SIG*NH)		! at bin centre
	      E=EUBOUND(I)
	      IF(ABS(ECUT(N)-EUBOUND(I)).LT.0.1E-5)E=ECUT(N)-0.1E-5
	      SIG=TOTLXS(E,AB,STATUS)
	      ABSORB=EXP(-SIG*NH)			! at bin top

	      IF(E.LT.20.0)THEN
	         CALL MATH_INTEGRT(DE,OLDAB,ABMID,ABSORB,AFAC)
	         FAC(I)=AFAC/DE			! mean absorption within bin
	      ELSE
	         FAC(I)=ABMID
	      ENDIF

*	      get ready for the next bin:
	      OLDAB=ABSORB
	      ELAST=EUBOUND(I)

	   ELSE
* 	      edge(s) within bin		! may be close to lower
	      AFAC=0.				! or upper bin boundary
	      DOWHILE(ECUT(N).LT.EUBOUND(I))
*	         integrate LHS of edge
	         E=ECUT(N)-0.1E-5		! LHS of edge
	         SIG=TOTLXS(E,AB,STATUS)
	         ABSORB=EXP(-SIG*NH)
                 AFAC=AFAC+(OLDAB+ABSORB)*(ECUT(N)-ELAST)
						! trapezium - divide by 2 later
                                                ! NB this may be zero if
                                                ! edge close Elbound
*                get ready for the next segment
		 E=ECUT(N)
	         SIG=TOTLXS(E,AB,STATUS)
	         OLDAB=EXP(-SIG*NH)
		 ELAST=E
		 N=N+1
	      ENDDO

* 	      No further edges in this bin: integrate RHS of last edge
	      E=EUBOUND(I)
	      SIG=TOTLXS(E,AB,STATUS)
	      ABSORB=EXP(-SIG*NH)

	      AFAC=AFAC+(OLDAB+ABSORB)*(E-ELAST)  ! may be zero if edge
	      FAC(I)=AFAC/(2.*DE)  		  ! close to Eubound
	      ELAST=E
	      OLDAB=ABSORB
	   ENDIF
	ENDDO

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR',
     :			      'from SPEC_AV',STATUS)
	END

*+  name - TOTLXS
      FUNCTION TOTLXS(E_KEV,AB,STATUS)
*    Description :
*     Calculates the effective absorption cross section in units of
*     cm**2/hydrogen atom at energy E in keV for the abundances of the
*     elements specified in vector AB
*     Reference:
*      Balucinska-Church M. and McCammon D.,
*      "Photoelectric Absorption Cross Sections with Variable Abundances"
*      Ap.J. 400, pp (1992, December 1, part 1)
*    Method :
*     Calls seventeen functions that calculate the mass absorption coeffs
*     in cm**2/g for the following elements: H, He, C, N, O, Ne, Na, Mg,
*     Al, Si, S, Cl, Ar, Ca, Cr, Fe, Ni.
*    Deficiencies :
*     Works only in the range of energy from 30 eV to 10,000 eV.
*    Bugs :
*    Authors :
*     Monika Balucinska-Church   (19775::MBC)
*     Dan McCammon               (47413::MCCAMMON)
*    History :
*     8.8.91  - original (BHVAD::MBC)
*     8.1.92  - modified to remove VAX fortran77
*               extensions (47413::MCCAMMON)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    mass absorption coefficients for the elements
      REAL HYDRO
      REAL HELIUM
      REAL CARBON
      REAL NITRO
      REAL OXYGEN
      REAL NEON
      REAL SODIUM
      REAL MAGNES
      REAL ALUM
      REAL SILICN
      REAL SULFUR
      REAL CHLORN
      REAL ARGON
      REAL CALC
      REAL CHROM
      REAL IRON
      REAL NICKEL
*    Local constants :
      INTEGER NUMB       		! number of elements
      PARAMETER (NUMB=17)
      REAL AV            		! Avogadro's number
      PARAMETER (AV=6.022045E23)
*    Local variables :
      INTEGER K          		! index through elements
      REAL A(17)         		! mass absorption cross sections
					! in cm**2/g for the cosmic elements
      REAL E                            ! energy in eV

*     Import:
      REAL E_KEV             		! energy in keV
      REAL AB(17)        		! log10 abundances relative to hydrogen
					! The order of the elements in AB
					! is as follows: H,He,C,N,O,Ne,Na,
					! Mg,Al,Si,S,Cl,Ar,Ca,Cr,Fe,Ni.
*     Export:
      REAL TOTLXS        		! effective cross section in
					! cm**2/H atom
*    Local data :
      REAL AW(17)     			! atomic weights of the elements
      DATA AW /1.00797, 4.0026, 12.01115, 14.0067, 15.9994, 20.183,
     :         22.9898, 24.312, 26.9815, 28.086, 32.064, 35.453,
     :         39.94, 40.08, 51.996, 55.847, 58.71/
*-

*     Start:
      E=E_KEV*1000.
      A(1)=HYDRO(E)
      A(2)=HELIUM(E)
      A(3)=CARBON(E)
      A(4)=NITRO(E)
      A(5)=OXYGEN(E)
      A(6)=NEON(E)
      A(7)=SODIUM(E)
      A(8)=MAGNES(E)
      A(9)=ALUM(E)
      A(10)=SILICN(E)
      A(11)=SULFUR(E)
      A(12)=CHLORN(E)
      A(13)=ARGON(E)
      A(14)=CALC(E)
      A(15)=CHROM(E)
      A(16)=IRON(E)
      A(17)=NICKEL(E)

      TOTLXS=0.

* Loop through elements:
      DO K=1,NUMB
         TOTLXS=TOTLXS + AW(K)*A(K)/AV*(10.**(AB(K)-AB(1)))
      ENDDO
      END

*  File XSECT.FOR
*
*        This set of subroutines calculates the photoelectric absorption
*   cross sections for the elements H, He, C, N, O, Ne, Na, Mg, Al, Si,
*   S, Cl, A, Ca, Cr, Fe, and Ni.  The result is in cm**2/g, given the
*   photon energy in eV.  These functions are valid only over the energy
*   range 30 - 10,000 eV, but do NOT check that the input energy is
*   within the valid range.  These functions are called by TOTLXS to
*   calculate the total effective cross section, given a set of relative
*   abundances.  They can also be used by themselves.
*
*
*   Reference:
*
*      Balucinska-Church M. and McCammon D.,
*      "Photoelectric Absorption Cross Sections with Variable Abunances"
*      Ap.J. 400, pp (1992, December 1 - part 1)
*
*
*  Real Function: ALUM
*  Source: Atomic : Nuclear Data Tables, January 1982
*
*  Description:
*      Calculates mass absorption coefficient (mu/rho) for aluminum.
*  History:  updated below L-edge (72.78 eV) - Aug 30, 1991 (BHVAD::MBC)
*
*  Usage:  FUNCTION ALUM(E)
*      E = Energy in eV.
*      ALUM returns mu/rho in cm**2/gm - same for all elements.
*
	FUNCTION ALUM(E)

	IMPLICIT NONE
	REAL E, ELOG, X, ALUM
        ELOG = ALOG(E)
        IF(E.LT.72.78)THEN
            X = 26.90487 + (3. - 9.135221)*ELOG + 1.175546*ELOG*ELOG
        ELSEIF(E.LT.1559.9)THEN
                X = -38.1232 + 29.5161*ELOG - 4.45416*ELOG*ELOG +
     :                        0.226204*ELOG*ELOG*ELOG
        ELSE
                X = 14.6897 + 4.22743*ELOG - 0.344185*ELOG*ELOG +
     :                        8.18542E-3*ELOG*ELOG*ELOG
        ENDIF
        ALUM = EXP(X)/(E*E*E)
      END
*------------------------------------------------------------------------
*
*  Real Function: ARGON
*  Source: Atomic : Nuclear Data Tables, January 1982
*  Description: ARGON calculates the mass absorbtion coefficient of argon.
*
*  works well for energies above 40 eV !!!
*
*  History: updated below L-edge (245 eV) - Aug 30, 1991 (BHVAD::MBC)
*
	FUNCTION ARGON(E)

	IMPLICIT NONE
	REAL E, ELOG, X, ARGON

        ELOG = ALOG(E)
        IF(E.LT.245.0)THEN
           X = -330.3509 + (267.7433 + 3.)*ELOG - 78.90498*ELOG*ELOG
     :         + 10.35983*(ELOG**3) - 0.5140201*(ELOG**4)

        ELSEIF(E.LT.3202.9)THEN
            X = -5.71870 + (8.85812*ELOG) + (-0.307357*ELOG*ELOG) +
     :             (0.00169351*(ELOG**3)) + (-0.0138134*(ELOG**4)) +
     :             (0.00120451*(ELOG**5))
        ELSE
               X = 19.1905 + (2.74276*ELOG) + (-0.164603*ELOG*ELOG) +
     :             (0.00165895*ELOG*ELOG*ELOG)
        ENDIF
        ARGON = EXP(X)/(E*E*E)
      END
*------------------------------------------------------------------------------
*     Real Function: CALC
*     Source: Atomic : Nuclear Data Tables, January 1982
*
*     Description:
*     Calculates mass absorption coefficient (mu/rho) for calcium
*
*     History: original Aug 6, 1991 (BHVAD::MBC)
*
	FUNCTION CALC(E)
*
	IMPLICIT NONE
	REAL E,ELOG,X,CALC
*
	ELOG = ALOG(E)
	IF(E.LT.349.31)THEN
	   X=-873.972 + (865.5231 + 3.)*ELOG - 339.678*ELOG*ELOG +
     :        66.83369*(ELOG**3) - 6.590398*(ELOG**4) +
     :        0.2601044*(ELOG**5)
        ELSEIF(E.LT.4038.1)THEN
           X=-3449.707 + (2433.409 + 3.)*ELOG
     :        - 682.0668*ELOG*ELOG + 95.3563*(ELOG**3)
     :        - 6.655018*(ELOG**4) + 0.1854492*(ELOG**5)
        ELSE
           X=18.89376 + (3. - 0.2903538)*ELOG
     :       - 0.1377201*ELOG*ELOG
	ENDIF
	CALC = EXP(X)/(E*E*E)
      END
*--------------------------------------------------------------------------
*
*  Real Function: CARBON
*  Source: Atomic : Nuclear Data Tables, Jan. 1982
*
*  Description: CARBON calculates the mass absorbtion cross-section of carbon.
*
	FUNCTION CARBON(E)
*
	IMPLICIT NONE
	REAL E,ELOG,X,CARBON
*
        ELOG = ALOG(E)
        IF(E.LT.284.0)THEN
           X = 8.74161 + (7.13348*ELOG) + (-1.14604*ELOG*ELOG) +
     :         (0.0677044*ELOG*ELOG*ELOG)
        ELSE
           X = 3.81334 + (8.93626*ELOG) + (-1.06905*ELOG*ELOG) +
     :         (0.0422195*ELOG*ELOG*ELOG)
        ENDIF
        CARBON = EXP(X)/(E*E*E)
      END
*---------------------------------------------------------------------------
*     Real Function: CHLORN
*     Source: Atomic : Nuclear Data Tables, January 1982
*
*     Description:
*     Calculates mass absorption coefficient (mu/rho) for chlorine
*
*     History: original Aug 6, 1991 (BHVAD::MBC)
*
        FUNCTION CHLORN(E)
*
	IMPLICIT NONE
	REAL E,ELOG,X,CHLORN
*
	ELOG = ALOG(E)
	IF(E.LT.202.0)THEN
           X=6253.247 +(3. - 8225.248)*ELOG + 4491.675*ELOG*ELOG
     :      - 1302.145*(ELOG**3) + 211.4881*(ELOG**4)
     :      - 18.25547*(ELOG**5) + 0.6545154*(ELOG**6)
	ELSEIF(E.LT.2819.6)THEN
           X=-233.0502 + (143.9776 + 3.)*ELOG
     :       - 31.12463*ELOG*ELOG +
     :         2.938618*(ELOG**3) - 0.104096*(ELOG**4)
        ELSE
            X=-23.74675 + (14.50997 + 3.)*ELOG
     :        - 1.857953*ELOG*ELOG + 6.6208832E-2*(ELOG**3)
        ENDIF
        CHLORN = EXP(X)/(E*E*E)
      RETURN
      END
*---------------------------------------------------------------------------
*     Real Function: CHROM
*     Source: Atomic : Nuclear Data Tables, January 1982
*
*     Description:
*     Calculates mass absorption coefficient (mu/rho) for chromium
*
*     History: original Aug 5, 1991 (BHVAD::MBC)
*
	FUNCTION CHROM(E)
	IMPLICIT NONE
	REAL E,ELOG,X,CHROM
*
	ELOG = ALOG(E)
	IF(E.LT.598.0)THEN
           X=-0.4919405 + (12.66939 + 3.)*ELOG - 5.199775*ELOG*ELOG +
     :        1.086566*(ELOG**3) - 0.1196001*(ELOG**4) +
     :        5.2152011E-3*(ELOG**5)
	ELSEIF(E.LT.691.0)THEN
           X=27.29282 +(3. - 2.703336)*ELOG
        ELSEIF(E.LT.5988.8)THEN
           X=-15.2525 + (13.23729 + 3.)*ELOG
     :       - 1.966778*ELOG*ELOG + 8.062207E-2*(ELOG**3)
        ELSE
           X=8.307041 + (2.008987 + 3.)*ELOG
     :       - 0.2580816*ELOG*ELOG
	ENDIF
	CHROM = EXP(X)/(E*E*E)
      END
*------------------------------------------------------------------------------
*     Real Function: HELIUM
*     Source: Atomic : Nuclear Data Tables, January 1982
*
*     Description:
*     Calculates mass absorption coefficient (mu/rho) for helium
*
*     History: original Aug 6, 1991 (BHVAD::MBC)
*
	FUNCTION HELIUM(E)
	IMPLICIT NONE
	REAL E,ELOG,X,HELIUM
*
	ELOG = ALOG(E)
        X=14.61546 + (1.682793 + 3.)*ELOG - 0.7323856*ELOG*ELOG
     :        + 4.6526663e-2*(ELOG**3) - 1.1172282e-3*(ELOG**4)
	HELIUM = EXP(X)/(E*E*E)
      END
*-----------------------------------------------------------------------
*       Real Function: HYDRO
*
*       Date:3/6/84
*       Author: Angie Betker
*       Source: Atomic : Nuclear Data Tables, January 1982
*
*        History: modified: 6/5/87 - J. Bloch - Created F77 Vax/VMS version.
*                 updated Aug 30, 1991 (BHVAD::MBC)
*
*       Description: Hydro calculates mu/rho for hydrogen in cm**2/gm
*
*
        FUNCTION HYDRO(E)
	IMPLICIT NONE
        REAL E, ELOG, X, HYDRO
*
        ELOG=ALOG(E)
        X=21.46941 + (3. - 2.060152)*ELOG - 0.1492932*ELOG*ELOG
     :    + 5.4634294E-3*(ELOG**3)
        HYDRO=EXP(X)/(E*E*E)
        END
*------------------------------------------------------------------------------
*     Real Function: IRON
*     Source: Atomic : Nuclear Data Tables, January 1982
*
*     Description:
*     Calculates mass absorption coefficient (mu/rho) for iron
*     History: original Aug 6, 1991 (BHVAD::MBC)
*
	FUNCTION IRON(E)

	IMPLICIT NONE
	REAL E,ELOG,X,IRON
*
	ELOG = ALOG(E)
	IF(E.LT.707.4)THEN
	   X=-15.07332 + (18.94335 + 3.)*ELOG - 4.862457*ELOG*ELOG +
     :     0.5573765*(ELOG**3) - 3.0065542E-2*(ELOG**4) +
     :     4.9834867E-4*(ELOG**5)
        ELSE IF(E.LT.7111.2)THEN
           X=-253.0979 + (135.4238 + 3.)*ELOG - 25.47119*ELOG*ELOG +
     :     2.08867*(ELOG**3) - 6.4264648E-2*(ELOG**4)
         ELSE
           X=-1.037655 + (4.022304 + 3.)*ELOG
     :      - 0.3638919*ELOG*ELOG
	ENDIF
	IRON = EXP(X)/(E*E*E)
      END
*-------------------------------------------------------------------------
*     Real Function: MAGNES
*     Source: Atomic : Nuclear Data Tables, January 1982
*
*     Description:
*     Calculates mass absorption coefficient (mu/rho) for magnesium
*     History: original Aug 6, 1991 (BHVAD::MBC)
*
      FUNCTION MAGNES(E)
*
	IMPLICIT NONE
	REAL E,ELOG,X,MAGNES
*
	ELOG = ALOG(E)

	IF(E.LT.49.45)THEN
	   X=7.107172 + (0.7359418 + 3.)*ELOG
        ELSEIF(E.LT.1303.4)THEN
           X=-81.32915 + (62.2775 + 3.)*ELOG
     :       - 15.00826*ELOG*ELOG
     :       + 1.558686*(ELOG**3) - 6.1339621E-2*(ELOG**4)
        ELSE
           X=-9.161526 + (10.07448 + 3.)*ELOG
     :       -1.435878*ELOG*ELOG + 5.2728362E-2*(ELOG**3)
	ENDIF
	MAGNES = EXP(X)/(E*E*E)
      END
*----------------------------------------------------------------------
*  Real Function: NEON
*  Source: Atomic and Nuclear Data Tables, Jan. 1982
*
*  Description:  NEON calculates the mass absorption coefficient
*      for neon gas.

	FUNCTION NEON(E)
*
	IMPLICIT NONE
	REAL E,ELOG,X,NEON
*
        ELOG = ALOG(E)
        IF(E.LT.867.)THEN
           X = -3.04041 + (13.0071*ELOG) + (-1.93205*ELOG*ELOG) +
     :          (0.0977639*ELOG*ELOG*ELOG)
        ELSE
           X = 17.6007 + (3.29278*ELOG) + (-0.263065*ELOG*ELOG) +
     :          (5.68290E-3*ELOG*ELOG*ELOG)
        ENDIF
        NEON = EXP(X)/(E*E*E)
      END
*---------------------------------------------------------------------------
*
*  Real Function: NICKEL
*  Source: Atomic : Nuclear Data Tables, January 1982
*
*  Description: NICKEL calculates the mass absorbtion coefficient for nickel.
*  History: updated below L-edge (853.6 eV) - Aug 30, 1991 (BHVAD::MBC)
*
      FUNCTION NICKEL(E)

	IMPLICIT NONE
	REAL E, ELOG, X, NICKEL

        ELOG = ALOG(E)
        IF(E.LT.853.6)THEN
           X = -7.919931 + (11.06475 + 3.)*ELOG -
     :		1.935318*ELOG*ELOG + 9.3929626e-2*(ELOG**3)
        ELSEIF(E.LT.8331.6)THEN
           X = 3.71129 + (8.45098*ELOG) + (-0.896656*ELOG*ELOG) +
     :      (0.0324889*ELOG*ELOG*ELOG)
        ELSE
           X = 28.4989 + (0.485797*ELOG)
        ENDIF
        NICKEL = EXP(X)/(E*E*E)
      END
*--------------------------------------------------------------------------
*
*  Real Function: NITRO
*  Source: Atomic : Nuclear Data Tables, January 1982
*
*  Description:  NITRO calculates the mass absorption cross-section of
*        nitrogen as a function of energy.
*
	FUNCTION NITRO(E)
*
	IMPLICIT NONE
	REAL E,ELOG,X,NITRO
*
        ELOG = ALOG(E)
        IF(E.LT.401.)THEN
           X = 9.24058 + (7.02985*ELOG) + (-1.08849*ELOG*ELOG) +
     :         (0.0611007*ELOG*ELOG*ELOG)
        ELSE
           X = -13.0353 + (15.4851*ELOG) + (-1.89502*ELOG*ELOG) +
     :          (0.0769412*ELOG*ELOG*ELOG)
        ENDIF
        NITRO = EXP(X)/(E*E*E)
      END
*-----------------------------------------------------------------------
*
*  Real Function: OXYGEN
*
*  Source:        Atomic : Nuclear Data Tables, January 1982
*
*  Description:  OXY Calculates the mass absorption cross-section of oxygen
*      as a function of energy.
*  History: updated above K-edge (531.7 eV) - Aug 30, 1991 (BHVAD::MBC)
*
	FUNCTION OXYGEN(E)

	IMPLICIT NONE
	REAL E, ELOG, X, OXYGEN

        ELOG = ALOG(E)
        IF(E.LT.531.7)THEN
           X = 2.57264 + (10.9321*ELOG) + (-1.79383*ELOG*ELOG) +
     :         (0.102619*ELOG*ELOG*ELOG)
        ELSE
           X = 16.53869 + (0.6428144 + 3.)*ELOG - 0.3177744*ELOG*ELOG
     :         + 7.9471897e-3*(ELOG**3)
        ENDIF
        OXYGEN = EXP(X)/(E*E*E)
      END
*--------------------------------------------------------------------------
*       Real Function: SILICN
*
*       Source: Atomic and Nuclear Data Tables, January 1982
*
*       Description: SILICN calculates the mass absorption cross section
*                for silicon in cm**2/g.
*       History: updated Aug 30, 1991 (BHVAD::MBC)
*                updated March 4, 1992 (BHVAD::MBC)
*
        FUNCTION SILICN(E)

	IMPLICIT NONE
        REAL E,ELOG,X,SILICN
*
        ELOG = ALOG(E)
        IF(E.LT.100.6)THEN
           X = -3.066295 + (7.006248 + 3.)*ELOG - 0.9627411*ELOG*ELOG
        ELSEIF(E.LT.1840.0)THEN
           X = -182.7217 + (125.061 + 3.)*ELOG - 29.47269*ELOG*ELOG
     :         + 3.03284*(ELOG**3) - 0.1173096*(ELOG**4)
        ELSE
           X = -33.39074 + (18.42992 + 3.)*ELOG
     :         - 2.385117*ELOG*ELOG + 8.887583e-2*(ELOG**3)
        ENDIF
        SILICN = EXP(X)/(E*E*E)
        END
*------------------------------------------------------------------------
*     Real Function: SODIUM
*     Source: Atomic : Nuclear Data Tables, January 1982
*
*     Description:
*     Calculates mass absorption coefficient (mu/rho) for sodium
*
*     History: original Aug 6, 1991 (BHVAD::MBC)
*
	 FUNCTION SODIUM(E)

	IMPLICIT NONE
	REAL E,ELOG,X,SODIUM
*
	ELOG = ALOG(E)
	IF(E.LT.1071.7)THEN
           X= -2737.598 + (2798.704 + 3.)*ELOG -
     :	      1009.892*ELOG*ELOG + 87.16455*(ELOG**3) +
     :	      43.20644*(ELOG**4) - 15.27259*(ELOG**5) +
     :        2.180531*(ELOG**6) - 0.1526546*(ELOG**7) +
     :        4.3137977E-3*(ELOG**8)
        ELSE
           X=1.534019 + (6.261744 + 3.)*ELOG
     :        - 0.9914126*ELOG*ELOG +
     :        3.5278253E-2*(ELOG**3)
	ENDIF
	SODIUM = EXP(X)/(E*E*E)
      END
*----------------------------------------------------------------------
*     Real Function: SULFUR
*     Source: Atomic : Nuclear Data Tables, January 1982
*
*     Description:
*     Calculates mass absorption coefficient (mu/rho) for sulfur
*
*     History: original Aug 6, 1991 (BHVAD::MBC)
*
	FUNCTION SULFUR(E)
*
	IMPLICIT NONE
	REAL E,ELOG,X,SULFUR
*
	ELOG = ALOG(E)
	IF(E.LT.165.0)THEN
           X=598.2911 + (3. - 678.2265)*ELOG + 308.1133*ELOG*ELOG
     :      - 68.99324*(ELOG**3) + 7.62458*(ELOG**4)
     :      - 0.3335031*(ELOG**5)
        ELSEIF(E.LT.2470.5)THEN
           X=3994.831 + (3. - 3693.886)*ELOG +
     :        1417.287*ELOG*ELOG - 287.9909*(ELOG**3) +
     :        32.70061*(ELOG**4) - 1.968987*(ELOG**5) +
     :        4.9149349E-2*(ELOG**6)
         ELSE
            X=-22.49628 + (14.24599+ 3.)*ELOG -
     :         1.848444*ELOG*ELOG + 6.6506132E-2*(ELOG**3)
	ENDIF
	SULFUR = EXP(X)/(E*E*E)
      END
