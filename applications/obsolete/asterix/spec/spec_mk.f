*+  SPEC_MK - Fast computation of flux from Raymond & Smith hot plasma model
      SUBROUTINE SPEC_MK(GENUS,NDS,NEN,ELBOUND,EUBOUND,PARAM,FLUX,
     :                   STATUS)
*
*    Summary:
*
*     Fast Raymond-Smith plasma model calculation, uses RS grid binned up to
*     match the instrument response and the object redshift (if such a grid
*     exists). New grids for a particular instrument and or redshift can be
*     created with RS_REBIN (in OBELIX)
*
*    Description :
*
*     Returns flux in each energy channel delimited by bounds from the hot,
*     optically thin plasma models of Raymond & Smith. Returned flux has
*     units of photons/(cm**2*s*keV).
*     Model parameters are:
*	A      = PARAM(1)	Emission measure/distance**2 (units
*                                            of 1E60cm**-3/(10 kpc)**2 )
*	T      = PARAM(2)	Plasma temperature (keV)
*       METALS = PARAM(3)	Metal abundance rel. to H (units of cosmic ab.)
*     Note that definition of emission measure used is Ne**2*V.
*
*     The limits to the validity of the model depend on the RS grid used
*     These limits can be set using RS_REBIN (in OBELIX)
*
*    Method :
*
*     Models are precalculated and stored in a cube, whose axes
*     are photon energy, element abundance and temperature.
*     The abundance dimension contains 2 values, corresponding to spectra for
*     (1) the H and He spectrum and (2) the contributions from
*     11 heavier elements (as listed below) individually.
*     The elements used and log (number) abundances
*     relative to H=12, are: He(10.99), C(8.56), N(8.05), O(8.93), Ne(8.09),
*     Na(6.33), Mg(7.58), Al(6.47), Si(7.55), S(7.21), Ar(6.56), Ca(6.36),
*     Fe(7.67), Ni(6.25).
*     Requested model is computed from the values in the cube by linear
*     interpolation over temperature, and by scaling of elements relative
*     to their cosmic abundances. All elements heavier than He are amalgamated
*     into `metals' for the MK model.
*     Emissivity grid has to be multiplied by Ne*NH*Volume to get radiated
*     power.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*     For description of R & S models see Ap.J.Supp.35, 419 (1977).
*     The version used here is the VMS version sent to John Pye by Raymond
*     in April 1987.
*
*     The code will automatically search the default RS grid directory and
*      check every grid found against the energy bounds of the
*      dataset to be fitted. The first matching grid will be used.
*      This has the disadvantage that two grids identical apart
*      from different kT ranges cannot be resolved. The best solution
*      is to place the grid to be fitted in a directory apart from the
*      other grids and assign the directory logical (SPEC_DIR) to this
*      directory
*
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*     Rob Jeffries (BHVAD::RDJ)
*     Martin Watt (BHVAD::MPW)
*
*    History :
*
*      2 Mar 89 : Original
*      5 Jul 89 : Fast version using optimised grids for each instrument
*     22 Mar 90 : Change to RM_CALC dims arguments (to standard FORTRAN)
*     13 Mar 91 : ASTERIX88 version, includes code to pick up a compact
*                 RS grid set up by RS_REBIN for a particular redshift (MPW)
*     15 Mar 91 : Automatic searching of the default RS grid directory for a
*                 matching grid. (MPW)
*      8 May 91 : Tidied for release (MPW)
*      1 Mar 94 : Added error buffering (DJA)
*      5 Jul 95 : Added GENUS to stop use of full grid in non-spectral
*                 code (DJA)
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
      CHARACTER*(*)		GENUS			! Fitting genus
	INTEGER NDS			! dataset number
	INTEGER NEN			! No of energy channels
	REAL ELBOUND(NEN)		! Lower bin bounds (keV)
	REAL EUBOUND(NEN)		! Upper bin bounds (keV)
	REAL PARAM(3)			! Model parameters

*    Export :
	REAL FLUX(NEN)			! Photon flux (phot/cm**2/s)

*    Status :
	INTEGER STATUS

*    Local constants :
	REAL EM10			! EM of 1E60 cm**-3 at 10 kpc in
	PARAMETER (EM10=8.35774E13)	! units of cm**-6*cm**3/cm**2

*    Local variables :
	REAL A				! Normalisation (e.m./distance**2)
	REAL T				! Temperature in keV
	REAL METALS			! [Metal/H] relative to cosmic
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Check for spot value
	IF(ELBOUND(1).EQ.EUBOUND(1))THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP( ' ','Spot values not supported by SPEC_ '//
     :     'routines',STATUS)
	  GO TO 9000
	ENDIF

* Model parameters
	A=PARAM(1)
	T=PARAM(2)
	METALS=PARAM(3)

* Convert emission measure from Ne**2*V to Ne*NH*V (i.e. *NH/Ne)
*   (assumes full ionisation)
	A=A/(1.17+0.011*METALS)

* Compute interpolated emissivity (in units of 1E-23 photons*cm**3/s)
	CALL SPEC_MK_EMISS(GENUS,NDS,T,METALS,NEN,ELBOUND,EUBOUND,
     :                                           PARAM,FLUX,STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000

* Multiply by 1E-23*EM/4*pi*R**2 to get spec photon flux in photons/(cm**2*s)
        CALL ARR_MULTR( A*1.0E-23*EM10, NEN, FLUX )

* Exit
 9000	IF(STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'SPEC_MK', STATUS )
        END IF

	END


*+  SPEC_MK_EMISS - Interpolate from the R+S grid to produce a source spectrum
      SUBROUTINE SPEC_MK_EMISS(GENUS,NDS,TEMP,METALS,NEN,ELBOUND,
     :                                 EUBOUND,PARAM,EMISS,STATUS)

*    Description :
*     This routine accesses HDS data cubes containing the
*     R+S line and continuum data and passes it back to the
*     spectral fitting program
*     Emissivity is interpolated from spectra adjacent to the
*     one required, and returned in units of 1E-23 photons*cm**3/s
*
*    Method :
*      The routine searches all files in the directory pointed to by SPEC_DIR for a
*    R+S grid which has been binned to matvh the energy response of the dataset and
*    the redshift of the object. If no grid is found, the full grid is used by default,
*    this is significantly slower. Note that all of these cubes must contain only 2
*    elements, (i) H+He and (ii) all others.
*
*    Authors :
*     Rob Jeffries (BHVAD::RDJ)
*     Martin Watt (BHVAD::MPW)
*
*    History :
*     27 Sep 88: original (BHVAD::RDJ)
*      2 Mar 89: Modified STATUS handling (TJP)
*      3 Jul 89: Fast version using optimised RS grids (MPW)
*     13 Mar 91: ASTERIX88 version (MPW)
*      3 May 91: Tidied version (MPW)
*      8 May 91: Include RS locator in common block (MPW)
*
*    Type Definitions
      IMPLICIT NONE

*    Global constants
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'USER_ERR'

*    Import
      CHARACTER*(*)		GENUS			! Fitting genus
	INTEGER NDS			! dataset number
	REAL TEMP                       ! Temperature in keV
	REAL METALS                   	! Metal abundance relative to cosmic
	INTEGER NEN                     ! No. of energy channels

	REAL ELBOUND(NEN)		! Lower bin bounds (keV)
	REAL EUBOUND(NEN)		! Upper bin bounds (keV)
        REAL PARAM(3)                   ! Model parameters

*    Export
	REAL EMISS(NEN)                 ! Interpolated emissivity

*    Status
	INTEGER STATUS

*    Local variables :
        CHARACTER*80 MKCUBEFILE         ! Full file name of MK_CUBE

	INTEGER RSPTR(NDSMAX)		! pointer to rs array

	INTEGER ITLOW			! Index to the spectrum which lies
					! immediately below the reqired spectrum

	INTEGER DIMS(3)			! Data cube dimensions
	INTEGER MAXBOUND		! Dummy return from call

	INTEGER MAXBINS			! Maximum number of energy bins
	PARAMETER (MAXBINS=3000)

	REAL RESP_BOUNDS(MAXBINS)	! bounds of response used to set up grid

	INTEGER XDIMS(3,NDSMAX)		! Dimensions of all data cubes

        REAL TMIN			! minimum temperature in grid in keV
        REAL TMAX			! maximum temperature in grid in keV
        REAL TLSTEP		        ! logarithmic temperature step in grid

	REAL BT1,BT2                    ! Temperature in keV of spectra
                                        ! corresponding to ITLOW and ITLOW+1
	REAL FRAC                       ! Fraction used to interpolate
                                        ! between BT1 and BT2

	INTEGER I			! loop counter

* parameters for searching RS files
	INTEGER MAXFILES		! maximum number of files allowed
	PARAMETER (MAXFILES=400)

	INTEGER BOUNDSIZE		! size of energy bounds component in grid
	INTEGER IDUM			! unwanted subroutine return
	LOGICAL BOUNDSTHERE		! bounds exists
	LOGICAL BOUNDSAGREE		! bounds in response match those in dataset
	CHARACTER*(80) FILES(MAXFILES)	! Filenames in RS directory to search
	INTEGER NFOUND			! number of datasets in RS directory
	INTEGER LOOP
	INTEGER TEMPDIMS(3)		! Data cube dimensions
	INTEGER TEMPDIM			!


* variables for full grid
        REAL TOT(MAXBINS)               ! Summed and interpolated RS spectrum
        REAL BRFLUX(MAXBINS)            ! BR spectrum
	INTEGER BREMSTART		! bin number to start bremsstrahlung

	LOGICAL SMALLGRID(NDSMAX)	! compact grid located

	INTEGER MAXDATASET 	        ! highest dataset yet used
	DATA MAXDATASET /0/

*    Global variables :
        INCLUDE 'SPEC_CMN_RZ'
*-

*  Reset logical for new invocation of SFIT
      IF ( S_FIRST ) THEN
	S_FIRST=.FALSE.
	MAXDATASET=0
      END IF

*  See if this dataset has been passed before
      IF ( NDS .GT. MAXDATASET ) THEN

*    Reset maximum dataset number
	MAXDATASET = NDS

*    Set logical to indicate compact grid detected
	SMALLGRID(NDS) = .FALSE.

*    To find the appropriate response, get the instrument name for this dataset
	CALL UTIL_FINDFILE('SPEC_DIR','mkl*.sdf',MAXFILES,FILES,NFOUND,
     :                                                           STATUS)

*        loop until a matching grid is found
	  I=0
	  DO WHILE(.NOT.SMALLGRID(NDS) .AND. I.LT.NFOUND)

	   I=I+1

*          open each file in turn
            CALL HDS_OPEN(FILES(I),'READ',S_MKLOC(NDS),STATUS)

*          first check size of energy bounds component...
	    CALL DAT_THERE(S_MKLOC(NDS),'ENERGY_BOUNDS',BOUNDSTHERE,
     :                                                        STATUS)
	    IF(BOUNDSTHERE) THEN
	      CALL CMP_SHAPE(S_MKLOC(NDS),'ENERGY_BOUNDS',1,BOUNDSIZE,
     :                                                    IDUM,STATUS)
	    ELSE
	      BOUNDSIZE=-1
	    END IF
D	    type*,'  energy bounds=',boundsize

*          ...second check the actual values of the bounds if the number agrees
	    IF(BOUNDSIZE.EQ.NEN+1) THEN

	      CALL CMP_GET1R(S_MKLOC(NDS),'ENERGY_BOUNDS',NEN+1,
     :                                   RESP_BOUNDS,MAXBOUND,STATUS)

	      BOUNDSAGREE=.TRUE.
	      LOOP=1
	      DO WHILE(LOOP.LE.NEN .AND. BOUNDSAGREE)
	        IF(ABS(ELBOUND(LOOP)-RESP_BOUNDS(LOOP)).GT.0.00001)
     :                                              BOUNDSAGREE=.FALSE.
	        LOOP=LOOP+1
	      END DO

	      IF(BOUNDSAGREE) THEN

*              third, check number of elements in grid
                CALL CMP_SHAPE(S_MKLOC,'DATA_ARRAY',3,TEMPDIMS,TEMPDIM,
     :                                                           STATUS)
	        IF(TEMPDIMS(2).EQ.2) THEN
	          CALL MSG_SETI('NDS',NDS)
		  CALL MSG_SETC('CGRID',FILES(I))
	          CALL MSG_PRNT(
     :                     'Dataset ^NDS : Using compact grid: ^CGRID')
	          SMALLGRID(NDS)=.TRUE.
D		ELSE
D		  type*,'number of elements incorrect'
		END IF
D	      ELSE
D	        type*,'  energy bounds values dont agree'
	      END IF

	    END IF


	    IF(.NOT.SMALLGRID(NDS)) CALL HDS_CLOSE(S_MKLOC(NDS),STATUS)

	  END DO


*        no compact grid found, default to full grid
	  IF(.NOT.SMALLGRID(NDS))THEN

*          Don't use full grid unless spectral fitting
            IF ( GENUS .NE. 'SPEC' ) THEN
              STATUS = SAI__ERROR
              CALL MSG_SETC( 'GENUS', GENUS )
              CALL ERR_REP( ' ', 'Full Raymond & Smith grid not '/
     :                  /'available for ^GENUS fitting', STATUS )
              GOTO 9000
            END IF

            CALL PSX_GETENV( 'MK_CUBE', MKCUBEFILE, STATUS )
            CALL HDS_OPEN( MKCUBEFILE, 'READ', S_MKLOC(NDS), STATUS )
            IF(STATUS.NE.SAI__OK) GO TO 9000

*          map energy bounds array for full grid
	    CALL CMP_GET1R(S_MKLOC(NDS),'ENERGY_BOUNDS',MAXBINS,
     :                               RESP_BOUNDS,MAXBOUND,STATUS)

	  END IF

*        get temperature values
	  CALL CMP_GET0R(S_MKLOC(NDS),'TMIN',TMIN,STATUS)
	  CALL CMP_GET0R(S_MKLOC(NDS),'TMAX',TMAX,STATUS)
	  CALL CMP_GET0R(S_MKLOC(NDS),'TLSTEP',TLSTEP,STATUS)

*        map data array
	  CALL CMP_MAPN(S_MKLOC(NDS),'DATA_ARRAY','_REAL','READ',3,
     :                                       RSPTR(NDS),DIMS,STATUS)

*        check full grid has just 2 elements and inform user of grid
	  IF(.NOT.SMALLGRID(NDS) .AND. DIMS(2).EQ.2) THEN
	    CALL MSG_SETI('NDS',NDS)
	    CALL MSG_PRNT('Dataset ^NDS : Full Raymond+Smith grid used')
	  ELSEIF(.NOT.SMALLGRID(NDS) .AND. DIMS(2).NE.2) THEN
	    CALL MSG_SETI('NDS',NDS)
	    CALL MSG_SETI('NEL',DIMS(2))
	    CALL MSG_PRNT('!! Dataset ^NDS : MK_CUBE '//
     :             'contains ^NEL elements, it should have 2 elements')
	    STATUS=SAI__ERROR
	    GOTO 9000
	  END IF

	  XDIMS(1,NDS)=DIMS(1)
	  XDIMS(2,NDS)=DIMS(2)
	  XDIMS(3,NDS)=DIMS(3)

*      end of code executed first time only for each dataset to be fitted
	END IF

*    Calculate the two nearest continuum temperature values
      IF ( TEMP .LT. TMIN ) THEN

*      Error buffering on?
        IF ( S_EBUF ) THEN
          S_NTLO = S_NTLO + 1
          S_TLO = MIN( S_TLO, TEMP )
        ELSE
	  CALL MSG_SETR('TEMP',TEMP)
	  CALL MSG_SETR('TMIN',TMIN)
	  CALL MSG_PRNT('kT requested is ^TEMP keV,'//
     :                  ' setting to minimum grid value of ^TMIN keV')
        END IF
        TEMP=TMIN
	ITLOW=1 	! set to min allowed value in RS grid
	FRAC=0

      ELSEIF(TEMP.GT.TMAX) THEN

*      Error buffering on?
        IF ( S_EBUF ) THEN
          S_NTHI = S_NTHI + 1
          S_THI = MAX(S_THI,TEMP)
        ELSE
	  CALL MSG_SETR('TEMP',TEMP)
	  CALL MSG_SETR('TMAX',TMAX)
	  CALL MSG_PRNT('kT requested is ^TEMP keV,'//
     :                    ' setting to maximum grid value of ^TMAX keV')
        END IF
        TEMP=TMAX
	ITLOW=XDIMS(3,NDS)-1	! set to max allowed value in RS grid
	FRAC=0.9999

      ELSE
*      interpolate between nearest adjacent temperatures
	ITLOW=INT((LOG10(TEMP/TMIN))/TLSTEP+1.0)
	BT1=TMIN*10.**(TLSTEP*ITLOW)
	BT2=TMIN*10.**(TLSTEP*(ITLOW-1))
	FRAC=(TEMP-BT2)/(BT1-BT2)
      ENDIF

* do the calculations
      IF(SMALLGRID(NDS)) THEN

        CALL SPEC_MK_CALC(ITLOW,FRAC,METALS,XDIMS(1,NDS),XDIMS(2,NDS),
     :    XDIMS(3,NDS),%VAL(RSPTR(NDS)),EMISS)

      ELSEIF(.NOT.SMALLGRID(NDS)) THEN

        CALL SPEC_MK_CALC(ITLOW,FRAC,METALS,XDIMS(1,NDS),XDIMS(2,NDS),
     :    XDIMS(3,NDS),%VAL(RSPTR(NDS)),TOT)

*      rebin the RS data into instrument bins
        CALL FIT_REBMOD1(XDIMS(1,NDS),TOT,RESP_BOUNDS,.FALSE.,NEN,
     :    ELBOUND,EUBOUND,EMISS,STATUS)

*      Warning if energy array extends below RS grid limits
        IF(STATUS.EQ.USER__001.OR.STATUS.EQ.USER__003)THEN
          CALL ERR_REP('L','Lowest energies not covered by RS grid',
     :                                                        STATUS)
          CALL ERR_FLUSH(STATUS)
        ENDIF

*      if energy array extends above RS grid upper limit then add bremss at top
        IF(STATUS.EQ.USER__002.OR.STATUS.EQ.USER__003) THEN

*        reset or flush status
          IF(STATUS.EQ.USER__002) CALL ERR_ANNUL(STATUS)
          IF(STATUS.EQ.USER__003) CALL ERR_FLUSH(STATUS)

*        calculate the bremsstrahlung spectrum using first 2 RS parameters
          CALL SPEC_BR(NEN,ELBOUND,EUBOUND,PARAM,BRFLUX,STATUS)

*        correct the bremsstrahlung normalisation to match the RS normalisation
          CALL SPEC_MK_CORRBR(NEN,ELBOUND,EUBOUND,PARAM,BRFLUX,STATUS)

*        add bremsstrahlung to channels above top of RS grid

*        find bin that straddles top of RS grid...
          BREMSTART=1
          DO I=1,NEN
            IF(EUBOUND(I).LT.RESP_BOUNDS(XDIMS(1,NDS))) THEN
              BREMSTART=I
            ENDIF
          ENDDO

*        ...and add the bremsstrahlung component
          CALL SPEC_MK_ADDBR(NEN,BREMSTART,BRFLUX,EMISS)

	END IF

      END IF

*    Tidy up
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SPEC_MK_EMISS', STATUS )
      END IF

      END


*+  SPEC_MK_CALC - Calculates spectrum
	SUBROUTINE SPEC_MK_CALC(ITLOW,FRAC,METALS,DIMS1,DIMS2,DIMS3,
     :  GRID,EMISS)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*      Imports
	INTEGER ITLOW			! Index to the spectrum which lies
					! immediately below the reqired spectrum
	REAL FRAC			! Fraction used to interpolate
					! between BT1 and BT2
	REAL METALS			! Metal abundance relative to cosmic

	INTEGER DIMS1,DIMS2,DIMS3	! size of RS data cube
	REAL GRID(DIMS1,DIMS2,DIMS3)	! RS data cube

*      Exports
        REAL EMISS(DIMS1)               ! Interpolated emissivity

*      Local variable
	INTEGER J			! loop counter
*-

* Interpolate between spectra
       DO J=1,DIMS1
         EMISS(J) = (GRID(J,1,ITLOW)*(1-FRAC)+GRID(J,1,ITLOW+1)*FRAC)
     :            + (GRID(J,2,ITLOW)*(1-FRAC)+GRID(J,2,ITLOW+1)*FRAC)
     :                                                        *METALS
       ENDDO

      END


*+  SPEC_MK_ADDBR - Adds bremsstrahlung to high energy bins
       SUBROUTINE SPEC_MK_ADDBR(NEN,BREMSTART,BREMS,EMISS)

*     Description
*      Adds on a bremsstrahlung model (from SPEC_BR) to the initial
*      RS model, for the instrument energy channels above and
*      including the one that starts at the top of the RS grid.
*
*     Authors :
*      Rob Jeffries  (BHVAD::RDJ)
*
*     History :
*      2 Mar 89: Original
*
*     Type Definitions :
       IMPLICIT NONE

*     Global constants :
       INCLUDE 'SAE_PAR'

*     Import :
       INTEGER NEN                ! Number of instrument energy channels
       INTEGER BREMSTART          ! Channel from which SPEC_BR model is
                                  ! to be used

       REAL BREMS(NEN)            ! The SPEC_BR model

*     Import-export :
       REAL EMISS(NEN)            ! The RS model and the finally computed
                                  ! spectral model
*     Local variables :
       INTEGER I                  ! Loop index

*-

* Add the arrays
       DO I=BREMSTART,NEN
          EMISS(I)=BREMS(I)
       ENDDO
       END


*+  SPEC_MK_CORRBR- Corrects the BR normalisation
       SUBROUTINE SPEC_MK_CORRBR(NEN,ELBOUND,EUBOUND,PARAM,BREMS,STATUS)

*     Description:
*      The Gould bremsstrahlung spectrum normalisation is modified
*      to units of 1E-23 erg*cm**3/s, to match the Raymond and Smith
*      spectrum.
*      A further adjustment is made to allow for the fact that non-cosmic
*      abundances will change the Sum{ N(Z)*Z**2} value assumed by SPEC_BR,
*      and to allow for the NH/Ne scaling in SPEC_MK (this should not be
*      applied to the BR output, which is already defined in terms of an
*      Ne**2*V emission measure).

*       Note: there is still a jump between RS and BR, RS is higher,
*      especially at low T
*
*     Author:
*      Rob Jeffries (BHVAD::RDJ)
*
*     History:
*      10 Feb 88 Original (RDJ)
*       2 Mar 89: Non-cosmic abundance correction added (TJP)
*      27 Nov 89: He/H fixed at 1 (TJP)
*
*     Type definitions:
       IMPLICIT NONE

*     Global constants
       INCLUDE 'SAE_PAR'

*     Import:
       INTEGER NEN
       REAL ELBOUND(NEN)
       REAL EUBOUND(NEN)
       REAL PARAM(3)

*     Import-export:
       REAL BREMS(NEN)

*     Local variables
       INTEGER I
       REAL ABCOR			! Correction to SZ2

*     Status
       INTEGER STATUS

*     Local constants :
       REAL SZ2                          ! sum Z*Z*N(Z)/N(e)
       PARAMETER (SZ2=1.258)             ! as used in SPEC_BR
       REAL EM10				! EM of 1E60 cm**-3 at 10 kpc in
       PARAMETER (EM10=8.35774E13)	! units of cm**-6*cm**3/cm**2
*-

*  Abundance/scaling correction
      ABCOR=((1+0.34+0.113*PARAM(3)))/SZ2

      DO I = 1, NEN
         BREMS(I)=BREMS(I)*ABCOR*1E23/(PARAM(1)*EM10)
      END DO

*   Exit
      IF(STATUS.NE.SAI__OK) CALL AST_REXIT( 'SPEC_MK_CORRBR', STATUS )

      END



*+  SPEC_MK_BOL - Returns bolometric emissivity from R & S grid file
      SUBROUTINE SPEC_MK_BOL(NDS,TEMP,METALS,EMTOT,STATUS)
*
*    Description :
*     Accesses the R&S HDS data cube (which must have already been
*     opened through a call to SPEC_MK_EMISS) and retrieves the
*     bolometric emissivity (in units of 1E-23 photon*cm**3/s),
*     interpolating over temperature as necessary.
*
*    Method :
*     Locator to the file is retrieved from the SPEC_CMN_RZ common block.
*
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*
*    History :
*     15 May 91: Original
*
*    Type definitions
      IMPLICIT NONE

*    Global constants
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'USER_ERR'

*
*    Global variables :
*
      INCLUDE 'SPEC_CMN_RZ'

*    Import
	INTEGER NDS			! dataset number
	REAL TEMP                       ! Temperature in keV
	REAL METALS                   	! Metal abundance relative to cosmic

*    Export
	REAL EMTOT			! Bolometric emissivity

*    Status
	INTEGER STATUS

*    Local variables :

	INTEGER EMBPTR			! Pointer to bol. emissivity array
	INTEGER ITLOW			! Index to the spectrum which lies
					! immediately below the reqired spectrum
	INTEGER NDIM			! Array dimensionality
	INTEGER DIMS(2)			! Array dimensions
	INTEGER NEL			! No of elements in EMBOL array
	INTEGER IND			! Array index value

        REAL TMIN			! minimum temperature in grid in keV
        REAL TMAX			! maximum temperature in grid in keV
        REAL TLSTEP		        ! logarithmic temperature step in grid

	REAL BT1,BT2                    ! Temperature in keV of spectra
                                        ! corresponding to ITLOW and ITLOW+1
	REAL FRAC                       ! Fraction used to interpolate
                                        ! between BT1 and BT2
	REAL EM1,EM2,EM3,EM4		! Four emissivity values for interpoln
*
*    Local data:
*
	LOGICAL FIRSTHERE		! first time in this subroutine
	DATA FIRSTHERE /.TRUE./
*-

* Check that cube has been accessed already
	IF(S_FIRST)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('NOCUBE','R&S cube not open',STATUS)
	  GO TO 9000
	ENDIF

* Check for first invocation of this subroutine
	IF(FIRSTHERE) THEN

	  FIRSTHERE=.FALSE.

*        Get temperature values
	  CALL CMP_GET0R(S_MKLOC(NDS),'TMIN',TMIN,STATUS)
	  CALL CMP_GET0R(S_MKLOC(NDS),'TMAX',TMAX,STATUS)
	  CALL CMP_GET0R(S_MKLOC(NDS),'TLSTEP',TLSTEP,STATUS)
	  CALL CMP_SHAPE(S_MKLOC(NDS),'EMBOL',2,DIMS,NDIM,STATUS)
	  IF(STATUS.NE.SAI__OK) GO TO 9000
	  IF(DIMS(1).NE.2)THEN
	    STATUS=SAI__ERROR
	    CALL ERR_REP('BADBOL','Bad metallicity dimension in EMBOL',
     :	      STATUS)
	    GO TO 9000
	  ENDIF

*        Map bolometric emissivity component
	  CALL CMP_MAPV(S_MKLOC(NDS),'EMBOL','_REAL','READ',EMBPTR,NEL,
     :                                                          STATUS)

	END IF

* Interpolate between two nearest temperature values in grid
	IF(TEMP.LT.TMIN) THEN
	  CALL MSG_SETR('TEMP',TEMP)
	  CALL MSG_SETR('TMIN',TMIN)
	  CALL MSG_PRNT('kT requested is ^TEMP keV, setting to minimum '/
     :          /'grid value of ^TMIN keV')
	  TEMP=TMIN
	  ITLOW=1 	! set to min allowed value in RS grid
	  FRAC=0
	ELSEIF(TEMP.GT.TMAX) THEN
	  CALL MSG_SETR('TEMP',TEMP)
	  CALL MSG_SETR('TMAX',TMAX)
	  CALL MSG_PRNT('kT requested is ^TEMP keV, setting to maximum '/
     :          /'grid value of ^TMAX keV')
	  TEMP=TMAX
	  ITLOW=DIMS(2)-1	! set to max allowed value in RS grid
	  FRAC=0.9999
	ELSE
	  ITLOW=INT((LOG10(TEMP/TMIN))/TLSTEP+1.0)
	  BT2=TMIN*10.**(TLSTEP*(ITLOW-1))	! Lower grid temp
	  BT1=TMIN*10.**(TLSTEP*ITLOW)		! Upper grid temp
	  FRAC=(TEMP-BT2)/(BT1-BT2)
	ENDIF

* Combine the EMBOL elements as appropriate
	IND=1+2*(ITLOW-1)		! Index of lower temp H+He EMBOL entry
	CALL ARR_ELEM1R(EMBPTR,NEL,IND,EM1,STATUS)	! ITLO H+He
	CALL ARR_ELEM1R(EMBPTR,NEL,IND+1,EM2,STATUS)	! ITLO metals
	CALL ARR_ELEM1R(EMBPTR,NEL,IND+2,EM3,STATUS)	! ITLO+1 H+He
	CALL ARR_ELEM1R(EMBPTR,NEL,IND+3,EM4,STATUS)	! ITLO+1 metals
	EMTOT=(EM1+METALS*EM2)*(1-FRAC)+(EM3+METALS*EM4)*FRAC

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_MK_BOL',
     :  STATUS)
	END
