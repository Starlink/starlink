*+CAL_ASPECT_ERROR	Get aspect error estimates for epoch
	SUBROUTINE CAL_ASPECT_ERROR(MJD,ERAZ,EREL,EROLL,ISTAT)
	DOUBLE PRECISION MJD
	REAL ERAZ,EREL,EROLL
	INTEGER ISTAT
*MJD	input	epoch
*ERAZ	output	error in azimuth radians
*EREL	output	error in elevation radians
*EROLL	output	error in roll radians
*ISTAT	in/out	returned status
*-Author Dick Willingale 1990-May-15
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC) LOC,LOCA,LOCE,LOCR,LOCC
	DOUBLE PRECISION MJDLO,MJDHI
	REAL EA,EE,ER
	SAVE EA,EE,ER,MJDLO,MJDHI
	LOGICAL CIN_VALID
C
	IF(ISTAT.NE.0) RETURN
C
	IF(.NOT.CIN_VALID(MJD,MJDLO,MJDHI,0,0)) THEN
C Find structure in MCF
		CALL CIN_INIT(LOC,ISTAT)
		CALL DAT_FIND(LOC,'ALIGN',LOCA,ISTAT)
		CALL DAT_FIND(LOCA,'AERRORS',LOCE,ISTAT)
C Get epoch cell
		CALL CIN_FEPOCH(MJD,LOCE,NE,MJDLO,MJDHI,ISTAT)
		CALL DAT_FIND(LOCE,'DATA_RECORD',LOCR,ISTAT)
		CALL DAT_CELL(LOCR,1,NE,LOCC,ISTAT)
C Get data
		CALL CMP_GET0R(LOCC,'AZIMUTH',EA,ISTAT)
		CALL CMP_GET0R(LOCC,'ELEVATION',EE,ISTAT)
		CALL CMP_GET0R(LOCC,'ROLL',ER,ISTAT)
C Clear
		CALL DAT_ANNUL(LOCC,ISTAT)
		CALL DAT_ANNUL(LOCR,ISTAT)
		CALL DAT_ANNUL(LOCE,ISTAT)
		CALL DAT_ANNUL(LOCA,ISTAT)
		IF(ISTAT.NE.0) THEN
			WRITE(*,*) '   error in CAL_ASPECT_ERROR'
		ENDIF
	ENDIF
C Return values
	ERAZ=EA
	EREL=EE
	EROLL=ER
	END
*+CAL_DETEFF	Calculate detector efficiency at spot times
	SUBROUTINE CAL_DETEFF(NTIME,MJDS,IFILT,EFFS,ISTAT)
	INTEGER NTIME,IFILT,ISTAT
	DOUBLE PRECISION MJDS(NTIME)
	REAL EFFS(NTIME)
*-Author Dick Willingale 1991-Nov-14
	REAL CIN_DETEFF
	EXTERNAL CIN_DETEFF
C
	IF(ISTAT.NE.0) RETURN
C
	DO J=1,NTIME
		EFFS(J)=CIN_DETEFF(MJDS(J),IFILT,ISTAT)
	ENDDO
	END


*+CAL_FILT_INFO	Get information about filter
	SUBROUTINE CAL_FILT_INFO (FILTER,BLURB,ENERGY,ISTAT)
	INTEGER FILTER,ISTAT
	CHARACTER BLURB*(*)
	REAL ENERGY
*FILTER	input	filter number (h/w)
*BLURB	output	description of filter
*ENERGY	output	approximate energy of band pass eV
*ISTAT	in/out	returned status
*-Author Dick Willingale 1990-May-15
	PARAMETER (MAXF=16)
	INTEGER POS(MAXF),NFIL
        INCLUDE 'DAT_PAR'
	CHARACTER LOC*(DAT__SZLOC),LOCF*(DAT__SZLOC)
	CHARACTER*80 INFO(MAXF)
	REAL BAND(MAXF)
	DATA NFIL/0/
	SAVE POS,NFIL,INFO,BAND
C
	IF(ISTAT.NE.0) RETURN
C Get stuff from MCF
	IF(NFIL.EQ.0) THEN
C Get locator to MCF
		ISTAT=0
		CALL CIN_INIT(LOC,ISTAT)
C Get locator to FILTERS structure
		CALL DAT_FIND(LOC,'FILTERS',LOCF,ISTAT)
C Get data
		CALL CMP_GET1I(LOCF,'POS',MAXF,POS,NFIL,ISTAT)
		CALL CMP_GET1R(LOCF,'ENERGY',MAXF,BAND,NFIL,ISTAT)
		CALL CMP_GET1C(LOCF,'TEXT',MAXF,INFO,NFIL,ISTAT)
		CALL DAT_ANNUL(LOCF,ISTAT)
		IF(ISTAT.NE.0) THEN
			WRITE(*,*) '   error in CAL_FILT_INFO'
		ENDIF
	ENDIF
	ENERGY=0.0
	DO J=1,NFIL
		IF(FILTER.EQ.POS(J)) THEN
		  ENERGY=BAND(J)
		  BLURB=INFO(J)
		ENDIF
	ENDDO
	IF(ENERGY.EQ.0.) THEN
		ISTAT=1
		WRITE(*,*) '   CAL_FILT_INFO filter',FILTER,' not found'
	ENDIF
	END
*+CAL_FILT_N2S	Convert filter number to text
	CHARACTER*(*) FUNCTION CAL_FILT_N2S (FILT)
* Input
	INTEGER		FILT		! Filter number
* M. Denby 2-Aug-89
* Modified to get info from MCF Dick Willingale 1990-May-14
* Returns *** if invalid FILT value
*-
* Local
	PARAMETER (MAXF=16)
	CHARACTER*3 RPS(MAXF)
	INTEGER POS(MAXF),NFIL
	SAVE RPS,POS,NFIL
	DATA NFIL/0/
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC) LOC,LOCF
C Get data from MCF if missing
	IF(NFIL.EQ.0) THEN
C Get locator to MCF
		ISTAT=0
		CALL CIN_INIT(LOC,ISTAT)
C Get locator to FILTERS structure
		CALL DAT_FIND(LOC,'FILTERS',LOCF,ISTAT)
C Get data
		CALL CMP_GET1C(LOCF,'RPS',MAXF,RPS,NFIL,ISTAT)
		CALL CMP_GET1I(LOCF,'POS',MAXF,POS,NFIL,ISTAT)
		CALL DAT_ANNUL(LOCF,ISTAT)
		IF(ISTAT.NE.0) THEN
			WRITE(*,*) '   error in CAL_FILT_N2S'
		ENDIF
	ENDIF
	CAL_FILT_N2S(1:)='***'
	DO J=1,NFIL
		IF(POS(J).EQ.FILT) THEN
		  CAL_FILT_N2S(1:) = RPS(J)
		ENDIF
	ENDDO
	END
*+CAL_FILT_NUM	Convert filter wheel postn. to filter number
	INTEGER FUNCTION CAL_FILT_NUM (IWHEEL)
* Input
	INTEGER		IWHEEL		! Filter wheel encoder
* M. Denby 2-Aug-89
* Modified to use info from MCF Dick Willingale 1990-May-14
* Returns -1 if not found
*-
	PARAMETER (MAXF=16)
	INTEGER POS(MAXF),TLM(MAXF),NFIL
	SAVE POS,NFIL,TLM
	DATA NFIL/0/
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC) LOC,LOCF
C Get data from MCF if missing
	IF(NFIL.EQ.0) THEN
C Get locator to MCF
		ISTAT=0
		CALL CIN_INIT(LOC,ISTAT)
C Get locator to FILTERS structure
		CALL DAT_FIND(LOC,'FILTERS',LOCF,ISTAT)
C Get data
		CALL CMP_GET1I(LOCF,'POS',MAXF,POS,NFIL,ISTAT)
		CALL CMP_GET1I(LOCF,'TLM',MAXF,TLM,NFIL,ISTAT)
		CALL DAT_ANNUL(LOCF,ISTAT)
		IF(ISTAT.NE.0) THEN
			WRITE(*,*) '   error in CAL_FILT_NUM'
		ENDIF
	ENDIF
	CAL_FILT_NUM=-1
	DO J=1,NFIL
		IF(TLM(J).EQ.IWHEEL) THEN
		  CAL_FILT_NUM = POS(J)
		ENDIF
	ENDDO
	END
*+CAL_FILT_S2N	Convert filter text to filter number
	INTEGER FUNCTION CAL_FILT_S2N (TEXT)
* Input
	CHARACTER*(*) 		TEXT		! Filter
* M. Denby 2-Aug-89
* Modified to get info from MCF Dick Willingale 1990-May-14
* Returns -1 if invalid text
*-
* Local
	PARAMETER (MAXF=16)
	CHARACTER*3 RPS(MAXF)
	INTEGER POS(MAXF),NFIL
	SAVE RPS,POS,NFIL
	DATA NFIL/0/
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC) LOC,LOCF
	CHARACTER TUP*3,RUP*3
C Get data from MCF if missing
	IF(NFIL.EQ.0) THEN
C Get locator to MCF
		ISTAT=0
		CALL CIN_INIT(LOC,ISTAT)
C Get locator to FILTERS structure
		CALL DAT_FIND(LOC,'FILTERS',LOCF,ISTAT)
C Get data
		CALL CMP_GET1C(LOCF,'RPS',MAXF,RPS,NFIL,ISTAT)
		CALL CMP_GET1I(LOCF,'POS',MAXF,POS,NFIL,ISTAT)
		CALL DAT_ANNUL(LOCF,ISTAT)
		IF(ISTAT.NE.0) THEN
			WRITE(*,*) '   error in CAL_FILT_S2N'
		ENDIF
	ENDIF
C
        TUP=TEXT
	CALL CHR_UCASE(TUP)
	CAL_FILT_S2N=-1
	DO J=1,NFIL
                RUP=RPS(J)
                  CALL CHR_UCASE(RUP)
		IF(RUP.EQ.TUP) THEN
		  CAL_FILT_S2N = POS(J)
		ENDIF
	ENDDO
	END
*+CAL_FRADEAD	Estimate fractional deadtime from LEVS
	FUNCTION CAL_FRADEAD(IFMT,ZOOM,LEVS)
	REAL CAL_FRADEAD,LEVS
	INTEGER IFMT
	LOGICAL ZOOM
*IFMT	telemetry format 1=normal, 2=hi-speed
*ZOOM	inactive at present
*LEVS	LEVS from HK (cts/sec)
*CAL_FRADEAD returned as fraction of time dead when rate LEVS
*Then FEVS or AEVS = LEVS/(1+CAL_FRADEAD)
*or   Corrected count=Actual count/(1-CAL_FRADEAD)
*LEVS is available from housekeeping as a function of time
*Returns 0.0 if LEVS=0
*Returns 1.0 if IFMT=0 or IFMT=3
*-Author Dick Willingale 1990-Sep-18
	PARAMETER (DTPE=3.0517E-4)	!Electronic deadtime per event secs
	REAL QS(0:3),QE(0:3),QMAX(0:3)
	DATA QS/0.0,190.0,430.0,0.0/	!LEVS at which queue deadtime starts
	DATA QE/0.0,240.0,495.0,0.0/	!LEVS at which queue saturates
	DATA QMAX/0.0,200.0,408.0,0.0/	!Maximum telemetry rates (AEVS) cts/sec
C
	IF(LEVS.GT.0.0) THEN
		IF(LEVS.GT.QE(IFMT)) THEN
C At high LEVS queue saturates
			AEVS=QMAX(IFMT)
		ELSEIF(LEVS.LT.QS(IFMT)) THEN
C At low LEVS get electronic deadtime
			AEVS=LEVS/(1.0+LEVS*DTPE)
		ELSE
C Queue introduces a knee
			DEL=LEVS-QS(IFMT)
			DA=QE(IFMT)-QS(IFMT)
			QA=QS(IFMT)/(1.0+QS(IFMT)*DTPE)
			AEVS=QA+(QMAX(IFMT)-QA)*DEL/DA
		ENDIF
		CAL_FRADEAD=1.0-AEVS/LEVS
	ELSE
		CAL_FRADEAD=0.0
	ENDIF
	END
*+CAL_GET_ALIGN	Gets an alignment set from the master Cal File
      OPTIONS /EXTEND_SOURCE
      SUBROUTINE CAL_GET_ALIGN (REQUEST,CAL,STATUS)

*  Calling Arguments
      CHARACTER*(*) REQUEST		! 'ALL' or one of 'ROSAT_TO_ROST',
				! 'WFC_TO_ST','ROSAT_TO_WFC','WFC_TO_FOV'
*   Returns structure CAL containing Euler angles
      INCLUDE 'CALLIB(CIN_ALIGN_LOW)'
      INCLUDE 'CALLIB(CIN_ALIGN)'

      INTEGER STATUS
* Original M.J. Ricketts
* Modified M.Denby, 16-May-89 to conform to CAL library conventions
* Modified to use standard epoch indexing Dick Willingale 1990-May-10
*-

*  Local Variables

      INCLUDE 'CALLIB(CIN_ALIGN_TYPES)'

      RECORD /ALIGN_REC/ AREC(2)
        INCLUDE 'DAT_PAR'
      CHARACTER*(DAT__SZLOC) ALOC,CELL, KLOC, RLOC
      CHARACTER*20 LOCATOR, REL_DATE
      CHARACTER*64 LOCALIGN
      INTEGER NREC, ISET, NGOT
      CHARACTER*(DAT__SZLOC)  LOC			! Locator to MCF
      DOUBLE PRECISION MJDLO,MJDHI

*  Executable Code
      IF (STATUS.NE.0) RETURN

* Get locator to MCF
      CALL CIN_INIT (LOC,STATUS)

*  Get issue date and version number
      CALL CMP_GET0C(LOC,'ISSUE_DATE',cal.ISSUE_DATE,STATUS)
      CALL CMP_GET0I(LOC,'VERSION',cal.VERSION_NUMBER,STATUS)

* Get next level 'align'
      CALL DAT_FIND(LOC,'ALIGN',KLOC,STATUS)


      IF (STATUS.NE.0) RETURN

      NGOT = 0

      DO ISET = 1,NALIGN_TYPES

         IF ((REQUEST.EQ.'ALL').OR.(REQUEST.EQ.ALTYPE(ISET))) THEN
            LOCATOR = ALTYPE(ISET)

            NGOT = NGOT + 1			! Count types got

            CALL DAT_FIND(KLOC,LOCATOR,ALOC,STATUS)

* Get epoch record
	    CALL CIN_FEPOCH(CAL.MJD,ALOC,NREC,MJDLO,MJDHI,STATUS)
	    CALL DAT_FIND(ALOC,'DATA_RECORD',RLOC,STATUS)
	    CALL DAT_CELL(RLOC,1,NREC,CELL,STATUS)

	    MJD_VALID=MJDHI
* Get cal data
	    IF(MJDLO.LT.40000.) THEN
		REL_DATE='BEFORE 1ST'
		CALL CIN_ALIGN_GETSET(AREC(1),CELL,STATUS)
	    ELSEIF(MJDHI.GT.70000.) THEN
		REL_DATE='AFTER LAST'
		CALL CIN_ALIGN_GETSET(AREC(1),CELL,STATUS)
	    ELSE
		REL_DATE='BETWEEN'
		CALL CIN_ALIGN_GETSET(AREC(1),CELL,STATUS)
		CALL DAT_ANNUL(CELL,STATUS)
		NREC=NREC+1
		CALL DAT_CELL(RLOC,1,NREC,CELL,STATUS)
		CALL CIN_ALIGN_GETSET(AREC(2),CELL,STATUS)
	    ENDIF

	    CALL DAT_ANNUL(CELL,STATUS)
            CALL DAT_ANNUL(ALOC,STATUS)
	    CALL DAT_ANNUL(RLOC,STATUS)

            CALL CIN_ALIGN_DATE(AREC,CAL.ALIGNMENT(ISET),CAL.MJD,REL_DATE)

         END IF

      END DO					! Loop for 1 or 4 sets

      CALL DAT_ANNUL(KLOC,STATUS)

      IF (STATUS.EQ.0 .AND. NGOT .EQ. 0) STATUS = -9

      END
*+ CAL_INIT explicit CAL initialise call
	SUBROUTINE CAL_INIT (STATUS)
*Input
	INTEGER		STATUS
* M. Denby 14-May-90
*-
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC)	LOC

* The routine gives the caller control of where the CAL Anounce message
* gets o/p

	CALL CIN_INIT (LOC, STATUS)

	END


*+CAL_INTER_LIN	Convert det. coords to local sphericals
	SUBROUTINE CAL_INTER_LIN (IX, IY, NEV, QMAP, ZOOM, AZ, EL, IQ)
* Input
	INTEGER		IX(*), IY(*) 		! Tx pstns 0-511
	INTEGER		NEV			! # events in frame
	PARAMETER (NLIN = 64)
	REAL 		QMAP(0:NLIN,0:NLIN,2)	! Lin table
	LOGICAL		ZOOM			! Use zoomed det
* Output
	REAL		AZ(*), EL(*)		! Local sherics (rads)
	INTEGER		IQ(*)			! Qual (0-OK,1-null,2-masked)

	DATA ISEED /325678945/

* Author Dick Willingale 1985-Sep-4
* The table must be obtained from MCF using CAL_SET_LIN
* Process a frame at a time M. Denby 14-Jan-88
* Modified to use new quality masking Dick Willingale 1990-Apr-20
* Add a random offset to pixels Mike Denby 16-May-90
* Corrected pixel offset error Dick Willingale 1990-May-30
* Major modification of masking Dick Willingale 1990-Sep-3
*-
* IX and IY are always 0-511 for zoomed and unzoomed data. For zoomed data
* this range corresponds to the centre quarter section giving twice the
* (linear) resolution
        REAL MATH_RND
	LOGICAL CIN_HIT_MASK

	NPIX = 512/NLIN
	NPIX2 = NPIX*2
	DO NE = 1, NEV
	    IF(IX(NE) .EQ. 0 .AND. IY(NE) .EQ. 0) THEN
C Flag null event
	      IQ(NE)=1
	    ELSE
C Find position in table, depends on zoom
C Add half a pixel to compensate (on average) for digitisation
	      IF(ZOOM) THEN
		IDX = IX(NE)/2 + 128
		IDY = IY(NE)/2 + 128
		IXX = IDX/NPIX
		IYY = IDY/NPIX
		XS = REAL(IX(NE) - (IXX-16)*NPIX2) + MATH_RND()
		YS = REAL(IY(NE) - (IYY-16)*NPIX2) + MATH_RND()
		XS=XS/REAL(NPIX2)
		YS=YS/REAL(NPIX2)
	      ELSE
		IXX=IX(NE)/NPIX
		IYY=IY(NE)/NPIX
	        XS=REAL(IX(NE)-IXX*NPIX) + MATH_RND()
		YS=REAL(IY(NE)-IYY*NPIX) + MATH_RND()
		XS=XS/REAL(NPIX)
		YS=YS/REAL(NPIX)
	      ENDIF
C Extract limits of cell from table and check for quality of cell
	      X1=QMAP(IXX,IYY,1)
	      Y1=QMAP(IXX,IYY,2)
	      X2=QMAP(IXX+1,IYY,1)
	      Y2=QMAP(IXX+1,IYY,2)
	      X3=QMAP(IXX,IYY+1,1)
	      Y3=QMAP(IXX,IYY+1,2)
C Now interpolate
	      AZ(NE)=X1+XS*(X2-X1)+YS*(X3-X1)
	      EL(NE)=Y1+YS*(Y3-Y1)+XS*(Y2-Y1)
C Check masking
	      IF(CIN_HIT_MASK(QMAP,AZ(NE),EL(NE))) THEN
		IQ(NE)=2
	      ELSE
		IQ(NE)=0
	      ENDIF
	    ENDIF
	ENDDO
	END
*+CAL_INV_LIN  Perform inverse linearization using lin table
	SUBROUTINE CAL_INV_LIN(AZ,EL,QMAP,ZOOM,DX,DY,IQ)
	REAL AZ,EL,QMAP(65,65,2),DX,DY
	INTEGER IQ
	LOGICAL ZOOM
*AZ,EL	input	position in FOV radians
*QMAP	input	linearization table
*ZOOM	input	zoom mode yes or no?
*DX,DY	output	detected position range 0.0 to 512. in both axes
*IQ	output	quality, 0 if OK, 2 if masked
* Note if IQ not 0 then DX,DY returned as 0.0,0.0
*-Dick Willingale 1990-May-16
	PARAMETER (MAXP=50)
	INTEGER ISX,ISY,IXT(MAXP),IYT(MAXP)
	REAL CXP(MAXP),CYP(MAXP),CXQ(MAXP),CYQ(MAXP)
	SAVE ISX,ISY
	DATA ISX,ISY/33,33/
	LOGICAL VIRGIN,SEARCHING,CIN_HIT_MASK
C Check if hit masked area
	IF(CIN_HIT_MASK(QMAP,AZ,EL)) THEN
		IQ=2
		DX=0.0
		DY=0.0
C Next time start at centre
		ISX=33
		ISY=33
		RETURN
	ENDIF
C Find cell of table which contains point using repeated 3-point
C linear interpolation to estimate position with respect to the bottom
C left and top right vertices starting with centre cell.
C When close to correct position and interpolation fails to give correct
C cell move around present cell in spiral search.
	IX=ISX
	IY=ISY
	NPASS=0
	NSPIRAL=0
	SEARCHING=.TRUE.
	DO WHILE(SEARCHING)
C Check that using new cell
		VIRGIN=.TRUE.
		J=0
		DO WHILE(VIRGIN.AND.J.LT.NPASS)
			J=J+1
			VIRGIN=(IX.NE.IXT(J).OR.IY.NE.IYT(J))
		ENDDO
		IF(VIRGIN) THEN
C Extract cell points
			X1=QMAP(IX,IY,1)
			Y1=QMAP(IX,IY,2)
			X2=QMAP(IX+1,IY,1)
			Y2=QMAP(IX+1,IY,2)
			X3=QMAP(IX,IY+1,1)
			Y3=QMAP(IX,IY+1,2)
			X4=QMAP(IX+1,IY+1,1)
			Y4=QMAP(IX+1,IY+1,2)
C Construct differences
			XZ1=AZ-X1
			X21=X2-X1
			X31=X3-X1
			YL1=EL-Y1
			Y21=Y2-Y1
			Y31=Y3-Y1
			XZ4=AZ-X4
			X24=X2-X4
			X34=X3-X4
			YL4=EL-Y4
			Y24=Y2-Y4
			Y34=Y3-Y4
C Interpolate in cell with bottom left as apex
			DEN=X31*Y21-Y31*X21
			IF(DEN.NE.0.0) THEN
				XP=(X31*YL1-Y31*XZ1)/DEN
				YP=(Y21*XZ1-X21*YL1)/DEN
			ELSE
				XP=0.0
				YP=0.0
			ENDIF
C Interpolate in cell with top right as apex
			DEN=X34*Y24-Y34*X24
			IF(DEN.NE.0.0) THEN
				XQ=(Y24*XZ4-X24*YL4)/DEN
				YQ=(X34*YL4-Y34*XZ4)/DEN
			ELSE
				XQ=0.0
				YQ=0.0
			ENDIF
C Save results for this cell
			NPASS=NPASS+1
			IXT(NPASS)=IX
			IYT(NPASS)=IY
			CXP(NPASS)=XP
			CYP(NPASS)=YP
			CXQ(NPASS)=XQ
			CYQ(NPASS)=YQ
C Test if in cell
			IF(XP.GE.0.0.AND.YP.GE.0.0.AND.
     +			XQ.GE.0.0.AND.YQ.GE.0.0) THEN
				SEARCHING=.FALSE.
			ELSE
C Abandon search if hit maximum number of passes
				SEARCHING=NPASS.NE.MAXP
			ENDIF
		ELSEIF(NSPIRAL.EQ.0) THEN
C Start spiral search
			NSPIRAL=1
			ISX=IXT(NPASS)
			ISY=IYT(NPASS)
		ENDIF
		IF(SEARCHING) THEN
			IF(NSPIRAL.GT.0) THEN
C Get next cell on spiral
				CALL CIN_SPIRAL(ISX,ISY,NSPIRAL,IX,IY)
				SEARCHING=NSPIRAL.NE.50
				NSPIRAL=NSPIRAL+1
			ELSE
C Get next cell using interpolated values
				IX=REAL(IX)+CXP(NPASS)
				IY=REAL(IY)+CYP(NPASS)
			ENDIF
C Keep in bounds of table
			IX=MAX(MIN(IX,64),1)
			IY=MAX(MIN(IY,64),1)
		ENDIF
	ENDDO
	IF(NSPIRAL.GT.40) THEN
C Given up search look for closest cell scanned
		NCLOSE=0
		RCLOSE=0.0
		DO J=1,NPASS
			IF(NCLOSE.EQ.0) THEN
				NCLOSE=J
				RCLOSE=SQRT(CXP(J)**2+CYP(J)**2)
			ELSE
				RC=SQRT(CXP(J)**2+CYP(J)**2)
				IF(RC.LT.RCLOSE) THEN
					RCLOSE=RC
					NCLOSE=J
				ENDIF
			ENDIF
		ENDDO
		IX=IXT(NCLOSE)
		IY=IYT(NCLOSE)
C	DO J=1,NPASS
C		WRITE(*,*) IXT(J),IYT(J),CXP(J),CYP(J)
C	ENDDO
C	write(*,*) 'failed',IX,IY,RCLOSE
	ELSE
C Closest cell has been found
		NCLOSE=NPASS
	ENDIF
	IQ=0
C Save position for next call
	ISX=IX
	ISY=IY
C Estimate raw position in range 0.0 to 512.0
	DX=CXP(NCLOSE)+REAL(IX-1)
	DY=CYP(NCLOSE)+REAL(IY-1)
	IF(ZOOM) THEN
		DX=(DX-16.)*16.0
		DY=(DY-16.)*16.0
	ELSE
		DX=DX*8.0
		DY=DY*8.0
	ENDIF
	END
*+CAL_IRRF	Integrated radial response function of WFC
	FUNCTION CAL_IRRF(DMJD,IFILT,ENERGY,AZ,EL,RADIUS,ISTAT)
	DOUBLE PRECISION DMJD
	INTEGER IFILT,ISTAT
	REAL CAL_IRRF,ENERGY,AZ,EL,RADIUS
*DMJD	  input	  Epoch
*IFILT	  input	  filter number ( not used at present)
*ENERGY	  input	  photon energy eV
*AZ	  input	  azimuth of centroid in FOV radians
*EL	  input	  elevation of centroid in FOV radians
*RADIUS	  input	  radial offset from the centroid radians
*ISTAT    in/out  status return
*CAL_IRRF output  fraction (integrated probability) of the total flux
*                 from a point source at AZ,EL contained in a
*                 radius RADIUS from the centroid of the PSF.
*-
C
       PARAMETER (N=11,PI=3.141592654,RTOA=206064.8062)
       REAL TOTAL,SUM,R,DAREA,DSUM,DR,DRT,RT,DAREAT,SUMT,PIX
       INTEGER NMAX

	PARAMETER (NCMAX=10)
	CHARACTER*32 FUNC_NAME
	REAL COEFF(NCMAX)
	INTEGER NCOEFF
C
	IF(ISTAT.NE.0) RETURN
C Get coeffs from MCF
        CALL CIN_SET_PSF(DMJD,IFILT,
     +	ENERGY,AZ,EL,NCMAX,COEFF,NCOEFF,FUNC_NAME,ISTAT)

       TOTAL=0.0
       SUM=0.0

       RMAX=1000.0/RTOA
       PIX=38.0/RTOA
       DR=PIX/FLOAT(N)
       NMAX=INT(RMAX/DR)
       NR=INT(RADIUS/DR)
C
       DO J=1,NMAX
          R=(FLOAT(J)-0.5)*DR
          IF (J.GT.1) THEN
              DAREA=PI*DR**2*(FLOAT(J)**2-FLOAT(J-1)**2)
          ELSE IF (J.EQ.1) THEN
              DAREA=PI*DR**2*FLOAT(J)**2
          END IF
          DSUM=CIN_RRF(NCOEFF,COEFF,FUNC_NAME,AZ,EL,R,ISTAT)*DAREA
          TOTAL=TOTAL+DSUM
          IF (J.LT.NR) THEN
              SUM=SUM+DSUM
          ELSE IF (J.EQ.NR) THEN
              SUM=SUM+DSUM
C      Add on contribution from parts of increments.
              RI=FLOAT(J)*DR
              DRT=RADIUS-RI
              RT=RADIUS-DRT/2.0
              DAREAT=PI*(RADIUS**2-RI**2)
              SUMT=CIN_RRF(NCOEFF,COEFF,FUNC_NAME,AZ,EL,RT,ISTAT)*DAREAT
              SUM=SUM+SUMT
          ELSE IF (NR.EQ.0) THEN
C      Add on contribution from part of increments.
              RT=RADIUS/2.0
              DAREAT=PI*(RADIUS**2)
              SUMT=CIN_RRF(NCOEFF,COEFF,FUNC_NAME,AZ,EL,RT,ISTAT)*DAREAT
              SUM=SUM+SUMT
          END IF
       END DO
C
C      Find fractional area
C
          CAL_IRRF=SUM/TOTAL
       END
*+CAL_ONAA	On-axis effective area (cm**2) of WFC
	SUBROUTINE CAL_ONAA(DMJD,IFILT,NE,ENERGY,AREA,ISTAT)
	DOUBLE PRECISION DMJD
	INTEGER IFILT,ISTAT,NE
	REAL ENERGY(NE),AREA(NE)
*DMJD	input	date required
*IFILT	input	filter number
*NE	input	number of energies required
*ENERGY	input	array of energies (eV)
*AREA	output	array of on-axis effective areas (cm**2)
*ISTAT	in/out	returned status
*
* AES March 89
* Modified to handle detector internally Dick Willingale 1990-Apr-24
* Included detector efficiency changes as function of time 1992-Apr-27
*-
C
	IF(ISTAT.NE.0) RETURN
C Get detector in use
	CALL CIN_SET_DET(DMJD,IDET,ISTAT)
	DETEF=CIN_DETEFF(DMJD,IFILT,ISTAT)
C Loop for energies
	DO I=1,NE
		AREA(I)=CIN_AREA(DMJD,IDET,IFILT,ENERGY(I),ISTAT)*DETEF
	END DO
	END


*+CAL_PSF	Evaluate PSF probability density at given position
       FUNCTION CAL_PSF(DMJD,IFILT,ENERGY,AZ,EL,DAZ,DEL,ISTAT)
       DOUBLE PRECISION DMJD
       INTEGER ISTAT,IFILT
       REAL AZ,EL,DAZ,DEL,ENERGY
*DMJD	input	date required
*IFILT	input	filter number (not used at present)
*ENERGY	input	energy (eV)
*AZ	input	azimuth (radians)
*EL	input	elevation (radians)
*DAZ	input	delta AZ wrt AZ (radians)
*DEL	input	delta EL wrt EL (radians)
*ISTAT	in/out	returned status
* Returns probability per steradian
*
* MGW Sept 1988
* Modified by: AES Feb 1989
* Changed MCF interface Dick Willingale 1990-Apr-25
*-
	PARAMETER (NCMAX=10)
	CHARACTER*32 FUNC_NAME
	REAL COEFF(NCMAX)
	INTEGER NCOEFF
C
	IF(ISTAT.NE.0) RETURN
C Get coeffs from MCF
        CALL CIN_SET_PSF(DMJD,IFILT,
     +	ENERGY,AZ,EL,NCMAX,COEFF,NCOEFF,FUNC_NAME,ISTAT)
C Evaluate fucntion
	CAL_PSF=CIN_PSF(FUNC_NAME,NCOEFF,COEFF,AZ,EL,DAZ,DEL,ISTAT)
C
	IF(ISTAT.NE.0) THEN
		WRITE(*,*) '   error in CAL_PSF'
	ENDIF
	END
*+CAL_PSF2D	PSF prob. density at a set of regular grid positions
       SUBROUTINE CAL_PSF2D(DMJD,IFILT,ENERGY,AZ,DELTA_AZ,N_AZ,
     &   EL,DELTA_EL,N_EL,OAZ,OEL,ARRAY,ISTAT)
       DOUBLE PRECISION DMJD
       INTEGER IFILT,ISTAT,N_AZ,N_EL
       REAL AZ,EL,DELTA_AZ,DELTA_EL,ENERGY,ARRAY(N_AZ,N_EL),OAZ,OEL
*DMJD	    input	date required
*IFILT	    input	filter number (not used at present)
*ENERGY	    input	energy (eV)
*AZ	    input	azimuth of PSF centre (radians)
*DELTA_AZ   input       grid spacing in azimuth (radians)
*N_AZ       input       number of grid positions in azimuth
*EL	    input	elevation of PSF centre (radians)
*DELTA_EL   input       grid spacing in elevation (radians)
*N_EL       input       number of grid positions in elevation
*OAZ        input       offset of PSF centre from grid centre in az (rads)
*OEL        input       offset of PSF centre from grid centre in el (rads)
*ARRAY      output      array of psf values
*ISTAT	    in/out	status return
* Returns probability per steradian
*
* AES June 1989
* Modified data access from MCF Dick Willingale 1990-Apr-25
*-
	PARAMETER (NCMAX=10)
	CHARACTER*32 FUNC_NAME
	REAL COEFF(NCMAX)
	INTEGER NCOEFF
C
	IF(ISTAT.NE.0) RETURN
C Get data from MCF
        CALL CIN_SET_PSF(DMJD,IFILT,
     +	ENERGY,AZ,EL,NCMAX,COEFF,NCOEFF,FUNC_NAME,ISTAT)
C Evalutate function over grid
        DO I=1,N_EL
		DEL=(REAL(I)-1.0-(REAL(N_EL)-1.0)/2.0)*DELTA_EL-OEL
                DO J=1,N_AZ
                 DAZ=(REAL(J)-1.0-(REAL(N_AZ)-1.0)/2.0)*DELTA_AZ-OAZ
		 ARRAY(J,I)=CIN_PSF(FUNC_NAME,NCOEFF,COEFF,AZ,EL,DAZ,DEL,ISTAT)
                END DO
        END DO
	IF(ISTAT.NE.0) THEN
		WRITE(*,*) '   error in CAL_PSF2D'
	ENDIF
	END
*+CAL_PSFT	Fraction of Point spread function of WFC, in a box
        FUNCTION CAL_PSFT(DMJD,IFILT,ENERGY,AZ,EL,
     &   DAZL,DAZH,DELL,DELH,ISTAT)

	DOUBLE PRECISION DMJD
	INTEGER IFILT,ISTAT
	REAL AZ,EL,DAZL,DAZH,DELL,DELH,ENERGY
*DMJD	 input	 MJD of required calibration
*IFILT	 input	 filter number (not used at present)
*ENERGY	 input	 energy (eV)
*AZ	 input	 azimuth of centroid in FOV radians
*EL	 input	 elevation of centroid in FOV radians
*DAZL    input   azimuth offset giving lower limit for area (rads.)
*DAZH    input   azimuth offset giving upper limit for area (rads.)
*DELL    input   elavation offset giving lower limit for area (rads.)
*DELH    input   elavation offset giving upper limit for area (rads.)
*ISTAT   in/out	 status return
*CAL_PSFT out	 Fraction (integrated probability) of the PSF within
*                an area at offsets DAZL,DAZH,DELL,DELH from the
*                centroid when the point source is at position AZ,EL
* AES July 1989
* Modified status handling and detector/filter Dick Willingale 1990-Apr-25
*-
       PARAMETER (N=11,RTOA=206264.8062,PIX=36.0/RTOA)
       REAL D,AREA,T,TX,TXS,TXYS,TXD,TYD,X,Y,PARTX,PARTY,PX,PY
       INTEGER NX,NY,IX,IY
C
	IF(ISTAT.NE.0) RETURN
C
C      Define some values.
C
       D=PIX/FLOAT(N)
       DDAZ=DAZH-DAZL
       DDEL=DELH-DELL
C Check that DAZH>DAZL and DELH>DELL. Inform user and exit if not.
       IF (DAZH.LE.DAZL.OR.DELH.LE.DELL) THEN
          WRITE (*,*) ' CAL_PSFT error: Corners invalid'
          WRITE (*,*) '   (DAZH must be > DAZL, DELH must be > DELL)'
          RETURN
       END IF
       NX=INT(DDAZ/D)
       NY=INT(DDEL/D)
       TXS=0.0
       TXYS=0.0
       PARTX=DDAZ/D-FLOAT(NX)
       PARTY=DDEL/D-FLOAT(NY)
C
C      Integrate over specified area in y then x directions.
C
       IF (NX.EQ.0) GOTO 201
       DO IX=1,NX
          TYS=0.0
          X=DAZL+(FLOAT(IX)-0.5)*D
          IF (NY.EQ.0) GOTO 200
          DO IY=1,NY
             Y=DELL+(FLOAT(IY)-0.5)*D
             T=CAL_PSF(DMJD,IFILT,ENERGY,AZ,EL,X,Y,ISTAT)
             TY=T*D**2
             TYS=TYS+TY
          END DO
  200     CONTINUE
C         Add on end part of increment in y
          PY=DELL+(FLOAT(NY)+PARTY/2.0)*D
          T=CAL_PSF(DMJD,IFILT,ENERGY,AZ,EL,X,PY,ISTAT)
          TYD=T*PARTY*D**2
          TXYS=TXYS+TYS+TYD
       END DO
  201  CONTINUE
C deal with last increment in x column
       TYS=0.0
       X=DAZL+(FLOAT(NX)+PARTX/2.0)*D
       IF (NY.EQ.0) GOTO 202
       DO IY=1,NY
          Y=DELL+(FLOAT(IY)-0.5)*D
          T=CAL_PSF(DMJD,IFILT,ENERGY,AZ,EL,X,Y,ISTAT)
          TY=T*PARTX*D**2
          TYS=TYS+TY
       END DO
  202  CONTINUE
C      Add on end part of increment in y, for last x increment
       PY=DELL+(FLOAT(NY)+PARTY/2.0)*D
       T=CAL_PSF(DMJD,IFILT,ENERGY,AZ,EL,X,PY,ISTAT)
       TYD=T*PARTX*PARTY*D**2
       TXYS=TXYS+TYS+TYD

       CAL_PSFT=TXYS
C
	IF(ISTAT.NE.0) THEN
		WRITE(*,*) '   error in CAL_PSFT'
	ENDIF
       END
*+CAL_PSFT2D	integrated PSF over a set of regular rectangular bins
       SUBROUTINE CAL_PSFT2D(DMJD,IFILT,ENERGY,AZ,DELTA_AZ,N_AZ,
     &   EL,DELTA_EL,N_EL,OAZ,OEL,ARRAY,ISTAT)

       DOUBLE PRECISION DMJD
       INTEGER IFILT,ISTAT,N_AZ,N_EL
       REAL AZ,EL,DELTA_AZ,DELTA_EL,ENERGY,ARRAY(N_AZ,N_EL),OAZ,OEL
*DMJD	    input	date required
*IFILT	    input	filter number (not used at present)
*ENERGY	    input	energy (eV)
*AZ	    input	azimuth of PSF centre (radians)
*DELTA_AZ   input       grid spacing in azimuth (radians)
*N_AZ       input       number of grid positions in azimuth
*EL	    input	elevation of PSF centre (radians)
*DELTA_EL   input       grid spacing in elevation (radians)
*N_EL       input       number of grid positions in elevation
*OAZ        input       offset of PSF centre from grid centre in az (rads)
*OEL        input       offset of PSF centre from grid centre in el (rads)
*ARRAY      output      array of psf values
*ISTAT	    in/out	status return
*
* AES June 1989
* Modified interface to MCF Dick Willingale 1990-Apr-25
* Modified to use dynamic choice of integration step Dick Willingale 1990-May-22
*-
	PARAMETER (NCMAX=10)
	CHARACTER*32 FUNC_NAME
	REAL COEFF(NCMAX),CIN_PSF
	INTEGER NCOEFF
C
	IF(ISTAT.NE.0) RETURN
C Get data from MCF
        CALL CIN_SET_PSF(DMJD,IFILT,
     +	ENERGY,AZ,EL,NCMAX,COEFF,NCOEFF,FUNC_NAME,ISTAT)
C
C Integration step within samples that overlap the central peak
C order 5 arc seconds
	RPEAK=0.5*SQRT(DELTA_AZ**2+DELTA_EL**2)+6.E-4
	PEAKSTEP=2.42E-5
C Integration step in wings order 1 arc minute
	WINGSTEP=2.91E-4
        DO I=1,N_EL
             DEL=(REAL(I)-1.0-(REAL(N_EL)-1.0)/2.0)*DELTA_EL-OEL
             DO J=1,N_AZ
                      DAZ=(REAL(J)-1.-(REAL(N_AZ)-1.)/2.)*DELTA_AZ-OAZ
C
C      Define some values.
C
			R=SQRT(DAZ**2+DEL**2)
			IF(R.GT.RPEAK) THEN
				NX=DELTA_AZ/WINGSTEP
				NY=DELTA_EL/WINGSTEP
			ELSE
				NX=DELTA_AZ/PEAKSTEP
				NY=DELTA_EL/PEAKSTEP
			ENDIF
			NX=MAX(NX,1)
			NY=MAX(NY,1)
			DX=DELTA_AZ/NX
			DY=DELTA_EL/NY
                      DAZL=DAZ-0.5*DELTA_AZ
                      DELL=DEL-0.5*DELTA_EL
                      TXYS=0.0
C
C      Integrate over specified area in y then x directions.
C
                      DO IX=1,NX
                         X=DAZL+(FLOAT(IX)-0.5)*DX
                         DO IY=1,NY
                            Y=DELL+(FLOAT(IY)-0.5)*DY
			    T=CIN_PSF(FUNC_NAME,NCOEFF,COEFF,AZ,EL,X,Y,ISTAT)
                            TXYS=TXYS+T*DX*DY
                         END DO
                      END DO
C Take the absolute value of the probability
		      ARRAY(J,I) = ABS(TXYS)
              END DO
        END DO
C
	IF(ISTAT.NE.0) THEN
		WRITE(*,*) '   error in CAL_PSFT2D'
	ENDIF
	END
*+CAL_PSFT2D_SUR	Survey integrated PSF over a set of regular bins
*                       in polar coordinates around the fov centre.
*     N.B. The algorithm currently uses a simple vignetting function.
      SUBROUTINE CAL_PSFT2D_SUR(DMJD,IFILT,ENERGY,ZOOM,RMIN,RMAX,
     &   DELTA_AZ,N_AZ,DELTA_EL,N_EL,OAZ,OEL,ARRAY,ISTAT)
      DOUBLE PRECISION DMJD
      INTEGER IFILT,ISTAT,N_AZ,N_EL
      REAL RMIN,RMAX
      REAL DELTA_AZ,DELTA_EL,ENERGY,ARRAY(N_AZ,N_EL),OAZ,OEL
      LOGICAL ZOOM
*DMJD	    input	date required
*IFILT	    input	filter number (not used at present)
*ENERGY	    input	energy (eV)
*ZOOM       input       zoom mode (expected to be .FALSE. during survey)
*RMIN       input	minimum radius in FOV radians
*RMAX       input	maximum radius in FOV radians
*DELTA_AZ   input       grid spacing in azimuth (radians)
*N_AZ       input       number of grid positions in azimuth
*DELTA_EL   input       grid spacing in elevation (radians)
*N_EL       input       number of grid positions in elevation
*OAZ        input       offset of PSF centre from grid centre in az (rads)
*OEL        input       offset of PSF centre from grid centre in el (rads)
*ARRAY      output      array of survey PSF values
*ISTAT	    in/out	status return
*
* Anne Sansom 1990-Apr-25
* Last modified 24th May 1990 (bug correction)
*-
      PARAMETER (DTOR=0.0174532, PI=3.1415927)
      PARAMETER (MAXPIX=101, MAXZONE=10, MAXANGLE=10)
*
* MAXPIX = max. allowed number of pixels in each dimension
* MAXZONE = max. allowed number of annular zones in instrument fov
* MAXANGLE = max. allowed number of angles round a semi-circle.
*
      REAL PSUM(MAXPIX,MAXPIX), RR(MAXZONE), AFACTOR(MAXZONE)
*
	IF(ISTAT.NE.0) RETURN
*
*
* Set default values for control parameters, etc. :
*
      RFOV=2.5*DTOR
* Fudge: Iris the field sampled if in zoom mode.
      IF (ZOOM) RMAX=1.4*DTOR
      NZONE = 5
      NANGLE = 5
* Define angular step size for sampling round a hemisphere
      DA = PI / REAL(NANGLE)
* Define radial step size in instrument FOV
      DR = RFOV / REAL(NZONE)
* Set up start first and last radial zone to include
      IZONE1=NZONE*(RMIN/RFOV)+1
      IZONE2=NZONE*(RMAX/RFOV)
*
* Evaluate relative weighting factor due to vignetting and area of annulus.
      DO L = IZONE1, IZONE2
	R2 = DR * REAL(L)
	R1 = R2 - DR
        SQ=R1*R1 + R2*R2
	IF( L .EQ. 1) THEN	! force first image to be on-axis case
	   RR(L) = 0.0
	ELSE
	   RR(L) = SQRT(SQ/2.0)
	END IF
* NB Vign TB relaced by a call. to the MCF
*       VIG =  CAL_TVIGN(DMJD,IFILT,ZOOM,ENERGY,AZ,EL,ISTAT)
	VIG = 1.0 - 0.132*RR(L)	! vignetting factor; 67% at 2.5 deg.
        AA=(R1*R1-R2*R2)
	AFACTOR(L) = VIG * AA   ! proportional to vig_fr * annulus_area
      END DO
* Find sum of weights for normalization.
      SFACTOR = 0.0
      DO L = IZONE1, IZONE2
         SFACTOR = SFACTOR + AFACTOR(L)
      END DO
*
* Step round in angle to obtain contributions to the survey PSF array
*
      DO K = 1 , NANGLE
         ANGLE= (REAL(K)-1.0) * DA
         CA = COS(ANGLE)
         SA = SIN(ANGLE)
*
* Accumulate a radially binned PSF array at each angle
* Off-axis angles are: 0.0 to 2.5 in steps of DR radians.
*
         DO L = IZONE1, IZONE2
*
* Evaluate detector coordinates for PSF centre
	    AZ = -RR(L) * CA
	    EL = RR(L) * SA
	    CALL CAL_PSFT2D( DMJD, IFILT, ENERGY, AZ, DELTA_AZ, N_AZ,
     &     EL, DELTA_EL, N_EL, OAZ, OEL, ARRAY, ISTAT)
*
*
            IF (L .EQ. IZONE1 .AND. K .EQ. 1) THEN
                DO J = 1, N_EL
	           DO I = 1, N_AZ
		      PSUM(I,J) = ARRAY(I,J)*AFACTOR(L)
	           END DO
                END DO
            ELSE
                DO J = 1, N_EL
	           DO I = 1, N_AZ
		      PSUM(I,J) = PSUM(I,J) + ARRAY(I,J)*AFACTOR(L)
	           END DO
                END DO
            END IF
          END DO
        END DO
*
* Overall normalization
        ANORM=REAL(NANGLE)*SFACTOR
* Renormalize PSF
        DO J = 1, N_EL
	   DO I = 1, N_AZ
	      ARRAY(I,J) = PSUM(I,J)/ANORM
	   END DO
        END DO
      END
*+CAL_REPORT  Tell the caller what version we are
	SUBROUTINE CAL_REPORT (ISTAT)
* R. Willingale/ M. Denby
* May-14-90
*-
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC)	LOC
	CHARACTER*20	ISSUE
	INTEGER		VERSION
	INTEGER		INTERFACE

	IF (ISTAT .NE. 0) RETURN

	CALL CIN_INIT (LOC, ISTAT)

C Get issue date and version and log
	CALL CMP_GET0C(LOC,'ISSUE_DATE',ISSUE,ISTAT)
	CALL CMP_GET0I(LOC,'VERSION',VERSION,ISTAT)
	CALL CMP_GET0I(LOC,'INTERFACE',INTERFACE,ISTAT)
	IF (ISTAT .NE. 0) RETURN

	WRITE(*,*) '   MCF version:',VERSION,'      issued:'//ISSUE
	WRITE(*,*) '   MCF interface version:',INTERFACE

	END
*+CAL_RRF	Radial response function for WFC
	SUBROUTINE CAL_RRF(DMJD,IFILT,ENERGY,AZ,EL,NRAD,RADII,ARRAY,ISTAT)
	DOUBLE PRECISION DMJD
	INTEGER IFILT,NRAD,ISTAT
	REAL ENERGY,AZ,EL,RADII(NRAD),ARRAY(NRAD)
*DMJD	input	date
*IFILT	input	filter number (not used at present)
*ENERGY	input	photon energy eV
*AZ	input	azimuth in field of view radians
*EL	input	elevation in field of view radians
*NRAD	input	number of radial samples
*RADII	input	radial positions radians
*ARRAY	output	average probability density of PSF at radii
*ISTAT	in/out	returned status
*-Author Dick Willingale 1990-Apr-5
	PARAMETER (NCMAX=10)
	CHARACTER*32 FUNC_NAME
	REAL COEFF(NCMAX)
	INTEGER NCOEFF
C
	IF(ISTAT.NE.0) RETURN
C Get coeffs from MCF
        CALL CIN_SET_PSF(DMJD,IFILT,
     +	ENERGY,AZ,EL,NCMAX,COEFF,NCOEFF,FUNC_NAME,ISTAT)
C Generate samples
	DO J=1,NRAD
		ARRAY(J)=CIN_RRF(NCOEFF,COEFF,FUNC_NAME,AZ,EL,RADII(J),ISTAT)
	ENDDO
	END
*+CAL_SET_LIN Read the Linearisation from the CAL file
	SUBROUTINE CAL_SET_LIN (MJDIN, FILTER, ZOOM, DET, LRR, STATUS)
* Input
	REAL*8		MJDIN		  ! Requested epoch
	INTEGER		FILTER		  ! Requested filter (0 to 8)
	LOGICAL		ZOOM		  ! ZOOM or not
* Output
	INTEGER		DET		  ! Detector # (1 or 2)
	REAL		LRR (65,65,2)     ! Linearisation matrix
	INTEGER		STATUS		  ! Status (0 = OK)
* M. Denby 28-Feb-89
* Modified to use new mask handling Dick Willingale 1990-Apr-20
* Modified to use new epoch window handling Dick Willingale 1990-Apr-23
* Removed masking of last col/row Dick Willingale 1990-July-25
* Major modification of masking Dick Willingale 1990-Sep-3
* At present ZOOM is inactive RW 1990-Sep-3
* If FILTER=0 then no masking within active area of detector
*-
* Local
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC)	LOC		  ! Locator to top level CAL
	CHARACTER*(DAT__SZLOC)	LOCLT		  ! Locator to Lin tables
	CHARACTER*(DAT__SZLOC)	LOCLIN		  ! Locator to LIN structure
	CHARACTER*(DAT__SZLOC)	LOCDR		  ! Locator to DATA RECORD
	CHARACTER*(DAT__SZLOC)	LOCTMP		  ! Temp locator
	INTEGER		NELS(3)		  ! Shape of Lin matrix
	REAL*8		MJDLO,MJDHI	  ! Last epoch window
	LOGICAL		CIN_VALID
	INTEGER 	FNOW		  ! Last filter
	CHARACTER*(DAT__SZLOC) LOC1,LOC2,LOC3,LOC4,LOC5
	SAVE MJDLO,MJDHI,FNOW
	DATA NELS /65, 65, 2/
	DATA FNOW/-1/
* NOTE that the same lin table is used for both unzoomed and zoomed
* data (but used to give more accurate interpolation for zoomed).
	IF (STATUS .NE. 0) RETURN
* Check if table already set
	IF(.NOT.CIN_VALID(MJDIN,MJDLO,MJDHI,FILTER,FNOW)) THEN
C Save filter number
		FNOW=FILTER
* Get locator to MCF
		CALL CIN_INIT(LOC,STATUS)
* Find the Linearisation tables in the Cal master file
		CALL DAT_FIND (LOC, 'LIN_TABLES', LOCLT, STATUS)
* Find locator to LIN structure
		CALL DAT_FIND (LOCLT, 'LIN', LOCTMP, STATUS)
* Get detector in use
		CALL CIN_SET_DET(MJDIN,DET,STATUS)
* Access the element of LIN corresponding to DET
		CALL DAT_CELL (LOCTMP, 1, DET, LOCLIN, STATUS)
* Get epoch window
		CALL CIN_FEPOCH(MJDIN,LOCLIN,NE,MJDLO,MJDHI,STATUS)
* Find the DATA_RECORD structure keyed by epoch
		CALL DAT_FIND (LOCLIN, 'DATA_RECORD', LOCDR,  STATUS)
		CALL DAT_CELL (LOCDR,  1, NE, LOCTMP, STATUS)
* Read the linearisation matrix
		CALL CMP_GETNR(LOCTMP, 'DATA_ARRAY', 3, NELS,LRR,NELS,STATUS)
* Get radius of edge
		CALL CMP_GET0R(LOCTMP,'REDGE',REDGE,STATUS)
C Hide edge radius in table
C If FILTER=0 then set radius beyond physical edge (2.6 degrees used)
		IF(FILTER.GT.0) THEN
			LRR(1,1,1)=REDGE**2
		ELSE
			RMAX=4.53786E-2
			LRR(1,1,1)=RMAX**2
		ENDIF
C Clear up
		CALL DAT_ANNUL(LOCLT,STATUS)
		CALL DAT_ANNUL(LOCLIN,STATUS)
		CALL DAT_ANNUL(LOCDR,STATUS)
		CALL DAT_ANNUL(LOCTMP,STATUS)
	ENDIF
C Report bad status
	IF (STATUS .NE. 0) THEN
	  WRITE(*,*) '   Error in CAL_SET_LIN'
	ENDIF
	END
*+CAL_TVIGN Calculate total vignetting of WFC
       FUNCTION CAL_TVIGN(DMJD,IFILT,ZOOM,ENERGY,AZ,EL,ISTAT)
       DOUBLE PRECISION DMJD
       INTEGER IFILT,ISTAT
       LOGICAL ZOOM
       REAL AZ,EL,ENERGY
*DMJD	input	date required
*IFILT	input	filter required
*ZOOM	input	zoom yes or no?
*ENERGY	input	photon energy eV
*AZ	input	azimuth (radians)
*EL	input	elevation (radians)
*ISTAT	output	status return
* AES Jan - 1989
* Modified to improve efficiency Dick Willingale 1990-Apr-20
* Modified to use zoom flag and energy Dick Willingale 1990-Apr-23
*-
	PARAMETER (NXM=31,NYM=31)
	REAL V(NXM,NYM),AX(NXM),AY(NYM)
	INTEGER NX,NY
        SAVE V,AX,AY,NX,NY
	REAL CALTVIGN
	REAL QMAP(65,65,2)
	SAVE QMAP
	LOGICAL CIN_HIT_MASK
C
	IF(ISTAT.NE.0) RETURN
C Get linearization matrix
	CALL CAL_SET_LIN(DMJD,IFILT,ZOOM,IDET,QMAP,ISTAT)
C Check mask
	IF(CIN_HIT_MASK(QMAP,AZ,EL)) THEN
		CAL_TVIGN=0.0
	ELSE
C Get vignetting table
		CALL CIN_SET_TVIGN(DMJD,IDET,IFILT,ENERGY,
     +		NXM,NYM,V,AX,AY,NX,NY,ISTAT)
C Interpolate
		CALL CIN_INTER_2DF(NXM,NX,NY,V,AX,AY,AZ,EL,CALTVIGN,ISTAT)
	        CAL_TVIGN = CALTVIGN
	ENDIF
	IF(ISTAT.NE.0) THEN
		WRITE(*,*) '   error in CAL_TVIGN'
	ENDIF
	END
*+CAL_TVIGS	Sampled total vignetting for WFC
	SUBROUTINE CAL_TVIGS(DMJD,IFIL,ZOOM,ENERGY,AZ,DELAZ,NAZ,
     +	EL,DELEL,NEL,ARRAY,ISTAT)
	DOUBLE PRECISION DMJD
	INTEGER IFIL,NAZ,NEL,ISTAT
	LOGICAL ZOOM
	REAL ENERGY,AZ,DELAZ,EL,DELEL,ARRAY(NAZ,NEL)
*DMJD	input	date
*IFIL	input	WFC filter number 1 to 8
*ZOOM	input	detector zoom yes or no?
*ENERGY	input	photon energy eV
*AZ	input	azimuth of centre of grid radians
*DELAZ	input	sample increment in azimuth radians
*NAZ	input	number of azimuth samples across grid
*EL	input	elevation in centre of grid radians
*DELEL	input	sample increment in elevation radians
*NEL	input	number of elevation samples up grid
*ARRAY	output	grid of vignetting values
*ISTAT	in/out	returned status
*-Author Dick Willingale 1990-Apr-5
	PARAMETER (NXM=31,NYM=31)
	REAL V(NXM,NYM),AX(NXM),AY(NYM)
	INTEGER NX,NY
        SAVE V,AX,AY,NX,NY
	REAL CALTVIGN
	REAL QMAP(65,65,2)
	SAVE QMAP
	LOGICAL CIN_HIT_MASK
C
	IF(ISTAT.NE.0) RETURN
C Get linearization matrix
	CALL CAL_SET_LIN(DMJD,IFIL,ZOOM,IDET,QMAP,ISTAT)
C Get vignetting table
	CALL CIN_SET_TVIGN(DMJD,IDET,IFIL,ENERGY,
     +	NXM,NYM,V,AX,AY,NX,NY,ISTAT)
C
	ABASE=AZ-REAL(NAZ-1)*0.5*DELAZ
	EBASE=EL-REAL(NEL-1)*0.5*DELEL
	DO J=1,NEL
		ELEV=EBASE+REAL(J-1)*DELEL
		DO K=1,NAZ
			AZIM=ABASE+REAL(K-1)*DELAZ
C Check mask
			IF(CIN_HIT_MASK(QMAP,AZIM,ELEV)) THEN
				ARRAY(K,J)=0.0
			ELSE
C Interpolate
				CALL CIN_INTER_2DF(NXM,NX,NY,V,AX,AY,AZIM,
     +				ELEV,CALTVIGN,ISTAT)
				ARRAY(K,J)=CALTVIGN
			ENDIF
		ENDDO
	ENDDO
	END
*+CIN_ALIGN_DATE Gets alignment set for the given date, either set or interp.
      SUBROUTINE CIN_ALIGN_DATE(AREC,ALIGNMENT,REQUEST_MJD,REL_DATE)

*  Calling Arguments
      REAL*8 REQUEST_MJD
      CHARACTER*20 REL_DATE

      INCLUDE 'CALLIB(CIN_ALIGN_LOW)'
      RECORD /ALIGN_REC/ AREC(2),ALIGNMENT
* Original M.J. Ricketts
* Modified for CALLIB M. Denby
* Modified to fix valid time error MJR 30-Jun-90
* Revised version placed in library, RW 4-Jul-90
*-
*  Local Variables
      REAL DVALID,DAYS_VALID
      REAL*8 DIFMJD

      IF (REL_DATE.NE.'NONE') THEN
       IF (REL_DATE.EQ.'BETWEEN') THEN
         FRAC = (REQUEST_MJD - AREC(1).MJD)/(AREC(2).MJD-AREC(1).MJD)

         DO I=1,3
            ALIGNMENT.EULER(I)=(1.-FRAC)*AREC(1).EULER(I)+
     &                                  FRAC * AREC(2).EULER(I)
         END DO
         DO I=1,3
            VAR1 = AREC(1).ERROR(I)
            VAR1 = VAR1 * VAR1
            VAR2 = AREC(2).ERROR(I)
            VAR2 = VAR2 * VAR2
            ALIGNMENT.ERROR(I) = SQRT( (1.-FRAC)*VAR1 + FRAC*VAR2 )
         END DO
         ALIGNMENT.CAL_TYPE = 'INTERPOLATED'

*     Find how long it is valid for
         ALIGNMENT.MJD = MIN(REQUEST_MJD + 1.0D0, AREC(2).MJD)

       ELSE
         DO I=1,3
            ALIGNMENT.EULER(I) = AREC(1).EULER(I)
            ALIGNMENT.ERROR(I) = AREC(1).ERROR(I)
         END DO
         ALIGNMENT.CAL_TYPE = REL_DATE//' '//AREC(1).CAL_TYPE
         ALIGNMENT.MJD = 0.0D0

       END IF
      ELSE
       ALIGNMENT.CAL_TYPE = 'NONE'
      END IF

      END
*+CIN_ALIGN_GETSET Gets an alignment set in the master Cal File
      SUBROUTINE CIN_ALIGN_GETSET(ALIGN,CELL,STATUS)

      INCLUDE 'CALLIB(CIN_ALIGN_LOW)'

      RECORD/ALIGN_REC/ ALIGN
        INCLUDE 'DAT_PAR'
      CHARACTER*(DAT__SZLOC) CELL
      INTEGER STATUS
* Original M.J. Ricketts
* Modified for CALLIB M. Denby
*-
*  Local Variables
      INTEGER NEUL, NERR

*  Executable Code

      IF (STATUS.NE.0) RETURN

      CALL CMP_GET0D(CELL,'DATE_MJD',ALIGN.MJD,STATUS)

*  Euler angles, errors
      CALL CMP_GET1R(CELL,'EULER',3,ALIGN.EULER,NEUL,STATUS)
      CALL CMP_GET1R(CELL,'ERROR',3,ALIGN.ERROR,NERR,STATUS)
      IF (NEUL.NE.3.OR.NERR.NE.3) THEN
         STATUS=-33
	 RETURN
      END IF

*  Calibration Type
      CALL CMP_GET0C(CELL,'CAL_TYPE',ALIGN.CAL_TYPE,STATUS)

      END
*+CIN_AREA evaluate efficiency at centre of the field of view
	FUNCTION CIN_AREA(DMJD,IDET,IFILT,ENERGY,ISTAT)
	DOUBLE PRECISION DMJD
	INTEGER IDET,IFILT,ISTAT
	REAL ENERGY
*DMJD	input	epoch
*IDET	input	detector
*IFILT  input   filter required
*ENERGY	input	energy (eV)
*ISTAT	input	status return
*
* AES March-89
* Modified alot Dick Willingale 1990-Apr-24
*-
	PARAMETER (NENMAX=1000)
	REAL TA(NENMAX),EN(NENMAX)
	SAVE TA,EN
	INTEGER FNOW,DNOW,NE,IELS(2)
	SAVE FNOW,DNOW,NE
	DOUBLE PRECISION MJDLO,MJDHI
	LOGICAL CIN_VALID
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC) LOC,LOCA,LOCB,LOCC,LOCD,LOCE
C
	IF(ISTAT.NE.0) RETURN
C Check if we need to access new data from MCF
	IF(.NOT.(CIN_VALID(DMJD,MJDLO,MJDHI,IFILT,FNOW).AND.IDET.EQ.DNOW)) THEN
		DNOW=IDET
		FNOW=IFILT
C Get locator to MC
		CALL CIN_INIT(LOC,ISTAT)
C Get locator to area structure
		CALL DAT_FIND(LOC,'ON_AXIS_AREA',LOCA,ISTAT)
C Get locator to required structure
		CALL DAT_FIND(LOCA,'AREA',LOCB,ISTAT)
		IELS(1)=IFILT
		IELS(2)=IDET
		CALL DAT_CELL(LOCB,2,IELS,LOCC,ISTAT)
C Find epoch window
		CALL CIN_FEPOCH(DMJD,LOCC,IW,MJDLO,MJDHI,ISTAT)
C Get data record for epoch
		CALL DAT_FIND(LOCC,'DATA_RECORD',LOCD,ISTAT)
		CALL DAT_CELL(LOCD,1,IW,LOCE,ISTAT)
C Get data arrays
		CALL CMP_GET1R(LOCE,'AXIS_ARRAY',NENMAX,EN,NE,ISTAT)
		CALL CMP_GET1R(LOCE,'DATA_ARRAY',NENMAX,TA,NE,ISTAT)
C Clear locators
		CALL DAT_ANNUL(LOCA,ISTAT)
		CALL DAT_ANNUL(LOCB,ISTAT)
		CALL DAT_ANNUL(LOCC,ISTAT)
		CALL DAT_ANNUL(LOCD,ISTAT)
		CALL DAT_ANNUL(LOCE,ISTAT)
C Take the log of the filter transmissions and energy axis values for
C input to linear interpolation routine
		DO K=1,NE
			IF (TA(K).LE.0.0) THEN
				TA(K)=-100.0
			ELSE
				TA(K)=LOG(TA(K))
			END IF
			EN(K)=LOG(EN(K))
		END DO
	ENDIF
C Check buffer
	IF(NE.GT.NENMAX) THEN
		WRITE(*,*) '   error: buffer overflow in CIN_AREA'
		ISTAT=1
	ENDIF
C Take log of input energy
	IF (ENERGY.LE.0.0) THEN
            EL=LOG(0.0001)
        ELSE
            EL=LOG(ENERGY)
        END IF
C Interpolate
	CALL CIN_INTER_1(TA,NE,EN,EL,CALTRANL,ISTAT)
C Take exponential
	CIN_AREA=EXP(CALTRANL)
C Check status
	IF(ISTAT.NE.0) THEN
	    WRITE(*,*) '   error in CIN_AREA'
	ENDIF
	END
*+CIN_CEMDIRECT Generate CEM c/s from HK counter and CAL params
	REAL FUNCTION CIN_CEMDIRECT(PCN3)
	INTEGER		PCN3
* M. Denby 10 OCT 89
*-
	REAL		CEM(256), GEIG(256)
	COMMON /CAL_BMD/ GTA, GTB, GTVP, CEMA, CEMB, CEMVP, CEM, GEIG
	SAVE CAL_BMD

* Use the look up table to generate CEM c/s
	PCN3 = MIN(PCN3,256)
	PCN3 = MAX(PCN3,1)

	CIN_CEMDIRECT = CEM(PCN3)

	END
*+CIN_DETEFF	Detector efficiency with time
	REAL FUNCTION CIN_DETEFF(MJD,IFILT,ISTAT)
	DOUBLE PRECISION MJD
	INTEGER IFILT,ISTAT
*MJD	input	time required
*IFILT	input	filter number
*ISTAT	in/out	returned status
*-Author Dick Willingale 1991-Nov-14
	LOGICAL FIRSTCALL
	DATA FIRSTCALL/.TRUE./
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC) LOC,LOCD,LOCT,LOCE
	INTEGER IPTT,IPTE
C
	IF(ISTAT.NE.0) RETURN
C Get data on first call
	IF(FIRSTCALL) THEN
		CALL CIN_INIT(LOC,ISTAT)
		CALL DAT_FIND(LOC,'DETECTOR',LOCD,ISTAT)
		CALL DAT_FIND(LOCD,'EFFMJD',LOCT,ISTAT)
		CALL DAT_FIND(LOCD,'EFFICIENCY',LOCE,ISTAT)
		CALL DAT_MAPV(LOCT,'_REAL','READ',IPTT,NT,ISTAT)
		CALL DAT_MAPV(LOCE,'_REAL','READ',IPTE,NE,ISTAT)
		FIRSTCALL=.FALSE.
	ENDIF
C Interpolate
	CALL CIN_INTEFF(NT,%VAL(IPTT),%VAL(IPTE),MJD,IFILT,EFF,ISTAT)
	CIN_DETEFF=EFF
	END


*+CIN_E2KING2  computes WFC PSF using supplied coefficients
       FUNCTION CIN_E2KING2(COEFF,AZ,EL,DAZ,DEL)

       REAL COEFF(7)
       REAL AZ,EL,DAZ,DEL,Q,IN

       PARAMETER (PI=3.1415927)
*
*COEFF   Input   Coefficients describing the PSF at AZ,EL (radians)
*AZ      Input   Azimuthal position of PSF centre wrt optical axis
*EL      Input   Elevational position of PSF centre wrt optical axis
*DAZ     Input   Offset azimuthal position wrt PSF centre
*DEL     Input   Offset elevational position wrt PSF centre
*
* NB. Assumes coefficients for half widths are in radians.
* Author A.E.Sansom  30th April 1990
* Modification to include a renormalisation coefficient, and
*  set model flux outside 30 arcmins to zero (AES 25th Feb 1991)
*-

C Rename coefficients to something more descriptive
       A1=COEFF(1)
       FRAC1=COEFF(2)
       IN=COEFF(3)
       A2=COEFF(4)
       FRAC2=COEFF(5)
       Q=COEFF(6)
       RN=COEFF(7)
C Evaluate expected orientation (FI) of PSF from its position (AZ,EL)
C (anticlockwise from positive elevation direction).
       AZM=ABS(AZ)
       ELM=ABS(EL)
       IF (AZ.LT.0.0.AND.EL.GE.0.0) THEN
           FI=ATAN(ELM/AZM)-PI/2.0
       ELSE IF (AZ.GT.0.0.AND.EL.GE.0.0) THEN
           FI=PI/2.0-ATAN(ELM/AZM)
       ELSE IF (AZ.GT.0.0.AND.EL.LT.0.0) THEN
           FI=ATAN(ELM/AZM)+PI/2.0
       ELSE IF (AZ.LT.0.0.AND.EL.LT.0.0) THEN
           FI=1.5*PI-ATAN(ELM/AZM)
       ELSE IF (AZ.EQ.0.0.AND.EL.GE.0.0) THEN
           FI=0.0
       ELSE IF (AZ.EQ.0.0.AND.EL.LT.0.0) THEN
           FI=PI
       END IF
C Find coordinates wrt major axis of PSF ellipse.
       DDAZ=DAZ*COS(FI)-DEL*SIN(FI)
       DDEL=DAZ*SIN(FI)+DEL*COS(FI)

C Find relevant scale lengths at orientation alpha=arctan(DDEL/DDAZ)
       DR1=SQRT(DDAZ**2.0+(DDEL/FRAC1)**2.0)
       DR2=SQRT(DDAZ**2.0+(DDEL/FRAC2)**2.0)
C Find normalization factors
       EN1=PI*(A1**2.0)*FRAC1/(IN-1.0)
       EN2=2.0*PI*(A2**2.0)*FRAC2
       E1=1.0/(1+(DR1/A1)**2.0)**IN
       E2=EXP(-(DR2/A2))
       E2KING2=E1/EN1+(Q/100.0)*E2/EN2
C renormalise to 1.00 under whole PSF.
       E2KING2 = E2KING2/(1.+(Q/100.0))
       CIN_E2KING2 = E2KING2/RN
C Set model flux to zero outside 30 arcmins from model PSF centre
       DROUTER=8.7266E-03
       DRAD=SQRT(DAZ**2.0+DEL**2.0)
       IF (DRAD.GT.DROUTER) CIN_E2KING2=0.0
       END
*+CIN_EXPN2X2  computes WFC PSF using supplied coefficients
       FUNCTION CIN_EXPN2X2(COEFF,AZ,EL,DAZ,DEL)

       REAL COEFF(5)
       REAL AZ,EL,DAZ,DEL,Q

       PARAMETER (PI=3.1415927)
*
*COEFF   Input   Coefficients describing the PSF at AZ,EL (radians)
*AZ      Input   Azimuthal position of PSF centre wrt optical axis
*EL      Input   Elevational position of PSF centre wrt optical axis
*DAZ     Input   Offset azimuthal position wrt PSF centre
*DEL     Input   Offset elevational position wrt PSF centre
*
* NB. Assumes coefficients for half widths are in arcseconds.
* Author A.E.Sansom  October 1988
* Last modified 27th Oct. 89
*-

C Rename coefficients to something more descriptive
       A1=COEFF(1)
       FRAC1=COEFF(2)
       A2=COEFF(3)
       FRAC2=COEFF(4)
       Q=COEFF(5)
C Evaluate expected orientation (FI) of PSF from its position (AZ,EL)
C (anticlockwise from positive elevation direction).
       AZM=ABS(AZ)
       ELM=ABS(EL)
       IF (AZ.LT.0.0.AND.EL.GE.0.0) THEN
           FI=ATAN(ELM/AZM)-PI/2.0
       ELSE IF (AZ.GT.0.0.AND.EL.GE.0.0) THEN
           FI=PI/2.0-ATAN(ELM/AZM)
       ELSE IF (AZ.GT.0.0.AND.EL.LT.0.0) THEN
           FI=ATAN(ELM/AZM)+PI/2.0
       ELSE IF (AZ.LT.0.0.AND.EL.LT.0.0) THEN
           FI=1.5*PI-ATAN(ELM/AZM)
       ELSE IF (AZ.EQ.0.0.AND.EL.GE.0.0) THEN
           FI=0.0
       ELSE IF (AZ.EQ.0.0.AND.EL.LT.0.0) THEN
           FI=PI
       END IF
C Find coordinates wrt major axis of PSF ellipse.
       DDAZ=DAZ*COS(FI)-DEL*SIN(FI)
       DDEL=DAZ*SIN(FI)+DEL*COS(FI)

C Find relevant scale lengths at orientation alpha=arctan(DDEL/DDAZ)
       DR1=SQRT(DDAZ**2.0+(DDEL/FRAC1)**2.0)
       DR2=SQRT(DDAZ**2.0+(DDEL/FRAC2)**2.0)

C Find normalization factors
       EN1=2.0*PI*(A1**2.0)*FRAC1
       EN2=2.0*PI*(A2**2.0)*FRAC2
       E1=EXP(-(DR1/A1))
       E2=EXP(-(DR2/A2))
       EXPN2X2=E1/EN1+(Q/100.0)*E2/EN2
C renormalise to 1.00 under whole PSF.
       CIN_EXPN2X2 = EXPN2X2/(1.+(Q/100.0))
       END
*+CIN_FEPOCH	Find epoch window
	SUBROUTINE CIN_FEPOCH(MJDIN,LOC,IWI,MJDLO,MJDHI,ISTAT)
	DOUBLE PRECISION MJDIN,MJDLO,MJDHI
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC) LOC
	INTEGER IWI,ISTAT
*MJDIN	input	requested epoch
*LOC	input	locator to structure containing epoch vector
*IWI	output	window index
*MJDLO	output	start of window
*MJDHI	output	end of window
*ISTAT	in/out	returned status
*-Author Dick Willingale 1990-Apr-23
	LOGICAL FOUND
	PARAMETER (NEMAX=200)
	DOUBLE PRECISION EARR(NEMAX)
C
	IF(ISTAT.NE.0) RETURN
C
	CALL CMP_GET1D(LOC,'MJD_ARRAY',NEMAX,EARR,NEP,ISTAT)
	IF(ISTAT.EQ.0) THEN
		FOUND=.FALSE.
		IWI=NEP
		DO WHILE(.NOT.FOUND)
			IF(MJDIN.GE.EARR(IWI).OR.IWI.EQ.1) THEN
				FOUND=.TRUE.
				IF(IWI.EQ.1) THEN
					MJDLO=0.0
				ELSE
					MJDLO=EARR(IWI)
				ENDIF
				IF(IWI.EQ.NEP) THEN
					MJDHI=1.E10
				ELSE
					MJDHI=EARR(IWI+1)
				ENDIF
			ELSE
				IWI=IWI-1
			ENDIF
		ENDDO
	ENDIF
	IF(ISTAT.NE.0) THEN
		WRITE(*,*) '   epoch window not found for',MJDIN
		WRITE(*,*) '   error in CIN_FEPOCH'
	ENDIF
	END
*+CIN_GTDIRECT Generate GEIG c/s from HK counter and CAL params
	REAL FUNCTION CIN_GTDIRECT(PGID)
	INTEGER		PGID
* M. Denby 10 OCT 89
*-
	REAL		CEM(256), GEIG(256)
	COMMON /CAL_BMD/ GTA, GTB, GTVP, CEMA, CEMB, CEMVP, CEM, GEIG
	SAVE CAL_BMD

* Use the look up table to generate CEM c/s
	PGID = MIN(PGID,256)
	PGID = MAX(PGID,1)

	CIN_GTDIRECT = GEIG(PGID)

	END
*+CIN_HIT_MASK  Test if in a window/defect mask area
	LOGICAL FUNCTION CIN_HIT_MASK(QMAP, AZ, EL)
* Input
	REAL		QMAP(65,65,2)   ! Linearization table
	REAL		AZ, EL		! Spherical coords (rads)
* Output
	INTEGER		STATUS
* M Denby 16-Jun-89
* Modified to use linearization table Dick Willingale 1990-Apr-20
* Major modification of masking Dick Willingale 1990-Sep-3
*-
C Maximum radius squared held in corner of linearization table
C Masked if outside this radius
	CIN_HIT_MASK=(AZ**2+EL**2).GT.QMAP(1,1,1)
	END
*+CIN_INIT return locator to top of MCF
	SUBROUTINE CIN_INIT(LOC,ISTAT)
        INCLUDE 'DAT_PAR'
	CHARACTER LOC*(DAT__SZLOC)

	INTEGER ISTAT
*LOC	output	locator
*ISTAT	In/Out	returned status
* A.E. Sansom May-89
* Changed beyond recognition Dick Willingale 1990-Apr-23
*-
	LOGICAL CAL_OPEN
	CHARACTER*(DAT__SZLOC) LOCSAV
	CHARACTER ISSUE*20,CALF*132
	INTEGER VERSION
	SAVE LOCSAV
	SAVE CAL_OPEN
	DATA CAL_OPEN/.FALSE./

C If the CAL_ interface is changed then SOFTFACE should be altered
C and the MCF reissued with a new value of object INTERFACE
	PARAMETER (SOFTFACE=2)
C
	IF(ISTAT.NE.0) RETURN
C See if already open
	IF (.NOT.CAL_OPEN) THEN
          CALL PSX_GETENV('CAL_WFC_MASTER',CALF,STATUS)
		CALL HDS_OPEN(CALF,'READ',LOCSAV,ISTAT)
		IF(ISTAT.NE.0) THEN
			WRITE(*,*) '   CIN_INIT failed to open MCF'
		ENDIF
C Get issue date and version and log
		CALL CMP_GET0C(LOCSAV,'ISSUE_DATE',ISSUE,ISTAT)
		CALL CMP_GET0I(LOCSAV,'VERSION',VERSION,ISTAT)
		CALL CMP_GET0I(LOCSAV,'INTERFACE',INTERFACE,ISTAT)
C Check interface
		IF(INTERFACE.NE.SOFTFACE) THEN
		  WRITE(*,*) '   CAL software-MCF mismatch'
		  WRITE(*,*) '   MCF version:',VERSION,'      issued:'//ISSUE
		  WRITE(*,*) '   MCF interface version:',INTERFACE
		  WRITE(*,*) '   CAL software interface version:',SOFTFACE
		  ISTAT=1
		ELSE
		  CAL_OPEN=.TRUE.
		ENDIF
	END IF
C Copy locator, not cloned so must not annul externally
	LOC=LOCSAV
C Check status
	IF(ISTAT.NE.0) THEN
		WRITE(*,*) '   error in CIN_INIT'
	ENDIF
	END
*+CIN_INTEFF	Interpolate detector efficiencies
	SUBROUTINE CIN_INTEFF(NT,TIMES,ETAB,MJD,IFILT,EFF,ISTAT)
	INTEGER NT,IFILT,ISTAT
	REAL TIMES(NT),ETAB(NT,8),EFF
	DOUBLE PRECISION MJD
*-Author Dick Willingale 1991-Nov-14
C
	IF(ISTAT.NE.0) RETURN
C Search for requested time interval
	II=0
	I=0
	DO WHILE(II.EQ.0)
		I=I+1
		IF(I.LE.NT) THEN
			IF(MJD.LT.TIMES(I)) II=I
		ELSE
			II=NT
		ENDIF
	ENDDO
	IF(II.EQ.1) II=2
C Interpolate interval, linear in LOG(eff)
	R=(TIMES(II)-REAL(MJD))/(TIMES(II)-TIMES(II-1))
	E0=LOG(ETAB(II-1,IFILT))
	E1=LOG(ETAB(II,IFILT))
	EFF=EXP(E1-R*(E1-E0))
	END


*+CIN_INTER_1 Interpolate 1-d array
	SUBROUTINE CIN_INTER_1(QE_ARRAY,NEL,EN_AXIS,ENERGY,QE,ISTAT)
	INTEGER NEL,ISTAT
	REAL QE_ARRAY(NEL)
	REAL EN_AXIS(NEL),QE
*QE_ARRAY   	Input   Quantum efficiencies at various energies
*NEL    	Input	Energies in CAL file
*EN_AXIS   	Input   Energies at which QE's are specified
*ENERGY  	Input   X-ray energy (eV)
*QE     	Output  QE at requested energy
*ISTAT		Output	status return
*
* linear interpolation in energy used
*
* AES Nov-88
* Modified to return end values if extrapolation Dick Willingale 1990-Apr-30
* Modified to take increasing energy values Dick Willingale 1990-Apr-30
*-
C get energy index
	IF(ENERGY.LT.EN_AXIS(1)) THEN
		QE=QE_ARRAY(1)
	ELSEIF(ENERGY.GE.EN_AXIS(NEL)) THEN
		QE=QE_ARRAY(NEL)
	ELSE
	   DO K=1,NEL-1
             IF(ENERGY.LT.EN_AXIS(K+1) .AND. ENERGY.GE.EN_AXIS(K)) THEN
	        IEN = K
             ENDIF
           ENDDO
C interpolate for value
	   DY = QE_ARRAY(IEN+1) - QE_ARRAY(IEN)
	   DX = EN_AXIS(IEN+1) - EN_AXIS(IEN)
	   ENGRAD = DY/DX
	   QE = (ENERGY - EN_AXIS(IEN))*ENGRAD + QE_ARRAY(IEN)
	ENDIF
	END
*+CIN_INTER_2 interpolate PSF coefficients to estimate in theta and energy
	SUBROUTINE CIN_INTER_2(NCM,NTM,NEM,
     +	COEFF_ARRAY,NC,NTH,NEN,TH_AXIS,EN_AXIS,ENERGY,AZ,EL,COEFF,ISTAT)
	INTEGER NCM,NTM,NEM
	INTEGER NC,NTH,NEN
	INTEGER ISTAT
	REAL COEFF_ARRAY(NCM,NTM,NEM)
	REAL EN_AXIS(NEM),TH_AXIS(NTM)
	REAL COEFF(NC)
	PARAMETER (NXMAX=10,NYMAX=50,RTOD=57.29578)
	REAL TEMP(NXMAX,NYMAX)
*COEFF_ARRAY   	Input   PSF coefficients array
*NCM,NTH,NEN   	Input	Dimensions of PSF coefficients array
*TH_AXIS   	Input   Radial positions of PSF coefficients (radians)
*EN_AXIS   	Input   Energies at which PSF coefficients are specified
*ENERGY  	Input   X-ray energy (eV)
*AZ      	Input   Azimuth of PSF centre wrt optical axis (radians)
*EL      	Input   Elevation of PSF centre wrt optical axis (radians)
*COEFF   	Output  Coefficients describing the PSF at AZ,EL
*ISTAT		output	status return
*
* linear interpolation used throughout
* mild extrapolation allowed over range x(1)-FACT*[x(2)-x(1)] and
* x(n) + FACT*[x(n)-x(n-1)]
* Where: FACT = 0.1 for energy axis
* and extrapolation is allowed for the PSF centre up to a radius
* of 2.6 degrees (to cover the whole WFC field).
* N.B. Contribution from exponential is zero off-axis
*
* MGW 25-9-88
* Modified by: AES Feb 1989
* Last modified to handle Panter 89 measured parameters: AES April 1990
*-
	IF(ISTAT.NE.0) RETURN
C
	IF(NC.GT.NXMAX .OR. NTH.GT.NYMAX) THEN
		WRITE(*,*) 'CIN_INTER_2: insufficient buffer space'
		ISTAT = 1
		RETURN
	ENDIF

C limits
	THMIN = 0.0
C theta measured from zero
	THETA = SQRT(AZ*AZ+EL*EL)
C *** Define a critical radius for 'on-axis' cases (Panter 89 specific)
        THCRIT = 0.1/RTOD
C ***
	THMAX = 2.6/RTOD
	ENMIN = EN_AXIS(1) - 0.1*(EN_AXIS(2)-EN_AXIS(1))
	ENMAX = EN_AXIS(NEN) + 0.1*(EN_AXIS(NEN)-EN_AXIS(NEN-1))
C check
	IF(THETA.GT.THMAX) THEN
		WRITE(*,*) 'CIN_INTER_2: theta',THETA,'radians'
		WRITE(*,*) 'CIN_INTER_2: theta too large, using',THMAX
	ELSEIF(THETA.LT.THMIN) THEN
		WRITE(*,*) 'CIN_INTER_2: theta',THETA,'radians'
		WRITE(*,*) 'CIN_INTER_2: theta too small, using',THMIN
	ENDIF
	THETA=MAX(MIN(THETA,THMAX),THMIN)
	IF(ENERGY.LT.ENMIN) THEN
		WRITE(*,*) 'CIN_INTER_2: energy',ENERGY,'eV'
		WRITE(*,*) 'CIN_INTER_2: energy too small, using',ENMIN
	ELSEIF(ENERGY.GT.ENMAX) THEN
		WRITE(*,*) 'CIN_INTER_2: energy',ENERGY,'eV'
		WRITE(*,*) 'CIN_INTER_2: energy too large, using',ENMAX
	ENDIF
	ENUSE=MAX(MIN(ENERGY,ENMAX),ENMIN)
C do 2-way interpolation
C get theta index (axis 2)
	IF (THETA.LT.TH_AXIS(1)) THEN
           ITH = 1
	ELSE IF (THETA.GE.TH_AXIS(NTH)) THEN
           ITH = NTH - 1
	ELSE
	   DO J=1,NTH-1
              IF(THETA.GE.TH_AXIS(J) .AND. THETA.LT.TH_AXIS(J+1)) THEN
	         ITH = J
	      ENDIF
	   ENDDO
	ENDIF
C get energy index (axis 3)
	IF(ENUSE.LT.EN_AXIS(1)) THEN
	   IEN  = 1
	ELSEIF(ENUSE.GE.EN_AXIS(NEN)) THEN
	   IEN = NEN - 1
	ELSE
	   DO K=1,NEN-1
	      IF(ENUSE.GE.EN_AXIS(K) .AND. ENUSE.LT.EN_AXIS(K+1)) THEN
		 IEN = K
	      ENDIF
	   ENDDO
	ENDIF
C interpolate for coeffs
	DO I=1,NC
C energy interpolation for each theta (axis 2 = J)
C TEMP() holds array after energy interpolation
	   DO J=1,NTH
            DY = COEFF_ARRAY(I,J,IEN+1) - COEFF_ARRAY(I,J,IEN)
	    DX = EN_AXIS(IEN+1) - EN_AXIS(IEN)
	    ENGRAD = DY/DX
	    TEMP(I,J) = (ENUSE - EN_AXIS(IEN))*ENGRAD + COEFF_ARRAY(I,J,IEN)
	   ENDDO

C now do theta interpolation on TEMP()

	DY = TEMP(I,ITH+1) - TEMP(I,ITH)
	DX = TH_AXIS(ITH+1) - TH_AXIS(ITH)
	THGRAD= DY/DX
C *** Use on-axis coefficients if near to on-axis
        IF (THETA.LE.THCRIT) THGRAD=0.0
C ***
	COEFF(I) = (THETA - TH_AXIS(ITH))*THGRAD + TEMP(I,ITH)

	ENDDO

C *** Zero contribution from exponential in off-axis cases
        IF (THETA.GT.THCRIT) COEFF(6)=0.0
C ***
	END
*+CIN_INTER_2DF linear interpolation of total vignetting
	SUBROUTINE CIN_INTER_2DF(NXM,NX,NY,V,AX,AY,AZ,EL,CALTVIGN,ISTAT)
	INTEGER NXM,NX,NY,ISTAT
	REAL V(NXM,NY),AX(NX),AY(NY),AZ,EL,CALTVIGN
*NXM		input	1st dimension of V in calling routine
*NX,NY		input	dimensions of vignetting array
*V	   	Input   Total Vignetting array
*AX	   	Input   Azimuthal axis coordinates (radians)
*AY	   	Input   Elevation axis coordinates (radians)
*AZ      	Input   Azimuthal position wrt optical axis (radians)
*EL      	Input   Elevational position wrt optical axis (radians)
*CALTVIGN   	output  Value if vignetting function at AZ,EL
*ISTAT		output	status return
*
* Bi-linear interpolation in field of view coordinates (AZ,EL)
* Assumes a regular grid in azimuth and elevation.
*
* AES April-89
* Modified argument list Dick Willingale 1990-Apr-24
* Allow extrapolation Dick Willingale 1990-Apr-25
*-
C do 2-way interpolation (first in azimuth then in elevation)
	TAZ=(AX(2)-AX(1))
	TEL=(AY(2)-AY(1))
	IAZ=(AZ-AX(1))/TAZ+1.
	IEL=(EL-AY(1))/TEL+1.
        IAZ=MAX(MIN(NX-1,IAZ),1)
        IEL=MAX(MIN(NY-1,IEL),1)
C interpolate in azimuth
        WAL=(AX(IAZ+1)-AZ)/TAZ
        WAH=(AZ-AX(IAZ))/TAZ
        DAZV=WAL*V(IAZ,IEL)+WAH*V(IAZ+1,IEL)
        UAZV=WAL*V(IAZ,IEL+1)+WAH*V(IAZ+1,IEL+1)
C interpolate in elevation
        WEL=(AY(IEL+1)-EL)/TEL
        WEH=(EL-AY(IEL))/TEL
        CALTVIGN=DAZV*WEL + UAZV*WEH
	END
*+CIN_INTER_INV	Convert local sphericals to det. coords
	SUBROUTINE CIN_INTER_INV (AZ, EL, NEV, QMAP, ZOOM, DX, DY, IQ)
* Input
	REAL 		AZ(*), EL(*)	! Spherical coords (rads)
	INTEGER		NEV		! # events in frame
	PARAMETER (NLIN = 64)
	REAL 		QMAP(0:NLIN,0:NLIN,2)
					! Inverse lin table
	LOGICAL		ZOOM		! Det zoomed flag
* Output
	REAL		DX(*), DY(*)	! Det coords (0.-511.)
	INTEGER		IQ(*)		! Qual (0-OK,1-null,2-masked)
* M Denby 1988
*-
* Local
	REAL 		DTOR
	REAL		RADPERCELL
	PARAMETER (DTOR = 0.01745329252)
	PARAMETER (RADPERCELL = (5.*DTOR)/64.)

* NB RADPERCELL must agree with the FOV limit set by the routine which
* inverts the detector lin table

	IF (.NOT. ZOOM) THEN
* Find position in table
	  DO NE = 1, NEV
	    IXX=((2.5*DTOR + AZ(NE)) / RADPERCELL)
	    IYY=((2.5*DTOR + EL(NE)) / RADPERCELL)

* Find position within cell of table
	    XCELL = (IXX-32)*RADPERCELL
	    YCELL = (IYY-32)*RADPERCELL
	    XS = AZ(NE) - XCELL
	    YS = EL(NE) - YCELL

* Extract limits of cell from table
	    XL=QMAP(IXX,IYY,1)
	    XH=QMAP(IXX+1,IYY,1)
	    YL=QMAP(IXX,IYY,2)
	    YH=QMAP(IXX,IYY+1,2)

* Now interpolate, Set quality to zero and return
	    DX(NE)=XL+(XH-XL)*(XS/RADPERCELL)
	    DY(NE)=YL+(YH-YL)*(YS/RADPERCELL)
	    IQ(NE) = 0
	  ENDDO

	ELSE
* Find position in table
	  DO NE = 1, NEV
	    IXX=((2.5*DTOR + AZ(NE)) / RADPERCELL)
	    IYY=((2.5*DTOR + EL(NE)) / RADPERCELL)

* Find position within cell of table
	    XCELL = (IXX-32)*RADPERCELL
	    YCELL = (IYY-32)*RADPERCELL
	    XS = AZ(NE) - XCELL
	    YS = EL(NE) - YCELL

* Extract limits of cell from table
	    XL=QMAP(IXX,IYY,1)
	    XH=QMAP(IXX+1,IYY,1)
	    YL=QMAP(IXX,IYY,2)
	    YH=QMAP(IXX,IYY+1,2)

* Now interpolate, Set quality to zero and return
	    DX(NE)=2.*(XL+(XH-XL)*(XS/RADPERCELL) - 128.)
	    DY(NE)=2.*(YL+(YH-YL)*(YS/RADPERCELL) - 128.)
	    IQ(NE) = 0
	  ENDDO
	ENDIF

	END
*+CIN_PSF	Generic point response function
	REAL FUNCTION CIN_PSF(FUNC_NAME,NCOEFF,COEFF,AZ,EL,DAZ,DEL,ISTAT)
	CHARACTER FUNC_NAME*(*)
	INTEGER NCOEFF,ISTAT
	REAL COEFF(NCOEFF),AZ,EL,DAZ,DEL
*FUNC_NAME	input	name of function to be used
*NCOEFF		input	number of coefficients
*COEFF		input	coefficients
*AZ,EL		input	position of centre of PSF in FOV (radians)
*DAZ,DEL	input	offset from centre (radians)
*ISTAT		in/out	returned status
C
	IF(ISTAT.NE.0) RETURN
C Evaluate fucntion
	IF(FUNC_NAME.EQ.'EXPN2X2') THEN
		CIN_PSF = CIN_EXPN2X2(COEFF,AZ,EL,DAZ,DEL)
	ELSEIF(FUNC_NAME.EQ.'E2KING2') THEN
		CIN_PSF=CIN_E2KING2(COEFF,AZ,EL,DAZ,DEL)
	ELSE
		WRITE(*,*) 'CIN_PSF: function ',FUNC_NAME,' not known'
		ISTAT = 1
	ENDIF
	END
*+CIN_RRF	Average value of PSF at specified radius from PSF centre
	FUNCTION CIN_RRF(NCOEFF,COEFF,FUNC,AZ,EL,RADIUS,ISTAT)
	INTEGER ISTAT,NCOEFF
	CHARACTER FUNC*(*)
	REAL CIN_RRF,AZ,EL,RADIUS,COEFF(NCOEFF)
*NCOEFF	input	  Number of psf coefficients
*COEFF  input	  Array of psf coefficients
*FUNC	input	  Function name
*AZ	input	  Centroid azimuth in FOV
*EL	input	  Centroid elevation in FOV
*RADIUS	input	  radial offset from the centroid radians
*ISTAT  in/out 	  status return
*CIN_RRF out	  average probability density of the PSF relative to the peak
*                 at a radius RADIUS from the centroid
* Modified interface Dick Willingale 1990-Apr-25
*-
C
       PARAMETER (N=101,PI=3.141592654,RTOA=206264.8062)
       REAL PT,T,DAZ,DEL,P
       INTEGER J
C
C      Integrate around a circle of radius RADIUS
C
	IF(ISTAT.NE.0) RETURN
	PT=0.0
	DO J=1,N
          T=(FLOAT(J)*2.0*PI)/(FLOAT(N))
          DAZ=RADIUS*SIN(T)
          DEL=RADIUS*COS(T)
          P=CIN_PSF(FUNC,NCOEFF,COEFF,AZ,EL,DAZ,DEL,ISTAT)
          PT=PT+P
	END DO
	CIN_RRF=PT/FLOAT(N)
	END
*+CIN_SET_BMD Get particle monitor params for epoch
	SUBROUTINE CIN_SET_BMD (MJDIN, STATUS)
* Input
	REAL*8		MJDIN		! requested epoch
* Output
	INTEGER		STATUS		! Status return
* M. Denby Oct 10 89
*-
* Local
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC)	LOC, LOCBM, LOCGT, LOCCM, LOCDR, LOCEL
	REAL		CEM(256)
	REAL		GEIG(256)
	DOUBLE PRECISION MJDLO,MJDHI
	COMMON /CAL_BMD/ GTA, GTB, GTVP, CEMA, CEMB, CEMVP,CEM, GEIG

	SAVE CAL_BMD

* The code assumes the following structure for BMD info in CAL_WFC_MASTER
*
* .BMD_PARM
*   .MJD_ARRAY()
*   .GT_DIRECT
*     .DATA_RECORD()
*       .A
*       .B
*       .VP
* and same for others (eg CEM)
*
* The counts used by the EVE system are as recommended by ICST and are
* PGID (Geiger) and PCN3 (CEM). These parameters, of those available,
* seem to be best behaved over the full dynamic range.
*

	IF (STATUS .NE. 0) RETURN

* Get locator to MCF
	CALL CIN_INIT (LOC,STATUS)

* Find the BMD structure
	CALL DAT_FIND (LOC, 'BMD_PARM', LOCBM, STATUS)

* Find epoch window
	CALL CIN_FEPOCH(MJDIN,LOCBM,NE,MJDLO,MJDHI,ISTAT)
C Extract data
	  CALL DAT_FIND (LOCBM, 'GT_DIRECT', LOCGT, STATUS)
	  CALL DAT_FIND (LOCGT, 'DATA_RECORD', LOCDR, STATUS)
	  CALL DAT_CELL (LOCDR, 1, NE, LOCEL, STATUS)
	  CALL CMP_GET0R (LOCEL, 'A', GTA, STATUS)
	  CALL CMP_GET0R (LOCEL, 'B', GTB, STATUS)
	  CALL CMP_GET0R (LOCEL, 'VP', GTVP, STATUS)

* Do the same for the CEM parameters
	  CALL DAT_FIND (LOCBM, 'CEM_DIRECT', LOCCM, STATUS)
	  CALL DAT_FIND (LOCCM, 'DATA_RECORD', LOCDR, STATUS)
	  CALL DAT_CELL (LOCDR, 1, NE, LOCEL, STATUS)
	  CALL CMP_GET0R (LOCEL, 'A', CEMA, STATUS)
	  CALL CMP_GET0R (LOCEL, 'B', CEMB, STATUS)
	  CALL CMP_GET0R (LOCEL, 'VP', CEMVP, STATUS)

* Generate the look up table for later use by the functions
	  DO I = 1, 256
	    CEM(I)  = EXP((CEMA*I - CEMVP)/CEMB)
	    GEIG(I) = EXP((GTA*I  - GTVP)/GTB)
	  ENDDO
C Clear up
	CALL DAT_ANNUL(LOCBM,STATUS)
	CALL DAT_ANNUL(LOCGT,STATUS)
	CALL DAT_ANNUL(LOCCM,STATUS)
	CALL DAT_ANNUL(LOCDR,STATUS)
	CALL DAT_ANNUL(LOCEL,STATUS)
C
	IF(ISTAT.NE.0) THEN
		WRITE(*,*) '   error in CIN_SET_BMD'
	ENDIF
	END
*+CIN_SET_DET	Find detector from given epoch
	SUBROUTINE CIN_SET_DET(MJDIN,IDET,ISTAT)
	DOUBLE PRECISION MJDIN
	INTEGER IDET,ISTAT
*MJDIN	input	required epoch
*IDET	output	detector in use
*ISTAT	in/out	returned status
*-Author Dick Willingale 1990-Apr-23
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC) LOC,LOC1
	INTEGER DNOW,DET(10)
	SAVE DNOW
	DATA DNOW/0/
	DOUBLE PRECISION MJDLO,MJDHI
	SAVE MJDLO,MJDHI
	DATA MJDLO/-1./
	DATA MJDHI/-1./
	LOGICAL CIN_VALID
C Check status
	IF(ISTAT.NE.0) RETURN
C See if current value OK
	IF(CIN_VALID(MJDIN,MJDLO,MJDHI,0,0)) THEN
		IDET=DNOW
	ELSE
C Get locator to MCF
		CALL CIN_INIT(LOC,ISTAT)
C Find detector structure
		CALL DAT_FIND(LOC,'DETECTOR',LOC1,ISTAT)
C Find slot
		CALL CIN_FEPOCH(MJDIN,LOC1,IWIN,MJDLO,MJDHI,ISTAT)
C Get detector
		CALL  CMP_GET1I(LOC1,'DATA_ARRAY',5,DET,MD,ISTAT)
		IDET=DET(IWIN)
		DNOW=IDET
C Clear up
		CALL DAT_ANNUL(LOC1,ISTAT)
	ENDIF
	IF(ISTAT.NE.0) THEN
		WRITE(*,*) '   error in CIN_SET_DET'
	ENDIF
	END
*+CIN_SET_INV Read the Inverse Linearisation from the CAL file
	SUBROUTINE CIN_SET_INV (MJDIN, DET, LINARR, STATUS)
* Input
	REAL*8		MJDIN		  ! Requested epoch
* Output
	INTEGER		DET		  ! Detector # (1 or 2)
	REAL		LINARR (65,65,2)  ! Linearisation matrix
	INTEGER		STATUS		  ! Status (0 = OK)
* M. Denby 28-Feb-89
* Modified to use new Epoch search Dick Willingale 1990-Apr-23
*-
* Local
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC)	LOCLT		  ! Locator to Lin tables
	CHARACTER*(DAT__SZLOC)	LOCLIN		  ! Locator to LIN structure
	CHARACTER*(DAT__SZLOC)	LOCDR		  ! Locator to DATA RECORD structure
	CHARACTER*(DAT__SZLOC)	LOCTMP		  ! Temp locator
	CHARACTER*(DAT__SZLOC)	LOC		  ! Locator to top level of CAL file
	INTEGER		NELS(3)		  ! Shape of Lin matrix
	REAL*8		MJDLO,MJDHI	  ! Current window
	DATA NELS /65, 65, 2/
C
	IF (STATUS .NE. 0) RETURN
* Get locator to MCF
	CALL CIN_INIT(LOC,STATUS)
* Get detector on use
	CALL CIN_SET_DET(MJDIN,DET,STATUS)
* Find the Linearisation tables
	CALL DAT_FIND (LOC, 'LIN_INV_TABLES', LOCLT, STATUS)
* Find locator to LIN structure
	CALL DAT_FIND (LOCLT, 'LIN', LOCTMP, STATUS)
* Get locator to appropriate element of LIN
	CALL DAT_CELL (LOCTMP, 1, DET, LOCLIN, STATUS)
* Get epoch window
	CALL CIN_FEPOCH(MJDIN,LOCLIN,NE,MJDLO,MJDHI,STATUS)
* Read the linearisation matrix
	CALL DAT_FIND (LOCLIN, 'DATA_RECORD', LOCDR,  STATUS)
	CALL DAT_CELL (LOCDR,  1, NE, LOCTMP, STATUS)
	CALL CMP_GETNR (LOCTMP, 'DATA_ARRAY', 3, NELS, LINARR, NELS, STATUS)
* clear locators
	CALL DAT_ANNUL(LOCLT,STATUS)
	CALL DAT_ANNUL(LOCTMP,STATUS)
	CALL DAT_ANNUL(LOCLIN,STATUS)
	CALL DAT_ANNUL(LOCDR,STATUS)
* Report bad status
	IF(STATUS.NE.0) THEN
		WRITE(*,*) '   error in CIN_SET_INV'
	ENDIF
	END
*+CIN_SET_MASK	Set bit mask for bad areas
	SUBROUTINE CIN_SET_MASK(MJDIN,FILTER,MASK,ISTAT)
	DOUBLE PRECISION MJDIN
	INTEGER FILTER,MASK(2,64),ISTAT
*MJDIN	input	Epoch
*FILTER	input	filter, if 0 then return all areas good
*MASK	output	bit mask returned
*ISTAT	in/out	returned status
* If MJDIN and FILTER are compatible with last call then MASK is returned
* unaltered
*-Author Dick Willingale 1990-Apr-20
	DOUBLE PRECISION MJDLO,MJDHI,MJDLOF,MJDHIF
	INTEGER FNOW,IMASK(2,64),MDIM(2),JMASK(2,64)
	DATA MDIM/2,64/
	LOGICAL CIN_VALID,NEWI,NEWF
	SAVE MJDLO,MJDHI,MJDLOF,MJDHIF,FNOW,IMASK,JMASK
	DATA FNOW/-1/
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC) LOC,LOC1,LOC2,LOC3,LOC4,LOC5
C
	IF(ISTAT.NE.0) RETURN
C If filter 0 then return all areas good
	IF(FILTER.EQ.0) THEN
		DO I=1,64
			DO J=1,2
				MASK(J,I)=0
			ENDDO
		ENDDO
		RETURN
	ENDIF
C See if already got filter independent mask
	NEWI=.FALSE.
	IF(.NOT.CIN_VALID(MJDIN,MJDLO,MJDHI,0,0)) THEN
C Get locator to MCF
		CALL CIN_INIT(LOC,ISTAT)
C Get locator to filter independent mask
		CALL DAT_FIND(LOC,'MASK_DETECTOR',LOC1,ISTAT)
C Find epoch window
		CALL CIN_FEPOCH(MJDIN,LOC1,N,MJDLO,MJDHI,ISTAT)
C Move into correct epoch structure
		CALL DAT_FIND(LOC1,'DATA_RECORD',LOC2,ISTAT)
		CALL DAT_CELL(LOC2,1,N,LOC3,ISTAT)
C Read mask array
		CALL CMP_GETNI(LOC3,'MASK_ARRAY',2,MDIM,IMASK,MDIM,ISTAT)
		CALL DAT_ANNUL(LOC1,ISTAT)
		CALL DAT_ANNUL(LOC2,ISTAT)
		CALL DAT_ANNUL(LOC3,ISTAT)
C Set flag to indicate new mask
		NEWI=.TRUE.
	ENDIF
C See if already got filter mask
	NEWF=.FALSE.
	IF(.NOT.CIN_VALID(MJDIN,MJDLOF,MJDHIF,FILTER,FNOW)) THEN
		FNOW=FILTER
C Get locator to MCF
		CALL CIN_INIT(LOC,ISTAT)
C get locator to FILT_VIGN structure
		CALL DAT_FIND(LOC,'MASK_FILTER',LOC1,ISTAT)
C get locator to requested filter structure
		CALL DAT_FIND(LOC1,'FILTER',LOC2,ISTAT)
		CALL DAT_CELL(LOC2,1,FILTER,LOC3,ISTAT)
C Find epoch window
		CALL CIN_FEPOCH(MJDIN,LOC3,N,MJDLOF,MJDHIF,ISTAT)
C Move into correct epoch structure
		CALL DAT_FIND(LOC3,'DATA_RECORD',LOC4,ISTAT)
		CALL DAT_CELL(LOC4,1,N,LOC5,ISTAT)
C Read mask array
		CALL CMP_GETNI(LOC5,'MASK_ARRAY',2,MDIM,JMASK,MDIM,ISTAT)
C close down
		CALL DAT_ANNUL(LOC1,ISTAT)
		CALL DAT_ANNUL(LOC2,ISTAT)
		CALL DAT_ANNUL(LOC3,ISTAT)
		CALL DAT_ANNUL(LOC4,ISTAT)
		CALL DAT_ANNUL(LOC5,ISTAT)
C Set flag to indicate new mask
		NEWF=.TRUE.
	ENDIF
C If changed then recombine
	IF(NEWF.OR.NEWI) THEN
		DO I=1,64
			DO J=1,2
				MASK(J,I)=IOR(JMASK(J,I),IMASK(J,I))
			ENDDO
		ENDDO
	ENDIF
C Check status
	IF(ISTAT.NE.0) THEN
		WRITE(*,*) '   error in CIN_SET_MASK'
	ENDIF
	END
*+CIN_SET_PSF accesses PSF data in CAL file
	SUBROUTINE CIN_SET_PSF(DMJD,IFILT,ENERGY,AZ,EL,NCM,COEFF,NCOEFF,
     &  FUNC_NAME,ISTAT)

	DOUBLE PRECISION DMJD
	INTEGER NCM,NCOEFF
	REAL COEFF(NCM),AZ,EL,ENERGY
	CHARACTER*(*) FUNC_NAME
	INTEGER ISTAT
*DMJD		input	epoch required
*IFILT		input	filter number (not used at present)
*ENERGY		input	photon energy
*AZ		input	azimuth in FOV (radians)
*EL		input	elevation in FOV (radians)
*NCM		input	max number of coeffs that can be returned
*COEFF		output	coefficient array for selected time etc.
*NCOEFF		output	number of coeffs returned
*FUNC_NAME	output	name of PSF function
*ISTAT		in/out	returned status
*
* No extrapolation
*
* 	MGW 88-9-27
*       (AES 89-1-13 - lin interpolation added)
* Modified rather alot Dick Willingale 1990-Apr-25
* If ENERGY, AZ and EL as previous call then COEFF, NCOEFF and
* FUNC_NAME returned unchanged.
*-
	DOUBLE PRECISION MJDLO,MJDHI
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC) LOC,LOC1,LOC2,LOC3,FNAME*40
	INTEGER NDIMS(3),IELS(3),NC,NR,NE
	PARAMETER (NCMAX=20,NRMAX=20,NEMAX=4)
	REAL CA(NCMAX,NRMAX,NEMAX),RAD(NRMAX),EN(NEMAX)
	DATA NDIMS/NCMAX,NRMAX,NEMAX/
	REAL ESAV,AZSAV,ELSAV
	SAVE CA,RAD,EN,NC,NE,NR,FNAME,ESAV,AZSAV,ELSAV
	LOGICAL CIN_VALID

CC
	IF(ISTAT.NE.0) RETURN
C Check if current coeffs ok
	IF(.NOT.CIN_VALID(DMJD,MJDLO,MJDHI,0,0)) THEN
C Get locator to MCF
		CALL CIN_INIT(LOC,ISTAT)
C get locator to PSF structure
		CALL DAT_FIND(LOC,'POINT_RESP',LOC1,ISTAT)
C Get epoch window
		CALL CIN_FEPOCH(DMJD,LOC1,IW,MJDLO,MJDHI,ISTAT)
C Get data record
		CALL DAT_FIND(LOC1,'DATA_RECORD',LOC2,ISTAT)
		CALL DAT_CELL(LOC2,1,IW,LOC3,ISTAT)
C Get axis info
		CALL CMP_GET1R(LOC3,'AXIS2_DATA',NRMAX,RAD,NR,ISTAT)
		CALL CMP_GET1R(LOC3,'AXIS3_DATA',NEMAX,EN,NE,ISTAT)
C Get coefficient array
		CALL CMP_SHAPE(LOC3,'DATA_ARRAY',3,NDIMS,IDIM,ISTAT)
		CALL CMP_GETNR(LOC3,'DATA_ARRAY',IDIM,NDIMS,CA,IELS,ISTAT)
		NC=IELS(1)
C Get function name
		CALL CMP_GET0C(LOC3,'FUNCTION_NAME',FNAME,ISTAT)
C close down
		CALL DAT_ANNUL(LOC3,ISTAT)
		CALL DAT_ANNUL(LOC2,ISTAT)
		CALL DAT_ANNUL(LOC1,ISTAT)
C Force new interpolation
		ESAV=-10.0
		AZSAV=-10.0
		ELSAV=-10.0
	ENDIF
C Interpolate in position and energy if changed
	IF(ENERGY.NE.ESAV.OR.AZ.NE.AZSAV.OR.EL.NE.ELSAV) THEN
		CALL CIN_INTER_2(NC,NR,NE,CA,NC,NR,NE,RAD,EN,ENERGY,
     +		AZ,EL,COEFF,ISTAT)
		ESAV=ENERGY
		AZSAV=AZ
		ELSAV=EL
C Set function name
		FUNC_NAME=FNAME
C Set number of coefficients
		NCOEFF=NC
	ENDIF
	END
*+CIN_SET_TVIGN  Get total vignetting data
	SUBROUTINE CIN_SET_TVIGN(DMJD,IMCP,IFILT,ENERGY,
     +	NXM,NYM,VA,AZ,EL,NX,NY,ISTAT)
	DOUBLE PRECISION DMJD
	INTEGER IMCP,IFILT,NXM,NYM,NX,NY,ISTAT
	REAL ENERGY,VA(NXM,NYM),AZ(NXM),EL(NYM)
*DMJD		input	MJD
*IMCP           input   detector required (1 or 2)
*IFILT          input   filter required
*ENERGY		input	photon energy eV
*NXM,NYM	input	size of arrays
*VA		output	total vignetting array for selected time
*AZ		output	azimuth sample positions
*EL		output	elevation sample positions
*NX,NY		output	dimensions of vignetting array returned
*ISTAT          in/out	returned status
*
*  AES April-89
* Modified to return all values as arguments Dick Willingale 1990-Apr-24
* If requested data compatible with last call then leave output arrays
* as they are.
*-
	INTEGER KV(3),IELS(2),JELS(2)
	PARAMETER (NEY=10,NMAX=31)
	REAL ENY(NEY),AT(NMAX,NMAX)
        INCLUDE 'DAT_PAR'
	CHARACTER*(DAT__SZLOC) LOC,LOC1,LOC2,LOC3,LOCT,LOCU
	DOUBLE PRECISION MJDLO,MJDHI
	INTEGER FNOW,DNOW
	REAL ENOW
	SAVE MJDLO,MJDHI,FNOW,DNOW,ENOW
	LOGICAL CIN_VALID
C
	IF(ISTAT.NE.0) RETURN
C Check if same as last call
	IF(CIN_VALID(DMJD,MJDLO,MJDHI,IFILT,FNOW)
     +	.AND.IMCP.EQ.DNOW.AND.ENERGY.EQ.ENOW) RETURN
C
	FNOW=IFILT
	DNOW=IMCP
	ENOW=ENERGY
C Get locator to MCF
	CALL CIN_INIT(LOC,ISTAT)
C get locator to TOTAL_VIGN structure
	CALL DAT_FIND(LOC,'VIGN',LOC1,ISTAT)
C Choose energy index
	CALL CMP_GET1R(LOC1,'ENERGIES',NEY,ENY,NE,ISTAT)
	J=1
	IENL=0
	DO WHILE(IENL.EQ.0)
		IF(J.EQ.1.AND.ENERGY.LT.ENY(J)) THEN
			IENH=1
			IENL=1
		ELSEIF(J.EQ.NE.AND.ENERGY.GE.ENY(J)) THEN
			IENH=NE
			IENL=NE
		ELSEIF(ENERGY.GE.ENY(J).AND.ENERGY.LT.ENY(J+1)) THEN
			IENL=J
			IENH=J+1
			RAT=(ENERGY-ENY(J))/(ENY(J+1)-ENY(J))
		ELSE
			J=J+1
		ENDIF
	ENDDO
C get locator to correct vignetting requested
        KV(1)=IFILT
        KV(2)=IMCP
	KV(3)=IENL
        CALL DAT_FIND(LOC1,'VIGN_DATA',LOC2,ISTAT)
        CALL DAT_CELL(LOC2,3,KV,LOC3,ISTAT)
C Get epoch window
	CALL CIN_FEPOCH(DMJD,LOC3,IW,MJDLO,MJDHI,ISTAT)
C Get required record
        CALL DAT_FIND(LOC3,'DATA_RECORD',LOCT,ISTAT)
	CALL DAT_CELL(LOCT,1,IW,LOCU,ISTAT)
	IELS(1)=NXM
	IELS(2)=NYM
	CALL CMP_GETNR(LOCU,'DATA_ARRAY',2,IELS,VA,JELS,ISTAT)
	CALL CMP_GET1R(LOCU,'AXIS1_DATA',NXM,AZ,NX,ISTAT)
	CALL CMP_GET1R(LOCU,'AXIS2_DATA',NYM,EL,NY,ISTAT)
	IF(IENL.NE.IENH) THEN
C Interpolate if within range
		KV(3)=IENH
		CALL DAT_ANNUL(LOC3,ISTAT)
	        CALL DAT_CELL(LOC2,3,KV,LOC3,ISTAT)
C Get epoch window
		CALL CIN_FEPOCH(DMJD,LOC3,IW,MJDLO,MJDHI,ISTAT)
C Get required record
		CALL DAT_ANNUL(LOCT,ISTAT)
		CALL DAT_FIND(LOC3,'DATA_RECORD',LOCT,ISTAT)
		CALL DAT_ANNUL(LOCU,ISTAT)
		CALL DAT_CELL(LOCT,1,IW,LOCU,ISTAT)
		IELS(1)=NMAX
		IELS(2)=NMAX
		CALL CMP_GETNR(LOCU,'DATA_ARRAY',2,IELS,AT,JELS,ISTAT)
		DO J=1,JELS(2)
			DO I=1,JELS(1)
				VA(I,J)=VA(I,J)+(AT(I,J)-VA(I,J))*RAT
			ENDDO
		ENDDO
	ENDIF
C close down
	CALL DAT_ANNUL(LOCU,ISTAT)
	CALL DAT_ANNUL(LOCT,ISTAT)
	CALL DAT_ANNUL(LOC3,ISTAT)
	CALL DAT_ANNUL(LOC2,ISTAT)
	CALL DAT_ANNUL(LOC1,ISTAT)
	END
*+CIN_SPIRAL	Index for spiral search
	SUBROUTINE CIN_SPIRAL(ISX,ISY,NPASS,IX,IY)
	INTEGER NPASS,ISX,ISY,IX,IY
*ISX	input	spiral centre x
*ISY	input	spiral centre y
*NPASS	in/out	index in spiral
*IX	output	x index
*IY	output	y index
* NPASS returned as 50 if completed spiral
*-Author Dick Willingale 1990-May-18
	IF(NPASS.EQ.1) THEN
		IX=ISX-1
		IY=ISY-1
	ELSEIF(NPASS.EQ.2) THEN
		IX=ISX-1
		IY=ISY
	ELSEIF(NPASS.EQ.3) THEN
		IX=ISX-1
		IY=ISY+1
	ELSEIF(NPASS.EQ.4) THEN
		IX=ISX
		IY=ISY+1
	ELSEIF(NPASS.EQ.5) THEN
		IX=ISX+1
		IY=ISY+1
	ELSEIF(NPASS.EQ.6) THEN
		IX=ISX+1
		IY=ISY
	ELSEIF(NPASS.EQ.7) THEN
		IX=ISX+1
		IY=ISY-1
	ELSEIF(NPASS.EQ.8) THEN
		IX=ISX
		IY=ISY-1
	ELSE
		NPASS=50
	ENDIF
	END
*+CIN_VALID	Check current epoch window and filter
	LOGICAL FUNCTION CIN_VALID(MJDIN,MJDLO,MJDHI,FILTER,FNOW)
	DOUBLE PRECISION MJDIN,MJDLO,MJDHI
	INTEGER FILTER,FNOW
*MJDIN	requested epoch
*MJDLO	previous low limit
*MJDHI	previous high limit
*FILTER	requested filter
*FNOW	previous filter
*CIN_VALID returned .TRUE. if previous values ok
*-Author Dick Willingale 1990-Apr-25
C
	CIN_VALID=((FILTER.EQ.FNOW).AND.(MJDIN.GE.MJDLO.AND.MJDIN.LT.MJDHI))
	END
