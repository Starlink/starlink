*+ AST2XSP - Converts Asterix spectral files to XSPEC format
       SUBROUTINE AST2XSP(STATUS)
*    Description :
*      Program converts an HDS ASTERIX88 spectrum file into a form
*      that can be read by XSPEC -(X-ray Spectral Analysis).
*      It outputs a pulse height analyser file (.PHA) and a
*      response file (.RSP).  It currently only handles data for the
*      Exosat ME, ROSAT XRT, and ROSAT WFC instruments.
*
*      7/11/91 RMJ allows bined up data from XRT to be handled
*
*    Environment parameters :
*    Method :
*     <description of how the application works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Paul McGale (XRA - University of Leicester).
*    History :
*     Feb 1991  - original
*     Apr 1991  - adamised version (LTVAD::RDS)
*     Nov 1991  - allows bined up data from XRT to be handled (XMV::RMJ)
*     Jun 1992  - takes a slice from a spectral_series and
*                 works on EXOSAT LE data (LTVAD::RDS)
*     Aug 1992  - fixed a bug in the slicing (LTVAD::RDS)
*     Apr 1993  - solved a problem which occured when an energy had a
*                 response in only one PH bin (LTVAD::RDS, XMV::RMJ)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
* XSPEC include files to define structures.
	INCLUDE 'XANADU:[SPECTRAL.INC]PHASF.INC'
	INCLUDE 'XANADU:[SPECTRAL.INC]SELECTORSF.INC'
	INCLUDE 'XANADU:[SPECTRAL.INC]DETECTORSF.INC'
	INCLUDE 'XANADU:[SPECTRAL.INC]WRITEBUF.INC'
	INCLUDE 'XANADU:[SPECTRAL.INC]PHAUNION.INC'
	INCLUDE 'XANADU:[SPECTRAL.INC]RESPONSESF.INC'
	INCLUDE 'UTILIB(INC_RSPUNI)'
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
	INTEGER  MAXELS, MAXENR
	PARAMETER (MAXELS=500000, MAXENR=10000)
*    Local variables :
	CHARACTER*60  INFILE, OUTPHA, OUTRSP
	CHARACTER*15  LOCIN, LOCASTX, LOCHDR, LOCINST
	CHARACTER*15  LOCSRT, LOCTIME, CELL, LOCAXIS, LOCAXARR
	CHARACTER*15  LOCENGY, LOCENRSP, LOCCHAN, LOCENBS
	CHARACTER*15  LOCRSP, LOCARR1, LOCARR2, LOCARR3, LOCE
	CHARACTER*40  SNAME,INSTMNT
        CHARACTER*80  STRING
*     Relate to HDS files.
	INTEGER EQUINOX, EQFLAG, DIMAXARR
	INTEGER ENGYARR(MAXELS), CHANARR(MAXELS)
	REAL*8  STRTIME, STPTIME, BTAIDAYS,BTAISECS
	REAL*4  INTGTIME, PHACNTS(MAXCHAN), CHNSPEC(MAXCHAN)
	REAL*4  VARPHA(MAXCHAN), BASEAXAR, CHNBNDS(MAXCHAN+1)
	REAL*4  RSPARR(MAXELS), ENBNDSA(MAXENR+1), SCALAXAR
        LOGICAL INPRIM
        LOGICAL OK,TWOD
        INTEGER NDIM,DIMS(4),ODIMS(4),AMIN(4),AMAX(4),ORD(4)
        INTEGER PHDIM,RADIM,SLICE
        INTEGER DPTR,VPTR,LP
*     Relate to XSPEC files.
	CHARACTER IDREC*78, PKGTYP*12, STRAR(3)*72, SDATE*9
	CHARACTER STIME*8
	CHARACTER GBEGIN, GIGNORE
	INTEGER*4  LUPHA, IERRSF, INFOAR(4), LEN, NGIGNORE
	INTEGER*4  NBINIG
	INTEGER*4  PHABINS, NCHAN, LURSP, NRANGES, BINRNG(2,MAXCHAN)
        INTEGER KDOT
	REAL*4  ERR(MAXCHAN), INRESP(MAXCHAN)
	DATA INFOAR/4*0/
*     Relate to main program.
        CHARACTER*80 PATH
	INTEGER*4 I, J, K, L, SIZARR1, SIZARR2, SIZARR3
	INTEGER*4 SIZENBS, IP, IFLAG, IE, NLEV
	REAL*4  GEOMAREA
	REAL*8  BSE72_80
*
*    Local data :
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'AST2XSP version 1.8-0')
*-
        IF (STATUS .NE. SAI__OK) RETURN
*
*    Initialise:
        CALL MSG_PRNT(VERSION)
*
        CALL AST_INIT()
*
	IERRSF=0
	GBEGIN  = '+'
	GIGNORE = '*'
*
* Get name of HDS file to convert.
* Assume filename extension of '.sdf'.
*
        CALL USI_ASSOCI('INPUT', 'READ', LOCIN, INPRIM, STATUS)
*
* Find name of input file
        CALL HDS_TRACE(LOCIN, NLEV, PATH, INFILE, STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*
* Get rid of the file extension
        KDOT=INDEX(INFILE,';')
*
        IF (KDOT .EQ. 0) KDOT=CHR_LEN(INFILE) + 1
*
	OUTPHA = INFILE(1:KDOT-5)
	OUTRSP = INFILE(1:KDOT-5)   ! Extensions are different.
*
* Define some HDS locators.
	CALL BDA_LOCAST(LOCIN, LOCASTX, STATUS)
*
	CALL BDA_LOCHEAD(LOCIN, LOCHDR, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error finding Asterix header')
           GOTO 999
        ENDIF
*
* Get instrument name
	CALL CMP_GET0C(LOCHDR, 'INSTRUMENT', INSTMNT, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error reading instrument value')
           GOTO 999
        ENDIF
*
* Get data dimensions
        CALL BDA_CHKDATA(LOCIN, OK, NDIM, DIMS, STATUS)
*
        IF (STATUS .NE. SAI__OK .OR. .NOT. OK) THEN
           CALL MSG_PRNT('Error reading data array')
           GOTO 999
        ENDIF
*
* If a 2-d XRT file assume that the other dimension is radial bins
        IF (NDIM .EQ. 2 .AND. INDEX(INSTMNT, 'XRT') .NE. 0) THEN
*
*   find which axis is the PH axis
           CALL AXIS_FIND(LOCIN, 'PHA', NDIM, PHDIM, STATUS)
*
           IF (STATUS .NE. SAI__OK) GOTO 999
*
*   set the other axis
           IF (PHDIM .EQ. 1) THEN
              RADIM = 2
           ELSEIF (PHDIM .EQ. 2) THEN
              RADIM = 1
           ELSE
              CALL MSG_PRNT('** PH axis not found **')
              GOTO 999
           ENDIF
*
*   ask user which slice he wants
           CALL MSG_SETI('NSPEC', DIMS(RADIM))
           CALL MSG_PRNT('This file contains ^NSPEC spectra')
*
           CALL USI_GET0I('SLICE', SLICE, STATUS)
*
           IF (STATUS .NE. SAI__OK) GOTO 999
*
           DIMS(3)=1
           DIMS(4)=1
*
           TWOD = .TRUE.
        ELSE
           PHDIM=1
           TWOD = .FALSE.
        ENDIF
*
*   Asterix instrument box
	CALL BDA_LOCINSTR(LOCIN, LOCINST, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error finding Asterix instrument box')
           GOTO 999
        ENDIF
*
	CALL DAT_FIND(LOCINST, 'SORT', LOCSRT, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error finding Asterix sort box')
           GOTO 999
        ENDIF
*
	CALL DAT_FIND(LOCIN, 'AXIS', LOCAXIS, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error finding Asterix axes')
           GOTO 999
        ENDIF
*
	CALL DAT_FIND(LOCASTX, 'ENERGY_RESP', LOCE, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error finding Asterix energy response')
           GOTO 999
        ENDIF
*
* If file is an array of spectra, need to get the right energy response
        IF (TWOD) THEN
           CALL DAT_CELL(LOCE, 1, SLICE, LOCENRSP, STATUS)
        ELSE
           CALL DAT_CLONE(LOCE, LOCENRSP, STATUS)
        ENDIF
*
	CALL DAT_FIND(LOCENRSP, 'CHANNEL', LOCCHAN, STATUS)
	CALL DAT_FIND(LOCENRSP, 'ENERGY', LOCENGY, STATUS)
	CALL DAT_FIND(LOCENRSP, 'RESPONSE', LOCRSP, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error reading energy response')
           GOTO 999
        ENDIF
*
* Get the base and scale values of the PHA axis
        CALL BDA_GETAXVAL(LOCIN, PHDIM, BASEAXAR, SCALAXAR,
     &                     DIMAXARR, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error checking Asterix file')
           GOTO 999
        ENDIF
*
* RMJs bit...
*
* BASE and SCALE are the binning pars of the PHA file
*
* If we have an XRT PHA file, then the matrix came from XRTRESP and
* will be binned as for the PHA file. There is no need for a grouping card.
* The BASE and SCALE factors are written into the DETID field to make
* sure that mismatches of detector and response don't happen.
*
* For other instruments I don't know what form the matrix is, so restrict
* prog to working on unbinned data. May need to write a proper grouping
* card in this case.
*
*
	IF (INDEX(INSTMNT,'XRT') .EQ. 0) THEN
	   IF ( BASEAXAR .GT. 1.0 .OR. SCALAXAR .GT. 1.0 ) THEN
	     WRITE(*,*) 'BASE is ',BASEAXAR,'  Should be 1.0'
	     WRITE(*,*) 'SCALE is ',SCALAXAR,' Should be 1.0'
	     WRITE(*,*) 'Regenerate HDS file using these vaules.'
             GOTO 999
	   ENDIF
	END IF

* Check if instrument is ROSAT XRT, WFC, EXOSAT ME or LE.
	IF (INDEX(INSTMNT, 'XRT') .GT. 0 ) THEN
	   GEOMAREA = 1141.0
*
* Standard ignores for XRT are channels 1-7.
*
* Find out which bin channel 7 comes in
*
	   NBINIG = NINT((7.0-BASEAXAR+1.0)/SCALAXAR + 0.49)
           NGIGNORE = MAX(0,NBINIG) + 1
c           print *,ngignore
*
	   CALL DAT_FIND(LOCSRT, 'TIME', LOCTIME, STATUS)
*
           IF (STATUS .NE. SAI__OK) THEN
              CALL MSG_PRNT('Error reading times from SORt box')
              GOTO 999
           ENDIF
*
	ELSE IF (INDEX(INSTMNT, 'ME') .GT. 0 ) THEN
	   GEOMAREA = 1676.71
	   NGIGNORE = 4
	   CALL DAT_FIND(LOCSRT, 'TIME', LOCTIME, STATUS)
	ELSE IF (INDEX(INSTMNT, 'LE') .GT. 0 ) THEN
	   GEOMAREA = 90.5
	   NGIGNORE = 0
	ELSE IF (INDEX(INSTMNT, 'WFC') .GT. 0 ) THEN
	   GEOMAREA = 456.0
	   NGIGNORE = 0
	ELSEIF (INDEX(INSTMNT, 'HEXE') .GT. 0) THEN
           GEOMAREA = 574.117
           NGIGNORE = 0
	ELSEIF (INDEX(INSTMNT, 'TTM') .GT. 0) THEN
           GEOMAREA = 200.0
           NGIGNORE = 0
        ELSE
	   CALL MSG_PRNT('Instrument not XRT, WFC, ME, LE, HEXE or TTM')
           GOTO 999
	ENDIF
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error reading times from SORT box')
           GOTO 999
        ENDIF
*
* Check to see if arrays have been declared large enough.
	CALL DAT_FIND(LOCENGY, 'DATA_ARRAY', LOCARR1, STATUS)
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error finding energy response array')
           GOTO 999
        ENDIF
*
	CALL DAT_SIZE(LOCARR1, SIZARR1, STATUS)
	IF (SIZARR1 .GT. MAXELS) THEN
	  CALL MSG_PRNT( 'Cannot cope with size of response arrays.')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF

	CALL DAT_FIND(LOCENGY, 'ENERGY_BOUNDS', LOCENBS, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error reading ENERGY_BOUNDS array')
           GOTO 999
        ENDIF
*
	CALL DAT_SIZE(LOCENBS, SIZENBS, STATUS)
	IF (SIZENBS .GT. MAXENR) THEN
	  CALL MSG_PRNT( 'Cannot cope with size of response arrays.')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF
*
* Deal with HDS and Pulse Height Analyser file.
* Open (unformatted) PHA file for output.
*

	LUPHA=30
	OPEN(UNIT=LUPHA, FILE=OUTPHA(1:CHR_LEN(OUTPHA))/
     &       /'.pha', STATUS='NEW', FORM='UNFORMATTED')

	IDREC        = 'SF01'     !  Define ID record.
	IDREC(5:16)  = 'XSPEC data'
	IDREC(17:24) = ' '  ! Spare.
	IDREC(25:25) = ' '  ! Assume no template package '*' if yes.
	IDREC(26:26) = ' '  ! Assume no history package ':' if yes.
	CALL DATE(SDATE)
	CALL TIME(STIME)
	IDREC(27:)   = ' by AST2XSP at '//STIME//' '//SDATE
	WRITE(LUPHA) IDREC
*
* Now write some history
*
c	WRITE (STRAR(1),'(A,X,I3,X,A,X,I3)')
c     :     'Binning parameters were: START=',
c     :     INT(BASEAXAR),' SCALE=',INT(SCALAXAR)
c	CALL AST2XSP_WSTRSF(LUPHA, STRAR, 1, IERRSF)
c
*
* Now deal with 'source info' package.
*

* Read in info from HDS file.
* Have to get locator to array cell.
C	IF (INDEX(INSTMNT, 'WFC') .GT. 0 ) THEN
	  STRTIME = 1.0E6
	  STPTIME = 1.1E6
C	ELSE
C	  CALL DAT_CELL(LOCTIME,1,1,CELL,STATUS)
C	  CALL CMP_GET0D(CELL, 'START', STRTIME, STATUS)
C	  CALL CMP_GET0D(CELL, 'STOP',  STPTIME, STATUS)
C	ENDIF
	CALL CMP_GET0D(LOCHDR, 'BASE_TAI', BTAIDAYS, STATUS)
	CALL CMP_GET0I(LOCHDR, 'EQUINOX', EQUINOX, STATUS)
	IF (EQUINOX .EQ. 1950) THEN
		EQFLAG=0
	ELSE
		EQFLAG=1 ! Any old value.
	ENDIF
	CALL CMP_GET0C(LOCHDR, 'TARGET', SNAME, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_ANNUL(STATUS)
        ENDIF
*
* No of secs between 1/1/1972 and 1/1/1980.
	BSE72_80=2922.*24.*60.*60.
	BTAISECS=BTAIDAYS*24.*60.*60.
*
* Ensure that we get a positive start and stop time
        BTAISECS = MAX (BTAISECS, BSE72_80)
*
* Output to PHA file.
	PKGTYP= 'source info'
	ALL.SINFO.EPOCH=  0.0 ! Observation epoch (1/1/1980).
	ALL.SINFO.START=  STRTIME+BTAISECS-BSE72_80 ! Obs start time (secs).
	ALL.SINFO.STOP=   STPTIME+BTAISECS-BSE72_80 ! Obs stop time (secs).
	ALL.SINFO.SYSTEM= EQFLAG   ! 0 if RA & DEC 1950 Epoch.
	ALL.SINFO.VECT(1)= 0.   ! Posn of source (dummy value).
	ALL.SINFO.VECT(2)= 0.
	ALL.SINFO.VECT(3)= 0.
	ALL.SINFO.NAME =  SNAME    ! Name of source.
	CALL AST2XSP_WPKHSF(LUPHA, PKGTYP, 0, 0, INFOAR,
     &     ALL.SINFO, LEN_SOURCE_INFO, IERRSF)
	IF (IERRSF.NE.0) THEN
	  CALL MSG_PRNT( 'Problem with PHA source info.')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF
*
* Deal with 'ass. files' package.

	PKGTYP = 'ass. files'
* Header first.
	CALL AST2XSP_WPKHSF(LUPHA, PKGTYP, 0, 3, INFOAR, 0, 0, IERRSF)
	IF (IERRSF.NE.0) THEN
	  CALL MSG_PRNT( 'Problem writing PHA ass. files header')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF

	STRAR(1) = 'none' ! Def. bckgrnd sub file/'NONE'/.
	STRAR(2) = '%match%' ! Def. Dect. response file/%match%.
	STRAR(3) = 'none' ! Corrections file/NONE/.
* Write ass. files package.
	CALL AST2XSP_WSTRSF(LUPHA, STRAR, 3, IERRSF)
	IF (IERRSF.NE.0)  THEN
	  CALL MSG_PRNT( 'Problem writing PHA ass. files header')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF

*
* Now deal with 'file info'.
*
* Read in info from HDS file first.
	CALL CMP_GET0R(LOCHDR, 'EXPOSURE_TIME', INTGTIME, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           INTGTIME=1000.0
           CALL ERR_ANNUL(STATUS)
        ENDIF
*
* Output to PHA file.
	PKGTYP = 'file info'
        IF (BASEAXAR .GT. 1.0 .OR. SCALAXAR .GT. 1.0) THEN
           WRITE (STRING,'(A,X,I3,X,I3)') INSTMNT(1:CHR_LEN(INSTMNT)),
     :            NINT(BASEAXAR), NINT(SCALAXAR)
           ALL.FINFO.DETID=STRING(1:CHR_LEN(STRING))
        ELSE
	   ALL.FINFO.DETID = INSTMNT
        END IF
	ALL.FINFO.NBIN  = DIMAXARR
	PHABINS         = ALL.FINFO.NBIN    ! No of data bins.
	ALL.FINFO.NCHAN = DIMAXARR ! No of channels.
	NCHAN           =ALL.FINFO.NCHAN  ! Unbinned channels (=binned).
	ALL.FINFO.T     =INTGTIME  ! Integration time (secs).
	ALL.FINFO.A     =GEOMAREA  ! On source area (cm**2).
	ALL.FINFO.DEL = 1.000  ! Background scale factor.
	ALL.FINFO.COR = 0.000  ! Correction scale factor(0 if none).
	ALL.FINFO.INDEX(1) = 0.00000  ! Indicies.  Are spare.
	ALL.FINFO.INDEX(2) = 0.00000
	ALL.FINFO.INDEX(3) = 0.00000
* Write file info package header.
	CALL AST2XSP_WPKHSF(LUPHA, PKGTYP, 0, 0, INFOAR, ALL.FINFO,
     &             LEN_FILE_INFO, IERRSF)
	IF (IERRSF.NE.0) THEN
	  CALL MSG_PRNT( 'Problem writing PHA file info package')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF
*
* Now deal with grouping.
*
	PKGTYP='grouping   '
	DO I=1,NCHAN
	  IF ( I .LT. NGIGNORE) THEN
		ALL.GROUPING(I:I)=GIGNORE
	  ELSE
		ALL.GROUPING(I:I)=GBEGIN
	  ENDIF
	ENDDO
* Write to PHA file.
	CALL AST2XSP_WPKHSF(LUPHA, PKGTYP, 0, 0, INFOAR, ALL.BUFFER,
     &              NCHAN, IERRSF)
	IF (IERRSF.NE.0) THEN
	  CALL MSG_PRNT( 'Problem writing PHA grouping package')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF

*
* Now deal with the data packages.
* For moment assume have only 'pha per sec'.
* Get data from HDS file first.
*
* If a 1-d array just get the data
        IF (.NOT. TWOD) THEN
	   CALL CMP_GET1R(LOCIN, 'DATA_ARRAY', PHABINS,
     &                 PHACNTS, PHABINS, STATUS)
	   CALL CMP_GET1R(LOCIN, 'VARIANCE', PHABINS,
     &                 VARPHA, PHABINS, STATUS)
        ELSE
           CALL BDA_MAPDATA(LOCIN, 'READ', DPTR, STATUS)
           CALL BDA_MAPVAR(LOCIN, 'READ', VPTR, STATUS)
*
           IF (STATUS .NE. SAI__OK) THEN
              CALL MSG_PRNT('Error reading data array or variance')
              GOTO 999
           ENDIF
*
* Set min and max values
           DO LP=1,4
              ORD(LP)=LP
              IF (LP .EQ. RADIM) THEN
                 AMIN(LP)=SLICE
                 AMAX(LP)=SLICE
                 ODIMS(LP)=1
              ELSE
                 AMIN(LP)=1
                 AMAX(LP)=DIMS(LP)
                 ODIMS(LP)=DIMS(LP)
              ENDIF
           ENDDO
*
           ODIMS(PHDIM)=MAXCHAN
*
* Copy the slice that you want into the output array
           CALL DTA_COPYSLICER(DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     &                 %val(DPTR), AMIN, AMAX, ORD, ODIMS(1), ODIMS(2),
     &                 ODIMS(3), ODIMS(4), PHACNTS)
*
           CALL DTA_COPYSLICER(DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     &                 %val(VPTR), AMIN, AMAX, ORD, ODIMS(1), ODIMS(2),
     &                 ODIMS(3), ODIMS(4), VARPHA)
        ENDIF
*
* Now write to PHA file.
	PKGTYP='pha per sec '
	DO K = 1, PHABINS
	  ALL.PHA(K) = PHACNTS(K)  ! Counts for bin/channel.
	  ERR(K) = SQRT(VARPHA(K)) ! Error on counts.
	END DO
* Write out 'pha per sec' package.
	CALL AST2XSP_WPKHSF(LUPHA, PKGTYP, 0, 0, INFOAR, ALL.PHA,
     &              PHABINS*4, IERRSF)
	IF (IERRSF.NE.0) THEN
	  CALL MSG_PRNT( 'Problem writing PHA per sec. package')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF

* Write out 'pha errors' package.
	CALL AST2XSP_WPKHSF(LUPHA, 'pha errors', 0, 0, INFOAR, ERR,
     &               PHABINS*4, IERRSF)
	IF (IERRSF.NE.0) THEN
	  CALL MSG_PRNT( 'Problem writing PHA errors package')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF
*
* Now deal with HDS and Response files correlation.
*
	LURSP = 31
	OPEN(UNIT=LURSP, FILE=OUTRSP(1:CHR_LEN(OUTRSP))/
     &            /'.rsp', STATUS='NEW', FORM='UNFORMATTED')

	IDREC        = 'SF01'              ! Define ID record.
	IDREC(5:16)  = 'XSPEC rspnse'
	IDREC(17:24) = ' '
	IDREC(25:25) = ' ' ! Assume no template package.
	IDREC(26:26) = ' ' ! Assume no history package.
	IDREC(27:)   = ' by AST2XSP at '//STIME//' '//SDATE
	WRITE(LURSP) IDREC
*
* Now write some history
*
c	WRITE (STRAR(1),'(A,X,I3,X,A,X,I3)')
c     :     'Binning parameters were: START=',
c     :     INT(BASEAXAR),' SCALE=',INT(SCALAXAR)
c	CALL AST2XSP_WSTRSF(LUPHA, STRAR, 1, IERRSF)
c	CALL AST2XSP_WSTRSF(LUPHA, STRAR, 1, IERRSF)

*
* Now deal with 'detect info ' package.
* Read in values from HDS file first to get number of energy
* ranges and value of lowest energy limit.
*

	CALL CMP_GET1I(LOCENGY, 'DATA_ARRAY', SIZARR1,
     &                 ENGYARR, SIZARR1, STATUS)
	CALL CMP_GET1R(LOCENGY, 'ENERGY_BOUNDS', SIZENBS, ENBNDSA,
     &                 SIZENBS, STATUS)

        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error reading energy info.')
           GOTO 999
        ENDIF
*
	PKGTYP = 'detect info '
        IF (BASEAXAR .GT. 1.0 .OR. SCALAXAR .GT. 1.0) THEN
           XXX.DINFO.DETID=STRING(1:CHR_LEN(STRING))
        ELSE
	   XXX.DINFO.DETID = INSTMNT
        END IF
c	XXX.DINFO.DETID = INSTMNT     ! Detector ID.
	XXX.DINFO.NBIN  = DIMAXARR    ! No PHA entries.
	XXX.DINFO.NCHAN = DIMAXARR    ! No PHA channels.
	NCHAN = XXX.DINFO.NCHAN
	XXX.DINFO.RSPAREA = 1.0  ! Rsp effect. area - should be 1.
	XXX.DINFO.NRANGE  = ENGYARR(SIZARR1) ! No of energy ranges.
	NRANGES = XXX.DINFO.NRANGE
	XXX.DINFO.E0 = ENBNDSA(1)   ! Mtrx low energ. lim.
	CALL AST2XSP_WPKHSF(LURSP, PKGTYP, 0, 0, INFOAR,
     &	            XXX.DINFO, LEN_DETECT_INFO, IERRSF)
	IF (IERRSF.NE.0) THEN
	  CALL MSG_PRNT( 'Problem writing RSP detect info.')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF

*
* Do not need to write a grouping package as NBIN = NCHAN.
*
* Now deal with chanel bound package.
* First read in appropriate arrays from HDS file.
*
	CALL CMP_GET1R(LOCCHAN, 'CHANNEL_SPEC', XXX.DINFO.NBIN,
     &                 CHNSPEC, XXX.DINFO.NBIN, STATUS)
	CALL CMP_GET1R(LOCCHAN, 'CHANNEL_BOUNDS', XXX.DINFO.NBIN+1,
     &	               CHNBNDS, XXX.DINFO.NBIN+1, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error reading channel info.')
           GOTO 999
        ENDIF
*
* Write to RSP file.
	PKGTYP='chanel bound'
	DO I=1, NCHAN
	  XXX.CHANEN(1,I) = CHNBNDS(I)
	  XXX.CHANEN(2,I) = CHNBNDS(I+1)
	ENDDO
* Write info to RSP file.
	CALL AST2XSP_WPKHSF(LURSP, PKGTYP, 0, 0, INFOAR, XXX.CHANEN,
     &             NCHAN*8, IERRSF)

	IF (IERRSF.NE.0) THEN
	  CALL MSG_PRNT( 'Problem writing RSP channel bound package')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF


*
* Now deal with response package.
* Read in correct arrays from HDS file.
* Note already read in LOCENGY DATA_ARRAY at detect info.
* Get size of arrays needed to build response matrix.
* SIZARR1=SIZARR2=SIZARR3.
*

	CALL DAT_FIND(LOCCHAN,  'DATA_ARRAY', LOCARR2, STATUS)
	 IF (STATUS.NE.0) STOP 'Problem with HDS object: DATA_ARRAY.'
	CALL DAT_FIND(LOCRSP,   'DATA_ARRAY', LOCARR3, STATUS)
	 IF (STATUS.NE.0) STOP 'Problem with HDS object: DATA_ARRAY.'
	CALL DAT_SIZE(LOCARR2, SIZARR2, STATUS)
	CALL DAT_SIZE(LOCARR3, SIZARR3, STATUS)

	CALL CMP_GET1I(LOCCHAN, 'DATA_ARRAY', SIZARR2,
     &                 CHANARR, SIZARR2, STATUS)
	CALL CMP_GET1R(LOCRSP, 'DATA_ARRAY', SIZARR3,
     &                 RSPARR, SIZARR3, STATUS)

* Write to RSP file.
	PKGTYP = 'response   '
* Header first.
	CALL AST2XSP_WPKHSF(LURSP, PKGTYP, 0, NRANGES*3,
     &                                 INFOAR, ' ',0, IERRSF)
	IF (IERRSF.NE.0) THEN
	  CALL MSG_PRNT( 'Problem writing RSP response header')
          STATUS = SAI__ERROR
          GOTO 999
	ENDIF

* Need to take account of HDS Energy Index not starting at 1.
	DO IP=1,ENGYARR(1)-1
	  RESP.E=ENBNDSA(IP+1)
c      print *,resp.e
	  RESP.NENTRY=0
	  RESP.NGROUP=0
	  CALL AST2XSP_WSUBSF(LURSP,RESP,LEN_RESP_DESCR,IERRSF)
	  CALL AST2XSP_WSUBSF(LURSP, BINRNG, 1, IERRSF)
	  CALL AST2XSP_WSUBSF(LURSP, INRESP, 1, IERRSF)
	ENDDO

* Now start on significant energy indexes.

	IP = 1   ! Reset array element pointer
	DO IE=ENGYARR(1),NRANGES
	  RESP.E=ENBNDSA(IE+1) ! Upper energy limit for range.
c      	print *,ie,resp.e
* Read in ENGYARR until index changes.
* These are contiguous values ie 11, 11, 11, 12, 12, 12, 13 etc not
* 11, 11, 11, 13, 13, 13, etc.

	  RESP.NENTRY = 1 ! Number of response entries.
	  IFLAG = 0       ! Note when change in index number.
	  I = IP
	  DO WHILE (IFLAG .EQ. 0 .AND. I .LE. SIZARR1-1)
	    IF(ENGYARR(I+1) .EQ. ENGYARR(I)) THEN
	      RESP.NENTRY=RESP.NENTRY+1
	      I=I+1
	    ENDIF
	    IF(ENGYARR(I+1) .GT. ENGYARR(I)) THEN
	      IFLAG=1  ! Get out of loop.
	    ENDIF
	  ENDDO

* Now see what channels are affected.
	  IFLAG=0 ! Checks when a group has been found.
	  RESP.NGROUP=1 ! No. of channel groups.
*
	  IF (INDEX(INSTMNT, 'WFC') .GT. 0 .OR.
     & 	              INDEX(INSTMNT, 'LE') .GT. 0 ) THEN
	     BINRNG(1,RESP.NGROUP) = CHANARR(IP+RESP.NENTRY-1)
	  ENDIF
*
* Change to cater for the case where only one response entry applies
* to this energy
          IF (RESP.NENTRY .EQ. 1) THEN
	      BINRNG(1,RESP.NGROUP)=CHANARR(IP) ! Fst chan in range.
	      BINRNG(2,RESP.NGROUP)=CHANARR(IP) ! Fst chan in range.
          ELSE
	     DO J=IP,IP+RESP.NENTRY-2
	       IF (IFLAG .EQ. 0) THEN
	         BINRNG(1,RESP.NGROUP)=CHANARR(J) ! Fst chan in range.
	         IFLAG = 1
	       ENDIF
	       IF ( (CHANARR(J+1)-CHANARR(J)) .GT. 1 ) THEN
                 BINRNG(2,RESP.NGROUP) = CHANARR(J) ! Lst chan in rnge.
	         RESP.NGROUP=RESP.NGROUP+1
	         IFLAG=0
	       ENDIF
	     ENDDO
	     BINRNG(2,RESP.NGROUP)=CHANARR(IP+RESP.NENTRY-1)
          ENDIF
*
* Write out response description for this energy level.
	  CALL AST2XSP_WSUBSF(LURSP,RESP,LEN_RESP_DESCR,IERRSF)

* Now get response values.
	  L=0
	  DO K=IP,IP+RESP.NENTRY-1
	    L=L+1
	    INRESP(L) = RSPARR(K)/GEOMAREA
	  ENDDO

* Write out values.
	  LEN=MAX(8*RESP.NGROUP,1)
	  CALL AST2XSP_WSUBSF(LURSP, BINRNG, LEN, IERRSF)
	  LEN=MAX(4*RESP.NENTRY,1)
	  CALL AST2XSP_WSUBSF(LURSP, INRESP, LEN, IERRSF)

	  IP=IP+RESP.NENTRY  ! Next starting place in ENGYARR.
	ENDDO

*
999     CONTINUE
*
	CALL AST_CLOSE()
        CALL AST_ERR(STATUS)

	END


c	wpkhsf	rashafer	8 March 1985
*+ AST2XSP_WPKHSF - SF subroutine to write a package header record
	subroutine AST2XSP_wpkhsf(iunit,pkgtyp,index,nsubs,infoar,
     &                    buffer,len,ierrsf)
c	iunit	i4	i: output unit
c	pkgtyp	c*	i: package type
c	index	i4	i: index
c	nsubs	i4	i: no. of subsidiary records
c	infoar	i4(4)	i: spare
c	buffer	b*	i: buffer
c	len	i4	i: len of buffer
c	ierrsf	i4	i/r: SF error flag
c			15 - Write error
	byte buffer(*)
	character*(*) pkgtyp
	integer*4 idrec(7),infoar(4)
	byte idrecb(28)
	character*12 pkgout
	equivalence(idrec(1),idrecb(1),pkgout)
	integer*4 iunit, index, nsubs, len, ierrsf, ios, lent, i
	pkgout=pkgtyp
	idrec(4)=index
	idrec(5)=nsubs
	idrec(6)=infoar(1)
	idrec(7)=infoar(2)
	lent=28+len
	write(iunit,iostat=ios)-lent,idrec,(buffer(i),i=1,len)
	if(ios.ne.0)then
		if(ierrsf.eq.0)write(*,*)'WPKHSF:Write error:',ios
		ierrsf=15
	else
		ierrsf=0
		end if
	return
	end

*+ AST2XSP_WSTRSF - SF subroutine to write an array of char. strings
	subroutine AST2XSP_wstrsf(iunit,strar,nstr,ierrsf)
c	wstrsf		rashafer 16 March 85
c		SF subroutine to write an array of character strings as
c		auxilary records to the current SF package.  N.B.  If
c		the package header shows an indeterminate no. of records
c		then good practice is for the user to add a terminating record.
c		(one with length 0).
c
c	iunit	i4		i: Write unit
c	strar	c*(nstr)	i: Array of character strings to be written.
c	nstr	i4		i: No. of strings
c	ierrsf	i4		i/r: SF error flag
c			16 -	io write error
	character*(*)strar(*)
	integer*4 istr, nstr, iunit, ierrsf, lc, CHR_LEN, ios
	do istr=1,nstr
		lc=max(CHR_LEN(strar(istr)),1)
		write(iunit,iostat=ios)lc,strar(istr)(:lc)
		if(ios.ne.0)then
			if(ierrsf.eq.0)write(*,*)'WSTRSF: Write i/o error ',
     &			  ios,' while porcessing string ',istr
			ierrsf=16
			return
			end if
		end do
	return
	end

*+ AST2XSP_WSUBSF - SF subroutine to write a single subsidiary record
	subroutine AST2XSP_wsubsf( unit, buffer, length, ierrsf)
c		rashafer 16 april 1986
c	SF routine to write a single subsidiary record to the current
c	package
c	unit	i4	i: unit no.
c	buffer	b(length)	i: the buffer
c	length	i4	i: the length of the buffer to be written
c	ierrsf	i4	i/r: the error flag
c			16 - i/o write error
c			1 - 0 length record for subsidiary (a 1 byte record
c			is actually written.  Use TERMSF to terminate the
c			package if part of an indeterminate no. of records
c			package.
	integer*4 unit,length,ierrsf
	byte buffer(*)
	integer*4 tmplen,i,ios
	if(length.le.0)then
	    if(ierrsf.eq.0)
     &         write(*,*)'WSUBSF: Attempt to write 0 length record'
	    tmplen=1
	    ierrsf=1
	else
	    tmplen=length
	    end if
	write(unit,iostat=ios)tmplen,(buffer(i),i=1,tmplen)
	if(ios.ne.0)then
	    if(ierrsf.eq.0)write(*,*)'WSUBSF: Write I/O error:',ios
	    ierrsf=16
	    end if
	return
	end
