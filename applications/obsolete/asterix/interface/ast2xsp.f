*+  AST2XSP - Converts Asterix spectral files to XSPEC format
       SUBROUTINE AST2XSP( STATUS )
*
*    Description :
*
*      Program converts an ASTERIX HDS spectrum file into a form
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
*
*     Paul McGale (XRA - University of Leicester).
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*        Feb 91 : Original
*        Apr 91 : Adamised version (LTVAD::RDS)
*      7 Nov 91 : Allows bined up data from XRT to be handled (XMV::RMJ)
*        Jun 92 : Takes a slice from a spectral_series and
*                 works on EXOSAT LE data (LTVAD::RDS)
*        Aug 92 : Fixed a bug in the slicing (LTVAD::RDS)
*        Apr 93 : Solved a problem which occured when an energy had a
*                 response in only one PH bin (LTVAD::RDS, XMV::RMJ)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     24 Feb 95 : V1.8-1 Rewrite for XANADU FITS formats. No longer needs
*                        linking against XANADU (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*
*    Local constants :
*
      INTEGER			MAXCOL
        PARAMETER		( MAXCOL = 6 )
	INTEGER  MAXELS, MAXENR, MAXCHAN
	PARAMETER (MAXELS=500000, MAXENR=10000,MAXCHAN=1024)
*
*    Local Variables :
*
	CHARACTER*132  INFILE, OUTPHA, OUTRSP
	CHARACTER*(DAT__SZLOC)  ILOC, LOCASTX, LOCHDR, LOCINST
	CHARACTER*(DAT__SZLOC)  LOCSRT, LOCTIME, CELL
	CHARACTER*(DAT__SZLOC)  LOCENGY, LOCENRSP, LOCCHAN, LOCENBS
	CHARACTER*(DAT__SZLOC)  LOCRSP, LOCE
	CHARACTER*40  SNAME,INSTMNT
        CHARACTER*80  STRING

      CHARACTER*8		TTYPE(MAXCOL)		! Column names
      CHARACTER*8		TFORM(MAXCOL)		! Column types
      CHARACTER*20		TUNIT(MAXCOL)		! Column units

*     Relate to HDS files.
	INTEGER DIMAXARR
	INTEGER ENGYARR(MAXELS), CHANARR(MAXELS)
	DOUBLE PRECISION  STRTIME, STPTIME, BTAIDAYS,BTAISECS
	REAL  INTGTIME, CHNSPEC(MAXCHAN)
	REAL  BASEAXAR, CHNBNDS(MAXCHAN+1)
	REAL  RSPARR(MAXELS), ENBNDSA(MAXENR+1), SCALAXAR
        LOGICAL INPRIM
        LOGICAL OK,TWOD
        INTEGER NDIM,DIMS(4),ODIMS(4),AMIN(4),AMAX(4),ORD(4)
        INTEGER E_AX,RADIM,SLICE,LUN
        INTEGER DPTR,VPTR,LP,NFIELDS,OLEN,WPTR,IFID,BSIZE,APtr

      INTEGER			DETID			! Detector configuration
      INTEGER			PIXID, PRJID, SYSID	! Astrometry details
      INTEGER			RMFID, ARFID		! Input response bits
      INTEGER			OPHA			! Output PHA dataset
      INTEGER			ORSP			! Output RMF dataset
      INTEGER			TIMID			! Timing info

      LOGICAL			EXPCOR			! Corrected spectrum?

*     Relate to XSPEC files.
	CHARACTER IDREC*78, PKGTYP*12, STRAR(3)*72, SDATE*9
	CHARACTER STIME*8
	CHARACTER GBEGIN, GIGNORE
	INTEGER  IERRSF, INFOAR(4), LEN, NGIGNORE
	INTEGER  NBINIG
	INTEGER  PHABINS, NCHAN, NRANGES, BINRNG(2,MAXCHAN)
        INTEGER KDOT
	REAL  ERR(MAXCHAN), INRESP(MAXCHAN)
	DATA INFOAR/4*0/
*     Relate to main program.
        CHARACTER*80 PATH
	INTEGER I, J, K, L, SIZARR1, SIZARR2, SIZARR3
	INTEGER SIZENBS, IP, IFLAG, IE, NLEV
	REAL  GEOMAREA
	DOUBLE PRECISION  BSE72_80
*
*    Local data :
*    Version :
      CHARACTER*30		VERSION
        PARAMETER 		( VERSION = 'AST2XSP version 1.8-1' )
*-

*    Check status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise:
      CALL MSG_PRNT( VERSION )
      CALL AST_INIT()

*    Get input file name
      CALL USI_TASSOCI( 'INP', '*', 'READ', IFID, STATUS )
      CALL ADI1_GETLOC( IFID, ILOC, STATUS )

*    Find name of input file
      CALL ADI_FTRACE( IFID, NLEV, PATH, INFILE, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Get rid of the file extension
      KDOT = MAX(INDEX(INFILE,'.sdf'),INDEX(INFILE,'.SDF'))
      IF ( KDOT .GT. 0 ) THEN
        CALL USI_DEF0C( 'OUT', INFILE(:KDOT-1), STATUS )
      END IF
      CALL USI_GET0C( 'OUT', OUTPHA, STATUS )
      OLEN = CHR_LEN(OUTPHA)
      OUTPHA = OUTPHA(:OLEN)//'.pha'
      OUTRSP = OUTPHA(:OLEN)//'.rsp'
      OLEN = OLEN + 4

*  Get hardware configuration and astrometry
      CALL DCI_GETID( IFID, DETID, STATUS )
      CALL WCI_GETIDS( IFID, PIXID, PRJID, SYSID, STATUS )
      CALL TCI_GETID( IFID, TIMID, STATUS )

*  Get instrument name
      CALL ADI_CGET0C( DETID, 'Instrument', INSTMNT, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Error reading instrument value')
        GOTO 99
      END IF

*  Get data dimensions
      CALL BDI_CHKDATA( IFID, OK, NDIM, DIMS, STATUS )
      IF (STATUS .NE. SAI__OK .OR. .NOT. OK) THEN
        CALL MSG_PRNT('Error reading data array')
        GOTO 99
      END IF

* If a 2-d XRT file assume that the other dimension is radial bins
        IF (NDIM .EQ. 2 .AND. INDEX(INSTMNT, 'XRT') .NE. 0) THEN
*
*   find which axis is the PH axis
           CALL AXIS_TFIND( IFID, 'PHA', NDIM, E_AX, STATUS)
*
           IF (STATUS .NE. SAI__OK) GOTO 99
*
*   set the other axis
           IF (E_AX .EQ. 1) THEN
              RADIM = 2
           ELSEIF (E_AX .EQ. 2) THEN
              RADIM = 1
           ELSE
              CALL MSG_PRNT('** PH axis not found **')
              GOTO 99
           ENDIF
*
*   ask user which slice he wants
           CALL MSG_SETI('NSPEC', DIMS(RADIM))
           CALL MSG_PRNT('This file contains ^NSPEC spectra')
*
           CALL USI_GET0I('SLICE', SLICE, STATUS)
*
           IF (STATUS .NE. SAI__OK) GOTO 99
*
           DIMS(3)=1
           DIMS(4)=1
*
           TWOD = .TRUE.
        ELSE
           E_AX=1
           TWOD = .FALSE.
           SLICE = 0
        ENDIF

*   Asterix instrument box
	CALL BDA_LOCINSTR(ILOC, LOCINST, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error finding Asterix instrument box')
           GOTO 99
        ENDIF
*
	CALL DAT_FIND(LOCINST, 'SORT', LOCSRT, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error finding Asterix sort box')
           GOTO 99
        ENDIF

*    Check axes present
      CALL BDI_CHKAXES( IFID, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_PRNT('Error finding Asterix axes')
         GOTO 99
      ENDIF

	CALL DAT_FIND(LOCASTX, 'ENERGY_RESP', LOCE, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error finding Asterix energy response')
           GOTO 99
        ENDIF

*      Locate response elements
        CALL ERI_GETIDS( IFID, SLICE, RMFID, ARFID, STATUS )

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
c        IF (STATUS .NE. SAI__OK) THEN
c           CALL MSG_PRNT('Error reading energy response')
c           GOTO 99
c        ENDIF
*
* Get the base and scale values of the PHA axis
        CALL BDI_GETAXVAL( IFID, E_AX, BASEAXAR, SCALAXAR,
     :                     DIMAXARR, STATUS )
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error checking Asterix file')
           GOTO 99
        ENDIF


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
             GOTO 99
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
              GOTO 99
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
           GOTO 99
	ENDIF
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error reading times from SORT box')
           GOTO 99
        ENDIF

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
C	CALL CMP_GET0D(LOCHDR, 'BASE_TAI', BTAIDAYS, STATUS)
CC	CALL CMP_GET0C(LOCHDR, 'TARGET', SNAME, STATUS)
*
C        IF (STATUS .NE. SAI__OK) THEN
C           CALL ERR_ANNUL(STATUS)
C        ENDIF
*
* No of secs between 1/1/1972 and 1/1/1980.
C	BSE72_80=2922.*24.*60.*60.
C	BTAISECS=BTAIDAYS*24.*60.*60.
*
* Ensure that we get a positive start and stop time
C        BTAISECS = MAX (BTAISECS, BSE72_80)
*
* Output to PHA file.
	PKGTYP= 'source info'
c	ALL.SINFO.EPOCH=  0.0 ! Observation epoch (1/1/1980).
c	ALL.SINFO.START=  cSTRTIME+BTAISECS-BSE72_80 ! Obs start time (secs).
c	ALL.SINFO.STOP=   STPTIME+BTAISECS-BSE72_80 ! Obs stop time (secs).
c	ALL.SINFO.NAME =  SNAME    ! Name of source.
C	CALL AST2XSP_WPKHSF(LUPHA, PKGTYP, 0, 0, INFOAR,
C     &     ALL.SINFO, LEN_SOURCE_INFO, IERRSF)
c	IF (IERRSF.NE.0) THEN
c	  CALL MSG_PRNT( 'Problem with PHA source info.')
c          STATUS = SAI__ERROR
c          GOTO 99
c	ENDIF
*
* Deal with 'ass. files' package.

C	PKGTYP = 'ass. files'
* Header first.
C	CALL AST2XSP_WPKHSF(LUPHA, PKGTYP, 0, 3, INFOAR, 0, 0, IERRSF)
C	IF (IERRSF.NE.0) THEN
C	  CALL MSG_PRNT( 'Problem writing PHA ass. files header')
C          STATUS = SAI__ERROR
C          GOTO 99
C	ENDIF

C	STRAR(1) = 'none' ! Def. bckgrnd sub file/'NONE'/.
C	STRAR(2) = '%match%' ! Def. Dect. response file/%match%.
C	STRAR(3) = 'none' ! Corrections file/NONE/.
* Write ass. files package.
C	CALL AST2XSP_WSTRSF(LUPHA, STRAR, 3, IERRSF)
C	IF (IERRSF.NE.0)  THEN
C	  CALL MSG_PRNT( 'Problem writing PHA ass. files header')
C          STATUS = SAI__ERROR
C          GOTO 99
C	ENDIF

*
* Now deal with 'file info'.
*
* Read in info from HDS file first.
      CALL ADI_CGET0R( TIMID, 'Exposure', INTGTIME, STATUS )
*
C        IF (STATUS .NE. SAI__OK) THEN
C           INTGTIME=1000.0
C           CALL ERR_ANNUL(STATUS)
C        ENDIF
*
* Output to PHA file.
c	PKGTYP = 'file info'
c        IF (BASEAXAR .GT. 1.0 .OR. SCALAXAR .GT. 1.0) THEN
c           WRITE (STRING,'(A,X,I3,X,I3)') INSTMNT(1:CHR_LEN(INSTMNT)),
c     :            NINT(BASEAXAR), NINT(SCALAXAR)
c           ALL.FINFO.DETID=STRING(1:CHR_LEN(STRING))
c        ELSE
c	   ALL.FINFO.DETID = INSTMNT
c        END IF
c	ALL.FINFO.NBIN  = DIMAXARR
c	PHABINS         = ALL.FINFO.NBIN    ! No of data bins.
c	ALL.FINFO.NCHAN = DIMAXARR ! No of channels.
c	NCHAN           =ALL.FINFO.NCHAN  ! Unbinned channels (=binned).
c	ALL.FINFO.T     =INTGTIME  ! Integration time (secs).
c	ALL.FINFO.A     =GEOMAREA  ! On source area (cm**2).
c	ALL.FINFO.DEL = 1.000  ! Background scale factor.
c	ALL.FINFO.COR = 0.000  ! Correction scale factor(0 if none).
c	ALL.FINFO.INDEX(1) = 0.00000  ! Indicies.  Are spare.
c	ALL.FINFO.INDEX(2) = 0.00000
c	ALL.FINFO.INDEX(3) = 0.00000
c* Write file info package header.
C	CALL AST2XSP_WPKHSF(LUPHA, PKGTYP, 0, 0, INFOAR, ALL.FINFO,
C     &             LEN_FILE_INFO, IERRSF)
c	IF (IERRSF.NE.0) THEN
c	  CALL MSG_PRNT( 'Problem writing PHA file info package')
c          STATUS = SAI__ERROR
c          GOTO 99
c	ENDIF
*
* Now deal with grouping.
*
c	PKGTYP='grouping   '
c	DO I=1,NCHAN
c	  IF ( I .LT. NGIGNORE) THEN
c		ALL.GROUPING(I:I)=GIGNORE
c	  ELSE
c		ALL.GROUPING(I:I)=GBEGIN
c	  ENDIF
c	ENDDO
* Write to PHA file.
C	CALL AST2XSP_WPKHSF(LUPHA, PKGTYP, 0, 0, INFOAR, ALL.BUFFER,
C     &              NCHAN, IERRSF)
c	IF (IERRSF.NE.0) THEN
c	  CALL MSG_PRNT( 'Problem writing PHA grouping package')
c          STATUS = SAI__ERROR
c          GOTO 99
c	ENDIF

*    Map the data and error arrays
      CALL BDI_MAPDATA( IFID, 'READ', DPTR, STATUS )
      CALL BDI_MAPERR( IFID, 'READ', VPTR, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_REP( ' ', 'Error reading data array or variance',
     :                STATUS )
        GOTO 99
      END IF

*    Extract slice if 2-D
      IF ( TWOD ) THEN

*      Set min and max values
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
          END IF
        END DO
        ODIMS(E_AX) = MAXCHAN

*      Copy the slice that you want into the output array
        CALL DYN_MAPR( 1, DIMAXARR, WPTR, STATUS )
        CALL DTA_COPYSLICER( DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                 %VAL(DPTR), AMIN, AMAX, ORD, ODIMS(1), ODIMS(2),
     :                 ODIMS(3), ODIMS(4), %VAL(WPTR) )
        DPTR = WPTR
        CALL DYN_MAPR( 1, DIMAXARR, WPTR, STATUS )
        CALL DTA_COPYSLICER( DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                 %VAL(VPTR), AMIN, AMAX, ORD, ODIMS(1), ODIMS(2),
     :                 ODIMS(3), ODIMS(4), %VAL(WPTR) )
        VPTR = WPTR

      END IF

*    Open the response file
      CALL ADI_FCREAT( OUTPHA(:OLEN)//'%fits', ADI__NULLID, OPHA,
     :                 STATUS )

*    Open new PHA file. Has a null primary header, into which we shove
*    most of the ancillary keywords

*    Write loads'a'keywords
C      CALL ADI2_PDATE( OPHA, STATUS )
      CALL ADI2_PKEY0C( OPHA, ' ', 'CONTENT', 'SPECTRUM',
     :             'Spectrum file contains', STATUS )
      CALL ADI2_PKEY0C( OPHA, ' ', 'CREATOR', VERSION,
     :             'Creator of this file', STATUS )
      CALL ADI2_PKEY0R( OPHA, 'SPECTRUM', 'EXPOSURE', INTGTIME,
     :             'Exposure time', STATUS )
      CALL ADI2_PKEY0D( OPHA, 'SPECTRUM', 'AREASCAL', 1.0D0,
     :             'Area scaling factor', STATUS )
      CALL ADI2_PKEY0R( OPHA, 'SPECTRUM', 'BACKSCAL', 1.0,
     :             'Background scaling factor', STATUS )
      CALL ADI2_PKEY0R( OPHA, 'SPECTRUM', 'CORRSCAL', 1.0,
     :             'Correction', STATUS )

*    Is the spectrum corrected?
      CALL PRF_GET( IFID, 'CORRECTED.EXPOSURE', EXPCOR, STATUS )

*    Create table names, types and units
      TTYPE(1) = 'CHANNEL'
      TFORM(1) = 'I'
      CALL BDI_GETAXUNITS( IFID, E_AX, TUNIT(1), STATUS )
      IF ( EXPCOR ) THEN
        TTYPE(2) = 'RATE'
        TFORM(2) = 'E'
      ELSE
        TTYPE(2) = 'COUNTS'
        TFORM(2) = 'I'
      END IF
      CALL BDI_GETUNITS( IFID, TUNIT(2), STATUS )
      TTYPE(3) = 'STAT_ERR'
      TFORM(3) = 'E'
      TUNIT(3) = TUNIT(2)
      NFIELDS = 3
      TTYPE(4) = 'QUALITY'
      TFORM(4) = 'I'
      TUNIT(4) = ' '

*    Create extension for spectrum
      CALL ADI2_DEFBTB( OPHA, 'SPECTRUM', DIMAXARR, NFIELDS, TTYPE,
     :                  TFORM, TUNIT, 0, STATUS )
      CALL ADI2_POGIPK( OPHA, 'SPECTRUM', 'SPECTRUM',
     :             ' ', ' ', ' ', ' ', ' ',
     :               STATUS )
      CALL ADI2_PKEY0I( OPHA, 'SPECTRUM', 'TLMIN1', 1,
     :              'Lowest legal channel number', STATUS )
      CALL ADI2_PKEY0I( OPHA, 'SPECTRUM', 'TLMAX1', DIMAXARR,
     :              'Highest legal channel number', STATUS )
      CALL ADI2_PKEY0C( OPHA, 'SPECTRUM', 'RESPFILE', OUTRSP(:OLEN),
     :             'Redistribution matrix file (RMF)', STATUS )
      CALL ADI2_PKEY0C( OPHA, 'SPECTRUM', 'PHAVERSN', '1992a',
     :             'OGIP classification of FITS format style', STATUS )

*    Write astrometry
      CALL WCI_PUTIDS( OPHA, PIXID, PRJID, SYSID, STATUS )
      CALL DCI_PUTID( OPHA, DETID, STATUS )

*    Write the spectrum
      CALL DYN_MAPR( 1, DIMAXARR, APTR, STATUS )
      CALL ARR_REG1R( BASEAXAR, SCALAXAR, DIMAXARR, %VAL(APTR), STATUS )
      CALL ADI2_GETLUN( OPHA, LUN, STATUS )
      CALL FTPCLE( LUN, 1, 1, 1, DIMAXARR, %VAL(APTR), STATUS )
      CALL FTPCLE( LUN, 2, 1, 1, DIMAXARR, %VAL(DPTR), STATUS )
      CALL FTPCLE( LUN, 3, 1, 1, DIMAXARR, %VAL(VPTR), STATUS )

*    Close the PHA file
      CALL ADI_FCLOSE( OPHA, STATUS )

*    Open the response file
      CALL ADI_FCREAT( OUTRSP(:OLEN)//'%fits', ADI__NULLID, ORSP,
     :                 STATUS )

      CALL ERI_PUTIDS( ORSP, RMFID, ARFID, STATUS )
      CALL DCI_PUTID( ORSP, DETID, STATUS )

*
c      CALL ADI2_PKEY0C( ORSP, ' ', 'CREATOR', VERSION, '*', STATUS )

*    Close the response file
      CALL ADI_FCLOSE( ORSP, STATUS )



* Now deal with HDS and Response files correlation.
*
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
           GOTO 99
        ENDIF
*
c	PKGTYP = 'detect info '
c        IF (BASEAXAR .GT. 1.0 .OR. SCALAXAR .GT. 1.0) THEN
c           XXX.DINFO.DETID=STRING(1:CHR_LEN(STRING))
c        ELSE
c	   XXX.DINFO.DETID = INSTMNT
c        END IF
c	XXX.DINFO.DETID = INSTMNT     ! Detector ID.
c	XXX.DINFO.NBIN  = DIMAXARR    ! No PHA entries.
c	XXX.DINFO.NCHAN = DIMAXARR    ! No PHA channels.
c	NCHAN = XXX.DINFO.NCHAN
c	XXX.DINFO.RSPAREA = 1.0  ! Rsp effect. area - should be 1.
c	XXX.DINFO.NRANGE  = ENGYARR(SIZARR1) ! No of energy ranges.
c	NRANGES = XXX.DINFO.NRANGE
c	XXX.DINFO.E0 = ENBNDSA(1)   ! Mtrx low energ. lim.
cC	CALL AST2XSP_WPKHSF(LURSP, PKGTYP, 0, 0, INFOAR,
C     &	            XXX.DINFO, LEN_DETECT_INFO, IERRSF)
c	IF (IERRSF.NE.0) THEN
c	  CALL MSG_PRNT( 'Problem writing RSP detect info.')
c          STATUS = SAI__ERROR
c          GOTO 99
c	ENDIF

* Now deal with response package.
* Read in correct arrays from HDS file.
* Note already read in LOCENGY DATA_ARRAY at detect info.
* Get size of arrays needed to build response matrix.
* SIZARR1=SIZARR2=SIZARR3.
*


* Write to RSP file.
c	PKGTYP = 'response   '
* Header first.
c	CALL AST2XSP_WPKHSF(LURSP, PKGTYP, 0, NRANGES*3,
c     &                                 INFOAR, ' ',0, IERRSF)
c	IF (IERRSF.NE.0) THEN
c	  CALL MSG_PRNT( 'Problem writing RSP response header')
c          STATUS = SAI__ERROR
c          GOTO 99
c	ENDIF

* Need to take account of HDS Energy Index not starting at 1.
c	DO IP=1,ENGYARR(1)-1
c	  RESP.E=ENBNDSA(IP+1)
c      print *,resp.e
c	  RESP.NENTRY=0
c	  RESP.NGROUP=0
c	  CALL AST2XSP_WSUBSF(LURSP,RESP,LEN_RESP_DESCR,IERRSF)
c	  CALL AST2XSP_WSUBSF(LURSP, BINRNG, 1, IERRSF)
c	  CALL AST2XSP_WSUBSF(LURSP, INRESP, 1, IERRSF)
c	ENDDO

* Now start on significant energy indexes.

c	IP = 1   ! Reset array element pointer
c	DO IE=ENGYARR(1),NRANGES
c	  RESP.E=ENBNDSA(IE+1) ! Upper energy limit for range.
c      	print *,ie,resp.e
* Read in ENGYARR until index changes.
* These are contiguous values ie 11, 11, 11, 12, 12, 12, 13 etc not
* 11, 11, 11, 13, 13, 13, etc.

c	  RESP.NENTRY = 1 ! Number of response entries.
c	  IFLAG = 0       ! Note when change in index number.
c	  I = IP
c	  DO WHILE (IFLAG .EQ. 0 .AND. I .LE. SIZARR1-1)
c	    IF(ENGYARR(I+1) .EQ. ENGYARR(I)) THEN
c	      RESP.NENTRY=RESP.NENTRY+1
c	      I=I+1
c	    ENDIF
c	    IF(ENGYARR(I+1) .GT. ENGYARR(I)) THEN
c	      IFLAG=1  ! Get out of loop.
c	    ENDIF
c	  ENDDO

* Now see what channels are affected.
c	  IFLAG=0 ! Checks when a group has been found.
c	  RESP.NGROUP=1 ! No. of channel groups.
*
c 	  IF (INDEX(INSTMNT, 'WFC') .GT. 0 .OR.
c     & 	              INDEX(INSTMNT, 'LE') .GT. 0 ) THEN
c 	     BINRNG(1,RESP.NGROUP) = CHANARR(IP+RESP.NENTRY-1)
c	  ENDIF
c*
c* Change to cater for the case where only one response entry applies
* to this energy
c          IF (RESP.NENTRY .EQ. 1) THEN
c	      BINRNG(1,RESP.NGROUP)=CHANARR(IP) ! Fst chan in range.
c	      BINRNG(2,RESP.NGROUP)=CHANARR(IP) ! Fst chan in range.
c          ELSE
c  	     DO J=IP,IP+RESP.NENTRY-2
c	       IF (IFLAG .EQ. 0) THEN
c	         BINRNG(1,RESP.NGROUP)=CHANARR(J) ! Fst chan in range.
c  	         IFLAG = 1
c	       ENDIF
c	       IF ( (CHANARR(J+1)-CHANARR(J)) .GT. 1 ) THEN
c                 BINRNG(2,RESP.NGROUP) = CHANARR(J) ! Lst chan in rnge.
c	         RESP.NGROUP=RESP.NGROUP+1
c	         IFLAG=0
c	       ENDIF
c	     ENDDO
c   	     BINRNG(2,RESP.NGROUP)=CHANARR(IP+RESP.NENTRY-1)
c          ENDIF
*
* Write out response description for this energy level.
c	  CALL AST2XSP_WSUBSF(LURSP,RESP,LEN_RESP_DESCR,IERRSF)

* Now get response values.
c	  L=0
c	  DO K=IP,IP+RESP.NENTRY-1
c	    L=L+1
c	    INRESP(L) = RSPARR(K)/GEOMAREA
c	  ENDDO

* Write out values.
c	  LEN=MAX(8*RESP.NGROUP,1)
c	  CALL AST2XSP_WSUBSF(LURSP, BINRNG, LEN, IERRSF)
c	  LEN=MAX(4*RESP.NENTRY,1)
c	  CALL AST2XSP_WSUBSF(LURSP, INRESP, LEN, IERRSF)
c
c	  IP=IP+RESP.NENTRY  ! Next starting place in ENGYARR.
c	ENDDO

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
