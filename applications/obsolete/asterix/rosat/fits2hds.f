*+ FITS2HDS  -  converts ascII or bInary FITS DATa Into HDS.
	SUBROUTINE FITS2HDS(STATUS)
*    DescrIptIon :
*     Converts FITS fIles Into HDS, usIng the WIllIam Pence FIO
*     lIbrary.  Based on the prevIous AFITS2HDS thIs program supports
*     all other major DATatypes.  If the ASTERIX parameter Is specIfIed
*     (default) Image fIts fIles are converted wIth the standard ASTERIX
*     header InformatIon appended.
*    EnvIronment parameters :
*    Method :
*     Determine file type.
*     Create an HDS file
*     Read FITS array names
*     Create correspondIng HDS arrays and map them
*     Write FITS DATa into HDS arrays
*    Deficiencies :
*      Handles I*2 as I*4
*    Bugs :
*     <description of any "bugs" which have not been fixed >
*      	Doesn't work correctly when upDATing CHAR*nn tables
*        - this is a possible FITSIO problem.
*    Authors :
*     Jeremy Ashley & Richard Saxton
*    History :
*     10-Nov-1993   -   original
*     17-Jan-1994 (LTVAD::JKA) (v1.01) auto detects FITS file type
*     17-Jan-1994 (LTVAD::JKA) (v1.02) bug fixed in character mapping
*     17-Jan-1994 (LTVAD::JKA) (v1.03) bug fixed in joining similar tables
*     17-Jan-1994 (LTVAD::JKA) (v1.04) uses extname for file extensions
*     23-Jan-1994 (LTVAD::JKA) (v1.05) JOIN defaults to TRUE and EXTNAME
*                               is compared on adjacent tables
*     22-Apr-1994 (LTVAD::JKA) (v1.06) converts ' ' & '.' into '_' for
*                               for column headings & HDS extension names.
*     24-Apr-1994 (LTVAD::JKA) (v1.7-0) for new release of asterix
*     28-Apr-1994 (LTVAD::JKA) (v1.7-1) handles 0 rows in a table
*     05-May-1994 (LTVAD::JKA) (v1.7-2) dummy parameter added to fitsio call
*                 to fool compiler with string lengths. FTGCVS(...
*     23-May-1994 (LTVAD::JKA) (v1.7-3) close BDA common areas for images
*     06-Jun-1994 (LTVAD::JKA) (v1.7-4) correct error in calculating total
*                 exposure time for image and duration.
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant defInitions>
*    Global variables :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
      Include 'XRTLIB(INC_XRTSRT)'
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
      INTEGER CHR_INDEX
        EXTERNAL CHR_INDEX
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
      RECORD /XRT_SCFDEF/ SRT
      CHARACTER*80 HNAME		! Generic name of HDS file(s)
      CHARACTER*(DAT__SZLOC) LOC        ! Locator to HDS file
      CHARACTER*(DAT__SZLOC) TLOC       ! Temporary locator to HDS
      CHARACTER*(DAT__SZLOC) ALOC(512)  ! Locator to HDS array
      INTEGER PNTR(512),TPNTR		! Pointer to mapped HDS array
      CHARACTER*10 FTYPE(512)		! Field types of arrays
      CHARACTER*80 FNAME		! name of fIts file
      INTEGER IUNIT			! Logical unit for FITS file
      INTEGER HTYPE			! FITS header type
      INTEGER FILENO			! File counter
      LOGICAL ASTERIX		        ! Use ASTERIX header info
      LOGICAL SAME                      ! Same arrays as the last time
      LOGICAL FNEW                      ! Create NEW HDS file ?
      LOGICAL HDSOPEN                   ! HDS file open
      REAL*4 BASE(2), SCALE(2)          ! Axis scaling on images
      CHARACTER*5 ORIGIN		! FITS file origin
      INTEGER ISTATUS                   ! Status for FIO routines
      CHARACTER*12 TTYPE(512),TTSAVE(512)
      CHARACTER*10 TUNIT(512),TFORM(512)
      CHARACTER*20 EXTNAME,S_EXTNAME    ! Table extension name
      INTEGER NKEYS,NMORE,BLOCK
      CHARACTER*30 ERRTXT               ! Error message from FIO routines
      INTEGER BITPIX,PCOUNT,GCOUNT
      INTEGER ROWLEN
      INTEGER HROWS			! Number of elements in HDS array
      INTEGER NROWS                     ! Number of rows in a table
      INTEGER TFIELDS                   ! Number of fields in a table
      INTEGER NAXIS                     ! Number of axes in an image
      INTEGER NAXES(512)                ! Dimensions of an image
      INTEGER DTYPE(512)                ! datatype of a field
      INTEGER TBCOL(512)
      INTEGER WIDTH(512)
      INTEGER VARIDAT,REPEAT,COL,NELEMS,II,NLEN
      INTEGER HBEG,HEND,FBEG,FNUM,HSAVE,NSAVE
      CHARACTER*80 CDUM			! Dummy character value
      INTEGER NDUM			! Dummy integer value
      LOGICAL SIMPLE,EXTEND,ANYF,EXISTS
      CHARACTER*6 MAPMODE
      INTEGER MAXRECS			! ROWS READ AT A TIME
      INTEGER NMAT,JJ			! Duplicate column names
      INTEGER CPIX			! Coords / Pixel
      INTEGER IND                       ! Index into names
      INTEGER SLEN                      ! String Length
      LOGICAL JOIN                      ! Join similar tables
      PARAMETER (MAXRECS = 10000)
*    VersIon :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'FITS2HDS Version 1.7-4')
*-
      IF (STATUS.NE.SAI__OK) RETURN

      CALL AST_INIT()
      CALL MSG_PRNT(VERSION)

***   InItIalIse varIables
      ISTATUS = 0
      HDSOPEN = .FALSE.
      FILENO = 0
      FNEW = .TRUE.
      S_EXTNAME = ' '

***   Get Input FITS fIle name
      CALL PAR_GET0C('INPUT',FNAME, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999

***   Open the FITS fIle
      CALL FIO_GUNIT(IUNIT,ISTATUS)
      CALL FTOPEN(IUNIT,FNAME,0,BLOCK,ISTATUS)
      IF (ISTATUS.NE.0) THEN
	 CALL MSG_SETC('FNAM',FNAME)
         CALL MSG_PRNT('** Error opening file ^FNAM **')
         GOTO 999
      ENDIF

***   is header info required
      CALL PAR_GET0L('ASTERIX',ASTERIX,STATUS)

***   Read Image header InformatIon for image data
      CALL FTGKYJ(IUNIT,'NAXIS',NAXIS,CDUM,ISTATUS)
      IF (ASTERIX.AND.NAXIS.NE.0) THEN
         CALL PAR_GET0C('ORIGIN',ORIGIN,STATUS)
         CALL CHR_UCASE(ORIGIN)
*
*        Auto detect data origins
         IF (ORIGIN.EQ.'AUTO') THEN
            CALL RAT_FITSSTYLE(IUNIT,ORIGIN,STATUS)
            CALL PAR_PUT0C('ORIGIN',ORIGIN,STATUS)
         ENDIF
*
*        is header information of a known type
         IF (ORIGIN.EQ.' ') THEN
            ASTERIX = .FALSE.
         ELSE
            CALL MSG_SETC('ORIG',ORIGIN)
            CALL MSG_PRNT('FITS file type is ^ORIG')
         ENDIF
         IF (ASTERIX) THEN
            CALL RAT_RDIMAGE(IUNIT,ORIGIN,HEAD,ISTATUS)
            IF (ISTATUS.NE.0) THEN
               CALL MSG_SETC('FNAM',FNAME)
               CALL MSG_PRNT('** Error reading image header '//
     &                       'information from file ^FNAM **')
***            cancel error and set ignore asterix header information
               ISTATUS = 0
               ASTERIX = .FALSE.
            ENDIF
         ENDIF
      ENDIF

***   Move to fIrst header (default)
      CALL FTMAHD(IUNIT,1,HTYPE,ISTATUS)

***   Get the generIc output fIlename
      CALL PAR_GET0C('OUTPUT',HNAME,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   Get action required for joining similar tables
      CALL PAR_GET0L('JOIN',JOIN,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   Loop for each header
      DO WHILE (ISTATUS.EQ.0)

***      For prImary or Image DATa
         IF (HTYPE.EQ.0) CALL FTGPRH(IUNIT,SIMPLE,BITPIX,NAXIS,
     &       NAXES,PCOUNT,GCOUNT,EXTEND,ISTATUS)
***      For ascII table DATa
         IF (HTYPE.EQ.1) CALL FTGTBH(IUNIT,ROWLEN,NROWS,TFIELDS,
     &	     TTYPE,TBCOL,TFORM,TUNIT,EXTNAME,ISTATUS)
***      for bInary table DATa
         IF (HTYPE.EQ.2) CALL FTGBNH(IUNIT,NROWS,TFIELDS,TTYPE,
     &       TFORM,TUNIT,EXTNAME,VARIDAT,ISTATUS)
         IF ((HTYPE.LT.0).OR.(HTYPE.GT.2)) THEN
             CALL MSG_PRNT('** Unknown FITS header type **')
             GOTO 999
         ENDIF

***      make sure extension name is lower case
         CALL CHR_LCASE(EXTNAME)

***      For non-empty prImary Images
         IF ((HTYPE.EQ.0).AND.(NAXIS.NE.0)) THEN

***         Open hds output fIle
            CALL AOPENHDS(HNAME,' ',FILENO,FNEW,LOC,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999
            HDSOPEN = .TRUE.

***         Create hds real array and map It
            CALL DAT_NEW(LOC,'DATA_ARRAY','_REAL',NAXIS,NAXES,STATUS)
            CALL CMP_MAPV(LOC,'DATA_ARRAY','_REAL','WRITE',
     &         TPNTR, NDUM, STATUS)
***
            IF (STATUS.NE.SAI__OK) THEN
               CALL MSG_PRNT('Error creatIng and mappIng real array')
               GOTO 999
            ENDIF

***         Calculate total number of elements In array
            NELEMS = 1
            DO II = 1,NAXIS
               NELEMS = NELEMS * NAXES(II)
            ENDDO

***         Read thIs column and fIll the array
            CALL FTGPVE(IUNIT,0,1,NELEMS,0,%val(TPNTR),ANYF,ISTATUS)

***         Unmap the DATa array
            CALL CMP_UNMAP(LOC,'DATA_ARRAY',STATUS)

            IF (FNEW.AND.ASTERIX) THEN

***            WrIte AsterIx InformatIon
***            UpDATe the SRT so the exposure tIme Is calculated
               SRT.NTIME = 1
               SRT.MIN_T(1) = HEAD.TSTART(1)
               SRT.MAX_T(1) = HEAD.TEND(HEAD.NTRANGE)

               CALL XSORT_CRE_ASTERIX(LOC,SRT,HEAD,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 999

***            Sort axes
               CPIX  = 1.0
***            multiply scale by 30 (coord increment/pixel) for MPE
               IF (ORIGIN.EQ.'MPE') CPIX = 30
               BASE(1) =  CPIX * HEAD.PIXEL / 3600. * (NAXES(1) / 2 )
               SCALE(1) = CPIX * HEAD.PIXEL / 3600.
               BASE(2) =  CPIX * HEAD.PIXEL / 3600. * (NAXES(2) / 2 )
               SCALE(2) = CPIX * HEAD.PIXEL / 3600.
***            sort out orientation etc.
               SCALE(1) = -SCALE(1)
               BASE(2) = -BASE(2)

C              CALL BDA_INIT()
               CALL BDA_CREAXES(LOC,2,STATUS)
               DO II = 1,2
                  CALL BDA_PUTAXUNITS(LOC,II,'degrees',STATUS )
                  CALL BDA_CREAXVAL(LOC,II,.TRUE.,512,STATUS )
                  CALL BDA_PUTAXVAL(LOC,II,BASE(II),SCALE(II)
     &                                                   ,512,STATUS)
               ENDDO
               CALL BDA_CLOSE()
            ENDIF
         ENDIF

***      For table DATa
         IF ((HTYPE.EQ.1).OR.(HTYPE.EQ.2)) THEN

***         Check If the column names have changed from the last header
            SAME = .TRUE.
            DO II=1,TFIELDS

***            replace any ' ' in field name
               SLEN = CHR_LEN(TTYPE(II))
               DO WHILE (CHR_INDEX(TTYPE(II)(1:SLEN),' ').NE.0)
                  IND = CHR_INDEX(TTYPE(II)(1:SLEN),' ')
                  TTYPE(II)(IND:IND) = '_'
               ENDDO

***            compare with last times column name
               IF (TTYPE(II).NE.TTSAVE(II)) SAME = .FALSE.

***            check no duplicates exist
               NMAT = 0
               DO JJ=1,II-1
                  IF (TTYPE(II).EQ.TTYPE(JJ)) NMAT = NMAT + 1
               ENDDO
               IF (NMAT.NE.0) THEN
***               Should write nmat here
                  TTYPE(II)=TTYPE(II)(1:CHR_LEN(TTYPE(II)))//'_1'
               ENDIF
               TTSAVE(II) = TTYPE(II)
            ENDDO

***         replace and '.' in extension name
            DO WHILE (CHR_INDEX(EXTNAME,'.').NE.0)
               IND = CHR_INDEX(EXTNAME,'.')
               EXTNAME(IND:IND) = '_'
            ENDDO

***         check if this is a new file/extension
            FNEW = .NOT.(SAME.AND.JOIN.AND.(EXTNAME.EQ.S_EXTNAME))
            S_EXTNAME = EXTNAME

***         Create an output HDS fIle
	    CALL AOPENHDS(HNAME,EXTNAME,FILENO,FNEW,LOC,STATUS)
            IF (STATUS.NE.SAI__OK) goto 999
            HDSOPEN = .TRUE.

            DO COL = 1,TFIELDS

***            calculate length of fIeldname
               NLEN = CHR_LEN(TTYPE(COL))

***            Get InformatIon for NEW fIelds
               IF (FNEW) THEN
                  HSAVE = 0
                  HROWS = NROWS
*** a fix for tables with no rows *** jka, 28 april
                  IF (HROWS.EQ.0) THEN
                     CALL MSG_PRNT('** Table has no rows **')
                     GOTO 100
                  ENDIF
***
                  MAPMODE = 'WRITE'
                  CALL FTBNFM(TFORM(COL),DTYPE(COL),REPEAT,
     &                                              WIDTH(COL),ISTATUS)

***               create hds structure
                  IF (DTYPE(COL).EQ.21) THEN
                     CALL DAT_NEW1I(LOC,TTYPE(COL)(1:NLEN),NROWS,STATUS)
                     FTYPE(COL) = '_INTEGER'
                  ELSEIF (DTYPE(COL).EQ.41) THEN
                     CALL DAT_NEW1I(LOC,TTYPE(COL)(1:NLEN),NROWS,STATUS)
                     FTYPE(COL) = '_INTEGER'
                  ELSEIF (DTYPE(COL).EQ.42) THEN
                     CALL DAT_NEW1R(LOC,TTYPE(COL)(1:NLEN),NROWS,STATUS)
                     FTYPE(COL) = '_REAL'
                  ELSEIF (DTYPE(COL).EQ.82) THEN
                     CALL DAT_NEW1D(LOC,TTYPE(COL)(1:NLEN),NROWS,STATUS)
                     FTYPE(COL) = '_DOUBLE'
                  ELSEIF (DTYPE(COL).EQ.16) THEN
                     CALL DAT_NEW1C(LOC,TTYPE(COL)(1:NLEN),
     &                                     WIDTH(COL),NROWS,STATUS)
                     FTYPE(COL) = '_CHAR*'
                     CALL CHR_ITOC(WIDTH(COL),FTYPE(COL)(7:),NDUM)
                  ELSEIF (DTYPE(COL).EQ.14) THEN
                     CALL DAT_NEW1L(LOC,TTYPE(COL)(1:NLEN),NROWS,STATUS)
                     FTYPE(COL) = '_LOGICAL'
                  ENDIF
***               Get a locator to the new array
                  CALL DAT_FIND(LOC,TTYPE(COL)(1:NLEN),ALOC(COL),STATUS)
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL MSG_SETC('ANAM',TTYPE(COL)(1:NLEN))
                     CALL MSG_PRNT('Error creating array ^ANAM')
                     GOTO 999
                  ENDIF
               ELSE
                  HROWS = NROWS + HSAVE
                  MAPMODE = 'UPDATE'
                  CALL DAT_FIND(LOC,TTYPE(COL)(1:NLEN),ALOC(COL),STATUS)
                  CALL DAT_ALTER(ALOC(COL),1,HROWS,STATUS)
               ENDIF
            ENDDO

            CALL MSG_SETI('ROWS',NROWS)
            CALL MSG_SETI('COLS',TFIELDS)
            CALL MSG_PRNT('Converting ^COLS columns and ^ROWS rows')

***         Read from the FITS file in groups of MAXRECS records
            NSAVE = 0
            DO WHILE (HSAVE.LT.HROWS)
               FNUM = MIN(NROWS-NSAVE,MAXRECS)
               HBEG = 1 + HSAVE
               HEND = HSAVE + FNUM
               FBEG = 1 + NSAVE
               HSAVE = HSAVE + FNUM
               NSAVE = NSAVE + FNUM

               CALL MSG_SETI('BEG',HBEG)
               CALL MSG_SETI('END',HEND)
               CALL MSG_PRNT('Reading/mapping from ^BEG to ^END')
               DO COL = 1,TFIELDS
***               Map the array segment
                  CALL DAT_SLICE(ALOC(COL),1,HBEG,HEND,TLOC,STATUS)
                  CALL DAT_MAPV(TLOC,FTYPE(COL),MAPMODE,PNTR(COL),
     &                                                 NDUM,STATUS)

                  IF (STATUS.NE.SAI__OK) GOTO 999

***               Read the data
                  IF (DTYPE(COL).EQ.21) THEN
                     CALL FTGCVJ(IUNIT,COL,FBEG,1,FNUM,0,
     &                                  %val(PNTR(COL)),ANYF,ISTATUS)
                  ELSEIF (DTYPE(COL).EQ.41) THEN
                     CALL FTGCVJ(IUNIT,COL,FBEG,1,FNUM,0,
     &                                  %val(PNTR(COL)),ANYF,ISTATUS)
                  ELSEIF (DTYPE(COL).EQ.42) THEN
                     CALL FTGCVE(IUNIT,COL,FBEG,1,FNUM,0.0,
     &                                 %val(PNTR(COL)),ANYF,ISTATUS)
                  ELSEIF (DTYPE(COL).EQ.82) THEN
                     CALL FTGCVD(IUNIT,COL,FBEG,1,FNUM,0.D0,
     &                                 %val(PNTR(COL)),ANYF,ISTATUS)
                  ELSEIF (DTYPE(COL).EQ.16) THEN
* A dummy argument needs to be tacked onto the end to fool the compiler
* with the length of the "string" passed by pointer, because there is already
* a string used in the function call, a dummy call will need to be made
* to correct the order.
                     CALL RAT_FTGCVS(IUNIT,COL,FBEG,1,FNUM,
     &                %val(PNTR(COL)),' ',ANYF,ISTATUS,%val(WIDTH(COL)))
                  ELSEIF (DTYPE(COL).EQ.14) THEN
                     CALL FTGCL(IUNIT,COL,FBEG,1,FNUM,
     &                                    %val(PNTR(COL)),ISTATUS)
                  ENDIF

***               Unmap the array segment
                  CALL DAT_UNMAP(TLOC,STATUS)
                  CALL DAT_ANNUL(TLOC,STATUS)
               ENDDO
            ENDDO

***         Unmap the columns/arrays
            DO COL = 1,TFIELDS
               CALL DAT_ANNUL(ALOC(COL),STATUS)
            ENDDO
         ENDIF

100      CONTINUE
***      Write FITS header informatIon to HDS file
         IF (FNEW.AND.HDSOPEN) THEN
            CALL DAT_THERE(LOC,'MORE',EXISTS,STATUS)
            IF (.NOT.EXISTS) CALL DAT_NEW(LOC,'MORE','EXT',0,0,STATUS)
            CALL DAT_FIND(LOC,'MORE',TLOC,STATUS)

***         Get header details
            CALL FTGHSP(IUNIT,NKEYS,NMORE,ISTATUS)

***         Create structure in HDS file
            CALL DAT_NEWC(TLOC,'FITS',80,1,NKEYS,STATUS)
            CALL CMP_MAPV(TLOC,'FITS','_CHAR*80','WRITE',
     &                                    TPNTR, NDUM, STATUS)

***         Read header record in FITS file
            CALL FXRDHD(IUNIT,%val(TPNTR),NKEYS,ISTATUS)
            CALL CMP_UNMAP(TLOC,'FITS',STATUS)

***         Produce a history record
            CALL HIST_ADD(LOC, VERSION, STATUS)

         ENDIF

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error writing FITS header info.')
            GOTO 999
         ENDIF

***      Close the HDS file
         IF (HDSOPEN) CALL HDS_CLOSE(LOC,STATUS)
         HDSOPEN = .FALSE.

***      move to next extensIon and loop back
         CALL FTMRHD(IUNIT,1,HTYPE,ISTATUS)

      ENDDO

***   If end of header/fIle clear error flag
      IF (ISTATUS.EQ.107) ISTATUS=0

***   close FITS fIle
      CALL FTCLOS(IUNIT,ISTATUS)
      CALL FIO_PUNIT(IUNIT,ISTATUS)

***   dIsplay any error messages
999   CONTINUE
      IF (ISTATUS.NE.0) THEN
         CALL FTGERR(ISTATUS,ERRTXT)
         CALL MSG_PRNT(ERRTXT)
      END IF
      IF (STATUS.NE.SAI__OK) THEN
         CALL ERR_REP(' ',VERSION,STATUS)
      ENDIF

      CALL AST_CLOSE()

      END

*+ FXRDHD - Read header records into an array
        SUBROUTINE FXRDHD(IUNIT,HDARR,NHDR,ISTATUS)
*
        INTEGER IUNIT,NHDR,ISTATUS
        CHARACTER*80 HDARR(NHDR)
        CHARACTER*132 LINE
        INTEGER I

        DO I=1,NHDR
           CALL FTGREC(IUNIT,I,LINE,ISTATUS)
           HDARR(I) = LINE(1:80)
        ENDDO
        END


*+ AOPENHDS - open or create a HDS fIle
        subroutIne aopenhds(hname,extname,fcnt,fNEW,LOC,STATUS)

      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    FunctIon declaratIons :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
      character*60 hname
        character*(*) extname
        character*(*) LOC
        character*2 cfcnt
        character*100 pname
	Integer fcnt,nchar,STATUS
        logIcal FNEW
      SAVE PNAME

        IF (FNEW) THEN
*
*    Create an output HDS fIle
            IF (EXTNAME.NE.' ') THEN
               PNAME = HNAME(1:CHR_LEN(HNAME))//'_'//extname
            ELSEIF (FCNT .GT. 0) THEN
               CALL CHR_ITOC(FCNT, CFCNT, NCHAR)
               PNAME = HNAME(1:CHR_LEN(HNAME))//'_'//CFCNT(1:NCHAR)
               FCNT = FCNT + 1
            ELSE
               PNAME = HNAME(1:CHR_LEN(HNAME))
               FCNT = FCNT + 1
            ENDIF
*
            CALL HDS_NEW(PNAME, 'FITSCONV', 'FITSCONV', 0, 0,
     &                                                 LOC, STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('Error opening output fIle')
               GOTO 999
            ELSE
               CALL MSG_SETC('PNAM', PNAME)
               CALL MSG_PRNT('Writing to ^PNAM')
            ENDIF
*
         ELSE
*
*      Open the old fIle
            CALL HDS_OPEN(PNAME,'UPDATE',LOC, STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('Error opening existing output file')
               GOTO 999
            ELSE
               CALL MSG_SETC('PNAM', PNAME)
               CALL MSG_PRNT('Updating ^PNAM')
            ENDIF
*
        ENDIF
 999    contInue
	end


*+ RAT_FTGCVS A dummy FITSIO procedure to pass character strings pointers
      SUBROUTINE RAT_FTGCVS(IUNIT,COL,FBEG,N,FNUM,PNTR,NVAL,
     &                                                ANYF,ISTATUS)
*     A dummy procedure to allow a character pointer to be passed.
*     This procedure is called with one extra parameter, the size of
*     the character string.
      INTEGER IUNIT,COL,FBEG,FNUM,ISTATUS,N
      CHARACTER*(*) NVAL
      CHARACTER*(*) PNTR(*)

      CALL FTGCVS(IUNIT,COL,FBEG,N,FNUM,NVAL,PNTR,ANYF,ISTATUS)
      END
