*+  XRTINDEX -  creates header & index file for US or Rationalised FITS
	SUBROUTINE XRTINDEX(STATUS)
*    Description :
*     Creates a header & index file from FITS using the William Pence FIO
*     library.  The user specicifies the event data file to use and the file
*     type (currently US/MPE/RAT supported)  An <rt>_hdr.sdf file is created
*     by default containing the HEAD & index structures.
*    Environment parameters :
*    Method :
*     Determine file type.
*     Read FITS array names
*     Fill the HEAD and SRT structure
*     Create an HDS file
*     Create corresponding HDS structures and map them
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jeremy Ashley
*    History :
*     20-Sep-1993   -   original
*     24-Apr-1994   - (v1.7-0) for new release of asterix
*     09-Jun-1994   - (v1.7-1) string length for file names increased (132)
*     16-Jun-1994   - (v1.7-2) Now handles new RDF files (2.3)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local variables :
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
      CHARACTER*(DAT__SZLOC) LOC                  ! Locator to HDS file
      CHARACTER*132 HNAME                         ! Name of HDS file
      CHARACTER*132 FNAME                         ! Name of FITS file
      CHARACTER*5 ORIGIN                          ! Type of FITS file
      INTEGER IUNIT,BLOCK
      INTEGER ISTAT
      INTEGER INDEX  ! structure to hold index
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'XRTINDEX Version 1.7-2')
*-
C     IF (STATUS.NE.SAI__OK) RETURN
*
      CALL AST_INIT()
*
      CALL MSG_PRNT(VERSION)
*
* Initialise

***** get free IO unit
      CALL FIO_GUNIT(IUNIT,STATUS)

***** get input file name
      CALL par_get0c('INPUT', fname, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999

***** Get the FITS file type (US/RAT)
      CALL par_get0c('ORIGIN', origin, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999
      CALL CHR_UCASE(origin)

***** open the existing FITS file
      CALL FTOPEN(iunit,fname,0,block,ISTAT)
      IF (ISTAT .NE. 0) THEN
	 CALL msg_setc('FNAM', FNAME)
         CALL msg_prnt('** Error opening file ^FNAM **')
         GOTO 999
      ENDIF

***** get header information from file
      CALL rat_rdhead(iunit,origin,head,ISTAT)

      IF (ISTAT .NE. 0) THEN
         CALL msg_setc('FNAM',FNAME)
         CALL msg_prnt('** Error reading header info from ^FNAM **')
         istat = 0
         GOTO 999
      ENDIF
***** close the fits file
      CALL ftclos(iunit,ISTAT)

***** release file IO unit
      CALL fio_punit(iunit,STATUS)

***** Get the output filename
      CALL par_get0c('OUTPUT', hname, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999

***** Create the Index file
      HNAME = HNAME(1:CHR_LEN(HNAME)) // '_hdr'
      CALL HDS_NEW(hname,'FITS_INDEX','INDEX',0,0,loc,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         HNAME = HNAME(1:CHR_LEN(HNAME)) // '.sdf'
         CALL msg_setc('HNAM',HNAME)
         CALL MSG_PRNT('** Error opening output file ^HNAM **')
         GOTO 999
      ENDIF

***** Write Rationalised fits version
      CALL HDX_PUTC(LOC,'VERSION',1,version,STATUS)

***** Write the file type
      CALL HDX_PUTC(LOC,'ORIGIN',1,origin(1:CHR_LEN(origin)),STATUS)

***** Write the filename
*     CALL STRNAME(fname,fname,STATUS)
      CALL HDX_PUTC(LOC,'SRCFILE',1,fname(1:CHR_LEN(fname)),STATUS)

***** Create and map the HEAD structure
      CALL RAT_PUTHEAD(LOC,'HEAD',HEAD,STATUS)

***** Create and map the index structure
      CALL RAT_PUTINDEX(LOC,'INDEX',INDEX,STATUS)

***** Produce a history record
      CALL HIST_ADD(loc, VERSION, STATUS)

***** Report any errors so far
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing to output file')
         GOTO 999
      ENDIF

***** close the HDS file
      CALL hds_close(loc,STATUS)

***** finish with asterix
      CALL AST_CLOSE()

999   continue
      end
