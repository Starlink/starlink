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
*      4 Sep 1995 V1.7-3 - Use HSI rather than HIST (DJA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'INC_XRTHEAD'
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local variables :
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
      CHARACTER*(DAT__SZLOC) LOC                  ! Locator to HDS file
      CHARACTER*132 HNAME                         ! Name of HDS file
      CHARACTER*132 PATH,FNAME                         ! Name of FITS file
      CHARACTER*5 ORIGIN                          ! Type of FITS file
      INTEGER IUNIT,OFID,IFID,NLEV
      INTEGER ISTAT
      INTEGER INDEX  ! structure to hold index
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'XRTINDEX Version 1.7-3')
*-
C     IF (STATUS.NE.SAI__OK) RETURN

*  Initialise
      CALL AST_INIT()
*
      CALL MSG_PRNT(VERSION)

*  Get free IO unit
      CALL FIO_GUNIT(IUNIT,STATUS)

*  Get input file name
      CALL USI_ASSOC( 'INPUT', 'FITSfile', 'READ', IFID, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Get the FITS file type (US/RAT)
      CALL USI_GET0C( 'ORIGIN', ORIGIN, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99
      CALL CHR_UCASE( ORIGIN )

*  Get header information from file
      CALL ADI_FTRACE( IFID, NLEV, PATH, FNAME, STATUS )
      CALL ADI2_GETLUN( IFID, IUNIT, STATUS )
      CALL RAT_RDHEAD( IUNIT, ORIGIN, HEAD, ISTAT )
      IF ( ISTAT .NE. 0 ) THEN
        CALL MSG_SETC( 'FNAM', FNAME )
        CALL MSG_PRNT( '** Error reading header info from ^FNAM **' )
        GOTO 99
      END IF

*  Get output file name
      CALL USI_GET0C( 'OUTPUT', HNAME, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Create the Index file
      CALL ADI_FCREAT( HNAME(:CHR_LEN(HNAME))//'_hdr%hds',
     :                                  ADI__NULLID, OFID, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Write Rationalised fits version
      CALL ADI1_GETLOC( OFID, LOC, STATUS )
      CALL HDX_PUTC(LOC,'VERSION',1,version,STATUS)

*  Write the file type
      CALL HDX_PUTC(LOC,'ORIGIN',1,origin(1:CHR_LEN(origin)),STATUS)

*  Write the filename
*     CALL STRNAME(fname,fname,STATUS)
      CALL HDX_PUTC(LOC,'SRCFILE',1,fname(1:CHR_LEN(fname)),STATUS)

*  Create and map the HEAD structure
      CALL RAT_PUTHEAD(LOC,'HEAD',HEAD,STATUS)

*  Create and map the index structure
      CALL RAT_PUTINDEX(LOC,'INDEX',INDEX,STATUS)

*  Produce a history record
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Report any errors so far
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT('Error writing to output file')
      END IF

*  Close the HDS file
      CALL ADI_FCLOSE( OFID, STATUS )

*  Finish with asterix
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
