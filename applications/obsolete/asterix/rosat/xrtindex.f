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
*     20 Sep 1993 V1.5-0 (RDS):
*        Original version
*     24 Apr 1994 V1.7-0 (JKA):
*        For new release of asterix
*      9 Jun 1994 V1.7-1 (JKA):
*        String length for file names increased
*     16 Jun 1994 V1.7-2 (JKA):
*        Now handles new RDF files (2.3)
*      4 Sep 1995 V1.7-3 (DJA):
*        Use HSI rather than HIST
*     18 Dec 1995 V2.0-0 (DJA):
*        ADI port. Split in two for cleaner XRTCONV interface
*      7 Apr 98 V2.2-1 Removed Structures (rjv)
*      7 Apr 98 v2.2-2 linux port (rjv)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*132		FNAME			! Input FITS file
      CHARACTER*132		HNAME			! Output root
      CHARACTER*5		ORIGIN			! File origin
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      CALL AST_INIT()

*  Get user input stuff
      CALL USI_GET0C( 'INPUT', FNAME, STATUS )
      CALL USI_GET0C( 'ORIGIN', ORIGIN, STATUS )
      CALL USI_GET0C( 'OUTPUT', HNAME, STATUS )
      CALL CHR_UCASE( ORIGIN )

*  Invoke internal routine
      CALL XRTINDEX_INT( FNAME, ORIGIN, HNAME, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  XRTINDEX_INT - Creates header & index file for US or Rationalised FITS
	SUBROUTINE XRTINDEX_INT( INP, ORIGIN, HNAME, STATUS )
*    Description :
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jeremy Ashley
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Import:
      CHARACTER*(*)	INP,ORIGIN,HNAME
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC                  ! Locator to HDS file
      CHARACTER*132 IFILE
      INTEGER IUNIT,OFID,IFID
      INTEGER ISTAT
      INTEGER INDEX  ! structure to hold index
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'XRTINDEX Version 2.2-0')
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version
      CALL MSG_PRNT( VERSION )

*  Open input file
      CALL ADI_FOPEN( INP, 'FITSfile', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get header information from file
      CALL ADI2_GETLUN( IFID, IUNIT, STATUS )
      CALL RAT_RDHEAD( IUNIT, ORIGIN, ISTAT )
      IF ( ISTAT .NE. 0 ) THEN
        CALL MSG_SETC( 'FNAM', INP )
        CALL MSG_PRNT( '** Error reading header info from ^FNAM **' )
        GOTO 99
      END IF

*  Create the index file
      IFILE=HNAME(1:CHR_LEN(HNAME))//'_hdr%hds'
      CALL ADI_FCREAT( IFILE,  ADI__NULLID, OFID, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Write Rationalised fits version
      CALL ADI1_GETLOC( OFID, LOC, STATUS )
      CALL HDX_PUTC( LOC, 'VERSION', 1, VERSION, STATUS )

*  Write the file type
      CALL HDX_PUTC( LOC, 'ORIGIN', 1, ORIGIN(1:CHR_LEN(ORIGIN)),
     :               STATUS )

*  Write the filename
      CALL HDX_PUTC( LOC, 'SRCFILE', 1, INP(1:CHR_LEN(INP)), STATUS )

*  Create and map the HEAD structure
      CALL RAT_PUTHEAD( OFID, 'HEAD', STATUS )

*  Create and map the index structure
      CALL RAT_PUTINDEX( OFID, 'INDEX', INDEX, STATUS )

*  Produce a history record
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Report any errors so far
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT('Error writing to output file')
      END IF

*  Close the FITS and HDS files
      CALL ADI_FCLOSE( IFID, STATUS )
      CALL ADI_FCLOSE( OFID, STATUS )

*  Finish with asterix
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'XRTINDEX_INT', STATUS )
      END IF

      END
