      SUBROUTINE CAL2AST( STATUS )
*+
*  Description:
*    Convert HEASARC caldb files into ASTERIX format

*  Authors:
*    RB: Richard Beard (ROSAT, University of Birmingham)

*  History:
*    25 Feb 98 - Original version.
*-

*  Type definitions:
      IMPLICIT NONE

*  External functions:
      LOGICAL			STR_ABBREV
       EXTERNAL			  STR_ABBREV

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'

*  Local constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'CAL2AST Version 2.2-0' )

*  Status:
      INTEGER			STATUS

*  Local variables:
      CHARACTER*256		IFILE			! Input file name
      CHARACTER*256		FFILE			! Filter file name
      CHARACTER*80		CTYPE			! Calibration type
      CHARACTER*5		INST			! Instrument type
      CHARACTER*(DAT__SZLOC)	ROOT			! ASTERIX file root locator
      CHARACTER*2		CC			! Number conversion
      CHARACTER*132		HTXT(9)			! History text lines
      CHARACTER*8		DATE(2)			! FITS file creation date

      REAL			NRG_LO(729)		! Low end of energy bin
      REAL			NRG_HI(729)		! High end of energy bin
      REAL			ANGLE(14)		! Off-axis angle
      REAL			RSPNS(729*14)		! Spectral response
      REAL			ENERGY(729)		! Middle of energy bin
      REAL			WIDTH(729)		! Width of energy bin
      REAL			SLICE(729)		! One slice of the reponse
      REAL			AXIS1(256)		! Channels axis
      REAL			AXIS2(729)		! Energy axis
      REAL			RESPONSE(256,729)	! Energy response

      INTEGER			LUN			! Input file logical unit number
      INTEGER			FSTAT			! FITS i/o status
      INTEGER			ROOTID			! ADI root object
      INTEGER			OFID			! Output file ID
      INTEGER			HDUTYP			! HDU type
      INTEGER			TYPE			! Numeric calibration type
      INTEGER			OFFSET			! Byte offset in binary table
      INTEGER			I, J 			! Loop variables
      INTEGER			NDIG			! Number of digits
      INTEGER			DIMS(2)			! Size of dimensions
      INTEGER			START			! Start channel of slice

      LOGICAL			ANYF			! Any null values
*  Local data:
      DATA DIMS			/256, 729/
*.

*  Version number
      CALL MSG_PRNT( VERSION )

*  Start ASTERIX
      CALL AST_INIT()
      FSTAT = 0

*  Open the caldb FITS file
      CTYPE = '*'
      CALL CAL2AST_OPEN( 'INP', CTYPE, LUN, IFILE, DATE(1), STATUS )

*  Can we convert this type of file?
      TYPE = 0
      IF ( CTYPE .EQ. 'SPECTRAL RESPONSE' ) THEN
        TYPE = 1
      ELSE IF ( CTYPE .EQ. 'RESPONSE MATRIX' ) THEN
        TYPE = 2
      ELSE IF ( CTYPE .EQ. 'CONV RESPONSE MATRIX' ) THEN
        TYPE = 3
      END IF
      CALL MSG_SETC( 'TYPE', CTYPE )
      IF ( TYPE .GT. 0 ) THEN
        CALL MSG_PRNT( 'File contains data of type ^TYPE' )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Cannot convert data of type ^TYPE', STATUS )
        GOTO 99
      END IF

*  Open the output file
      INST = IFILE(1:5)
      CALL CHR_UCASE( INST )
      IF ( TYPE .EQ. 1 ) THEN
        CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
        CALL ADI1_GETLOC( OFID, ROOT, STATUS )
        CALL DAT_RENAM( ROOT, 'EFFAREA_'//INST, STATUS )
        CALL DAT_RETYP( ROOT, 'EVENT_FILE', STATUS )
      ELSE IF ( TYPE .EQ. 2 .OR. TYPE .EQ. 3 ) THEN
        CALL BDI_NEW( 'BinDS', 2, DIMS, 'REAL', ROOTID, STATUS )
        CALL USI_CREAT( 'OUT', ROOTID, OFID, STATUS )
        CALL ADI1_GETLOC( OFID, ROOT, STATUS )
        CALL DAT_RENAM( ROOT, 'DRM'//INST, STATUS )
        CALL DAT_RETYP( ROOT, 'NDF', STATUS )
      END IF

*  Convert type 'SPECTRAL RESPONSE'
      IF ( TYPE .EQ. 1 ) THEN

*    Read in the data
        CALL FTMAHD( LUN, 2, HDUTYP, FSTAT )
        OFFSET = 1
        CALL FTGTBB( LUN, 1, OFFSET, 729*4, NRG_LO, FSTAT )
        OFFSET = OFFSET + 729*4
        CALL FTGTBB( LUN, 1, OFFSET, 729*4, NRG_HI, FSTAT )
        OFFSET = OFFSET + 729*4
        CALL FTGTBB( LUN, 1, OFFSET, 14*4, ANGLE, FSTAT )
        OFFSET = OFFSET + 14*4
        CALL FTGTBB( LUN, 1, OFFSET, 729*14*4, RSPNS, FSTAT )

*    Calculate some products
        DO I = 1, 729
          ENERGY(I) = ( NRG_LO(I) + NRG_HI(I) ) / 2.0
          WIDTH(I) = NRG_HI(I) - NRG_LO(I)
        END DO

*    Write out the data
        CALL DAT_NEW1R( ROOT, 'ENERGY', 729, STATUS )
        CALL CMP_PUT1R( ROOT, 'ENERGY', 729, ENERGY, STATUS )
        CALL DAT_NEW1R( ROOT, 'E_BIN_WIDTH', 729, STATUS )
        CALL CMP_PUT1R( ROOT, 'E_BIN_WIDTH', 729, WIDTH, STATUS )
        CALL DAT_NEW1R( ROOT, 'OFF_ANGLE', 14, STATUS )
        CALL CMP_PUT1R( ROOT, 'OFF_ANGLE', 14, ANGLE, STATUS )
        DO I = 1, 14
          CALL CHR_ITOC( I, CC, NDIG )
          CALL DAT_NEW1R( ROOT, 'EFFAR_'//CC(1:NDIG), 729, STATUS )
          DO J = 1, 729
            SLICE(J) = RSPNS( (I - 1) * 729 + J )
          END DO
          CALL CMP_PUT1R( ROOT, 'EFFAR_'//CC(1:NDIG), 729, SLICE,
     :                    STATUS )
        END DO

*    Open the filter file
        CALL CAL2AST_OPEN( 'FILTER', 'FILTER TRANSMISSION', LUN,
     +                     FFILE, DATE(2), STATUS )

*    Copy over the filter data
        CALL FTMAHD( LUN, 2, HDUTYP, FSTAT )
        CALL FTGTBB( LUN, 1, 1, 729*4, SLICE, FSTAT )
        CALL DAT_NEW1R( ROOT, 'FILTER_1', 729, STATUS )
        CALL CMP_PUT1R( ROOT, 'FILTER_1', 729, SLICE, STATUS )

*  Convert type 'RESPONSE MATRIX'
      ELSE IF ( TYPE .EQ. 2 .OR. TYPE .EQ. 3 ) THEN

*    Write out the title and axes
        CALL BDI_PUT0C( OFID, 'Title', 'DET_RESP_MATRIX', STATUS )
        CALL BDI_PUT0C( OFID, 'Units', 'Detec_Resp', STATUS )
        CALL BDI_AXPUT0C( OFID, 1, 'Label', 'Channels', STATUS )
        CALL ARR_REG1R( 1.0, 1.0, 256, AXIS1, STATUS )
        CALL BDI_AXPUT1R( OFID, 1, 'Data', 256, AXIS1, STATUS )
        CALL BDI_AXPUT0C( OFID, 2, 'Label', 'Energy', STATUS )
        CALL ARR_REG1R( 1.0, 1.0, 729, AXIS2, STATUS )
        CALL BDI_AXPUT1R( OFID, 2, 'Data', 729, AXIS2, STATUS )

*    Read in the data
        CALL FTMAHD( LUN, 2, HDUTYP, FSTAT )
        DO I = 1, 729
          CALL FTGCVJ( LUN, 4, I, 1, 1, -1, START, ANYF, FSTAT )
          CALL FTGCVJ( LUN, 5, I, 1, 1, -1, NDIG, ANYF, FSTAT )
          CALL FTGCVE( LUN, 6, I, 1, NDIG, -1.0, SLICE, ANYF, FSTAT )
          DO J = 1, NDIG
            RESPONSE(START+J-1, I) = SLICE(J)
          END DO
        END DO

*    Write out the data
        CALL DAT_NEW( ROOT, 'DATA_ARRAY', '_REAL', 2, DIMS, STATUS )
        CALL CMP_PUTNR( ROOT, 'DATA_ARRAY', 2, DIMS, RESPONSE, DIMS,
     :                  STATUS )
      END IF

*  Finished with the input file
      CALL FTCLOS( LUN, FSTAT )
      CALL FTFIOU( LUN, FSTAT )

*  Put in some history
      CALL HSI_NEW( OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )
      IF ( TYPE .EQ. 1 ) THEN
        CALL MSG_SETC( 'FILE', IFILE )
        CALL MSG_MAKE( 'Spectral response file: ^FILE', HTXT(1), NDIG )
        CALL MSG_SETC( 'DATE', DATE(1) )
        CALL MSG_MAKE( 'Created on date: ^DATE', HTXT(2), NDIG )
        CALL MSG_SETC( 'FILE', FFILE )
        CALL MSG_MAKE( 'Filter transimission file: ^FILE', HTXT(3),
     :                 NDIG )
        CALL MSG_SETC( 'DATE', DATE(2) )
        CALL MSG_MAKE( 'Created on date: ^DATE', HTXT(4), NDIG )
        CALL HSI_PTXT( OFID, 4, HTXT, STATUS )
      ELSE IF ( TYPE .EQ. 2 .OR. TYPE .EQ. 3 ) THEN
        CALL MSG_SETC( 'FILE', IFILE )
        CALL MSG_MAKE( 'Response matrix file: ^FILE', HTXT(1), NDIG )
        CALL MSG_SETC( 'DATE', DATE(1) )
        CALL MSG_MAKE( 'Created on date: ^DATE', HTXT(2), NDIG )
        CALL HSI_PTXT( OFID, 2, HTXT, STATUS )
      END IF

*  Finished with the output file
c     CALL DAT_ANNUL( ROOT, STATUS )

*  Tidy up
   99 IF ( FSTAT .NE. 0 ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'ERR', FSTAT )
        CALL ERR_REP( ' ', 'FITSIO error ^ERR reported', STATUS )
      END IF
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE CAL2AST_OPEN( PARAM, TYPE, LUN, FNAME, DATE, STATUS )
*+
*  Description:
*    Open a CALDB FITS file of the required TYPE. If TYPE is '*'
*    then the file type is returned

*  Authors:
*    RB: Richard Beard (ROSAT, University of Birmingham)

*  History:
*    26 Feb 98 - Original version.
*-

*  Type definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Import:
      CHARACTER*(*)		PARAM			! Input parameter

*  Import/Export:
      CHARACTER*(*)		TYPE			! FIle type

*  Export:
      INTEGER			LUN			! File logical unit number
      CHARACTER*(*)		FNAME			! File name
      CHARACTER*(*)		DATE			! File creation date

*  Global status:
      INTEGER			STATUS

*  Local variables:
      CHARACTER*80		LTYPE			! Opened file type
      CHARACTER*80		CMNT			! FITS keyword comment

      INTEGER			FSTAT			! FITSIO status
      INTEGER			BLKSZ			! FITS file block size
      INTEGER			HDUTYP			! FITS HDU type

      LOGICAL			CHECK			! Check file type?
*.

*  Check global status
      IF ( STATUS .NE. SAI__OK ) RETURN
      FSTAT = 0

*  Check or return file type
      IF ( TYPE .EQ. '*' ) THEN
        CHECK = .FALSE.
      ELSE
        CHECK = .TRUE.
      END IF

*  Get a valid LUN
      CALL FTGIOU( LUN, FSTAT )

*  Get the parameter value
      CALL USI_GET0C( PARAM, FNAME, STATUS )

*  Open the file
      CALL FTOPEN( LUN, FNAME, 0, BLKSZ, FSTAT )
      IF ( FSTAT .NE. 0 ) THEN
        STATUS = SAI__ERROR
        FSTAT = 0
        CALL MSG_SETC( 'INP', FNAME )
        CALL ERR_REP( ' ', 'Unable to open file ^INP', STATUS )
        GOTO 99
      END IF

*  Get the file type
      CALL FTMAHD( LUN, 1, HDUTYP, FSTAT )
      CALL FTGKYS( LUN, 'CONTENT', LTYPE, CMNT, FSTAT )
      IF ( FSTAT .NE. 0 ) THEN
        STATUS = SAI__ERROR
        FSTAT = 0
        CALL MSG_SETC( 'INP', FNAME )
        CALL ERR_REP( ' ', 'Cannot find keyword CONTENT in file ^INP',
     +                STATUS )
        CALL FTCLOS( LUN, FSTAT )
        CALL FTFIOU( LUN, FSTAT )
        GOTO 99
      END IF

*  Check the file type
      IF ( TYPE .NE. LTYPE .AND. CHECK ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'INP', FNAME )
        CALL MSG_SETC( 'TYPE', TYPE )
        CALL ERR_REP( ' ', 'File ^INP is of type ^TYPE', STATUS )
        CALL FTCLOS( LUN, FSTAT )
        CALL FTFIOU( LUN, FSTAT )
        GOTO 99
      END IF

*  Extract the file creation date
      CALL FTGKYS( LUN, 'DATE', DATE, CMNT, FSTAT )

*  Return file type if required
      IF ( .NOT. CHECK ) TYPE = LTYPE

*  Report errors
   99 IF ( FSTAT .NE. 0 ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'ERR', FSTAT )
        CALL ERR_REP( ' ', 'FITSIO error ^ERR reported', STATUS )
      END IF
      IF ( STATUS .EQ. SAI__ERROR ) THEN
        CALL AST_REXIT( 'CAL2AST_OPEN', STATUS )
      END IF

      END
