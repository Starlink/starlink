*+  TEXT2HDS - Converts a text file to an event data file
      SUBROUTINE TEXT2HDS( STATUS )
*
*    Description :
*
*      Converts a text file containing columns of numbers into an HDS
*      file containing a primitive array for each column.
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
*     Richard Saxton (LTVAD::RDS)
*
*    History :
*
*     28 Jun 90 : V1.3-0 Original (RDS)
*      8 Sep 90 : V1.3-1 Extended to allow upto 40 columns and a line width of
*                        512 characters (RDS)
*     31 Aug 93 : V1.3-2 Reads from SCAR-type file into appropriate type
*                        instead of assuming REAL  (RJV)
*      7 Nov 93 : V1.7-0 Uses FIO to do all i/o (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     30 Jul 95 : V1.8-1 Use new history routines (DJA)
*     25 Aug 95 : V1.8-2 Use ADI top create event datasets (DJA)
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
*    Local constants :
      INTEGER MAXCOL
         PARAMETER (MAXCOL=40)
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'TEXT2HDS Version 2.2-0')
*    Local variables :
      CHARACTER*3 STRING
      CHARACTER*40 CNAME(MAXCOL)
      CHARACTER*80 INAME                       ! Name of input file
      CHARACTER*80 DFILE                       ! Name of SCAR descriptor file
      CHARACTER*80 TEXT
      CHARACTER*40 NULL                        ! Null string
      CHARACTER*(DAT__SZLOC) LOCO
      CHARACTER*(DAT__SZLOC) LOCT(MAXCOL)      ! Locators to HDS arrays
      CHARACTER*11 		UNITS(MAXCOL)           ! Units of lists
      CHARACTER*(DAT__SZTYP) TYPE(MAXCOL)

      REAL 			RMIN,RMAX		! List min and max

      INTEGER 			DFD			! FIO dscf descriptor
      INTEGER 			IFD			! FIO input descriptor
      INTEGER			LID(MAXCOL)		! List identifiers
      INTEGER			OFID, OFILID		! Output identifiers

      INTEGER 			NCOLS,NROWS
      INTEGER 			TPNTR(MAXCOL)           ! Pointers to o/p arrays
      INTEGER 			LP,IVAL
      INTEGER 			NREAD                   ! # lines in file
      LOGICAL SCAR                             ! Is SCAR descriptor file ready ?
      LOGICAL EVENT                            ! Event dataset output format ?
      INTEGER STPOS(MAXCOL)                    ! Start position of each field
      INTEGER LENGTH(MAXCOL)                   ! Length of each field in file

*    Local data :
      DATA NULL/'                                        '/
*-

      CALL AST_INIT(STATUS)

*    Initialise name strings
      DO LP=1,MAXCOL
        CNAME(LP) = NULL
      END DO

*    Get name of input file
      CALL USI_GET0C( 'INFILE', INAME, STATUS )

*    Ask if SCAR descriptor file available and get name of it
      CALL USI_GET0L( 'SCAR', SCAR, STATUS )
      IF ( SCAR ) THEN
        CALL USI_GET0C( 'DSCFILE', DFILE, STATUS )
      END IF
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Open input file
      CALL FIO_OPEN( INAME, 'READ', 'LIST', 0, IFD, STATUS )

*    Open descriptor file
      IF ( SCAR ) THEN
        CALL FIO_OPEN( DFILE, 'READ', 'LIST', 0, DFD, STATUS )
      END IF
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Ask if an event dataset is wanted rather than a list of HDS arrays.
      CALL USI_GET0L( 'EVENT', EVENT, STATUS )

*  Get a description of the file either from the user or from the SCAR
*  descriptor file
      IF ( SCAR ) THEN
        CALL TEXT2HDS_DSCFREAD( DFD, MAXCOL, NCOLS, NROWS, CNAME,
     :                       UNITS, STPOS, LENGTH, TYPE, STATUS )

      ELSE

*    Get the number of text columns from the user
        NCOLS = 0
        CALL USI_GET0I( 'NCOLS', NCOLS, STATUS )
        IF ( NCOLS .GT. MAXCOL ) THEN
          CALL MSG_SETI('MAXCOL', MAXCOL)
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', '* Can''t handle more than ^MAXCOL '/
     :                    /'columns *', STATUS )
          GOTO 99
        END IF

*    Get the upper limit on the number of lines of text in the file
        CALL USI_GET0I( 'NROWS', NROWS, STATUS )
        IF (STATUS .NE. SAI__OK) GOTO 99

*    Get list names from environment
        DO LP = 1, NCOLS
          TYPE(LP) = '_REAL'
          CALL CHR_ITOC( LP, STRING, IVAL )
          CALL USI_GET0C( 'CNAME'//STRING(1:IVAL), CNAME(LP), STATUS )
        END DO
        IF ( STATUS .NE. SAI__OK ) GOTO 99

      END IF

*    Open output file
      IF ( EVENT ) THEN
        CALL ADI_NEW0( 'EventDS', OFID, STATUS )
        CALL ADI_CPUT0I( OFID, 'NEVENT', NROWS, STATUS )
        CALL USI_CREAT( 'OUTFILE%hds', OFID, OFILID, STATUS )
      ELSE
        CALL USI_CREAT( 'OUTFILE%hds', ADI__NULLID, OFID, STATUS )
        CALL ADI1_GETLOC( OFID, LOCO, STATUS )
      END IF

*  Create output arrays and map them
      IF ( EVENT ) THEN

*    Create lists in output file
        DO LP = 1, NCOLS
          IF ( TYPE(LP) .EQ. '_DOUBLE' ) THEN
            CALL EDI_CREL0D( OFID, CNAME(LP), .FALSE.,
     :                   0.0, 0.0, 0.0, UNITS(LP), LID(LP), STATUS )
          ELSE IF ( TYPE(LP) .EQ. '_REAL' ) THEN
            CALL EDI_CREL0R( OFID, CNAME(LP), .FALSE.,
     :                   0.0, 0.0, 0.0, UNITS(LP), LID(LP), STATUS )
          ELSE
            CALL EDI_CREL0I( OFID, CNAME(LP), .FALSE.,
     :                   0.0, 0.0, 0.0, UNITS(LP), LID(LP), STATUS )
          END IF
          CALL EDI_CREAT( OFID, LID(LP), STATUS )
          CALL EDI_MAPR( OFID, CNAME(LP), 'WRITE', 0, 0, TPNTR(LP),
     :                   STATUS )
        END DO

      ELSE

*    Create arrays in output file
        DO LP = 1, NCOLS
          CALL DAT_NEW( LOCO, CNAME(LP), TYPE(LP), 1, NROWS, STATUS )
          CALL DAT_FIND( LOCO, CNAME(LP), LOCT(LP), STATUS )
          CALL DAT_MAPV( LOCT(LP), TYPE(LP), 'WRITE',
     :                                TPNTR(LP), NROWS, STATUS )
         END DO

      END IF

      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Error creating arrays in output file',
     :                STATUS )
        GOTO 99
      END IF

*    Read input file into output arrays
      IF ( SCAR ) THEN
        CALL TEXT2HDS_SCREAD( IFD, MAXCOL, NCOLS, NROWS, STPOS,
     :                     LENGTH, TYPE, TPNTR, NREAD, STATUS )
      ELSE
        CALL TEXT2HDS_READ( IFD, NCOLS, NROWS, TYPE, TPNTR, NREAD,
     :                     STATUS )
      END IF

*  If an EVENT dataset has been produced find the min and max
      IF ( EVENT ) THEN

*    Unmap the mapped lists, and write field extrema
        DO LP = 1, NCOLS

*      Get min and max
          CALL ARR_RANG1R( NREAD, %VAL(TPNTR(LP)), RMIN, RMAX, STATUS )
          CALL ADI_CPUT0R( LID(LP), 'Min', RMIN, STATUS )
          CALL ADI_CPUT0R( LID(LP), 'Max', RMAX, STATUS )

*      Unmap the list
          CALL EDI_UNMAP( OFID, CNAME(LP), STATUS )

*      Update the list attributes
          CALL EDI_LUPDT( OFID, LID(LP), 'Min,Max', STATUS )

        END DO

*    Adjust list lengths
        CALL EDI_ALTLEN( OFID, NREAD, STATUS )

      ELSE
        DO LP = 1, NCOLS
          CALL DAT_UNMAP( LOCT(LP), STATUS )
          IF ( NREAD .LT. NROWS ) THEN
            CALL DAT_ALTER( LOCT(LP), 1, NREAD, STATUS )
          END IF
          CALL DAT_ANNUL( LOCT(LP), STATUS )
        END DO

      END IF

*  Produce a history record
      CALL HSI_ADD( OFID, VERSION, STATUS )
      TEXT = 'Produced from '//INAME
      CALL HSI_PTXT( OFID, 1, TEXT, STATUS )

*  Close the input files
      CALL FIO_CLOSE( IFD, STATUS )
      IF ( SCAR ) THEN
        CALL FIO_CLOSE( DFD, STATUS )
      END IF

*  Tidy up
 99   CALL AST_CLOSE( )
      CALL AST_ERR( STATUS )

      END


*+  TEXT2HDS_READ - Reads columns of a text file into an HDS array
      SUBROUTINE TEXT2HDS_READ( IFD, NCOLS, NROWS, TYPE, PTRS,
     :                                         NREAD, STATUS )
*    Description :
*    History :
*     29-Jun-1990   original (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER IFD                         ! FIO descriptor of text file
      INTEGER NCOLS                       ! Number of different columns
      INTEGER NROWS                       ! Maximum number of lines of text
*    Import-Export :
      CHARACTER*(*) TYPE(NCOLS)
      INTEGER PTRS(NCOLS)
*    Export :
      INTEGER NREAD                       ! Number of lines of text found
*    Local constants :
      CHARACTER*1 SPACE,COMMA,TAB
      PARAMETER (SPACE=' ',COMMA=',',TAB=CHAR(9))
      INTEGER MAXCOL
         PARAMETER (MAXCOL=40)
*    Local variables :
      CHARACTER*512 S
      INTEGER PTR
      INTEGER LP,I
      INTEGER C1,C2
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      NREAD=0
      DO LP=1,NROWS

*      Read line from file
        CALL FIO_READF( IFD, S, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 100
        NREAD=NREAD+1

        C1=1
        DO I=1,NCOLS
          DO WHILE (S(C1:C1).EQ.SPACE.OR.
     :              S(C1:C1).EQ.COMMA.OR.
     :              S(C1:C1).EQ.TAB)
            C1=C1+1
          ENDDO
          C2=C1
          DO WHILE (S(C2:C2).NE.SPACE.AND.
     :              S(C2:C2).NE.COMMA.AND.
     :              S(C2:C2).NE.TAB)
            C2=C2+1
          ENDDO

          PTR=PTRS(I)
          IF ( TYPE(I) .EQ. '_REAL' ) THEN
            CALL TEXT2HDS_REAL( S(C1:C2), LP, %val(PTR) )
          ELSE IF ( TYPE(I) .EQ. '_DOUBLE' ) THEN
            CALL TEXT2HDS_DBLE( S(C1:C2), LP, %val(PTR) )
          ELSE IF ( TYPE(I) .EQ. '_INTEGER' ) THEN
            CALL TEXT2HDS_INT( S(C1:C2), LP, %val(PTR) )
          END IF

          C1 = C2 + 1

        END DO

      END DO

*    Annul end of file status
 100  IF ( STATUS .EQ. FIO__EOF ) THEN
        CALL ERR_ANNUL( STATUS )

*    Check that there are no more lines in the file
      ELSE IF ( NREAD .EQ. NROWS ) THEN

        CALL FIO_READF( IFD, S, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL MSG_SETI('NROWS', NROWS)
          CALL MSG_PRNT('* The file contained more than ^NROWS lines *')
        ELSE
          CALL ERR_ANNUL( STATUS )
        END IF

      END IF

      END


*+  TEXT2HDS_SCREAD - Reads columns of a text file using SCAR description
      SUBROUTINE TEXT2HDS_SCREAD( IFD, MAXCOL, NCOLS, NROWS, STPOS,
     :                            LENGTH, TYPE, PTRS, NREAD, STATUS )
*
*    Description :
*
*     Reads each record from a text file into a 512 character buffer and
*     decodes the buffer into NCOLS REAL numbers. Each number starts from
*     position STPOS(i) and is LENGTH(i) characters long.
*
*    History :
*
*     29 Jun 90 : Original (LTVAD::RDS)
*     18 Oct 90 : Sets array value to zero if data file has a blank field
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER STATUS
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
*
*    Import :
*
      INTEGER MAXCOL                      ! Maximum number of columns
      INTEGER IFD                         ! FIO descriptor of text file
      INTEGER NCOLS                       ! Number of different columns
      INTEGER NROWS                       ! Maximum number of lines of text
      INTEGER STPOS(MAXCOL)               ! Start position in the string of
*                                         ! each number
      INTEGER LENGTH(MAXCOL)              ! Length of each field in the record
      INTEGER PTRS(MAXCOL)
      CHARACTER*(*) TYPE(MAXCOL)
*
*    Export :
*
      INTEGER NREAD                       ! Number of lines of text found
*
*    Local variables :
*
      CHARACTER*512 DUMMY
      CHARACTER*40 VAL
      INTEGER LP,FLP
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      NREAD=0
      DO LP=1,NROWS

*      Read record
        CALL FIO_READF( IFD, DUMMY, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 100

        NREAD=NREAD+1
*
*   Decode the character string
         DO FLP=1,NCOLS
            VAL=DUMMY(STPOS(FLP):(STPOS(FLP)+LENGTH(FLP)-1))

            IF (TYPE(FLP).EQ.'_REAL') THEN
              CALL TEXT2HDS_REAL(VAL(:LENGTH(FLP)),LP,%val(PTRS(FLP)))
            ELSEIF (TYPE(FLP).EQ.'_DOUBLE') THEN
              CALL TEXT2HDS_DBLE(VAL(:LENGTH(FLP)),LP,%val(PTRS(FLP)))
            ELSEIF (TYPE(FLP).EQ.'_INTEGER') THEN
              CALL TEXT2HDS_INT(VAL(:LENGTH(FLP)),LP,%val(PTRS(FLP)))
            ENDIF

*
         ENDDO
*
      ENDDO
*
100   IF ( STATUS .EQ. FIO__EOF ) THEN
        CALL ERR_ANNUL( STATUS )

*    Check that there are no more lines in the file
      ELSE IF ( NREAD .EQ. NROWS ) THEN
        CALL FIO_READF( IFD, DUMMY, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL MSG_SETI( 'NROWS', NROWS )
          CALL MSG_PRNT('* The file contained more than ^NROWS lines *')
        ELSE
          CALL ERR_ANNUL( STATUS )
        END IF

      ENDIF

      END


      SUBROUTINE TEXT2HDS_REAL(STR,N,ARRAY)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      CHARACTER*(*) STR
      INTEGER N
*    Import-Export :
*    Export :
      REAL ARRAY(*)
*    Status :
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ISTAT
*-
      ISTAT=0
      READ(STR,*,IOSTAT=ISTAT) ARRAY(N)

*    If the field is blank then set the array value to zero
      IF ( ISTAT .NE. 0 ) ARRAY(N) = 0.0

      END


      SUBROUTINE TEXT2HDS_DBLE(STR,N,ARRAY)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      CHARACTER*(*) STR
      INTEGER N
*    Import-Export :
*    Export :
      DOUBLE PRECISION ARRAY(*)
*    Status :
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ISTAT
*-
      ISTAT=0
      READ(STR,*,IOSTAT=ISTAT) ARRAY(N)
*
*      If the field is blank then set the array value to zero
          IF (ISTAT .NE. 0) THEN
             ARRAY(N)=0.0D0
          ENDIF


      END

      SUBROUTINE TEXT2HDS_INT(STR,N,ARRAY)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      CHARACTER*(*) STR
      INTEGER N
*    Import-Export :
*    Export :
      INTEGER ARRAY(*)
*    Status :
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ISTAT
*-
      ISTAT=0
      READ(STR,*,IOSTAT=ISTAT) ARRAY(N)
*
*      If the field is blank then set the array value to zero
          IF (ISTAT .NE. 0) THEN
             ARRAY(N)=0
          ENDIF


      END



*+  TEXT2HDS_DSCFREAD - Reads a SCAR descriptor file
      SUBROUTINE TEXT2HDS_DSCFREAD( DFD, MAXCOL, NCOLS, NROWS, CNAME,
     :                           UNITS, STPOS, LENGTH, TYPE, STATUS )
*    Description :
*    History :
*     29-Jun-1990   original (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER DFD                         ! FIO descriptor of DSCF file
      INTEGER MAXCOL                      ! Maxnumber of columns allowed
*    Import-Export :
*    Export :
      INTEGER NCOLS                       ! Number of different columns
      INTEGER NROWS                       ! Number of lines of text
      CHARACTER*(*) CNAME(MAXCOL)         ! Name for output arrays
      CHARACTER*(*) UNITS(MAXCOL)         ! Units of each list
      CHARACTER*(*) TYPE(MAXCOL)
      INTEGER STPOS(MAXCOL)               ! Start position in the string of
*                                         ! each number
      INTEGER LENGTH(MAXCOL)              ! Length of each field in the record
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*132 DUMMY
      LOGICAL JUMPOUT
      INTEGER LP,ICNT,START
*-

*    Check status
      IF (STATUS.NE.SAI__OK) RETURN

      JUMPOUT=.FALSE.
      ICNT=0
*
      DO WHILE (.NOT. JUMPOUT)

*      Read record
        CALL FIO_READF( DFD, DUMMY, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 100

*      Test the string for various keywords
        IF (INDEX(DUMMY, 'P NRECORDS') .NE. 0) THEN
          READ(DUMMY(61:72), *)NROWS
        ELSE IF ( INDEX(DUMMY(1:3), 'F ') .NE. 0 ) THEN
          ICNT=ICNT+1
          START = INDEX(DUMMY, 'F ')
          CNAME(ICNT)(1:17)=DUMMY(START+2:START+18)

*        Ensure name strings aren't duplicated
          DO LP=1,ICNT-1
            IF (CNAME(ICNT) .EQ. CNAME(LP)) THEN
              CNAME(ICNT)=CNAME(ICNT)(1:CHR_LEN(CNAME(ICNT)))
     :                        // '_ZZZ'
            ENDIF
          END DO

          READ(DUMMY(20:40), *) STPOS(ICNT), LENGTH(ICNT)
          UNITS(ICNT) = DUMMY(45:55)

*        Determine type
          IF (INDEX(DUMMY(20:40),'D').NE.0) THEN
            TYPE(ICNT)='_DOUBLE'
          ELSE IF (INDEX(DUMMY(20:40),'E').NE.0) THEN
            TYPE(ICNT)='_REAL'
          ELSE IF (INDEX(DUMMY(20:40),'I').NE.0) THEN
            TYPE(ICNT)='_INTEGER'
          ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'F', CNAME(ICNT) )
            CALL ERR_REP( ' ', 'AST_ERR: DSCF field ^F is not a '/
     :                    /'numeric type', STATUS )
          END IF

        END IF

      END DO

*    Annul end-of-file status code
 100  IF ( STATUS .EQ. FIO__EOF ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

*    Find number of COLUMNS
      NCOLS = ICNT
      IF (STATUS.NE.SAI__OK) THEN
        CALL ERR_REP( ' ', '...from TEXT2HDS_DSCFREAD', STATUS )
      END IF

      END
