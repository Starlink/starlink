*+  SSDUMP - Dump sources from SSDS to ascii
      SUBROUTINE SSDUMP( STATUS )
*
*    Description :
*
*     Dumps the contents of both simple SSDS and SSDS_SETs to ascii. A
*     header can be output describing the contents of the table.
*
*    Method :
*
*     Create table description
*      If ssds_set display ID
*      X_CORR then Y_CORR
*      RA and DEC
*
*    Environment parameters :
*
*     INP                   = UNIV(R)
*           The SSDS to dump
*     HEADER                = LOGICAL(R)
*           Display informative header - default Y
*     HMS                   = LOGICAL(R)
*           Display equatorial coords in sexigessimal - default Y
*     DEVICE                = LITERAL(R)
*           Where to send the output. P(rinter), N(ewfile), O(ldfile),
*           T(erminal), or filename.
*     ERROR                 = LOGICAL(R)
*           Display errors on fields if present
*     FLDS                  = CHAR(R)
*           List of fields to display
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Feb 90 : V1.2-0  Original (DJA)
*     14 Feb 90 : V1.2-1  Now copes with corrected flux lists too (DJA)
*      6 Mar 90 : V1.2-2  Bit more info into header (DJA)
*     14 Apr 90 : V1.2-3  Extended to look at BOOK structure when a header
*                         is displayed. (DJA)
*     28 Apr 90 : V1.2-4  Checks for presence of positional errors. Valid
*                         SSDS can have this component missing if PSS was
*                         run in parameteisation mode (DJA)
*      9 May 90 : V1.2-5  Added DECIMAL option (DJA)
*     11 May 90 : V1.2-6  Outputs flux units in header (DJA)
*     19 Jul 90 : V1.2-7  Outputs EQNZ list if present. Tests for existance
*                         of equinox value (DJA)
*     10 Aug 90 : V1.2-8  Uses SSO_ASSOCI to get input locator (DJA)
*     13 Aug 90 : V1.2-9  Output file number format changed to I3 (DJA)
*      2 Oct 90 : V1.3-0  Processes background list if present (DJA)
*     24 Feb 91 : V1.4-0  HMS keyword (DJA)
*     20 Aug 91 : V1.5-1  Updated SSO stuff. Output proceddure generalised (DJA)
*      4 Dec 91 : V1.6-0  Supports vector fields. Uses STR_DRADTOC. (DJA)
*     10 Mar 92 : V1.6-1  Use new SSO BOOK structure access routines (DJA)
*      1 Jul 92 : V1.6-2  Writes source number (DJA)
*      6 Aug 92 : V1.6-3  Field access allows new fields (DJA)
*     14 Sep 92 : V1.6-4  Copes with line overflow using FLDS parameter and
*                         second table if needed (DJA)
*     14 Jan 93 : V1.7-0  Copes with arbitrary number of records (DJA)
*     25 Mar 93 : V1.7-1  Improved precision for HRI data (DJA)
*     12 May 93 : V1.7-2  Fixed bug displaying double precision data (DJA)
*      4 May 94 : V1.7-3  Upgraded i.o to use AIO (DJA)
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*      8 Feb 95 : V1.8-1  Fixed bug in multi-page output (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'SSDUMP_STR'
*
*    Status :
*
      INTEGER STATUS
*
*    Function definitions :
*
      INTEGER               CHR_LEN
      INTEGER               SSDUMP_FN
      LOGICAL               CHR_INSET
*
*    Local constants :
*
      INTEGER               MXCOL                     ! Max columns of data
        PARAMETER           ( MXCOL = 20 )
*
*    Local variables :
*
      RECORD /FIELD/        FLD(MXCOL)                ! Field display info

      CHARACTER             CLOC*(DAT__SZLOC)         ! POSIT component
      CHARACTER*(DAT__SZLOC)EQLOC                     ! Equinox component
      CHARACTER*200         DASHES
      CHARACTER*20          DATE                      ! Date of search
      CHARACTER*132         FILENAME                  ! File searched
      CHARACTER*80          FLIST                     ! Field name list
      CHARACTER             NAME*(DAT__SZNAM)         ! Object name
      CHARACTER*132         PATH, FILE
      CHARACTER             POSLOC*(DAT__SZLOC)       ! POSIT object
      CHARACTER*30          PROGNAME                  ! Search program name
      CHARACTER             SLOC*(DAT__SZLOC)         ! SSDS to dump
      CHARACTER*200         TEXT                      ! Output data
      CHARACTER             TYPE*(DAT__SZNAM)         ! Object type

      REAL                  EQUINOX                   ! Equinox of observation

      INTEGER               CCOL                      ! Current column
      INTEGER               CDIF                      ! Column difference
      INTEGER               CRHS                      ! Field RHS column
      INTEGER               DCOL                      ! Extreme header column
      INTEGER               DEVWID                    ! Output width
      INTEGER               FFCOL                     ! First field column
      INTEGER               FLEN                      ! Field name list
      INTEGER               FN                        ! Field number
      INTEGER               ICOMP                     ! Loop over items in POSIT
      INTEGER               OCH                       ! Output channel
      INTEGER               MLEN
      INTEGER               NCOMP                     ! # files in SSDS
      INTEGER               I, J, NPLEV
      INTEGER               NPAGE                     ! # pages of output
      INTEGER               NPCOMP                    ! # items in POSIT
      INTEGER               NSRC                      ! # sources in SSDS
      INTEGER               NFLD                      ! # fields to display
      INTEGER               NFLDF                     ! Filtered NFLD

      LOGICAL               DOERR                     ! Output field errors?
      LOGICAL               HEADER                    ! Display a header?
      LOGICAL               HMS                       ! RA/DEC in HMS?
      LOGICAL               IS_SET                    ! Is the SSDS a set?
      LOGICAL               OK                        ! Validity test
      LOGICAL               POINT_OK                  ! Pointing data there?
      LOGICAL               SYMMETRIC                 ! Symmetric errors?
*
*    Version id :
*
      CHARACTER*30          VERSION
        PARAMETER           ( VERSION = 'SSDUMP Version 1.8-0' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version announcement
      CALL MSG_PRNT( VERSION )

*    Get Asterix going
      CALL AST_INIT( )
      CALL SSO_INIT( )

*    Initialise the table description
      NFLD = 0
      CALL CHR_FILL( '-', DASHES )

*    Get input object from user
      CALL SSO_ASSOCI( 'INP', 'READ', SLOC, IS_SET, STATUS )

*    Check for POSIT structure, otherwise nothing much to report
      CALL SSO_GETNSRC( SLOC, NSRC, STATUS )

*    Output field errors?
      CALL USI_GET0L( 'ERRORS', DOERR, STATUS )

*    Open device
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, DEVWID, STATUS )

*    Locate book-keeping structure
      CALL SSO_CHKBOOK( SLOC, OK, NCOMP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Number of sources defines width of first column
      FFCOL = 3 + INT(LOG10(REAL(MAX(NSRC,1))))
      CCOL = FFCOL

*    Skip mapping if no sources
      IF ( NSRC .EQ. 0 ) GOTO 49

*    Search for fields
      CALL DAT_FIND( SLOC, 'POSIT', POSLOC, STATUS )
      CALL DAT_NCOMP( POSLOC, NPCOMP, STATUS )
      DO ICOMP = 1, NPCOMP

*      Locate component
        CALL DAT_INDEX( POSLOC, ICOMP, CLOC, STATUS )

*      Get component name & type
        CALL DAT_NAME( CLOC, NAME, STATUS )

*      Special structures?
        IF ( NAME(1:10) .EQ. 'IMG_COORDS' ) THEN
          CALL SSDUMP_FIND( SLOC, 'X_CORR', DOERR, CCOL, NFLD,
     :                                           FLD, STATUS )
          CALL SSDUMP_FIND( SLOC, 'Y_CORR', DOERR, CCOL, NFLD,
     :                                           FLD, STATUS )

        ELSE IF ( NAME(1:10) .EQ. 'CEL_COORDS' ) THEN

*        Celestial coordinates
          CALL SSDUMP_FIND( SLOC, 'RA', DOERR, CCOL, NFLD, FLD,
     :                                                 STATUS )
          CALL SSDUMP_FIND( SLOC, 'DEC', DOERR, CCOL, NFLD, FLD,
     :                                                  STATUS )
          POINT_OK = ( ( FLD(NFLD-1).FLD(1:2) .EQ. 'RA' ) .AND.
     :                 ( FLD(NFLD).FLD(1:3) .EQ. 'DEC' ) )

*        Display in hms?
          IF ( POINT_OK ) THEN
            CALL USI_GET0L( 'HMS', HMS, STATUS )
            FLD(NFLD-1).HMS = HMS
            FLD(NFLD).HMS = HMS
          END IF
          IF ( STATUS .NE. SAI__OK ) GOTO 99

        ELSE

*        Make sure component is a field
          CALL DAT_TYPE( CLOC, TYPE, STATUS )
          IF ( (TYPE(1:4) .EQ. 'LIST') .OR.
     :         (TYPE(1:9).EQ.'EXTENSION') ) THEN
            CALL SSDUMP_FIND( SLOC, NAME(:CHR_LEN(NAME)), DOERR,
     :                                 CCOL, NFLD, FLD, STATUS )
          END IF

        END IF

*      Free object
        CALL DAT_ANNUL( CLOC, STATUS )

      END DO
      CALL DAT_ANNUL( POSLOC, STATUS )

*    Construct field list string
      DO I = 1, NFLD
        IF ( I .EQ. 1 ) THEN
          FLIST = FLD(I).FLD
          FLEN = CHR_LEN(FLIST)
        ELSE
          CALL MSG_SETC( 'REST', FLIST )
          CALL MSG_SETC( 'FLD', FLD(I).FLD )
          CALL MSG_MAKE( '^REST,^FLD', FLIST, FLEN )
        END IF
      END DO

*    Define default for fields if overflowing page width
      IF ( CCOL .GT. 131 ) THEN
        CALL USI_DEF0C( 'FLDS', FLIST(:FLEN), STATUS )
      END IF
      CALL USI_GET0C( 'FLDS', FLIST, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Parse field list string
      FLEN = CHR_LEN(FLIST)
      IF ( FLIST(:FLEN) .NE. '*' ) THEN

*      Check presence of each field
        NFLDF = 0
        DO I = 1, NFLD
          IF ( CHR_INSET(FLIST(:FLEN),FLD(I).FLD) ) THEN
            NFLDF = NFLDF + 1
            FLD(NFLDF) = FLD(I)
          ELSE IF ( I .LT. NFLD ) THEN
            CDIF = FLD(I+1).COL - FLD(I).COL
            IF ( DOERR .AND. FLD(I).ETHERE ) THEN
              CDIF = CDIF + FLD(I+1).ECOL - FLD(I).ECOL
            END IF
            DO J = I + 1, NFLD
              FLD(J).COL = FLD(J).COL - CDIF
              IF ( DOERR .AND. FLD(I).ETHERE ) THEN
                FLD(J).ECOL = FLD(J).ECOL - CDIF
              END IF
            END DO
            CCOL = CCOL - CDIF
          END IF
        END DO
        NFLD = NFLDF

      END IF

*    Output still too wide?
      IF ( CCOL .GT. 131 ) THEN

*      Print warning
        CALL MSG_PRNT( '! Selected fields fill more than 1 page - '/
     :                                     /'output will be split' )

*      Adjust columns to fit on pages
        CDIF = 0
        NPAGE = 1
        DO I = 1, NFLD
          IF ( DOERR .AND. FLD(I).ETHERE ) THEN
            CRHS = FLD(I).ECOL + FLD(I).EWID
          ELSE
            CRHS = FLD(I).COL + FLD(I).WID
          END IF
          IF ( CRHS .GT. 132 ) THEN
            IF ( CRHS .GT. 132*NPAGE ) THEN
              CDIF = CDIF + FLD(I).COL + (NPAGE-1)*132 - FFCOL
              NPAGE = NPAGE + 1
            END IF
            FLD(I).COL = FLD(I).COL - CDIF
            IF ( DOERR .AND. FLD(I).ETHERE ) THEN
              FLD(I).ECOL = FLD(I).ECOL - CDIF
            END IF
          END IF
        END DO

      ELSE
        NPAGE = 1
      END IF

*    Symmetric errors?
      CALL SSO_GETPAR0L( SLOC, 1, 'SYMMETRIC', SYMMETRIC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        SYMMETRIC = .TRUE.
      END IF

*    Enough room for the files table
 49   CCOL = MAX( 110, CCOL )

*    Write out heading
      CALL USI_GET0L( 'HEADER', HEADER, STATUS )
      IF ( HEADER ) THEN

*      Last column of header
        DCOL = MIN( 132, CCOL )

*      Top line
        CALL AIO_WRITE( OCH, DASHES(:DCOL), STATUS )

*      Get dataset name
        CALL HDS_TRACE( SLOC, NPLEV, PATH, FILE, STATUS )
        CALL MSG_SETC( 'SSDS', FILE )
        IF ( NCOMP .GT. 1 ) THEN
          CALL MSG_SETI( 'N', NCOMP )
          CALL MSG_MAKE( 'Results file ^SSDS contains data'/
     :             /' from ^N source searches', TEXT, MLEN )
        ELSE
          CALL MSG_MAKE( 'Results file ^SSDS contains data'/
     :              /' from one source search', TEXT, MLEN )
        END IF
        CALL AIO_WRITE( OCH, TEXT(:MLEN), STATUS )
        CALL AIO_BLNK( OCH, STATUS )

*      Display contents of BOOK structure
        WRITE( TEXT, '(5X,A,T10,A,T90,A)' ) 'N', 'Searched Dataset','At'
        CALL AIO_WRITE( OCH, TEXT, STATUS )
        DO I = 1, NCOMP
          CALL SSO_GETPAR0C( SLOC, I, 'SEARCHED', FILENAME, STATUS )
          CALL SSO_GETPAR0C( SLOC, I, 'CREATED', DATE, STATUS )
          WRITE( TEXT, '(3X,I3,T10,A,T90,A)' ) I, FILENAME
     :                         (:CHR_LEN(FILENAME)), DATE
          CALL AIO_WRITE( OCH, TEXT, STATUS )
        END DO
        CALL AIO_BLNK( OCH, STATUS )

*      Tell user about program which generated data
        CALL SSO_GETPAR0C( SLOC, 1, 'CREATOR', PROGNAME, STATUS )
        CALL AIO_WRITE( OCH, '   Dataset was searched by '//PROGNAME,
     :                  STATUS )
        CALL AIO_BLNK( OCH, STATUS )

*      Abort if no sources
        IF ( NSRC .EQ. 0 ) THEN
          CALL AIO_WRITE( OCH, DASHES(:DCOL), STATUS )
          GOTO 69
        END IF

*      RA,DEC
        IF ( POINT_OK ) THEN
          CALL HDX_FIND( SLOC, 'BOOK(1).MORE.ASTERIX.HEADER.EQUINOX',
     :                                                EQLOC, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_GET0R( EQLOC, EQUINOX, STATUS )
 20         FORMAT( 1X,'Equatorial coordinates, equinox ', F6.1, :, A )
            IF ( HMS ) THEN
              WRITE( TEXT, 20 ) EQUINOX
            ELSE
              WRITE( TEXT, 20 ) EQUINOX, ', units DEGREES'
            END IF
            CALL AIO_WRITE( OCH, TEXT, STATUS )
            CALL DAT_ANNUL( EQLOC, STATUS )
          ELSE
            CALL ERR_ANNUL( STATUS )
          END IF
        END IF

*      Positional error data
        FN = SSDUMP_FN( 'ERRORS', NFLD, FLD )
        IF ( DOERR .AND. (FN.GT.0) ) THEN
          CALL SSDUMP_EFMT( SLOC, 'ERRORS', STATUS )
          CALL MSG_MAKE( 'Positional errors are at ^ELEVS confidence'/
     :                        /', units '//FLD(FN).UNITS, TEXT, MLEN )
          CALL AIO_WRITE( OCH, TEXT(:MLEN), STATUS )
        END IF

*      Tell user about flux units
        FN = SSDUMP_FN( 'FLUX', NFLD, FLD )
        IF ( FN .GT. 0 ) THEN
          IF ( FLD(FN).ETHERE ) THEN
            CALL SSDUMP_EFMT( SLOC, 'FLUX', STATUS )
            CALL MSG_SETC( 'UNIT', FLD(FN).UNITS )
            CALL MSG_MAKE( 'Flux units are ^UNIT, errors are at ^ELEVS'/
     :                                      /' confidence', TEXT, MLEN )
          ELSE
            CALL MSG_SETC( 'UNIT', FLD(FN).UNITS )
            CALL MSG_MAKE( 'Flux units are ^UNIT', TEXT, MLEN )
          END IF
          CALL AIO_WRITE( OCH, TEXT(:MLEN), STATUS )
        END IF

*      Tell user about bgnd units
        FN = SSDUMP_FN( 'BACK', NFLD, FLD )
        IF ( FN .GT. 0 ) THEN
          IF ( FLD(FN).ETHERE ) THEN
            CALL SSDUMP_EFMT( SLOC, 'BACK', STATUS )
            CALL MSG_SETC( 'UNIT', FLD(FN).UNITS )
            CALL MSG_MAKE( 'Bgnd units are ^UNIT, errors are at ^ELEVS'/
     :                                      /' confidence', TEXT, MLEN )
          ELSE
            CALL MSG_SETC( 'UNIT', FLD(FN).UNITS )
            CALL MSG_MAKE( 'Bgnd units are ^UNIT', TEXT, MLEN )
          END IF
          CALL AIO_WRITE( OCH, TEXT(:MLEN), STATUS )
        END IF

*      Extension test
        IF ( SSDUMP_FN( 'EXTEN', NFLD, FLD ) .NE. 0 ) THEN
          CALL AIO_WRITE( OCH, 'Extension in arcmin, error '/
     :                         /'at 68% confidence', STATUS )
        END IF

*      End of header
        CALL AIO_WRITE( OCH, DASHES(:DCOL), STATUS )
        CALL AIO_BLNK( OCH, STATUS )

      END IF

*    Do output
      IF ( NSRC .GT. 0 ) THEN
        CALL SSDUMP_INT( OCH, NPAGE, NSRC, NFLD, FLD, SYMMETRIC,
     :                                                  STATUS )
      END IF

*    Close
 69   CALL SSO_RELEASE( SLOC, STATUS )

*    Close output channel
      CALL AIO_CANCL( 'DEV', STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL SSO_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  SSDUMP_INT - Performs output for SSDUMP
      SUBROUTINE SSDUMP_INT( OCH, NPAGE, NSRC, NFLD, FLD, SYMMETRIC,
     :                                                      STATUS )
*
*    Description :
*
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Feb 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'MATH_PAR'
*
*    Structure definitions :
*
      INCLUDE 'SSDUMP_STR'

      STRUCTURE /ADATUM/
       UNION
        MAP
         REAL               RVAL
        END MAP
        MAP
         DOUBLE PRECISION   DVAL
        END MAP
        MAP
         INTEGER            IVAL
        END MAP
       END UNION
      END STRUCTURE
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      REAL                  NULLERROR
        PARAMETER           ( NULLERROR = -1.0 )
*
*    Functions :
*
      INTEGER               CHR_LEN
*
*    Import :
*
      INTEGER               OCH                       ! Output channel
      INTEGER               NPAGE                     ! Number of pages
      INTEGER               NSRC                      ! No. of sources in SSDS
      INTEGER               NFLD                      ! Number of fields
      RECORD /FIELD/        FLD(*)                    ! Field data
      LOGICAL               SYMMETRIC                 ! Error data symmetric?
*
*    Local variables :
*
      RECORD /ADATUM/       DATUM                     ! General data object

      CHARACTER*20          ESTR                      ! Error value string
      CHARACTER*132         LINE                      ! Output line

      INTEGER               CRHS                      ! Right hand column
      INTEGER               FCW                       ! Width of number column
      INTEGER               FSTAT                     ! i/o status
      INTEGER               HLEN 		      ! Length of HEAD
      INTEGER               I                         ! Loop over sources
      INTEGER               IEB                       ! Indexes EB array
      INTEGER               IFLD, JFLD                ! Field counters
      INTEGER               J                         ! Loop over fields
      INTEGER               L                         ! Loop over o/p lines
      INTEGER               LPS                       ! Lines per source
      INTEGER               P                         ! Loop over pages

      LOGICAL               ANYERR                    ! Any errors present?
      LOGICAL               FIRST_CEL                 ! First of RA,DEC
      LOGICAL               FOUND                     ! Found last field on page
*
*    Local data :
*
      CHARACTER*2           EB(3)
        DATA                EB/'+-',' +',' -'/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      IFLD = 1

*    Width of first column in digits
      FCW = INT(LOG10(REAL(NSRC))) + 1

*    Loop over pages
      DO P = 1, NPAGE

*      Find last field on this page
        JFLD = IFLD + 1
        FOUND = .FALSE.
        DO WHILE ( (JFLD.LE.NFLD) .AND. .NOT. FOUND )
          IF ( JFLD .GT. 1 ) THEN
            IF ( FLD(JFLD).COL .GT. FLD(JFLD-1).COL ) THEN
              JFLD = JFLD + 1
            ELSE
              FOUND = .TRUE.
              JFLD = MAX(IFLD,JFLD - 1)
            END IF
          ELSE
            JFLD = JFLD + 1
          END IF
        END DO
        JFLD = MIN(JFLD,NFLD)

*      Do any of the fields on this page have errors?
        ANYERR = .FALSE.
        DO J = IFLD, JFLD
          ANYERR = ( ANYERR .OR. FLD(J).ETHERE )
        END DO

*      Rightmost column
        IF ( FLD(JFLD).ETHERE ) THEN
          CRHS = FLD(JFLD).ECOL
        ELSE
          CRHS = FLD(JFLD).COL
        END IF

*      Construct column headings
        CALL CHR_FILL( ' ', LINE )
        DO J = IFLD, JFLD
          HLEN = CHR_LEN(FLD(J).HEAD)
          IF ( FLD(J).ETHERE ) THEN
            LINE(FLD(J).COL+(FLD(J).WID+2+FLD(J).EWID-
     :            HLEN)/2:) = FLD(J).HEAD
          ELSE
            LINE(FLD(J).COL+(FLD(J).WID-HLEN)/2:) = FLD(J).HEAD
          END IF
        END DO
        CALL AIO_WRITE( OCH, LINE(:MAX(131,CRHS)), STATUS )

*      Lines per source
        LPS = 1
        DO J = IFLD, JFLD
          LPS = MAX( LPS, FLD(J).NELM, FLD(J).ENELM )
        END DO

*      Loop over data and output source data
        DO I = 1, NSRC

*        For each line of output per source
          DO L = 1, LPS

*          Clear buffer
            CALL CHR_FILL( ' ', LINE )

*          Write source number
            IF ( L .EQ. 1 ) THEN
              WRITE( LINE, 10 ) I
 10           FORMAT( I<FCW> )
            END IF

*          Loop over fields
            FIRST_CEL = .TRUE.
            DO J = IFLD, JFLD

*            Field datum to do?
              IF ( L .LE. FLD(J).NELM ) THEN

*              Get the value
                CALL ARR_COP1B( FLD(J).SIZE, %VAL(FLD(J).PTR +
     :               ((I-1)*FLD(J).NELM+(L-1))*FLD(J).SIZE),
     :                          DATUM, STATUS )

*              Write the value
                IF ( FLD(J).HMS ) THEN
                  IF ( FIRST_CEL ) THEN
                    FIRST_CEL = .FALSE.
                    CALL STR_DRADTOC( DBLE(DATUM.DVAL*MATH__DTOR),
     :                 'HH MM SS.SS', LINE(FLD(J).COL:), STATUS )
                  ELSE
                    CALL STR_DRADTOC( DBLE(DATUM.DVAL*MATH__DTOR),
     :                  'SDD MM SS.S', LINE(FLD(J).COL:), STATUS )
                  END IF
                ELSE
                  IF ( FLD(J).TYPE(1:5) .EQ. '_REAL' ) THEN
                    WRITE( LINE(FLD(J).COL:), '('//
     :                   FLD(J).DFMT//')', IOSTAT=FSTAT ) DATUM.RVAL
                  ELSE IF ( FLD(J).TYPE(1:7) .EQ. '_DOUBLE' ) THEN
                    WRITE( LINE(FLD(J).COL:), '('//
     :                   FLD(J).DFMT//')', IOSTAT=FSTAT ) DATUM.DVAL
                  ELSE
                    WRITE( LINE(FLD(J).COL:), '('//
     :                   FLD(J).DFMT//')', IOSTAT=FSTAT ) DATUM.IVAL
                  END IF
                END IF

              END IF

*            Write the error value
              IF ( FLD(J).ETHERE .AND. (L.LE.FLD(J).ENELM) ) THEN

*              Get the value
                CALL ARR_COP1R( 1, %VAL(FLD(J).EPTR +((I-1)*FLD(J).ENELM
     :                                +(L-1))*VAL__NBR), DATUM, STATUS )

*              Error item
                IF ( SYMMETRIC ) THEN
                  IEB = 1
                ELSE
                  IF ( ((L/2)*2) .EQ. L ) THEN
                    IEB = 2
                  ELSE
                    IEB = 3
                  END IF
                END IF

*              Null error?
                IF ( DATUM.RVAL .EQ. NULLERROR ) THEN
                  ESTR = ' *NA*'
                ELSE
                  WRITE( ESTR, '('//FLD(J).EFMT//')',
     :                       IOSTAT=FSTAT ) DATUM.RVAL
                END IF

*              Write to buffer
                WRITE( LINE(FLD(J).ECOL:), '(2A)', IOSTAT=FSTAT )
     :                               EB(IEB),ESTR(:CHR_LEN(ESTR))

              END IF

            END DO

*          Write the line of text
            CALL AIO_WRITE( OCH, LINE, STATUS )

          END DO

        END DO

*      Next lot of fields
        IFLD = JFLD + 1

*      Blank line before next page of output
        IF ( IFLD .LE. NFLD ) THEN
          CALL AIO_BLNK( OCH, STATUS )
        END IF

      END DO


      END


*+  SSDUMP_FIND - Locate and map a field if present
      SUBROUTINE SSDUMP_FIND( SLOC, FNAME, DO_ERRORS, CCOL, NFLD,
     :                                              FLD, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     19 Aug 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'SSDUMP_STR'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)   SLOC                    ! Results file
      CHARACTER*(*)            FNAME                   ! Field to map
      LOGICAL                  DO_ERRORS               ! Output field errors?
*
*    Import / Export :
*
      INTEGER                  CCOL                    ! Current printing column
      INTEGER                  NFLD                    ! Number of fields
      RECORD /FIELD/           FLD(*)                  ! Field data
*
*    Local constants :
*
      INTEGER                  NFIELD
        PARAMETER              ( NFIELD = 12 )
*
*    Local variables :
*
      CHARACTER                FLOC*(DAT__SZLOC)       ! Field locator
      CHARACTER*10             DFMT                    ! Field data format
      CHARACTER                TYPE*(DAT__SZTYP)       ! Field type

      INTEGER                  I                       ! Loop over internal data
      INTEGER                  PTR                     ! Mapped field data
      INTEGER                  SIZE                    ! Size of a type
      INTEGER                  WID                     ! Field width

      LOGICAL                  FOUND                   ! Found field in table?
      LOGICAL                  GOT_ERRORS              ! Field erros to be used?
      LOGICAL                  OK                      ! Validity test
*
*    Local data :
*
      CHARACTER*(DAT__SZNAM)   FIELD(NFIELD)           ! Field names
      DATA      FIELD          /'ID'    ,'X_CORR' ,'Y_CORR',
     :                          'RA'    ,'DEC'    ,'FLUX',
     :                          'SIGNIF','ERRORS' ,'BACK',
     :                          'EXTEN', 'EXSIG'  ,'CFLUX'/
      CHARACTER*(DAT__SZNAM)   FTYPE(NFIELD)           ! Mapping type
      DATA      FTYPE          /'_INTEGER' ,'_REAL'   ,'_REAL',
     :                          '_DOUBLE'  ,'_DOUBLE' ,'_REAL',
     :                          '_REAL'    ,'_REAL'   ,'_REAL',
     :                          '_REAL'    ,'_REAL'   ,'_REAL'/
      LOGICAL                  FERRORS(NFIELD)         ! Field errors sensible?
      DATA      FERRORS        /.FALSE. ,.FALSE.  ,.FALSE.,
     :                          .FALSE. ,.FALSE.  ,.TRUE.,
     :                          .FALSE.  ,.FALSE.  ,.TRUE.,
     :                          .TRUE.  ,.FALSE.  ,.TRUE./
      CHARACTER*(SZHEAD)       FHEAD(NFIELD)           ! Field description
      DATA      FHEAD          /'File'		,'X_CORR'
     :			       ,'Y_CORR'	,'RA'
     :			       ,'DEC'		,'Raw Flux'
     :                         ,'Signif'        ,'Perr'
     :                         ,'Bgnd'          ,'Extension'
     :                         ,'Ext_Sig'       ,'Cor_Flux'/
      CHARACTER*10             FDFMT(NFIELD)           ! Field data format
      DATA      FDFMT          /'I4'    ,'F9.4'   ,'F9.4',
     :                          'F11.6' ,'F11.6'  ,'1PG10.3',
     :                          'F8.3'  ,'F6.3'   ,'1PG9.3',
     :                          'F5.2'  ,'F6.2'   ,'1PE10.4'/
      CHARACTER*10             FEFMT(NFIELD)           ! Field error format
      DATA      FEFMT          /' '    ,'F9.4'   ,'F9.4',
     :                          'F10.6' ,'F10.6'  ,'1PG9.2',
     :                          'F8.3'  ,' '   ,'1PG9.2','F5.2',' ',
     :                          '1PE10.4'/
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Does field exist?
        CALL SSO_CHKFLD( SLOC, FNAME, OK, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          OK = .FALSE.
        END IF

        IF ( OK ) THEN

*        Look up in data table
          I = 1
          FOUND = .FALSE.
          DO WHILE ( ( I .LE. NFIELD ) .AND. .NOT. FOUND )
            IF ( FNAME .EQ. FIELD(I)(1:LEN(FNAME)) ) THEN
              FOUND = .TRUE.
            ELSE
              I = I + 1
            END IF
          END DO

*        Extract data from table if found...
          IF ( FOUND ) THEN

*          Map with required type
            TYPE = FTYPE(I)

*          Field errors to be processed?
            IF ( FERRORS(I) .AND. DO_ERRORS ) THEN
              CALL SSO_CHKFLDERR( SLOC, FNAME, GOT_ERRORS, STATUS )
            ELSE
              GOT_ERRORS = .FALSE.
            END IF

*          Data from table
            DFMT = FDFMT(I)

*        If not found, map as _REAL, use errors if present
          ELSE

            TYPE = '_REAL'
            CALL SSO_CHKFLDERR( SLOC, FNAME, GOT_ERRORS, STATUS )
            DFMT = '1PG9.2'

          END IF

*        Map the field
          CALL SSO_MAPFLD( SLOC, FNAME, TYPE, 'READ', PTR, STATUS )
          CALL HDX_TYPSIZ( TYPE, SIZE, STATUS )

*        Get width of the format
          CALL UTIL_FMTWID( DFMT, WID, STATUS )

*        Only bump up the field counter if status good
          IF ( STATUS .EQ. SAI__OK ) THEN

*          Store data
            NFLD = NFLD + 1
            FLD(NFLD).FLD = FNAME
            FLD(NFLD).TYPE = TYPE
            FLD(NFLD).PTR = PTR
            FLD(NFLD).DFMT = DFMT
            FLD(NFLD).COL = CCOL
            FLD(NFLD).SIZE = SIZE
            FLD(NFLD).WID = WID
            FLD(NFLD).HMS = .FALSE.
            CCOL = CCOL + WID + 2

*          Store field heading
            IF ( FOUND ) THEN
              FLD(NFLD).HEAD = FHEAD(I)
            ELSE
              FLD(NFLD).HEAD = FNAME
            END IF

*          Locate field structure
            CALL SSO_LOCFLD( SLOC, FNAME, FLOC, STATUS )

*          Get field data dimensions
            CALL CMP_SHAPE( FLOC, 'DATA_ARRAY', DAT__MXDIM,
     :                        FLD(NFLD).DIMS, FLD(NFLD).NDIM, STATUS )
            CALL ARR_SUMDIM( FLD(NFLD).NDIM-1, FLD(NFLD).DIMS,
     :                                        FLD(NFLD).NELM )

*          Field units present?
            CALL DAT_THERE( FLOC, 'UNITS', OK, STATUS )
            IF ( OK ) THEN
              CALL CMP_GET0C( FLOC, 'UNITS', FLD(NFLD).UNITS, STATUS )
              IF ( FLD(NFLD).UNITS .LE. ' ' )
     :                          FLD(NFLD).UNITS = 'UNDEFINED'
            ELSE
              FLD(NFLD).UNITS = 'UNDEFINED'
            END IF

*          Field errors?
            IF ( GOT_ERRORS ) THEN

*            Map error
              CALL SSO_MAPFLDERR( SLOC, FNAME, '_REAL', 'READ',
     :                                 FLD(NFLD).EPTR, STATUS )

*            Find column
              FLD(NFLD).EFMT = FEFMT(I)
              CALL UTIL_FMTWID( FEFMT(I), FLD(NFLD).EWID, STATUS )
              FLD(NFLD).ECOL = CCOL - 2
              CCOL = CCOL + FLD(NFLD).EWID + 2

            END IF
            FLD(NFLD).ETHERE = ( GOT_ERRORS .AND. (STATUS.EQ.SAI__OK) )

*          Field error data
            IF ( FLD(NFLD).ETHERE ) THEN
              CALL CMP_SHAPE( FLOC, 'ERROR', DAT__MXDIM,
     :                        FLD(NFLD).EDIMS, FLD(NFLD).ENDIM, STATUS )
              CALL ARR_SUMDIM( FLD(NFLD).ENDIM-1, FLD(NFLD).EDIMS,
     :                                           FLD(NFLD).ENELM )
            ELSE
              FLD(NFLD).ENELM = 0
            END IF

*          Free field
            CALL DAT_ANNUL( FLOC, STATUS )

          END IF

        END IF

*      Tidy
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSDUMP_FIND', STATUS )
        END IF

      END IF

      END



*+  SSDUMP_FN - Look up field in field table
      INTEGER FUNCTION SSDUMP_FN( FNAME, NFLD, FLD )
*
*    Description :
*
*     Returns position of a named field in the field table, or zero if
*     not present.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 Aug 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'SSDUMP_STR'
*
*    Import :
*
      CHARACTER*(*)            FNAME                   ! Field to map
      INTEGER                  NFLD                    ! Number of fields
      RECORD /FIELD/           FLD(*)                  ! Field data
*
*    Local variables :
*
      INTEGER                  I                       ! Loop through fields

      LOGICAL                  FOUND                   ! Found the field yet?
*-

*    Scan table
      I = 1
      FOUND = .FALSE.
      DO WHILE ( ( I .LE. NFLD ) .AND. .NOT. FOUND )
        IF ( FNAME .EQ. FLD(I).FLD(1:LEN(FNAME)) ) THEN
          FOUND = .TRUE.
        ELSE
          I = I + 1
        END IF
      END DO

*    Set return value
      IF ( FOUND ) THEN
        SSDUMP_FN = I
      ELSE
        SSDUMP_FN = 0
      END IF

      END



*+  SSDUMP_EFMT - Sets ELEVS token equal to the formatted error levels of FIELD
      SUBROUTINE SSDUMP_EFMT( LOC, FIELD, STATUS )
*
*    Description :
*
*     Retrieves the error confidence levels for a field and formats them
*     into the MSG system token ELEVS.
*
*    Method :
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      5 Dec 91 : Original (DJA)
*
*    Type definitions :
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
      CHARACTER*(DAT__SZLOC)         LOC              ! SSDS dataset
      CHARACTER*(*)                  FIELD            ! Field name
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      INTEGER               MXLEV                     ! Max error levels
        PARAMETER           ( MXLEV = 5 )
*
*    Local variables :
*
      CHARACTER*200         TEXT                      ! Buffer

      REAL                  LEVS(MXLEV)               ! Error conf levels

      INTEGER               I                         ! Loop over levels
      INTEGER               NLEV                      ! Number of levels
      INTEGER               TLEN                      ! Text length
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get levels
      CALL SSO_GETFITEM1R( LOC, FIELD, 'ELEVS', MXLEV,
     :                            LEVS, NLEV, STATUS )

      TEXT = ' '
      TLEN = 1
      IF ( STATUS .EQ. SAI__OK ) THEN

        DO I =1, NLEV - 1
          CALL MSG_SETR( 'LEV', LEVS(I) )
          CALL MSG_MAKE( TEXT(:TLEN)//' ^LEV%,', TEXT, TLEN )
        END DO
        CALL MSG_SETR( 'LEV', LEVS(NLEV) )
        IF ( NLEV .EQ. 1 ) THEN
          CALL MSG_MAKE( TEXT(:TLEN)//' ^LEV%', TEXT, TLEN )
        ELSE
          CALL MSG_MAKE( TEXT(:(TLEN-1))//' and ^LEV%', TEXT, TLEN )
        END IF

      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF
      CALL MSG_SETC( 'ELEVS', TEXT(3:TLEN) )

      END
