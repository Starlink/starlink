*+  HDISPLAY - displays contents of HDS data objects up to 7 dimensions
      SUBROUTINE HDISPLAY( STATUS )
*
*    Description :
*
*     This application outputs the contents of a specified subset of a
*     primitive HDS data object of up to 7 dimensions. For integer data
*     hex, octal or decimal formats may be chosen
*
*    Parameters :
*
*     INP = HDS data object
*            object to be output
*     DEVICE = CHAR
*            output device  { C)ONSOLE,P)RINTER,O)LDFILE,N)EWFILE }
*     SLICE = CHAR
*            description of range of object to be output eg. '(1:10,1:5)'
*     FORMAT = CHAR
*            optional output format
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*              (BHVAD::RJV)
*    History :
*
*      5 May 89 : V1.0-1 Optional format for all types (RJV)
*     15 May 89 : V1.0-2 Fixed bug where program crashed with scalars (DJA)
*      5 Dec 90 : V1.3-0 Allows full 80 char for device name (DJA)
*     28 May 91 : V1.4-0 Full 7D treatment (DJA)
*     29 Aug 91 : V1.5-0 Use PRS_GETSLICE to parse slice spec (DJA)
*      8 May 92 : V1.6-0 Bug fix when displaying _DOUBLEs (DJA)
*     22 May 92 : V1.6-1 Fixed probelm with display of _CHAR*81 objects (DJA)
*      1 Jun 92 : V1.6-2 And cured problem with wid=132 caused by above (DJA)
*      4 May 94 : V1.7-0 Fixed UNIX character problems by using AIO (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) OBJLOC        ! Locator to data object
      CHARACTER*40 RANGESTR                ! String describing range of data
      CHARACTER*15 FMT                     ! Output format

      INTEGER DIMS(DAT__MXDIM)             ! Dimensions
      INTEGER DEFWIDTH                     ! Output width
      INTEGER IDIM                         ! Loop over dimensions
      INTEGER NDIM                         ! Dimensionality
      INTEGER			OCH			! Output channel id
      INTEGER RANGES(2,DAT__MXDIM)         ! Ranges to be output
      INTEGER WIDTH                        ! User supplied width

      LOGICAL OBJOK                        ! Whether valid data object
*
*    Version :
*
      CHARACTER*40             VERSION
        PARAMETER              ( VERSION = 'HDISPLAY Version 1.7-0' )
*-

*    Version id
      CALL MSG_PRNT( VERSION )

*    Get locator to data object and validate it
      CALL DAT_ASSOC( 'INP', 'READ', OBJLOC, STATUS )
      CALL HDISPLAY_VALIDOBJ( OBJLOC, OBJOK, STATUS )

*    If OK so far carry on
      IF ( OBJOK .AND. (STATUS.EQ.SAI__OK) ) THEN

*      Set up output channel
        CALL AIO_ASSOCO( 'DEVICE', 'LIST', OCH, DEFWIDTH, STATUS )

*      Override device width?
        CALL PAR_GET0I( 'WIDTH', WIDTH, STATUS )
        IF ( STATUS .EQ. PAR__NULL ) THEN
          WIDTH = DEFWIDTH
          CALL ERR_ANNUL( STATUS )
        END IF

*      If non-scalar get range to be output
        CALL DAT_SHAPE( OBJLOC, DAT__MXDIM, DIMS, NDIM, STATUS )
        IF ( NDIM .GT. 0 ) THEN
          CALL PAR_GET0C( 'SLICE', RANGESTR, STATUS )
          CALL PRS_GETSLICE( NDIM, DIMS, RANGESTR, RANGES, STATUS )

        ELSE

*        Otherwise fill ranges array with 1's for HDISPLAY_OUT
          CALL ARR_INIT1R( 1, 2*DAT__MXDIM, RANGES, STATUS )

        END IF

*      Pad dimensions to 7D
        DO IDIM = NDIM + 1, DAT__MXDIM
          DIMS(IDIM) = 1
        END DO

*      See if format specified
        CALL PAR_GET0C( 'FORMAT', FMT, STATUS )
        IF ( STATUS .EQ. PAR__NULL ) THEN
          FMT = ' '
          CALL ERR_ANNUL( STATUS )
        END IF

*      Output data values
        CALL HDISPLAY_OUT( OBJLOC, NDIM, DIMS, RANGES, OCH, FMT,
     :                                           WIDTH, STATUS )

*      Close down output channel
        CALL AIO_CANCL( 'DEVICE', STATUS )

      END IF

      END



*+
      SUBROUTINE HDISPLAY_VALIDOBJ(OBJLOC,OBJOK,STATUS)
*
*    Description :
*
*     Validates object by checking that it is a primitive and has
*     data set.
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) OBJLOC    ! locator to object to be validated
*    Export :
      LOGICAL OBJOK                    ! whether object is valid
*    Status :
      INTEGER STATUS
*    Local variables :
      LOGICAL PRIM                     ! whether object primitive
      LOGICAL SET                      ! whether object has been set
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OBJOK = .FALSE.

      CALL DAT_PRIM( OBJLOC, PRIM, STATUS )
      IF ( PRIM ) THEN
        CALL DAT_STATE( OBJLOC, SET, STATUS )
      END IF
      IF ( STATUS .EQ. SAI__OK ) THEN

*      check that object is primitive
        IF ( .NOT. PRIM ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Object is not primitive', STATUS )

*      check data values are set
        ELSE IF ( .NOT. SET ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Data values not set', STATUS )

        ELSE
          OBJOK = .TRUE.

        END IF
      END IF

      END



*+  HDISPLAY_OUT -
      SUBROUTINE HDISPLAY_OUT( LOC, NDIM, DIMS, RANGES, OCH,
     :                            OUTFMT, OUTWIDTH, STATUS )

*    Description :
*
*    Method :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) LOC       ! locator to data object
      INTEGER NDIM                     ! Dimensionality
      INTEGER DIMS(DAT__MXDIM)         ! Dimensions
      INTEGER RANGES(2,DAT__MXDIM)     ! ranges to be output
      INTEGER 			OCH                      ! Output channel
      INTEGER OUTWIDTH                 ! output width
      CHARACTER*(*) OUTFMT             ! output format
*    Status :
      INTEGER STATUS
*
*    Functions :
*
      LOGICAL          HDX_TYPINT
*
*    Local Constants :
*
      CHARACTER*10 MODE                ! mode for mapping
      PARAMETER (MODE='READ')
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')
*
*    Local variables :
*
      CHARACTER*(DAT__SZTYP) TYPE      ! Type of data object
      CHARACTER*80 FILE                ! Container file name
      CHARACTER*80 PATH                ! Full object specification
      CHARACTER*80 STR

      INTEGER FWID                     ! Field width
      INTEGER NLEV                     ! Number of levels of data
      INTEGER OPTR                     ! Pointer to mapped data
      INTEGER FCO                      ! Converter object

      LOGICAL INTYP                    ! Whether integer type
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Output header information
      CALL AIO_BLNK( OCH, STATUS )
      CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )
      CALL AIO_WRITE( OCH, 'File name===>'//FILE, STATUS )
      CALL AIO_WRITE( OCH, 'Object name=>'//PATH, STATUS )
      CALL AIO_BLNK( OCH, STATUS )
      CALL STR_OBDESC( LOC, STR, STATUS )
      CALL AIO_WRITE( OCH, STR, STATUS )
      CALL AIO_BLNK( OCH, STATUS )

      IF ( NDIM .GT. 0 ) THEN

*      Get type of object
        CALL DAT_TYPE( LOC, TYPE, STATUS )
        INTYP = HDX_TYPINT( TYPE )

*      Get format and field width (OUTFMT optionally contains user value)
        CALL HDISPLAY_GETFMT( TYPE, FWID, OUTFMT, STATUS )

*      Choose formatter
        CALL AIO_CREFCO( TYPE, OUTFMT, FCO, STATUS )

*      Map data
        IF ( INTYP ) THEN
          CALL DAT_MAPI( LOC, MODE, NDIM, DIMS, OPTR, STATUS )
        ELSE
          CALL DAT_MAP( LOC, TYPE, MODE, NDIM, DIMS, OPTR, STATUS )
        END IF

*      Display the data - add one to field width to separate columns
        CALL HDISPLAY_INT( NDIM, DIMS, RANGES, OPTR, FWID,
     :                     FCO, OCH, OUTWIDTH, STATUS )

*      And free it
        CALL DAT_UNMAP( LOC, STATUS )

*      Free the formatter
        CALL AIO_FREFCO( FCO, STATUS )

      END IF

      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HDISPLAY_OUT', STATUS )
      END IF

      END




*+  HDISPLAY_GETFMT - Find field width of a given data type and return format
      SUBROUTINE HDISPLAY_GETFMT( TYPE, WID, FORM, STATUS )
*
*    Description :
*
*     Chooses a default format based on object type, unless user has
*     supplied one in FORM. The field width is found.
*
*    Author  :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     12 May 89 : Original
*
*    Type declarations :
*
      IMPLICIT NONE
*
      INTEGER	       STATUS		      ! Run-time error code
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      CHARACTER        TYPE*(DAT__SZTYP)      ! Locator to data object
*
*    Export :
*
      INTEGER          WID                    ! Field width required
      CHARACTER*(*)    FORM                   ! Output format
*
*    Functions :
*
      INTEGER          CHR_LEN
*
*    Local variables :
*
      INTEGER          BEG,END                ! Character pointers
      INTEGER          FSTAT                  ! I/O status code
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Use default format?
      IF ( FORM .EQ. ' ' ) THEN

        IF ( TYPE(:5) .EQ. '_CHAR' ) THEN
          IF ( TYPE(7:) .GT. ' ' ) THEN
            FORM = 'A'//TYPE(7:)
            CALL CHR_CTOI( TYPE(7:), WID, STATUS )
          ELSE
            FORM = 'A1'
          END IF

        ELSE IF ( TYPE(:5) .EQ. '_REAL' ) THEN
          FORM = '1PG14.6'

        ELSE IF ( TYPE(:7) .EQ. '_DOUBLE' ) THEN
          FORM = '1PG20.12'

        ELSE IF ( TYPE(:8) .EQ. '_INTEGER' ) THEN
          FORM = 'I11'

        ELSE IF ( TYPE(:5) .EQ. '_BYTE' ) THEN
          FORM = 'I4'

        ELSE IF ( TYPE(:6) .EQ. '_UBYTE' ) THEN
          FORM = 'I3'

        ELSE IF ( TYPE(:5) .EQ. '_WORD' ) THEN
          FORM = 'I6'

        ELSE IF ( TYPE(:6) .EQ. '_UWORD' ) THEN
          FORM = 'I5'

        ELSE IF ( TYPE(:8) .EQ. '_LOGICAL' ) THEN
          FORM = 'L1'

        END IF

      END IF

*    Locate the meaty character of the format
      CALL CHR_UCASE( FORM )
      BEG = 1
      DO WHILE ( ( INDEX( 'AEFGIZBOL', FORM(BEG:BEG) ) .EQ. 0 )
     :                           .AND. ( BEG .LT. LEN(FORM) ) )
        BEG = BEG + 1
      END DO

*    Abort if end of string reached
      IF ( BEG .EQ. LEN(FORM) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unrecognised format '//FORM, STATUS )

      ELSE

*      Width is always after the format character
        BEG = BEG + 1
        END = INDEX( FORM(BEG:), '.' )
        IF ( END .EQ. 0 ) THEN
          END = CHR_LEN(FORM)
        ELSE
          END = BEG + END - 2
        END IF

*      Get width
        READ( FORM(BEG:END), '(I)', IOSTAT=FSTAT ) WID
        IF ( FSTAT .NE. 0 ) THEN
          CALL FIO_SERR( FSTAT, STATUS )
          CALL ERR_REP( ' ', 'Cannot find display width from format '/
     :                                                 /FORM, STATUS )
        ELSE

*        Add brackets
          FORM = '('//FORM(:CHR_LEN(FORM))//')'

        END IF

      END IF

      END



*+  HDISPLAY_INT - Perform actual output for HDISPLAY
      SUBROUTINE HDISPLAY_INT( NDIM, IDIMS, RANGES, PTR, FWID,
     :                         FCO, OCH, PAGE_WID, STATUS )
*
*    Description :
*
*     Displays any array (up to 7D) of objects which can be displayed in
*     a character slot FWID characters wide. Output is in the form of 2D
*     planes - higher dimensionalities are just stacks of planes, stacks
*     of stacks of planes, etc.  The actual conversion from data type to
*     is performed by the routine pointed to by TOC.
*
*    History :
*
*     22 Jun 89 : Original (DJA)
*
*    Type declarations :
*
      IMPLICIT NONE
*
      INTEGER	       STATUS		      ! Run-time error code
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER          NDIM                   ! Dimensionality
      INTEGER          IDIMS(DAT__MXDIM)      ! Actual array dimensions
      INTEGER          RANGES(2,DAT__MXDIM)   ! Ranges to display
      INTEGER          PTR                    ! Pointer to data array
      INTEGER          FWID                   ! Output field width
      INTEGER          FCO                    ! Format convertor
      INTEGER          OCH                    ! Output channel
      INTEGER          PAGE_WID               ! Width of output device
*
*    Functions :
*
      INTEGER          CHR_LEN
*
*    Local constants :
*
      CHARACTER        VLIN, DASH, BLANK
        PARAMETER      ( VLIN = '|', DASH = '-', BLANK = ' ' )
*
*    Local variables :
*
      CHARACTER        DSTR*255               ! Data item buffer
      CHARACTER        LINE*132               ! Output buffer

      INTEGER          IND(DAT__MXDIM)        ! Current elemnt to display

      INTEGER          DITEM                  ! Total elements to display
      INTEGER          EDIM                   ! Effective dimensionality
      INTEGER          END                    ! Character pointer
      INTEGER          IIND                   ! Loop over IND array
      INTEGER          IL,IH,JL,JH            ! Edges of window
      INTEGER          ILINE                  ! Loop over lines for NLINE>0
      INTEGER          ILOW,IHIGH             ! Lower and upper limits of data
      INTEGER          JLOW,JHIGH             ! Data to be output
      INTEGER          I,J,K,L,M,X,Y          ! Loop variables
      INTEGER          IPOS                   ! Position in string
      INTEGER          NDIGIT                 ! Number of digits in string
      INTEGER          NI,NJ                  ! Number of fields in each dimension
      INTEGER          NLINE                  ! # whole line per item
      INTEGER          TLEN                   ! Length of a bit of text
      INTEGER          WNDI,WNDJ              ! Size of output window
      INTEGER          SIDE_COL, FIR_COL      ! Size of data area in window
*
*    Data mapping :
*
      EQUIVALENCE      (IND(1),X), (IND(2),Y),! Maps loop variables to array
     :                 (IND(3),M), (IND(4),L),! indexing array IND
     :                 (IND(5),K), (IND(6),J),
     :                 (IND(7),I)
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Total number of elements to display
      DITEM = 1
      DO I = 1, DAT__MXDIM
        DITEM = DITEM * RANGES(2,I) - RANGES(1,I) + 1
      END DO

*    Find effective dimensionality
      IF ( DITEM .EQ. 1 ) THEN
        EDIM = 1
      ELSE
        EDIM = DAT__MXDIM
        DO WHILE ( IDIMS(EDIM) .EQ. 1 )
          EDIM = EDIM - 1
        END DO
      END IF

*    How many columns do we require to display indices at the side
      SIDE_COL = INT(LOG10(REAL(RANGES(2,2)))) + 3
      FIR_COL = SIDE_COL + 2 + FWID/2

*    How many of columns of data across the page
      WNDI = ( PAGE_WID - SIDE_COL + 1 ) / (FWID+1)
      WNDJ = ( PAGE_WID/4)*(PAGE_WID/50)-(PAGE_WID/100)*11
      IF ( WNDI .EQ. 0 ) THEN
        WNDI = 1
        NLINE = FWID / (PAGE_WID - SIDE_COL) + 1
        WNDJ = WNDJ/NLINE
      ELSE
        NLINE = 0
      END IF

*    Loop over all dimensions displaying in 2D sections
      DO I = RANGES(1,7), RANGES(2,7)
        DO J = RANGES(1,6), RANGES(2,6)
          DO K = RANGES(1,5), RANGES(2,5)
            DO L = RANGES(1,4), RANGES(2,4)
              DO M = RANGES(1,3), RANGES(2,3)

*              Get upper and lower limits of data to be output
                ILOW = RANGES(1,1)
                IHIGH = RANGES(2,1)
                JLOW = RANGES(1,2)
                JHIGH = RANGES(2,2)

*              Construct section descriptor if NDIM greater than 2
                IF ( NDIM .GT. 2 ) THEN
                  LINE = ')'
                  TLEN = 1
                  DO IIND = NDIM, 3, -1
                    CALL CHR_ITOC( IND(IIND), DSTR, NDIGIT )
                    LINE = ','//DSTR(:NDIGIT)//LINE(:TLEN)
                    TLEN = TLEN + NDIGIT + 1
                  END DO
                  CALL MSG_SETC( 'BIT', LINE(:TLEN) )
                  CALL MSG_SETI( 'L1', ILOW )
                  CALL MSG_SETI( 'U1', IHIGH )
                  CALL MSG_SETI( 'L2', JLOW )
                  CALL MSG_SETI( 'U2', JHIGH )
                  CALL MSG_MAKE( 'Section (^L1:'/
     :                      /'^U1,^L2:^U2^BIT', LINE, TLEN )
                  CALL AIO_WRITE( OCH, LINE(:PAGE_WID), STATUS )
                END IF

*              Calculate size to be output in each direction
                NI = IHIGH - ILOW + 1
                NJ = JHIGH - JLOW + 1

*              Top of first window
                JL = JLOW

*              Loop for each row of windows
                DO WHILE ( JL .LE. JHIGH )

*                Set bottom of window
                  JH = JL + WNDJ - 1
                  JH = MIN( JH, JHIGH )

*                Set LHS of first window in row
                  IL = ILOW
                  DO WHILE ( IL .LE. IHIGH )

*                  Set RHS of window
                    IH = IL + WNDI - 1
                    IH = MIN( IH, IHIGH )

*                  Construct top border
                    CALL CHR_FILL( BLANK, LINE(:PAGE_WID) )
                    CALL CHR_FILL( DASH, LINE((SIDE_COL-1):PAGE_WID) )

*                  Construct column headings
                    X = IL
                    DO WHILE ( X .LE. IH )
                      CALL CHR_ITOC( X, DSTR, NDIGIT )
                      IF ( NLINE .GT. 0 ) THEN
                        IPOS = 38
                      ELSE
                        IPOS = FIR_COL + ( X - IL ) * (FWID+1) - 1
                      END IF
                      LINE(IPOS:(IPOS+NDIGIT+1)) = '('//DSTR
     :                                        (:NDIGIT)//')'
                      IF ( X .EQ. IL ) THEN
                        X = 5 * ( 1 + X / 5 )
                      ELSE
                        X = X + 5
                      END IF
                    END DO

*                  Output column headings
                    CALL AIO_WRITE( OCH, LINE(:PAGE_WID), STATUS )
                    
*                  Construct each row of data in window
                    DO Y = JL, JH
                      CALL CHR_FILL( BLANK, LINE(:PAGE_WID) )

*                    Including border and periodic row labels on left
                      IF ( ( MOD(Y-JL,5) .EQ. 0 ) .AND.
     :                                        ( EDIM .GT. 1 ) ) THEN
                        CALL CHR_ITOC(Y, DSTR, NDIGIT )
                        DSTR = DSTR(:NDIGIT)//')'
                        NDIGIT = NDIGIT + 1
                      ELSE
                        DSTR = VLIN
                        NDIGIT = 1
                      END IF
                      IPOS = SIDE_COL - NDIGIT
                      WRITE( LINE(IPOS:(IPOS+NDIGIT-1)),
     :                                      '(A)' ) DSTR(:NDIGIT)

*                    Construct data part
                      IPOS = SIDE_COL + 1
                      IF ( NLINE .EQ. 0 ) THEN
                        DO X = IL, IH
                          CALL AIO_APPFCO( FCO, IDIMS, PTR,
     :                                     IND, DSTR, STATUS )
                          WRITE(LINE(IPOS:),'(A)') DSTR(:CHR_LEN(DSTR))
                          IPOS = IPOS + FWID + 1
                        END DO

*                      Put on RH border
                        LINE(PAGE_WID:PAGE_WID) = VLIN

*                      Output line of data
                        CALL AIO_WRITE( OCH, LINE(:PAGE_WID), STATUS )

                      ELSE

                        X = IL
                        CALL AIO_APPFCO( FCO, IDIMS, PTR,
     :                                   IND, DSTR, STATUS )
                        IPOS = 1
                        DO ILINE = 1, NLINE
                          END = IPOS + PAGE_WID - SIDE_COL - 1
                          IF ( END .GT. FWID ) END = FWID
                          LINE(SIDE_COL:PAGE_WID-1) = DSTR(IPOS:END)
                          IPOS = END + 1
                          IF ( ILINE .GT. 1 ) THEN
                            LINE(:SIDE_COL) = BLANK
                            LINE(SIDE_COL-1:SIDE_COL-1) = VLIN
                          END IF
                          LINE(PAGE_WID:PAGE_WID) = VLIN
                          CALL AIO_WRITE( OCH, LINE(:PAGE_WID), STATUS )
                        END DO

                      END IF

                    END DO

*                  Construct bottom border
                    CALL CHR_FILL( BLANK, LINE(:PAGE_WID) )
                    CALL CHR_FILL( DASH, LINE((SIDE_COL-1):PAGE_WID) )

*                  Output bottom border
                    CALL AIO_WRITE( OCH, LINE(:PAGE_WID), STATUS )

*                  Next window in row
                    IL = IH + 1

                  END DO

*                Next row of windows
                  JL = JH + 1

                END DO

*              Subtract displayed items
                DITEM = DITEM - NI*NJ

*              Blank line if EDIM > 2 and there are more sections to do
                IF ( ( EDIM .GT. 2 ) .AND.( DITEM .GT. 0 ) ) THEN
                  CALL AIO_BLNK( OCH, STATUS )
                END IF

              END DO                             ! Next cube
            END DO                               ! Next 4-cube
          END DO                                 ! and so on
        END DO
      END DO

 99   CONTINUE

      END
