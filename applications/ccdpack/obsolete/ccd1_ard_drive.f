      SUBROUTINE ARD_DRIVE( ID, IPMSK, NCOLS, NLINES, XORIG, YORIG,
     :                      STATUS )
*+
*  Name:
*     ARD_DRIVE

*  Purpose:
*     Drives the ARD language interpreter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD_DRIVE( ID, IPMSK, NCOLS, NLINES, XORIG, YORIG, STATUS )

*  Description:
*     ARD is a package of routines which supports the interpretation
*     of files containing ARD language descriptions. This routine
*     is user a modifiable template for processing the ARD description
*     into a form which the calling application requires.
*     Local Modification:
*     Incorporated into CCDPACK, no difference between include and
*     exclude regions, disabled composites and dimension.


*  Arguments:
*     ID = INTEGER (Given)
*        The FIO system descriptor of the file containing the ARD
*        description.
*     IPMSK = INTEGER (Given and Returned)
*        Pointer to LOGICAL mask.
*     NCOLS = INTEGER (Given)
*        First dimension of MASK array.
*     NLINES = INTEGER (Given)
*        Second dimension of MASK array.
*     XORIG = INTEGER (Given)
*        The X origin of the first pixel of the byte array.
*     YORIG = INTEGER (Given)
*        The Y origin of the first pixel of the byte array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-OCT-1991 (PDRAPER):
*        Original version.
*     25-OCT-1991 (PDRAPER):
*        Modified from ARD original ARD_DRIVE.
*     10-AUG-1995 (PDRAPER):
*        Removed ERR_RLSE from end of code. This didn't have a
*        corresponding ERR_MARK.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_PAR'          ! ARD system buffer sizes
      INCLUDE 'FIO_PAR'          ! FIO system buffer sizes

*  Arguments Given:
      INTEGER ID
      INTEGER IPMSK
      INTEGER NCOLS
      INTEGER NLINES
      INTEGER XORIG
      INTEGER YORIG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( ARD__MAXLEN ) LINE ! Buffer to contain processed
                                       ! lines extracted from input file
      CHARACTER BINOP * ( 5 )    ! Binary operator
      CHARACTER COMNAM * ( 15 )  ! Composite block name
      CHARACTER KEYWRD * ( 15 )  ! Current ARD keyword
      CHARACTER UNIOP * ( 5 )    ! Unary operator
      CHARACTER FNAME * ( FIO__SZFNM ) ! Input file name
      INTEGER IPVAL              ! Pointer to list of values
      INTEGER ISTART             ! Start of string excluding keyword and
                                 ! initial operators.
      INTEGER LINNUM             ! Current line number
      INTEGER NCHAR              ! Number of characters in extracted
                                 ! line
      INTEGER NITEM              ! Number of words after keyword
      INTEGER NREQD              ! Number of words which may be
                                 ! required when mapping w/s
      INTEGER NUMDIM             ! Current dimensionality
      INTEGER PRODIM             ! The PROJECT dimension

      INTEGER PRORAN( 2 )        ! The PROJECT range
      INTEGER LSTAT              ! Local status
      INTEGER IPAT               ! Pointer to current mask array
      INTEGER NCOORD             ! Number of trailing coordinate values
      LOGICAL ALLINT             ! Whether extracted coordinates were
                                 ! all integers or not
      LOGICAL DIMEN              ! Whether statement DIMENSION
      LOGICAL EOF                ! True when end of file
      LOGICAL EXCLUD             ! Whether current region is exclude
                                 ! or include
      LOGICAL SPARE              ! Spare logical flag for storage
      LOGICAL FIRST              ! Signifies that this is the first
                                 ! line after a COMPOSITE statement,
                                 ! used to check operators.
      LOGICAL INCOM              ! Signifies whether we're within a
                                 ! COMPOSITE block or not.
      LOGICAL BGNCOM             ! Signifies that a COMPOSITE
                                 ! statement has been encountered.
      LOGICAL ENDCOM             ! Signifies that a END COMPOSITE
                                 ! statement has been encountered.
      LOGICAL PROJEC             ! True if current statement is PROJECT
      LOGICAL STATE              ! True if the current line contains a
                                 ! statement
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up an error context for deferring all error messages.


*  Initialise ARD.
*  Not in COMPOSITE block.
      INCOM = .FALSE.

*  Not first none statement after COMPOSITE.
      FIRST = .FALSE.

*  Not End_Of_file.
      EOF = .FALSE.

*  No lines read yet.
      LINNUM = 0

*  Initialise the ARD keyword description tables - CCDPACK specific.
      CALL ARD_INIT( STATUS )

*  Initialise the number of dimensions. (Only allowing 2 in this
*  routine.)
      NUMDIM = 2

*  Loop while not End_Of_File and no error.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( .NOT.EOF .AND. STATUS .EQ. SAI__OK ) THEN

*  Read in a line of useful information.
         CALL ARD_RDLIN( ID, ARD__MAXLEN, LINE, NCHAR, LINNUM, EOF,
     :                      STATUS )
         IF ( EOF .OR. STATUS .NE. SAI__OK ) GO TO 1

*  Find out if the line is a valid statement.
*  Look for a DIMENSION statement, PROJECT or COMPOSITE-END COMPOSITE.
*  DIMENSION must be 2, PROJECT and COMPOSITE are disabled for now.
         CALL ARD_STATE( LINE, NCHAR, LINNUM, STATE, DIMEN, NUMDIM,
     :                   PROJEC, PRODIM, PRORAN, BGNCOM, INCOM, ENDCOM,
     :                   SPARE, COMNAM, STATUS )
         IF ( STATE ) THEN

*  Have intercepted (and interpreted) a statement, no further work to do
*  this time, just inform user if present capabilities have been
*  exceeded.
            IF ( DIMEN .AND. NUMDIM .NE. 2 ) THEN
               NUMDIM = 2
               CALL MSG_OUT( ' ',
     :         '  Dimensionality of regions must be 2', STATUS )

*  Look for other unsupported items.
            ELSE IF ( PROJEC ) THEN
               CALL MSG_OUT( ' ',
     :         '  CCDPACK does not support PROJECT statements - '//
     :         'ignored', STATUS )
            ELSE IF ( BGNCOM .OR. ENDCOM ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARD_DRIVB1',
     :         '  CCDPACK does not support COMPOSITE structures',
     :         STATUS )
               GO TO 99
            ELSE

*  Don't know how it got here, just pass through.
            END IF

         ELSE
*  Separate out the keyword, and any other operators.
            CALL ARD_KEYW( LINE, LINNUM, INCOM, FIRST, KEYWRD, EXCLUD,
     :                     UNIOP, BINOP, NITEM, ISTART, STATUS )

*  Time to extract the numeric items, round the required space up to
*  a multiple of the current dimensionality. This should allow a safe
*  vectorisation of the stored items. Get all trailing value as double
*  precision. But find out if the values are really integers.
            NREQD = ( ( NITEM / NUMDIM ) + 1 ) * NITEM
            CALL ARD_MALLOC( NREQD, '_DOUBLE', IPVAL, STATUS )

*  Extract the values.
            CALL ARD_EXVAL( KEYWRD, LINE, LINNUM, ISTART, NITEM,
     :                      NUMDIM, %VAL( IPVAL ), ALLINT, NCOORD,
     :                      STATUS )

*  Point at mask.
            IPAT = IPMSK

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*  Convert input coordinates to array coordinates.
            CALL ARD_COCON( %VAL( IPVAL ), NCOORD, ALLINT, KEYWRD,
     :                      XORIG, YORIG, STATUS )

*  End of coordinate conversion.
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*  Process regions block.

*  Put current region into array. Using the appropriate operators
            IF ( KEYWRD .EQ. 'POLYGON' ) THEN
               CALL FIL_POLYL( %VAL( IPAT ), NCOLS, NLINES, UNIOP,
     :                         BINOP, %VAL( IPVAL), NITEM, STATUS )
            ELSE IF ( KEYWRD .EQ. 'ELLIPSE' ) THEN
               CALL FIL_ELLPS( %VAL( IPAT ), NCOLS, NLINES, UNIOP,
     :                         BINOP, %VAL( IPVAL ), STATUS )
            ELSE IF ( KEYWRD .EQ. 'COLUMN' ) THEN
               CALL FIL_COL( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), NITEM, STATUS )
            ELSE IF ( KEYWRD .EQ. 'ROW' ) THEN
               CALL FIL_ROW( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), NITEM, STATUS )
            ELSE IF ( KEYWRD .EQ. 'CIRCLE' ) THEN
               CALL FIL_CIR( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), STATUS )
            ELSE IF ( KEYWRD .EQ. 'LINE' ) THEN
               CALL FIL_LINE( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), STATUS )
            ELSE IF ( KEYWRD .EQ. 'PIXEL' ) THEN
               CALL FIL_PIXS( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), NITEM, STATUS )
            ELSE IF ( KEYWRD .EQ. 'BOX' ) THEN
               CALL FIL_BOX( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), STATUS )
            ELSE IF ( KEYWRD .EQ. 'NDF' ) THEN
               CALL FIL_NDF( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       STATUS )
            ELSE

*  Not supported.
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'KEYWRD', KEYWRD )
               CALL MSG_SETI( 'LINNUM', LINNUM )
               CALL ERR_REP( 'ARD_DRIVE1',
     :         '  region type ^KEYWRD not supported within this'//
     :         ' application - line ^LINNUM'  , STATUS )
            END IF

*  End of processing block, free work space.
            CALL ARD_FREE( IPVAL, STATUS )

*-----------------------------------------------------------------------
*  End of looking for region keyword block
         END IF

*  Next line of input file.
         GO TO 1
      END IF

*  If exiting with an error status, write out name of file.

 99   IF ( STATUS .NE. SAI__OK ) THEN

*  Set up a new error context, issue error and flush it.
         CALL ERR_MARK
         LSTAT = SAI__OK
         CALL FIO_FNAME( ID, FNAME, LSTAT )
         CALL MSG_SETC( 'FNAME', FNAME )
         LSTAT = SAI__ERROR
         CALL ERR_REP( 'ARD_DRIVE2',
     :   '  ARD - Error translating file ^FNAME', LSTAT )
         CALL ERR_FLUSH( LSTAT )
         CALL ERR_RLSE

      END IF

      END
* $Id$
