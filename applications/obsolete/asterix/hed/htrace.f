*+  HTRACE - Displays a hierarchical listing of an HDS object
      SUBROUTINE HTRACE( STATUS )
*
*    Description :
*
*      Does a TRACE like output of an HDS object to a selected output device
*      which may be user CONSOLE, linePRINTER or text file with logical name
*      AST_LIST.  In latter case output may be appended to an OLDFILE or
*      written to a NEWFILE.
*
*    Parameters :
*
*      INP = UNIV(R)
*        Object to be traced
*      DEV = CHAR(R)
*        Output device { C)ONSOLE,P)RINTER,O)LDFILE,N)EWFILE }
*      FULL = LOGICAL(R)
*	 Write out every element of structure arrays?
*      TYPWID = INTEGER(R)
*        Indentation from object name to type
*      VALWID = INTEGER(R)
*        Indentation from object type to value
*      NEWLINE = LOGICAL(R)
*        Put data on new line
*      EACHLINE = LOGICAL(R)
*        Put each element of a character array on a new line
*      NLINES = INTEGER(R)
*        Number of lines allowed for data output
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     15 Nov 93 : V1.7-0  Original (DJA)
*     11 Jan 94 : V1.7-1  Corrected use of DAT_VALID instead of DAT_STATE (DJA)
*     27 Apr 94 : V1.7-2  Recoded to use AIO_ routines for i/o (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Global variables :
*
      INCLUDE 'HTRACE_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) 	OBJLOC          ! Object to trace

      LOGICAL			FULL		! Full o/p of structure arrays?
*
*    External references :
*
      EXTERNAL 			HTRACE_ITERATOR
*
*    Version id :
*
      CHARACTER*30 		VERSION
        PARAMETER    ( VERSION = 'HTRACE Version 1.7-2' )
*-

*    Version number
      CALL MSG_PRNT( VERSION )

*    Get parameter values
      CALL DAT_ASSOC('INP','READ',OBJLOC,STATUS)

*    Get output channel
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, OUTWIDTH, STATUS )

*    Full output of structure arrays?
      CALL PAR_GET0L( 'FULL', FULL, STATUS )

*    Get indentation control
      CALL PAR_GET0I( 'TYPIND', TYPIND, STATUS )
      CALL PAR_GET0I( 'VALIND', VALIND, STATUS )

*    Value placement control
      CALL PAR_GET0L( 'NEWLINE', NEWLINE, STATUS )
      CALL PAR_GET0I( 'NLINES', NLINES, STATUS )
      CALL PAR_GET0L( 'EACHLINE', EACHLINE, STATUS )

*    Perform the trace
      CALL HTRACE_INT( OBJLOC, FULL, HTRACE_ITERATOR, STATUS )

*    Close output channel
      CALL AIO_CANCL( 'DEV', STATUS )

*    Tidy up
      CALL AST_ERR( STATUS )

      END



*+  HTRACE_ITERATOR - gives list (directory) of HDS data object
      SUBROUTINE HTRACE_ITERATOR( LOC, LEVEL, NDIM, DIMS,
     :                            DOBLANK, STATUS )
*
*    Description :
*
*      Produces a description of a single HDS object given the information
*      supplied by the HDS tree-walker.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     15 Nov 93 : Original (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Global variables :
*
      INCLUDE 'HTRACE_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) 	LOC		! locator for data object
      INTEGER                	LEVEL
      INTEGER               	NDIM
      INTEGER                	DIMS(DAT__MXDIM)
      LOGICAL			DOBLANK		! Blank line after object?
*
*    Functions :
*
      INTEGER			CHR_LEN
*
*    Local variables :
*
      CHARACTER*40		DSTR		! String for dimensions
      CHARACTER*(DAT__SZNAM)   	NAME		! Object name
      CHARACTER*132		OUT		! Output buffer
      CHARACTER*(DAT__SZTYP)   	TYPE		! Object type
      CHARACTER*132		VSTR		! Value string

      INTEGER			CLEN		! Length of character component
      INTEGER			CURC		! Current character index
      INTEGER			DLEN		! Length of DSTR used
      INTEGER			IND		! Indentation
      INTEGER			NB1, NB2, NB3	! Blank lengths
      INTEGER			NELM		! No. of elements in array
      INTEGER			NLEN		! Length of NAME used
      INTEGER			TLEN		! Length of TYPE used
      INTEGER			VLEN		! Length of VSTR used

      LOGICAL 			DONE		! Finished output?
      LOGICAL 			NEWL		! New line for object value?
      LOGICAL 			PRIM		! Object is primitive?
      LOGICAL 			VALID		! Object data is ok?
*-

*    Initialise
      NEWL = NEWLINE

*    Get object name and type
      CALL DAT_NAME( LOC, NAME, STATUS )
      CALL DAT_TYPE( LOC, TYPE, STATUS )

*    Write dimensions into string
      IF ( NDIM .GT. 0 ) THEN
        CALL STR_DIMTOC( NDIM, DIMS, DSTR )
        DLEN = CHR_LEN(DSTR)
        DSTR(1:1) = '['
        DSTR(DLEN:DLEN) = ']'
      ELSE
        DSTR = ' '
        DLEN = 1
      END IF

*    Get length of name and type
      NLEN = CHR_LEN(NAME)
      TLEN = CHR_LEN(TYPE)

*    Object is primitive?
      CALL DAT_PRIM( LOC, PRIM, STATUS )

*    Data is valid?
      IF ( PRIM .AND. (NDIM.EQ.0) ) THEN
        CALL DAT_STATE( LOC, VALID, STATUS )
      ELSE
        VALID = .TRUE.
      END IF

*    Find padding lengths
      NB1 = MAX(1,LEVEL*2+1)
      NB2 = MAX(1,TYPIND-(NLEN+DLEN))
      NB3 = MAX(1,VALIND-(TLEN+2))

*    Write the buffer, excluding value
 10   FORMAT( <NB1>X, A, A, <NB2>X, 3A )
      WRITE(OUT,10) NAME(:NLEN), DSTR(:DLEN), '<', TYPE(:TLEN), '>'
      CURC = NB1 + NLEN + DLEN + NB2 + 1 + TLEN + 1 + NB3

*   Find value string
*    Invalid data
      DONE = .FALSE.
      IF ( PRIM .AND. .NOT. VALID ) THEN
        VSTR = '{undefined}'
        VLEN = 11
        NEWL = .FALSE.

*    Structure array?
      ELSE IF ( (NDIM.GT.0) .AND. .NOT. PRIM ) THEN
        VSTR = '{array of structures}'
        VLEN = 21
        NEWL = .FALSE.

*    Scalar structure
      ELSE IF ( (NDIM.EQ.0) .AND. .NOT. PRIM ) THEN
        VSTR = '{structure}'
        VLEN = 11
        NEWL = .FALSE.

*    Scalar primitive
      ELSE IF ( (NDIM.EQ.0) .AND. PRIM ) THEN
        CALL DAT_GET0C( LOC, VSTR, STATUS )
        VLEN = CHR_LEN(VSTR)

*      Add quotes for character strings
        IF ( TYPE(1:5) .EQ. '_CHAR' ) THEN
          CALL CHR_CTOI( TYPE(7:), CLEN, STATUS )
          VSTR = CHAR(34)//VSTR(:CLEN)//CHAR(34)
          VLEN = CLEN + 2
        END IF

*    Array of primitives
      ELSE

*      Find number of elements
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*      Write values to string
        IF ( NEWL ) THEN
          IND = NB1
        ELSE
          IND = CURC
        END IF
        CALL HTRACE_PUTVALUE( LOC, NDIM, DIMS, NELM, IND,
     :                        OUT(1:OUTWIDTH), CURC, STATUS )
        IF ( OUT .GT. ' ' ) THEN
          CALL AIO_WRITE( OCH, OUT(1:CURC), STATUS )
        END IF
        DONE = .TRUE.

      END IF

*    Write the buffer
      IF ( .NOT. DONE ) THEN

*      Will the length of the data put it over the edge of the page?
        IF ( (NB1+NB2+NB3+2+NLEN+DLEN+TLEN+VLEN) .GT. OUTWIDTH ) THEN

*        Put data on new line
          NEWL = .TRUE.
          CALL AIO_WRITE( OCH, OUT(1:CURC), STATUS )

*        Does it still overflow?
          IF ( (NB1+1+VLEN) .GT. OUTWIDTH ) THEN

*          Put the truncation marker in
            VLEN = OUTWIDTH - (NB1+1)
            VSTR(VLEN-3:VLEN) = CHAR(34)//'...'

          END IF

        END IF

        IF ( NEWL ) THEN
          CALL AIO_IWRITE( OCH, NB1, VSTR(:VLEN), STATUS )
        ELSE
          CALL AIO_WRITE( OCH, OUT(1:CURC)//VSTR(1:VLEN), STATUS )
        END IF

      END IF

*    Blank line? Do it if requested unless structure array
      IF ( DOBLANK .AND. .NOT. ((NDIM.GT.0).AND..NOT.PRIM) ) THEN
        CALL AIO_BLNK( OCH, STATUS )
      END IF

      END



      SUBROUTINE HTRACE_PUTVALUE( LOC, NDIM, DIMS, SIZE, INDNTC,
     :                            LINE, LENG, STATUS )
*+
*  Name:
*     TRA_PUTL

*  Purpose:
*     Report the first and last few values of an object.

*  Description:
*     A number of values are read from the object and coded into one
*     or more text lines in a concise manner.  The information may
*     be written to an open ASCII file.

*     The values are normally listed at the end of one line, but may
*     start on a new line.  The maximum number of lines of data values
*     may also be set.  For all but the smallest arrays where the values
*     of all elements can be displayed in the space provided, the last
*     few values in the array as well as the first few are presented.
*     The last few values appear on a new line, indented the same as
*     the line above with the ellipsis notation to indicate any missing
*     values. Note the number of elements shown depends on the number of
*     characters that will fit on the line.

*  Arguments:
*     LOC = CHARACTER * (DAT__SZLOC) (Given)
*        Locator to the object.
*     NDIM = INTEGER (Given)
*        Dimensionality of the object.
*     DIMS( DAT__MXDIM ) = INTEGER (Given)
*        Dimensions of the object.
*     SIZE = INTEGER (Given)
*        The number of elements in the object if treated as a vector.
*     INDNTC = INTEGER (Given)
*        Indentation level for continuation lines of values.
*     LINE = CHARACTER * ( * ) (Returned)
*        Line of text to be output.
*     LENG = INTEGER (Given and Returned)
*        Current length of characters in LINE excluding trailing
*        blanks.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 January 31 (MJC):
*        Original version based upon TRA_PUTx.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*
*    Type Definitions:
*
      IMPLICIT NONE            ! Switch off the default typing
*
*    Global Constants:
*
      INCLUDE 'SAE_PAR'        ! SAI Constants
      INCLUDE 'DAT_PAR'        ! HDS Constants
*
*    Global variables :
*
      INCLUDE 'HTRACE_CMN'

*  Arguments Given:
      CHARACTER*(*)
     :  LOC                    ! Object locator

      INTEGER
     :  NDIM,                  ! Number of dimensions
     :  DIMS(*),               ! Dimensions
     :  SIZE,                  ! Size of object as if vector
     :  INDNTC                 ! Indentation column number for
                               ! continuation lines


*  Arguments Given and Returned:
      CHARACTER*(*) LINE       ! Line to receive numbers
      INTEGER LENG             ! Current line length


*  External References:
      INTEGER CHR_SIZE         ! String size
      LOGICAL HDX_TYPINT

*  Local Constants:
      INTEGER SZELIP           ! Size of ellipsis (...)
      PARAMETER( SZELIP=3 )
      INTEGER MAXVAL           ! Maximum number of values that can be
                               ! read
      PARAMETER( MAXVAL=100 )
      INTEGER MAXSTR           ! Size of temporary string for number
      PARAMETER( MAXSTR=120 )


*  Local Variables:
      CHARACTER*2048            CBUF
      CHARACTER*( DAT__SZLOC )
     :  SLICE,                 ! Slice locator
     :  VEC                    ! Vector locator
      CHARACTER*( MAXSTR ) STR ! String to hold coded number
      CHARACTER*20 FMT
      CHARACTER*(DAT__SZTYP) TYPE,MTYPE

      INTEGER			FCO			! Format controller

      INTEGER FMTWID,BCOL,IDIM

      INTEGER
     :  CPOS,                  ! Column position of last character in
                               ! a final value string
     :  FVALUE,                ! Value index for the last elements
     :  I,                     ! Loop variable
     :  INDENT,                ! Level of indention
     :  INDS( DAT__MXDIM ),    ! Array indices
     :  LDIMS( DAT__MXDIM ),    ! Array indices
     :  IVALUE,                ! Value index for the first elements
     :  MXVAL,                 ! Maximum number of elements to be read
     :  MXLENG,                ! Length of the line buffer
     :  NCHAR,                 ! Number of characters in STR
     :  NOLINE,                ! Number of line currently being formed
     :  NOREAD,                ! Number of elements read in
     :  NVALUE,                ! Number of values accessed
     :  STATUS,                 ! Local status
     :  VPTR,		       ! Ptr to values
     :  VSIZE                  ! Number of bytes per value

      LOGICAL                  ! True if:
     :  FULL                   ! Text line is full

*.

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise the level of indention.
      INDENT = LENG

*    Find the maximum length of the line.
      MXLENG = CHR_SIZE(LINE)

*    Read a limited part of the variable.
      NVALUE = MIN( MAXVAL, SIZE )

*    Find a reasonable number to read in for the output.
      MXVAL = MIN( NVALUE, ( MXLENG - INDENT + 1 ) / 2 * NLINES )

*    Map workspace to hold values
      CALL DAT_TYPE( LOC, TYPE, STATUS )
      IF ( HDX_TYPINT(TYPE) ) THEN
        MTYPE = '_INTEGER'
      ELSE
        MTYPE = TYPE
      END IF
      CALL HDX_TYPSIZ( MTYPE, VSIZE, STATUS )

*    Pad dimensions and indices to 7D
      DO I = 1, DAT__MXDIM
        LDIMS(I) = 1
        INDS(I) = 1
      END DO
      LDIMS(1) = SIZE

*    Get the default format for the data type
      FMT = ' '
      CALL HDISPLAY_GETFMT( TYPE, FMTWID, FMT, STATUS )

*    Create format convertor
      CALL AIO_CREFCO( MTYPE, FMT, FCO, STATUS )

*    Get a locator to the vectorised object.
      CALL DAT_VEC( LOC, VEC, STATUS )

*    Obtain a slice of the chosen length.
      CALL DAT_SLICE( VEC, 1, 1, MXVAL, SLICE, STATUS )

*    Get the values in the slice and copy the data
      CALL DAT_MAPV( SLICE, MTYPE, 'READ', VPTR, LDIMS(1), STATUS )

*    Format all the data into the buffer
      BCOL = 1
      DO IDIM = 1, LDIMS(1)
        INDS(1) = IDIM
        CALL AIO_APPFCO( FCO, LDIMS, VPTR, INDS, CBUF(BCOL:), STATUS )
        BCOL = BCOL + FMTWID
      END DO

*    Tidy the locators after use.
      CALL DAT_UNMAP( LOC, STATUS )
      CALL DAT_ANNUL( SLICE, STATUS )
      CALL DAT_ANNUL( VEC, STATUS )

*    Handle the error transparently.
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL CHR_PUTC( '{undefined}', LINE, LENG )

      ELSE

*      Initialise counters.
        NOLINE = 1
        NOREAD = 0

*      Write the values to a buffer.
        DO I = 1, SIZE

*        Find the element to output.
          IVALUE = I - NOREAD

*        Has this element been read in?
          IF ( IVALUE .GT. NVALUE ) THEN

*          No, so increment the number read so far.
            NOREAD = NOREAD + NVALUE

*          Find a reasonable number to read in for trace output.
            MXVAL = MIN( MAXVAL, SIZE - NOREAD,
     :                 ( MXLENG - INDENT + 1 ) / 2 * NLINES )

*          Get a locator to the vectorised object.
            CALL DAT_VEC( LOC, VEC, STATUS )

*          Obtain a slice of the chosen length.
            CALL DAT_SLICE( VEC, 1, NOREAD+1, NOREAD+MXVAL, SLICE,
     :                      STATUS )

*          Get the values in the slice.
            CALL DAT_MAPV( SLICE, MTYPE, 'READ', VPTR, LDIMS(1),
     :                     STATUS )

*          Reset the element number within the slice.
            IVALUE = 1

*          Format all the data into the buffer
            BCOL = 1
            DO IDIM = 1, LDIMS(1)
              INDS(1) = IDIM
              CALL AIO_APPFCO( FCO, LDIMS, VPTR, INDS, CBUF(BCOL:),
     :                         STATUS )
              BCOL = BCOL + FMTWID
            END DO

*          Tidy the locators after use.
            CALL DAT_UNMAP( LOC, STATUS )
            CALL DAT_ANNUL( SLICE, STATUS )
            CALL DAT_ANNUL( VEC, STATUS )

          END IF

*        Convert the numeric value to a string.
          STR = CBUF((IVALUE-1)*FMTWID+1:IVALUE*FMTWID)
          IF ( MTYPE(1:5) .EQ. '_CHAR' ) THEN
            STR = CHAR(34)//STR(:FMTWID)//CHAR(34)
            NCHAR = FMTWID+2
          ELSE
            CALL CHR_LDBLK( STR )
            CALL MSG_SETC( 'C', STR )
            CALL MSG_MAKE( '^C', STR, NCHAR )
          END IF

*        Put the value into the buffer unless it overflows the line.
          CALL HTRACE_PUT( IVALUE, STR(1:NCHAR),
     :                     I .NE. SIZE, LINE, LENG, FULL, STATUS )

*        Is the line full?
          IF ( FULL ) THEN

*          Yes, so will need to output the current line.
            CALL AIO_WRITE( OCH, LINE(1:LENG), STATUS )

*          Should values continue to a new line?
            IF ( NOLINE .NE. NLINES ) THEN

*            Start a new line indenting the requested amount.
              LENG = INDNTC + 1
              LINE = ' '

*            Increment the count of the number of output lines.
              NOLINE = NOLINE + 1

*            Write the value that would not fit into previous line
*            at the start of the new line.
              CALL HTRACE_PUT( IVALUE, STR(1:NCHAR), I.NE.SIZE,
     :                         LINE, LENG, FULL, STATUS )
     :
            ELSE

*            The last value is not displayed if the line was full.
*            So decrement the count of the number of values and
*            exit the loop.
              IVALUE = I - 1
              GOTO 1
            END IF

          END IF
        END DO
    1   CONTINUE

*      Are there end values to be output on a new line?
        IF ( IVALUE .NE. SIZE .AND. NOLINE .EQ. NLINES ) THEN

*        Start a new line.
          IF ( NLINES .GT. 1 ) INDENT = INDNTC + 1
          LINE = ' '

*        Determine whether or not further values are to be read in.
          IF ( SIZE .GT. NVALUE + NOREAD ) THEN

*          Find a reasonable number to read in for output.
            MXVAL = MIN( MAXVAL, ( MXLENG - INDENT + 1 ) / 2,
     :                      SIZE - IVALUE )

*          Get a locator to the vectorised object.
            CALL DAT_VEC( LOC, VEC, STATUS )

*          Obtain a slice of the chosen length.
            CALL DAT_SLICE( VEC, 1, SIZE-MXVAL+1, SIZE, SLICE, STATUS )

*          Get the values in the slice.
            CALL DAT_MAPV( SLICE, MTYPE, 'READ', VPTR, LDIMS(1),
     :                     STATUS )

*          Modify the last value to new co-ordinates.
            IVALUE = IVALUE - NOREAD - SIZE + MXVAL

*          Format all the data into the buffer
            BCOL = 1
            DO IDIM = 1, LDIMS(1)
              INDS(1) = IDIM
              CALL AIO_APPFCO( FCO, LDIMS, VPTR, INDS, CBUF(BCOL:),
     :                         STATUS )
              BCOL = BCOL + FMTWID
            END DO

*          Tidy the locators after use.
            CALL DAT_UNMAP( LOC, STATUS )
            CALL DAT_ANNUL( SLICE, STATUS )
            CALL DAT_ANNUL( VEC, STATUS )

          ELSE

*          No more values are to be read in.
            IVALUE = IVALUE - NOREAD

          END IF

*        Loop from the last value.
          FULL = .FALSE.
          FVALUE = LDIMS(1)
          CPOS = MXLENG

*        Search backwards until the line is full.
          DO WHILE ( FVALUE .GT. IVALUE .AND. .NOT. FULL )

*          Convert data to character
            STR = CBUF((FVALUE-1)*FMTWID+1:FVALUE*FMTWID)
            IF ( MTYPE(1:5) .EQ. '_CHAR' ) THEN
              STR = CHAR(34)//STR(:FMTWID)//CHAR(34)
              NCHAR = FMTWID+2
            ELSE
              CALL CHR_LDBLK( STR )
              CALL MSG_SETC( 'C', STR )
              CALL MSG_MAKE( '^C', STR, NCHAR )
            END IF

*          Insert a comma unless it is the last value.
            IF ( FVALUE .NE. LDIMS(1) ) THEN
              NCHAR = NCHAR + 1
              STR( NCHAR:NCHAR ) = ','
            END IF

*          Determine whether or not there is room to insert the
*          string, with or without an ellipsis as necessary.
            IF ( ( INDENT .GT. CPOS-NCHAR+1 .AND.
     :             FVALUE .EQ. IVALUE + 1 ) .OR.
     :            ( INDENT + SZELIP + 1 .GT. CPOS-NCHAR ) ) THEN

*            There is no room.
              FULL = .TRUE.

*            Test whether ellipsis is needed.
              IF ( FVALUE .GT. IVALUE ) THEN

*              Allow for the special case when there is no room
*              to output last value at full precision...
                IF ( FVALUE .EQ. LDIMS(1) ) THEN

*                ... provided any text be output
                  IF ( INDENT+SZELIP+4 .LT. CPOS ) THEN
                    STR = CBUF((FVALUE-1)*FMTWID+1:FVALUE*FMTWID)
                    IF ( MTYPE(1:5) .EQ. '_CHAR' ) THEN
                      LINE(1+SZELIP+INDENT:CPOS) =
     :                           CHAR(34)//STR(:FMTWID)//CHAR(34)
                    ELSE
                      CALL CHR_LDBLK( STR )
                      CALL MSG_SETC( 'C', STR )
                      CALL MSG_MAKE( '^C', STR, NCHAR )
		      LINE(1+SZELIP+INDENT:CPOS) = STR(:NCHAR)
                    END IF

*                  Insert the ellipsis into the output line.
                    LINE( INDENT:INDENT+SZELIP+1 ) = '... '
                  END IF

*                The column position is at the indentation point.
                  CPOS = INDENT

                ELSE

*                Omit the value which will not fit into the
*                available space and insert the ellipsis into
*                the output line.
                  LINE( CPOS-SZELIP:CPOS ) = '... '

*                Move the column position left by the length of
*                the ellipsis and the space.
                  CPOS = CPOS - SZELIP - 1

                END IF

              ELSE

*              Allow for the special case when there is no room to
*              output last value at full precision, but without the ellipsis.
                IF ( FVALUE .EQ. LDIMS(1) ) THEN

*                Convert data to character
                  STR = CBUF((FVALUE-1)*FMTWID+1:FVALUE*FMTWID)
                  IF ( MTYPE(1:5) .EQ. '_CHAR' ) THEN
                    LINE(INDENT:CPOS) =
     :                           CHAR(34)//STR(:FMTWID)//CHAR(34)
                  ELSE
                    CALL CHR_LDBLK( STR )
                    CALL MSG_SETC( 'C', STR )
                    CALL MSG_MAKE( '^C', STR, NCHAR )
		    LINE(INDENT:CPOS) = STR(:NCHAR)
                  END IF

*                The column position is at the indentation point.
                  CPOS = INDENT

                END IF
              END IF

            ELSE

*            Insert the string into the line.
              LINE( CPOS - NCHAR + 1:CPOS ) = STR( 1:NCHAR )

*            Move the column position by the width of the added string.
              CPOS = CPOS - NCHAR

            END IF

*          Decrement the element index as we are filling the line
*          from the last value.
            FVALUE = FVALUE - 1

          END DO

*        See if the current position is at the indentation point.
          IF ( CPOS .GT. INDENT ) THEN

*          It is not so slide the text along for alignment at the
*          indentation column.
            DO  I = CPOS + 1, MXLENG
              LINE( I-CPOS+INDENT:I-CPOS+INDENT ) = LINE( I:I )
            END DO

*          Shorten the line length accordingly.
            MXLENG = MXLENG - CPOS + INDENT

          END IF

          LENG = MXLENG

        END IF

      END IF

*    Create format convertor
      CALL AIO_FREFCO( FCO, STATUS )

      END


*+  HTRACE_PUT - Put the supplied value string in the supplied text line.
      SUBROUTINE HTRACE_PUT( IVALUE, VALUE, COMMA,
     :                       LINE, LENG, FULL, STATUS )
*
*  Purpose:
*     Put the supplied value string in the supplied text line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL HTRACE_PUT( IVALUE, VALUE, COMMA, LINE, LENG, FULL, STATUS )

*  Description:
*     The supplied value string is planted in the text line followed by
*     an optional comma.  The line length and array indices are updated
*     as appropriate.  The routine determines whether or not there is
*     enough room to plant the value string, and returns a logical flag
*     stating what has happened.

*  Arguments:
*     IVALUE = INTEGER (Given)
*        Index to the value if the object is a vector.
*     VALUE = CHARACTER * ( * ) (Given)
*        Value string.
*     COMMA = LOGICAL (Given)
*        If true a comma is written after the value.
*     LINE = CHARACTER*(*) (Given and Returned)
*        Line of text to be appended
*     LENG = INTEGER (Given and Returned)
*        Current length of the characters in LINE excluding any trailing
*        blanks.
*     FULL = LOGICAL (Returned)
*        If true the line given was full.
*     STATUS = INTEGER (Given)
*        Global status

*  Authors:
*     JRG: Jack Giddings (UCL)
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-FEB-1983 (JRG):
*        Original version.
*     1989 May 16 (MJC):
*        Tidied and extended the prologue; added COMMA argument.
*     1989 Jun 15 (MJC):
*        Renamed from LSDIR to avoid confusion with the original TRACE
*        version; added STATUS argument and check.
*     1991 January 30 (MJC):
*        Converted to SST prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off the default typing


*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SAI Constants


*  Arguments Given:
      INTEGER IVALUE           ! Value index as if object is vector
      CHARACTER * ( * ) VALUE  ! Value string
      LOGICAL COMMA            ! True if a comma is to be appended to
                               ! value


*  Arguments Given and Returned:
      CHARACTER * ( * ) LINE   ! Line to receive numbers
      INTEGER LENG             ! Current line length

*    Export :                  ! True if:
      LOGICAL FULL             ! Line was found full


*  Status:
      INTEGER STATUS           ! Global status


*  External References:
      INTEGER CHR_LEN          ! String length
      INTEGER CHR_SIZE         ! String size


*  Local Variables:
      INTEGER NCHAR            ! Value string length
      INTEGER NEED             ! Space needed

*.

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find the number of characters needed allowing for the ellipsis and
*    a prefixing comma if it is not the first element of an array.
      NCHAR = CHR_LEN( VALUE )
      NEED = NCHAR + 3
      IF ( COMMA ) THEN
        NEED = NEED + 1
      END IF

*    See if there is space in the line.
      FULL = ( CHR_SIZE( LINE ) - LENG ) .LE. NEED

      IF ( .NOT. FULL ) THEN

*      There is room to accommodate the value, so append it to the
*      line.  Append the comma if required.
        CALL CHR_PUTC( VALUE( 1:NCHAR ), LINE, LENG )
        IF ( COMMA ) THEN
          CALL CHR_PUTC( ',', LINE, LENG )
        END IF

      END IF

      END


*+  HTRACE_SARRAY - Announce structure array cell
      SUBROUTINE HTRACE_SARRAY( LOC, LEVEL, NDIM, INDICES, STATUS )
*
*    Description :
*
*      Produces a description of a single HDS object given the information
*      supplied by the HDS tree-walker.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     15 Nov 93 : Original (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Global variables :
*
      INCLUDE 'HTRACE_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) 	LOC		! locator for data object
      INTEGER                	LEVEL
      INTEGER               	NDIM
      INTEGER                	INDICES(DAT__MXDIM)
*
*    Functions :
*
      INTEGER			CHR_LEN
*
*    Local variables :
*
      CHARACTER*40		DSTR		! String for dimensions
      CHARACTER*(DAT__SZNAM)   	NAME		! Object name

      INTEGER			DLEN		! Length of DSTR used
      INTEGER			NB1        	! Blank lengths
      INTEGER			NLEN		! Length of NAME used
*-

*    Get object name
      CALL DAT_NAME( LOC, NAME, STATUS )

*    Write structure array indices into string
      IF ( NDIM .GT. 0 ) THEN
        CALL STR_DIMTOC( NDIM, INDICES, DSTR )
        DLEN = CHR_LEN(DSTR)
        DSTR(1:1) = '['
        DSTR(DLEN:DLEN) = ']'
      ELSE
        DSTR = ' '
        DLEN = 1
      END IF

*    Get length of name and type
      NLEN = CHR_LEN(NAME)

*    Find padding lengths
      NB1 = MAX(1,LEVEL*2+1)

*    Write the buffer
      CALL AIO_BLNK( OCH, STATUS )
      CALL AIO_IWRITE( OCH, NB1, 'Contents of '/
     :               /NAME(:NLEN)//DSTR(:DLEN), STATUS )

      END
