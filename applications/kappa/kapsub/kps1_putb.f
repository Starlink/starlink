      SUBROUTINE KPS1_PUTB( LOC, NDIM, DIMS, SIZE, NLINES, INDNTC,
     :                        LOGEXM, FD, LINE, LENG, STATUS )
*+
*  Name:
*     KPS1_PUTx

*  Purpose:
*     Report the first and last few values of an BYTE object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PUTx( LOC, NDIM, DIMS, SIZE, NLINES, INDNTC, LOGEXM, FD,
*    :                LINE, LENG, STATUS )

*  Description:
*     A number of values are read from the object and coded into one
*     or more text lines in a concise manner.  The information may
*     be written to an open text file.
*
*     The values are normally listed at the end of one line, but may
*     start on a new line.  The maximum number of lines of data values
*     may also be set.  For all but the smallest arrays where the values
*     of all elements can be displayed in the space provided, the last
*     few values in the array as well as the first few are presented.
*     The last few values appear on a new line, indented the same as
*     the line above with the ellipsis notation to indicate any missing
*     values. Note the number of elements shown depends on the number of
*     characters that will fit on the line.  Bad values are indicated
*     by an asterisk.
*
*     This routine is not suitable for handling character objects.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to the object.
*     NDIM = INTEGER (Given)
*        Dimensionality of the object.
*     DIMS( DAT__MXDIM ) = INTEGER (Given)
*        Dimensions of the object.
*     SIZE = INTEGER (Given)
*        The number of elements in the object if treated as a vector.
*     NLINES = INTEGER (Given)
*        Number of lines to store the reported values.
*     INDNTC = INTEGER (Given)
*        Indentation level for continuation lines of values.
*     LOGEXM = LOGICAL (Given)
*        If true a log of the header records is written to an text
*        file.
*     FD = INTEGER (Given)
*        The text file descriptor, ignored if LOGEXM is false.
*     LINE = CHARACTER * ( * ) (Returned)
*        Line of text to be output.
*     LENG = INTEGER (Given and Returned)
*        Current length of characters in LINE excluding trailing
*        blanks.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for each of the numeric data types: replace
*     "x" in the routine name by B, D, I, R, UB, UW or W as appropriate.

*  Algorithm:
*     If status is bad then return
*     Read a limited part of the data object slicing if an array
*     If an error occurred then
*        Output {undefined} message
*     Else
*        Initialise line and element counts
*        For each value
*           If value has value not been read then
*              Increment count of number of elements read
*              Read the next portion of the data object
*           Endif
*           Write values to a buffer or asterisk if bad
*           Write values to a message buffer if not full
*           If line is full then
*              Output current line
*              Output to the log file if requested
*              If values continue on the next line then
*                 Start a new line from the continuation indentation
*                 Increment the line counter
*                 Write the value that would not fit into the previous
*                   line at the start of the new line
*              Else
*                 Exit loop
*              Endif
*           Endif
*        Endfor
*        If there are the last values of an array to be output on a
*          new line then
*           Start a new line
*           If further values need to be read in then
*              Read in the most that could fit on one line
*           Endif
*           Loop from the last value in the array
*              Convert to character string or use an asterisk if value
*                is bad
*              If not the last value, append a comma to it
*              If there is room to insert the string then
*                 Do so
*                 Move current column position left by the number of
*                   characters written in the line buffer for the
*                   current element
*              Else
*                 Set switch to exit from the loop
*                 Prefix the string with an ellipsis if needed (i.e.
*                   there are missing values), and reformat the value
*                   to fit the available space if there is no room to
*                   give the full precision
*              Endif
*              Decrement element count
*           Endfor
*           Align text with the indentation level within the line buffer
*        Endif
*     Endif
*     End

*  Prior Requirements:
*     -  The text file associated with descriptor FD must already be
*     opened if LOGEXM is .TRUE..

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 May 9 (MJC)
*        Original version based upon HDSTRACE routine TRA_PUTX.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! Switch off the default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAI Constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PRM_PAR'          ! VAL__ Constants

*  Arguments Given:
      CHARACTER * ( * ) LOC      ! Object locator
      INTEGER NDIM               ! Number of dimensions
      INTEGER DIMS( * )          ! Dimensions
      INTEGER SIZE               ! Size of object as if vector
      INTEGER NLINES             ! Number of lines of values (excluding
                                 ! line for last values of an array)
      INTEGER INDNTC             ! Indentation column number for
                                 ! continuation lines
      LOGICAL LOGEXM             ! Output to go to the log file?
      INTEGER FD                 ! Log file's description

*  Arguments Given and Returned:
      CHARACTER * ( * ) LINE     ! Line to receive numbers
      INTEGER LENG               ! Current line length

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_SIZE           ! String size

*  Local Constants:
      INTEGER SZELIP             ! Size of ellipsis (...)
      PARAMETER( SZELIP=3 )

      INTEGER MAXVAL             ! Maximum number of values that can be
                                 ! read
      PARAMETER( MAXVAL=100 )

      INTEGER MAXSTR             ! Size of temporary string for number
      PARAMETER( MAXSTR=20 )

*  Local Variables:
      LOGICAL BAD                ! Current value is bad?
      LOGICAL BWTYPE             ! Data type is _BYTE, _UBYTE, _WORD, or
                                 ! _UWORD?
      INTEGER CPOS               ! Column position of last character in
                                 ! a final value string
      INTEGER DUMMY              ! Dummy variable to convert byte and word
                                 ! data into integer
      LOGICAL FULL               ! Text line is full?
      INTEGER FVALUE             ! Value index for the last elements
      INTEGER I                  ! Loop variable
      INTEGER INDENT             ! Level of indention
      INTEGER INDS( DAT__MXDIM ) ! Array indices
      INTEGER IVALUE             ! Value index for the first elements
      INTEGER MXVAL              ! Maximum number of elements to be read
      INTEGER MXLENG             ! Length of the line buffer
      INTEGER NCHAR              ! Number of characters in STR
      INTEGER NOLINE             ! Number of line currently being formed
      INTEGER NOREAD             ! Number of elements read in
      INTEGER NVALUE             ! Number of values accessed
      CHARACTER * ( DAT__SZLOC ) SLICE ! Slice locator
      CHARACTER * ( MAXSTR ) STR ! String to hold coded number
      BYTE VALUES( MAXVAL )   ! Values
      CHARACTER * ( DAT__SZLOC ) VEC ! Vector locator

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'    ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine whether or not the data type requires special processing.
      BWTYPE = 'B' .EQ. 'B' .OR. 'B' .EQ. 'UB' .OR.
     :         'B' .EQ. 'W' .OR. 'B' .EQ. 'UW'

*  Initialise the level of indention.
      INDENT = LENG

*  Find the maximum length of the line.
      MXLENG = CHR_SIZE( LINE )

*  Read a limited part of the variable.
      NVALUE = MIN( MAXVAL, SIZE )

*  Start a new error context.
      CALL ERR_MARK

*  Obtain a scalar.
      IF ( NDIM .EQ. 0 ) THEN
         CALL DAT_GET( LOC, '_BYTE', NDIM, DIMS, VALUES, STATUS )

*  or a vector.
      ELSE

*  Find a reasonable number to read in for the output.
         MXVAL = MIN( NVALUE, ( MXLENG - INDENT + 1 ) / 2 * NLINES )

*  Now read a limited part of the vector variable.
*  ===============================================

*  Get a locator to the vectorised object.
         VEC = ' '
         CALL DAT_VEC( LOC, VEC, STATUS )

*  Obtain a slice of the chosen length.
         SLICE = ' '
         CALL DAT_SLICE( VEC, 1, 1, MXVAL, SLICE, STATUS )

*  Get the values in the slice.  Use the general routine throughout so
*  as to obtain byte and word, signed and unsigned types.
         CALL DAT_GET( SLICE, '_BYTE', 1, MXVAL, VALUES, STATUS )
         NVALUE = MXVAL

*  Tidy the locators after use.
         CALL DAT_ANNUL( SLICE, STATUS )
         CALL DAT_ANNUL( VEC, STATUS )

*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      END IF

*  Handle the error transparently.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL CHR_PUTC( '{undefined}', LINE, LENG )
      ELSE

*  Initialise counters.
         NOLINE = 1
         NOREAD = 0

*  Write the values to a buffer.
         DO I = 1, SIZE

*  Find the element to output.
            IVALUE = I - NOREAD

*  Has this element been read in?
            IF ( IVALUE .GT. NVALUE ) THEN

*  No, so increment the number read so far.
               NOREAD = NOREAD + NVALUE

*  Find a reasonable number to read in for trace output.
               MXVAL = MIN( MAXVAL, SIZE - NOREAD,
     :                 ( MXLENG - INDENT + 1 ) / 2 * NLINES )

*  Now read a limited part of the vector variable.
*  ===============================================

*  Get a locator to the vectorised object.
               VEC = ' '
               CALL DAT_VEC( LOC, VEC, STATUS )

*  Obtain a slice of the chosen length.
               SLICE = ' '
               CALL DAT_SLICE( VEC, 1, NOREAD+1, NOREAD+MXVAL, SLICE,
     :                         STATUS )

*  Get the values in the slice.
               CALL DAT_GET( SLICE, '_BYTE', 1, MXVAL, VALUES,
     :                       STATUS )
               NVALUE = MXVAL

*  Tidy the locators after use.
               CALL DAT_ANNUL( SLICE, STATUS )
               CALL DAT_ANNUL( VEC, STATUS )

*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*  Reset the element number within the slice.
               IVALUE = 1
            END IF

*  Test for a bad value.
            IF ( VALUES( IVALUE ) .EQ. VAL__BADB ) THEN

*  Use an asterisk to indicate a bad value.
               STR = '*'
               NCHAR = 1
            ELSE

*  Convert the numeric value to a string.  Note that since there are no
*  conversions from the byte and word data types, these must be handled
*  as integers.
               IF ( BWTYPE ) THEN
                  DUMMY = NUM_BTOI( VALUES( IVALUE ) )
                  CALL CHR_ITOC( DUMMY, STR, NCHAR )

               ELSE IF ( 'B' .EQ. 'I' ) THEN
                  CALL CHR_ITOC( VALUES( IVALUE ), STR, NCHAR )

               ELSE IF ( 'B' .EQ. 'R' ) THEN
                  CALL CHR_RTOC( VALUES( IVALUE ), STR, NCHAR )

               ELSE IF ( 'B' .EQ. 'D' ) THEN
                  CALL CHR_DTOC( VALUES( IVALUE ), STR, NCHAR )

               END IF
            END IF

*  Put the value into the buffer unless it overflows the line.
            CALL KPS1_PUTS( NDIM, DIMS, IVALUE, STR( 1:NCHAR ),
     :                      I .NE. SIZE, INDS, LINE, LENG, FULL,
     :                      STATUS )

*  Is the line full?
            IF ( FULL ) THEN

*  Yes, so will need to output the current line.
               CALL MSG_SETC( 'LINE', LINE( 1:LENG ) )
               CALL MSG_OUT( ' ', '^LINE', STATUS )
               IF ( LOGEXM )
     :            CALL FIO_WRITE( FD, LINE( 1:LENG ), STATUS )

*  Should values continue to a new line?
               IF ( NOLINE .NE. NLINES ) THEN

*  Start a new line indenting the requested amount.
                  LENG = INDNTC + 1
                  LINE = ' '

*  Increment the count of the number of output lines.
                  NOLINE = NOLINE + 1

*  Write the value that would not fit into previous line at the start
*  of the new line.
                  CALL KPS1_PUTS( NDIM, DIMS, IVALUE, STR( 1:NCHAR ),
     :                            I .NE. SIZE, INDS, LINE, LENG,
     :                            FULL, STATUS )
               ELSE

*  The last value is not displayed if the line was full.  So decrement
*  the count of the number of values and exit the loop.
                  IVALUE = I - 1
                  GOTO 1
               END IF
            END IF
         END DO
    1    CONTINUE

*  Are there end values to be output on a new line?
         IF ( IVALUE .NE. SIZE .AND. NOLINE .EQ. NLINES ) THEN

*  Start a new line.
            IF ( NLINES .GT. 1 ) INDENT = INDNTC + 1
            LINE = ' '

*  Determine whether or not further values are to be read in.
            IF ( SIZE .GT. NVALUE + NOREAD ) THEN

*  Find a reasonable number to read in for output.
               MXVAL = MIN( MAXVAL, ( MXLENG - INDENT + 1 ) / 2,
     :                      SIZE - IVALUE )

*  Get a slice containing the last few values.
*  ===========================================

*  Get a locator to the vectorised object.
               VEC = ' '
               CALL DAT_VEC( LOC, VEC, STATUS )

*  Obtain a slice of the chosen length.
               SLICE = ' '
               CALL DAT_SLICE( VEC, 1, SIZE-MXVAL+1, SIZE, SLICE,
     :                         STATUS )

*  Get the values in the slice.
               CALL DAT_GET( SLICE, '_BYTE', 1, MXVAL, VALUES,
     :                       STATUS )
               NVALUE = MXVAL

*  Tidy the locators after use.
               CALL DAT_ANNUL( SLICE, STATUS )
               CALL DAT_ANNUL( VEC, STATUS )

*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*  Modify the last value to new co-ordinates.
               IVALUE = IVALUE - NOREAD - SIZE + MXVAL
            ELSE

*  No more values are to be read in.
               IVALUE = IVALUE - NOREAD
            END IF

*  Loop from the last value.
            FULL = .FALSE.
            FVALUE = NVALUE
            CPOS = MXLENG

*  Search backwards until the line is full.
            DO WHILE ( FVALUE .GT. IVALUE .AND. .NOT. FULL )

*  Test for a bad value.
               IF ( VALUES( FVALUE ) .EQ. VAL__BADB ) THEN

*  Use an asterisk to indicate a bad value.
                  STR = '*'
                  NCHAR = 1
                  BAD = .TRUE.
               ELSE

*  Convert the numeric value to a string.  Note that since there are no
*  conversions from the byte and word data types, these must be handled
*  as integers.
                  IF ( BWTYPE ) THEN
                     DUMMY = NUM_BTOI( VALUES( FVALUE ) )
                     CALL CHR_ITOC( DUMMY, STR, NCHAR )

                  ELSE IF ( 'B' .EQ. 'I' ) THEN
                     CALL CHR_ITOC( VALUES( FVALUE ), STR, NCHAR )

                  ELSE IF ( 'B' .EQ. 'R' ) THEN
                     CALL CHR_RTOC( VALUES( FVALUE ), STR, NCHAR )

                  ELSE IF ( 'B' .EQ. 'D' ) THEN
                     CALL CHR_DTOC( VALUES( FVALUE ), STR, NCHAR )

                  END IF
                  BAD = .FALSE.
               END IF

*  Insert a comma unless it is the last value.
               IF ( FVALUE .NE. NVALUE ) THEN
                  NCHAR = NCHAR + 1
                  STR( NCHAR:NCHAR ) = ','
               END IF

*  Determine whether or not there is room to insert the string, with or
*  without an ellipsis as necessary.
               IF ( ( INDENT .GT. CPOS-NCHAR+1 .AND.
     :                FVALUE .EQ. IVALUE + 1 ) .OR.
     :              ( INDENT + SZELIP + 1 .GT. CPOS-NCHAR ) ) THEN

*  There is no room.
                  FULL = .TRUE.

*  Ignore bad value as its character representation cannot be
*  shortened.
                  IF ( .NOT. BAD ) THEN

*  Test whether ellipsis is needed.
                     IF ( FVALUE .GT. IVALUE ) THEN

*  Allow for the special case when there is no room to output last
*  value at full precision...
                        IF ( FVALUE .EQ. NVALUE ) THEN

*  ... provided any text be output
                           IF ( INDENT+SZELIP+4 .LT. CPOS ) THEN

*  Convert the numeric value to a string.  Note that since there are no
*  conversions from the byte and word data types, these must be handled
*  as integers.
                              IF ( BWTYPE ) THEN
                                 DUMMY = NUM_BTOI( VALUES( FVALUE ) )
                                 CALL CHR_ITOC( DUMMY, LINE( 1 + SZELIP
     :                                          + INDENT:CPOS ), I )

                              ELSE IF ( 'B' .EQ. 'I' ) THEN
                                 CALL CHR_ITOC( VALUES( FVALUE ),
     :                                          LINE( 1 + SZELIP +
     :                                          INDENT:CPOS ), I )

                              ELSE IF ( 'B' .EQ. 'R' ) THEN
                                 CALL CHR_RTOC( VALUES( FVALUE ),
     :                                          LINE( 1 + SZELIP +
     :                                          INDENT:CPOS ), I )

                              ELSE IF ( 'B' .EQ. 'D' ) THEN
                                 CALL CHR_DTOC( VALUES( FVALUE ),
     :                                          LINE( 1 + SZELIP +
     :                                          INDENT:CPOS ), I )
                              END IF

*  Insert the ellipsis into the output line.
                              LINE( INDENT:INDENT+SZELIP+1 ) = '... '
                           END IF

*  The column position is at the indentation point.
                           CPOS = INDENT
                        ELSE

*  Omit the value which will not fit into the available space and
*  insert the ellipsis into the output line.
                           LINE( CPOS-SZELIP:CPOS ) = '... '

*  Move the column position left by the length of the ellipsis and the
*  space.
                           CPOS = CPOS - SZELIP - 1
                        END IF
                     ELSE

*  Allow for the special case when there is no room to output last
*  value at full precision, but without the ellipsis.
                        IF ( FVALUE .EQ. NVALUE ) THEN

*  Convert the numeric value to a string. Note that since there are no
*  conversions from the byte and word data types, these must be handled
*  as integers.
                           IF ( BWTYPE ) THEN
                              DUMMY = NUM_BTOI( VALUES( FVALUE ) )
                              CALL CHR_ITOC( DUMMY, LINE( INDENT:CPOS ),
     :                                       I )

                           ELSE IF ( 'B' .EQ. 'I' ) THEN
                              CALL CHR_ITOC( VALUES( FVALUE ),
     :                                       LINE( INDENT:CPOS ), I )

                           ELSE IF ( 'B' .EQ. 'R' ) THEN
                              CALL CHR_RTOC( VALUES( FVALUE ),
     :                                       LINE( INDENT:CPOS ), I )

                           ELSE IF ( 'B' .EQ. 'D' ) THEN
                              CALL CHR_DTOC( VALUES( FVALUE ),
     :                                       LINE( INDENT:CPOS ), I )
                           END IF

*  The column position is at the indentation point.
                           CPOS = INDENT
                        END IF
                     END IF

*  End of the check for the current value being bad.
                  END IF

               ELSE

*  Insert the string into the line.
                  LINE( CPOS - NCHAR + 1:CPOS ) = STR( 1:NCHAR )

*  Move the column position by the width of the added string.
                  CPOS = CPOS - NCHAR
               END IF

*  Decrement the element index as we are filling the line from the last
*  value.
               FVALUE = FVALUE - 1
            END DO

*  See if the current position is at the indentation point.
            IF ( CPOS .GT. INDENT ) THEN

*  It is not so slide the text along for alignment at the indentation
*  column.
               DO  I = CPOS + 1, MXLENG
                  LINE( I-CPOS+INDENT:I-CPOS+INDENT ) = LINE( I:I )
               END DO

*  Shorten the line length accordingly.
               MXLENG = MXLENG - CPOS + INDENT
            END IF

            LENG = MXLENG

         END IF
      END IF

*  Release the new error context.
      CALL ERR_RLSE

      END
