      SUBROUTINE TRA_PUTC( LOC, NDIM, DIMS, SIZE, NLINES, INDNTC,
     :                     ONEPLN, LOGEXM, FD, LINE, LENG, STATUS )
*+
*  Name:
*     TRA_PUTC

*  Purpose:
*     Puts out the first and last few values of a CHARACTER component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRA_PUTC( LOC, NDIM, DIMS, SIZE, NLINES, INDNTC,
*    :               ONEPLN, LOGEXM, FD, LINE, LENG, STATUS )

*  Description:
*     A number of values are read from the object and coded into one
*     or more text lines in a concise manner.  The information may
*     be written to an ASCII file.
*
*     The values are normally listed at the end of one line, but may
*     start on a new line.  The maximum number of lines of data values
*     may also be set.  For all but the smallest arrays where the values
*     of all elements can be displayed in the space provided, the last
*     few values in the array as well as the first few are presented.
*     The last few values appear on a new line, indented the same as
*     the line above with the ellipsis notation to indicate any missing
*     values.  Note the number of elements shown depends on the number
*     of characters that will fit on the line.
*
*     The reporting of values of character arrays may be formatted in
*     one of two ways. One is that character-array-element values are
*     concatenated to fill the available space and may span lines.
*     The alternative is to write the value of each element on a new
*     line. The latter usually improves readability for long strings.
*     (Note to preserve the alignment the ellipsis for any missing
*     values appears (for readability) at the end of previous line,
*     rather than the beginning of the line containing the value of the
*     last element.) In both formats, elements are delimited by commas.
*
*     If only a substring can be fitted into the space available then
*     the ellipsis notation is used before the closing quotation mark
*     to indicate this fact.

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
*        Number of lines to store values.
*     INDNTC = INTEGER (Given)
*        Indentation level for continuation lines of values.
*     ONEPLN = LOGICAL (Given)
*        If true the elements of a character array each appear on a
*        separate line
*     LOGEXM = LOGICAL (Given)
*        If true a log of the header records is written to an ASCII
*        file.
*     FD = INTEGER (Given)
*        The ASCII file descriptor, ignored if LOGEXM is false.
*     LINE = CHARACTER*(*)( WRITE )
*        The line of text to be output.
*     STATUS = INTEGER( UPDATE )
*        The global status.

*  Algorithm:
*     If status is bad then return
*     Read a limited part of the data object slicing if an array
*     If an error occurred then
*        Output {undefined} message
*     Else if one value to appear per line for an array
*        For each value to be output
*           If current element has not been read in then
*              Increment count of number of elements read in so far
*              Read in the next slice of values
*           Endif
*           Find how many characters may be fitted on the current line,
*             and how many to write
*           Build the text in a buffer...
*           If there is room to write part of the value then
*              Write current value, put ellipsis before trailing quote
*                in buffer if string was truncated; put comma after
*                trailing quote unless last value
*           Endif
*           If there are more lines to output then
*              Report the line and to the log file if requested
*              Reset line buffer and identation level
*           Endif
*        Endfor
*        If the last value has not been reported, do so as above,
*          perhaps getting another slice containing just the last
*          element
*     Else
*        Initialise line count
*        For each value
*           If current element has not been read in then
*              Increment count of number of elements read in so far
*              Read in the next slice of values
*           Endif
*           Loop until all characters are output for the current element
*             or there is no space to take any more
*           Find how many characters may be fitted on the current line
*             allowing for an ellipsis where required, and how many to
*             write
*           Build the text in a buffer...
*           If there is room to write part of the value then
*              Write current value, put ellipsis before trailing quote
*                in buffer if string was truncated; put comma after
*                trailing quote unless last value
*              Set number of characters written to the number in the
*                string, if the value fully transcribed to the buffer
*           Else
*              Set number of characters to write to zero
*           Endif
*           If there is no room to start the next element
*              If there are more lines to output then
*                 Report the line and to the log file if requested
*                 Reset line buffer and identation level
*              Endif
*              If there is no more room for any more values exit from
*                the loop
*              Increment the line count
*           Endif
*        Endfor
*        If there are the last values of an array to be output on a
*          new line then
*           Start a new line
*           Read in the last few values (most that could fit on a line)
*             should they not already be stored in the data slice
*           Loop from the last value in the array
*              Convert to character string
*              Find the number of characters of the current element
*                that may be output
*              If not the last value, append a comma to it
*              If there is room to insert the string then
*                 Do so
*                 Move current column position left by the number of
*                   characters written in the line buffer for the
*                   current element
*              Else
*                 Set switch to exit from the loop
*                 Prefix the string with an ellipsis if needed (i.e.
*                   there are missing values), and apend with a comma
*                   if not the last value
*                 Move current column position to the indentation level
*              Endif
*           Endfor
*           Align text with the indentation level within the line buffer
*        Endif
*     Endif
*     End

*  Prior Requirements:
*     -  The ASCII file associated with descriptor FD must already be
*     opened.

*  Copyright:
*     Copyright (C) 1983, 1989, 1991 Science & Engineering Research
*     Council. Copyright (C) 2007 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     JRG: Jack Giddings (UCL)
*     MJC: Malcolm J. Currie  (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-FEB-1983 (JRG):
*        Original version.
*     1989 May 16 (MJC):
*        Largely rewritten; many extra parameters to control
*        the layout; tidied.
*     1989 Jun 15 (MJC):
*        Renamed from LSPUTC to avoid confusion with the original TRACE
*        version; added ONEPLN, LOGEXM, FD and STATUS arguments.
*     1991 January 30 (MJC):
*        Converted to the SST prologue.
*     2007 January 4 (TIMJ):
*        Prevent FVALUE going to 0 in VALUES(FVALUE) loop
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off the default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SAI Constants
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'DAT_ERR'        ! Data-system errors

*  Arguments Given:
      CHARACTER * ( DAT__SZLOC )
     :  LOC                    ! Locator to a component.

      INTEGER
     :  NDIM,                  ! Number of dimensions
     :  DIMS(*),               ! Dimensions
     :  SIZE,                  ! Size of object as if vector
     :  NLINES,                ! Number of lines of values (excluding
                               ! line for last values of an array)
     :  INDNTC,                ! Indentation column number for
                               ! continuation lines
     :  FD                     ! Log file's description

      LOGICAL                  ! True if:
     :  LOGEXM,                ! Output to go to the log file
     :  ONEPLN                 ! Elements of a character array each
                               ! appear on a new line

*  Arguments Given and Returned:
      CHARACTER * ( * )
     :  LINE                   ! Line to receive numbers
      INTEGER
     :  LENG                   ! Current line length

*  Status:
      INTEGER
     :  STATUS                 ! Local status

*  External References:
      INTEGER CHR_LEN          ! String size ignoring trailing blanks
      INTEGER CHR_SIZE         ! String size including trailing blanks

*  Local Constants:
      INTEGER SZELIP           ! Size of ellipsis (...)
      PARAMETER( SZELIP=3 )
      INTEGER MAXVAL           ! Maximum number of values read
      PARAMETER( MAXVAL=20 )
      INTEGER MAXSTR           ! Size of temporary string for characters
      PARAMETER( MAXSTR=1000 ) ! How long is a piece of string?

*  Local Variables:
      CHARACTER*( DAT__SZLOC )
     :  SLICE,                 ! Slice locator
     :  VEC                    ! Vector locator
      CHARACTER*( MAXSTR )
     :  STR                    ! String to hold coded number

      CHARACTER*( MAXSTR-2 )
     :  VALUES( MAXVAL )       ! Values

      INTEGER
     :  CPOS,                  ! Column position of last character in
                               ! a final value string
     :  ELLCH,                 ! Characters allowed for ellipsis
     :  FVALUE,                ! Value index for the last elements
     :  I,                     ! Temporary length
     :  INDENT,                ! Level of indention
     :  IVALUE,                ! Value index for the first elements
     :  J,                     ! Loop counter
     :  MXVAL,                 ! Maximum number of elements to be read
     :  MXLENG,                ! Length of the line buffer
     :  NACT,                  ! Actual string value length used
     :  NCHAR,                 ! Number of characters in STR
     :  NCW,                   ! Number of data characters written so
                               ! far
     :  NDL,                   ! Number of delimiters to be written
     :  NOLINE,                ! Number of line currently being formed
     :  NOREAD,                ! Number of elements read in
     :  NTW,                   ! Number of data characters to be
                               ! written
     :  NVALUE                 ! Number of values accessed

                               ! True if:
      LOGICAL FULL             ! Text line is full

*.

      NCHAR = 0
      IVALUE = 0

*    Check global status for an error.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise the level of indention.

      INDENT = LENG

*    Find the maximum length of the line.

      MXLENG = CHR_SIZE( LINE )

*    Read a limited part of the variable.

      NVALUE = MIN( MAXVAL, SIZE )

*    Start a new error context.

      CALL ERR_MARK

*    Obtain a scalar...

      IF ( NDIM .EQ. 0 ) THEN
         CALL DAT_GET0C( LOC, VALUES, STATUS )
         MXVAL = 1

*    or a vector.

      ELSE

*       One per line up to the maximum that can be stored in the VALUES
*       buffer.

         IF ( ONEPLN ) THEN
            MXVAL = MIN( NLINES, NVALUE )
         ELSE

*          Find a reasonable number to read in for the output.

            MXVAL = MIN( NVALUE, ( MXLENG - INDENT + 1 ) / 2 * NLINES )
         END IF

*       Now read a limited part of the vector variable.
*       ===============================================

*       Get a locator to the vectorised object.

         VEC = ' '
         CALL DAT_VEC( LOC, VEC, STATUS )

*       Obtain a slice of the chosen length.

         SLICE = ' '
         CALL DAT_SLICE( VEC, 1, 1, MXVAL, SLICE, STATUS )

*       Get the values in the slice.

         CALL DAT_GETVC( SLICE, MAXVAL, VALUES, NVALUE, STATUS )
         IF ( STATUS .EQ. DAT__TRUNC ) CALL ERR_ANNUL( STATUS )

*       Tidy the locators after use.

         CALL DAT_ANNUL( SLICE, STATUS )
         CALL DAT_ANNUL( VEC, STATUS )

*       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      END IF

*    Initialise a counter.

      NOREAD = 0

*    Handle the error transparently.

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL CHR_PUTC( '{undefined}', LINE, LENG )


*    One value per line for an array.

      ELSE IF ( ONEPLN .AND. SIZE .GT. 1 ) THEN

*       Write the values to the buffer.

         DO J = 1, MIN( SIZE, NLINES )

*          Find the element to output.

            IVALUE = J - NOREAD

*          Has this element been read in?

            IF ( IVALUE .GT. NVALUE ) THEN

*             No, so increment the number read so far.

               NOREAD = NOREAD + NVALUE

*             Find a reasonable number to read in for trace output.

               MXVAL = MIN( MAXVAL, SIZE - NOREAD,
     :                 ( MXLENG - INDENT + 1 ) / 2 * NLINES - NOREAD )

*             Now read a limited part of the vector variable.
*             ===============================================

*             Get a locator to the vectorised object.

               VEC = ' '
               CALL DAT_VEC( LOC, VEC, STATUS )

*             Obtain a slice of the chosen length.

               SLICE = ' '
               CALL DAT_SLICE( VEC, 1, NOREAD+1, NOREAD+MXVAL, SLICE,
     :                         STATUS )

*             Get the values in the slice.

               CALL DAT_GETVC( SLICE, MAXVAL, VALUES, NVALUE, STATUS )

*             Tidy the locators after use.

               CALL DAT_ANNUL( SLICE, STATUS )
               CALL DAT_ANNUL( VEC, STATUS )

*             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*             Reset the element number within the slice.

               IVALUE = 1
            END IF

*          Find the length of the value in characters.

            NCHAR = MAX( CHR_LEN( VALUES( IVALUE ) ), 1 )
            I = 0

*          Find how many characters can be displayed on the current
*          line.  Note to preserve alignment the ellipsis for missing
*          elements of the array appears at the end of the last line,
*          rather than at the beginning of the line containing the
*          last element.

            IF ( J .EQ. NLINES ) THEN
               ELLCH = SZELIP + 1
            ELSE
               ELLCH = 0
            END IF
            IF ( IVALUE .EQ. SIZE ) THEN
               NACT = CHR_SIZE( LINE ) - ( LENG + 2 + ELLCH )
            ELSE
               NACT = CHR_SIZE( LINE ) - ( LENG + 3 + ELLCH )
            END IF

*          Find number of data characters to write.

            NTW = MIN( NCHAR, NACT )

*          Write leading quotation mark to the buffer.

            CALL CHR_PUTC( '''', STR, I )

*          If not all the characters can be reported, then allow for
*          the ellipsis.

            IF ( NTW .NE. NCHAR ) THEN
               NTW = NTW - SZELIP
            END IF

*          Check there is room to output the value.

            IF ( NTW .GE. 1 ) THEN
               CALL CHR_PUTC( VALUES( IVALUE )( 1:NTW ), STR, I )

               IF ( NTW .NE. NCHAR ) THEN

*                Put ellipsis before trailing quote if string was
*                truncated.

                  CALL CHR_PUTC( '...', STR, I )
                  CALL CHR_PUTC( '''', STR, I )
               ELSE

                  CALL CHR_PUTC( '''', STR, I )
               END IF

*             Put a comma after the trailing quote to separate from the
*             next element.

               IF ( IVALUE + NOREAD .LT. SIZE )
     :           CALL CHR_PUTC( ',', STR, I )

*             Append an ellipsis if there are missing values.

               IF ( IVALUE + NOREAD + 1 .LT. SIZE .AND. J .EQ. NLINES )
     :           CALL CHR_PUTC( ' ...', STR, I )

*             Append the value string in the temporary buffer to the
*             output line.

               CALL CHR_PUTC( STR( 1:I ), LINE, LENG )
            END IF

*          Have we exhausted the number of lines?

            IF ( ( J .EQ. NLINES .AND. IVALUE + NOREAD .LT. SIZE )
     :           .OR. J .NE. NLINES ) THEN

*             Report line (to log file as well if requested)

               CALL MSG_SETC( 'LINE', LINE( 1:LENG ) )
               CALL MSG_OUT( ' ', '^LINE', STATUS )

*             Record the line in the log file.

               IF ( LOGEXM ) CALL FIO_WRITE( FD, LINE(1:LENG), STATUS )

*             Reset the line buffer for a new line aligning with line
*             above.

               IF ( NLINES .EQ. 1 ) THEN
                  LENG = INDENT
               ELSE
                  LENG = INDNTC + 1
               END IF
               LINE = ' '
            END IF

         END DO

*       If the previous value was not the last element in the array,
*       we need to output the last element on a new line.

         IF ( IVALUE + NOREAD .NE. SIZE ) THEN
            I = 0

*          Find how many characters can be displayed on the current
*          line.

            NACT = CHR_SIZE( LINE ) - ( LENG + 2 )

*          Find number of data characters to write.

            NTW = MIN( NCHAR, NACT )

*          If not all the characters can be reported, then allow for
*          the ellipsis within the string.

            IF ( NTW .NE. NCHAR ) THEN
               NTW = NTW - SZELIP
            END IF

*          Write the leading quotation mark to the buffer.

            CALL CHR_PUTC( '''', STR, I )

*          Has the last data object already been read?

            IF ( NVALUE + NOREAD .LT. SIZE ) THEN

*             No so get the last value.
*             =========================

*             Get a locator to the vectorised object.

               VEC = ' '
               CALL DAT_VEC( LOC, VEC, STATUS )

*             Obtain a slice containing the last value.

               SLICE = ' '
               CALL DAT_SLICE( VEC, 1, SIZE, SIZE, SLICE, STATUS )

*             Get the last value.

               CALL DAT_GETVC( SLICE, MAXVAL, VALUES, NVALUE, STATUS )

*             Tidy the locators after use.

               CALL DAT_ANNUL( SLICE, STATUS )
               CALL DAT_ANNUL( VEC, STATUS )

*             ^^^^^^^^^^^^^^^^^^^^^^^^^

*             Find how many characters there are to output.

               NCHAR = MAX( CHR_LEN( VALUES( NVALUE ) ), 1 )
               NACT = CHR_SIZE( LINE ) - ( LENG + 2 )
               NTW = MIN( NCHAR, NACT )
            END IF

*          Append the value to the string.

            CALL CHR_PUTC( VALUES( NVALUE )( 1:NTW ), STR, I )

*          Is there room to write the string.

            IF ( NTW .NE. NCHAR ) THEN

*             Put an ellipsis before the trailing quote if the string
*             was truncated.

               CALL CHR_PUTC( '...', STR, I )
               CALL CHR_PUTC( '''', STR, I )
            ELSE

               CALL CHR_PUTC( '''', STR, I )
            END IF

*          Append the value string in the temporary buffer to the
*          output line.

            CALL CHR_PUTC( STR( 1:I ), LINE, LENG )

*       End of check for last value to be output.

         END IF

*    Now for the general case.
*    =========================

      ELSE

*       Start at the first line.

         NOLINE = 1

*       Write the values to the buffer.

         DO J = 1, MXVAL

*          Find the element to output.

            IVALUE = J - NOREAD

*          Has this element been read in?

            IF ( IVALUE .GT. NVALUE ) THEN

*             No, so increment the number read so far.

               NOREAD = NOREAD + NVALUE

*             Find a reasonable number to read in for the trace output.

               MXVAL = MIN( MAXVAL, SIZE - NOREAD,
     :                 ( MXLENG - INDENT + 1 ) / 4 * NLINES - NOREAD )

*             Now read a limited part of the vector variable.
*             ===============================================

*             Get a locator to the vectorised object.

               VEC = ' '
               CALL DAT_VEC( LOC, VEC, STATUS )

*             Obtain a slice of the chosen length.

               SLICE = ' '
               CALL DAT_SLICE( VEC, 1, NOREAD+1, NOREAD+MXVAL, SLICE,
     :                         STATUS )

*             Get the values in the slice.

               CALL DAT_GETVC( SLICE, MAXVAL, VALUES, NVALUE, STATUS )

*             Tidy the locators after use.

               CALL DAT_ANNUL( SLICE, STATUS )
               CALL DAT_ANNUL( VEC, STATUS )

*             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*             Reset the element number within the slice.

               IVALUE = 1
            END IF

*          Find the length of the value in characters.

            NCHAR = MAX( CHR_LEN( VALUES( IVALUE ) ), 1 )
            I = 0
            NCW = 0

*          Write leading quotation mark to the buffer.

            CALL CHR_PUTC( '''', STR, I )

*          Loop until all characters output for current element, or
*          there is no space left to take any more.

            DO WHILE ( NCW .LT. NCHAR .AND. NOLINE .LE. NLINES )

*             Find how many characters can be displayed on the current
*             line.

               IF ( J .EQ. SIZE ) THEN
                  NACT = CHR_SIZE( LINE ) - ( LENG + 2 )
               ELSE
                  NACT = CHR_SIZE( LINE ) - ( LENG + 3 )
               END IF

*             Find number of data characters to write.

               NTW = MIN( NCHAR-NCW, NACT )

*             If not all the characters can be reported, then allow for
*             the ellipsis.

               IF ( NOLINE .EQ. NLINES .AND. NTW+NCW .NE. NCHAR ) THEN
                  ELLCH = SZELIP
                  NTW = NTW - ELLCH
               ELSE
                  ELLCH = 0
               END IF

*             Watch out for the case where there is no room to output
*             text on the line.

               IF ( NTW .GE. 1 ) THEN

*                Write as much of the value as will fit (less space for
*                the ellipsis).

                  CALL CHR_PUTC( VALUES( IVALUE )( NCW+1:NCW+NTW ),
     :                           STR, I )

*                Is this the last line to output?

                  IF ( NOLINE .EQ. NLINES ) THEN

*                   Are there missing characters?

                     IF ( NTW+NCW .NE. NCHAR ) THEN

*                      Put an ellipsis before the trailing quote if the
*                      string was truncated.

                        CALL CHR_PUTC( '...', STR, I )
                        CALL CHR_PUTC( '''', STR, I )
                        NCW = NCHAR
                     ELSE

*                      Put comma after the trailing quote to separate
*                      from the next element.

                        CALL CHR_PUTC( '''', STR, I )
                        IF ( J .LT. SIZE )
     :                    CALL CHR_PUTC( ',', STR, I )
                     END IF

                  ELSE IF ( NTW .NE. NACT ) THEN

*                   Put a comma after the trailing quote to separate it
*                   from the next element.

                     CALL CHR_PUTC( '''', STR, I )
                     IF ( J .LT. SIZE )
     :                 CALL CHR_PUTC( ',', STR, I )
                  END IF

*                Append the value string in the temporary buffer to the
*                output line.

                  CALL CHR_PUTC( STR( 1:I ), LINE, LENG )
               ELSE

*                No more to write.

                  NTW = 0
               END IF

*             Go to a new line.  The minus one is to prevent a sole
*             quotation mark appearing at the end of the line.

               IF ( NTW + ELLCH .GT. NACT-1 ) THEN

*                The last line is written outside this routine unless
*                a further line of last values is to be output.

                  IF ( ( NOLINE .EQ. NLINES .AND. J .LT. SIZE )
     :                 .OR. NOLINE .LT. NLINES ) THEN

*                   The line is full, so report it.

                     CALL MSG_SETC( 'LINE', LINE( 1:LENG ) )
                     CALL MSG_OUT( ' ', '^LINE', STATUS )

*                   Record the line in the log file.

                     IF ( LOGEXM )
     :                 CALL FIO_WRITE( FD, LINE( 1:LENG ), STATUS )

*                   Start a new line aligning with line above.  Clear
*                   the line buffer.

                     IF ( NLINES .EQ. 1 ) THEN
                        LENG = INDENT
                     ELSE
                        LENG = INDNTC + 1
                     END IF
                     LINE = ' '
                     I = 0
                  END IF

*                No room for any more values so exit from the loop.

                  IF ( NOLINE .EQ. NLINES .AND. J .LT. SIZE )
     :               GOTO 1

*                Increment the line count.

                  NOLINE = NOLINE + 1
               END IF

*             Update the tally of the number of characters written.

               NCW = NCW + NTW
            END DO

         END DO
    1    CONTINUE


*       Are there end values to be output on a new line?

         IF ( IVALUE + NOREAD .NE. SIZE .AND. NOLINE .GE. NLINES ) THEN

*          Start a new line indenting at the specified column.

            IF ( NLINES .GT. 1 ) INDENT = INDNTC + 1
            LINE = ' '

*          Determine whether or not further values are to be read in.

            IF ( SIZE .GT. NVALUE + NOREAD ) THEN

*             Yes, so find a reasonable number to read in for the trace
*             output.

               MXVAL = MAX( MIN( MAXVAL, ( MXLENG - INDENT + 1 ) / 4 ),
     :                      1 )

*             Get a slice containing the last few values.
*             ===========================================

*             Get a locator to the vectorised object.

               VEC = ' '
               CALL DAT_VEC( LOC, VEC, STATUS )

*             Obtain a slice of the chosen length.

               SLICE = ' '
               CALL DAT_SLICE( VEC, 1, SIZE-MXVAL+1, SIZE, SLICE,
     :                         STATUS )

*             Get the values in the slice.

               CALL DAT_GETVC( SLICE, MAXVAL, VALUES, NVALUE, STATUS )

*             Tidy the locators after use.

               CALL DAT_ANNUL( SLICE, STATUS )
               CALL DAT_ANNUL( VEC, STATUS )

*             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*             Modify the last value to the new co-ordinates.

               IVALUE = IVALUE + NOREAD - SIZE + MXVAL
            END IF

*          Loop backwards from the last value.  The column position is
*          at the end of the line.

            FULL = .FALSE.
            FVALUE = NVALUE
            CPOS = MXLENG
            DO WHILE ( FVALUE .GT. IVALUE .AND. .NOT. FULL
     :                 .AND. FVALUE .GT.0 )

*             Copy the value to the string.

               CALL CHR_CTOC( VALUES( FVALUE ), STR, I )

*             Allow for the comma and quotation marks.

               NCHAR = I + 2
               IF ( FVALUE .NE. NVALUE ) THEN
                  NCHAR = NCHAR + 1
               END IF

*             Determine whether or not there is room to insert the
*             string, with or without an ellipsis as necessary.

               IF ( ( INDENT .GT. CPOS-NCHAR+1 .AND.
     :                FVALUE .EQ. IVALUE + 1 ) .OR.
     :              ( INDENT + SZELIP + 1 .GT. CPOS-NCHAR ) ) THEN

*                There is no room.

                  FULL = .TRUE.

*                Find the maximum number of characters that can be
*                accommodated by determining the number of special
*                characters (ellipses for missing characters and values,
*                comma and quotes) and allowing for the indentation.

                  NDL = INDENT + 2 + SZELIP
                  IF ( FVALUE .NE. NVALUE ) NDL = NDL + 1
                  IF ( FVALUE .GT. IVALUE ) NDL = NDL + SZELIP + 1
                  NTW = CPOS - NDL

*                Provided there is at least a data character to be
*                written...

                  IF ( NTW .GE. 1 ) THEN

*                   This is messy as we are going backwards (no reverse
*                   CHR_PUTC), and there are four conditions:
*
*                   There is no trailing comma as it is the last
*                   element of the array.

                     IF ( FVALUE .EQ. NVALUE ) THEN

*                      An ellipsis is added if there are some missing
*                      elements not reported.

                        IF ( FVALUE .GT. IVALUE ) THEN
                           LINE( CPOS-NDL-NTW+1+INDENT:CPOS ) =
     :                       '... '''//STR( 1:NTW )//'...'''

*                      No missing elements.

                        ELSE
                           LINE( CPOS-NDL-NTW+1+INDENT:CPOS ) =
     :                       ''''//STR( 1:NTW )//'...'''
                        END IF

*                   Not the last element.

                     ELSE

*                      An ellipsis is added if there are some missing
*                      elements not reported.

                        IF ( FVALUE .GT. IVALUE ) THEN
                           LINE( CPOS-NDL-NTW+1+INDENT:CPOS ) =
     :                       '... '''//STR( 1:NTW )//'...'','

*                      No missing elements.

                        ELSE
                           LINE( CPOS-NDL-NTW+1+INDENT:CPOS ) =
     :                       ''''//STR( 1:NTW )//'...'','
                        END IF
                     END IF

*                   Reset the current position.

                     CPOS = CPOS - NDL - NTW + INDENT
                  ELSE

*                   Cannot write the value so just insert an ellipsis
*                   to the left of the current column position.

                     LINE( CPOS-SZELIP:CPOS ) = '... '
                  END IF

*             The value can be accommodated in full.

               ELSE

*                Insert the string in the output line with surrounded
*                by quotes and suffixed by a comma delimeter if
*                required.

                  IF ( FVALUE .NE. NVALUE ) THEN
                     LINE( CPOS:CPOS ) = ','
                     CPOS = CPOS - 1
                  END IF
                  LINE( CPOS - I - 1:CPOS ) = ''''//STR( 1:I )//''''
                  CPOS = CPOS - ( I + 2 )
               END IF

*             Decrement the element count as we are going from right to
*             left.

               FVALUE = FVALUE - 1
            END DO

*          See if the current position is at the indentation point.

            IF ( CPOS .GT. INDENT ) THEN

*             It is not so slide the text along for alignment at the
*             indentation column.

               DO  I = CPOS + 1, MXLENG
                  LINE( I-CPOS+INDENT:I-CPOS+INDENT ) = LINE( I:I )
               END DO

*             Shorten the line length accordingly.

               MXLENG = MXLENG - CPOS + INDENT
            END IF

            LENG = MXLENG

         END IF
      END IF

*    Release the new error context.

      CALL ERR_RLSE

      END
