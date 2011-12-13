      SUBROUTINE LISTSB ( ARRAY, DIM1, DIM2, XLOW, YLOW, XUPP, YUPP,
     :                    OPENF, FDI, FILNAM, STATUS )
*+
*  Name:
*     LISTSB

*  Purpose:
*     Lists a specified section of a 2-d array to a file for
*     printing

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*     CALL LISTSB( ARRAY, DIM1, DIM2, XLOW, YLOW, XUPP, YUPP, OPENF,
*    :             FDI, FILNAM, STATUS )

*  Description:
*     This routine takes an input 2-d array and lists out a specified
*     section of that image, as defined by the x-and-y lower-and-upper
*     bounds to a Fortran file. The file must either be already opened
*     and specified by the input file descriptor, or be created in this
*     routine and will be associated with the supplied parameter name.
*     In the former case the width of a record must be at least 132
*     characters.  The format chosen gives ten numbers across a full
*     printout page (132 chars), and a new page is thrown at the right
*     point.

*  Arguments:
*     ARRAY( DIM1, DIM2 )  =  REAL( READ )
*         2-d array to be listed.
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     XLOW  =  INTEGER( READ )
*         x co-ord of lower left corner of sub-array to be listed out.
*     YLOW  =  INTEGER( READ )
*         y co-ord of lower left corner of sub-array to be listed out.
*     XUPP  =  INTEGER( READ )
*         x co-ord of upper right corner of sub-array to be listed out.
*     YUPP  =  INTEGER( READ )
*         y co-ord of upper right corner of sub-array to be listed out.
*     OPENF =  LOGICAL( READ )
*         If true the Fortran file is to be associated with %FILNAM and
*           opened otherwise the file is assumed to have been opened and
*           has descriptor %FD.
*     FDI  =  INTEGER( READ )
*         The descriptor associated with the previously opened Fortran
*           file.
*     FILNAM  =  CHAR( READ )
*         Parameter name of the file to be opened and to contain the
*           listing output.
*     STATUS  =  INTEGER( READ, WRITE )
*         Global status value

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Open file for listing if required
*     If an error occurred then abort
*     Loop round selected columns in strips of ten
*        Output new page and current strip number to file
*        If no error then
*           Work out the x indices of strip start and end
*           For all columns in current strip
*              Convert column index into a string
*              Stick this string into the output string
*           Endfor
*           If no error then
*              Output column index string to file
*              For all selected lines in current strip and no error
*                 Convert line index into string and put into output
*                   string
*                 For all selected pixels in current line
*                    If pixel does not lie on array
*                       Indicate with special string
*                    Elseif pixel is invalid
*                       Indicate with special string
*                    Else
*                       Get value from array at this pixel location
*                       Convert value to a string
*                       Stick the string into the output string at
*                         the right point
*                    Endif
*                 Endfor
*                 Output string corresponding to current line to file
*                 If an error occurred report it
*              Endfor
*              Increment loop counter
*           Endfor
*        Endif
*     Endfor
*     Close file if opened in the routine
*     End

*  Copyright:
*     Copyright (C) 1986, 1988-1990, 1992 Science & Engineering
*     Research Council. All Rights Reserved.

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
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*     {enter_new_authors_here}

*  History:
*     14-JUN-1986 (REVA::MJM):
*        : First implementation - partly from KFH
*     1986 Aug 14: Renamed from LISTSUB, completed prologue, removed
*                  format statements and added IOSTAT to file open,
*                  nearly conformed to Starlink programming standards
*                  (RL.STAR::CUR).
*     1986 Sep 4 : Renamed parameters section to arguments, tidied
*                  and applied bad-pixel handling (RL.STAR::CUR).
*     1988 Feb 15: Bug fix --- JJ assignment (RL.STAR::CUR)
*     1988 Jun 27: Converted to FIO, added error reporting, made FILNAM
*                  the parameter name rather than the file name itself,
*                  and restructured (RL.STAR::CUR).
*     1989 Jul 27: Used packaged FIO_ASSOC to open the x,y file, and
*                  passed the array dimensions as two variables
*                  (RL.STAR::CUR).
*     1990 Feb 20: AIF_OPFIO renamed AIF_ASFIO (RAL::CUR).
*     1990 Feb 22: Added new OPEN and FDI arguments so that listings
*                  can be inserted in the middle of other output in a
*                  Fortran file, i.e. to make it more modular
*                  (RAL::CUR).
*     1992 Jan 26: Used functional FIO_ASSOC and FIO_CANCL. (RAL::MJC)
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-

*  Type Definitions:

      IMPLICIT  NONE           ! no default typing allowed

*  Global Constants:

      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'PAR_ERR'        ! Parameter-system error constants
      INCLUDE 'PRM_PAR'        ! PRIMDAT public constants

*  Arguments Given:

      INTEGER
     :  DIM1, DIM2,
     :  FDI,
     :  XLOW,
     :  YLOW,
     :  XUPP,
     :  YUPP

      REAL
     :  ARRAY( DIM1, DIM2 )

      LOGICAL
     :  OPENF

      CHARACTER*(*)
     :    FILNAM

*  Status:

      INTEGER  STATUS

*  Local Constants:

      INTEGER
     :    NCHLIN               ! maximum number of characters in an
                               ! output record
      PARAMETER ( NCHLIN = 132 )

*  Local Variables:

      INTEGER
     :    FD,                  ! file description
     :    NSTRIP,              ! current strip number
     :    STRIPS,              ! start x co-ord of current strip
     :    STRIPE,              ! end   "   "    "    "      "
     :    POINT,               ! pointer into output line
     :    NCHAR,               ! dummy to hold return from CHR_ calls
     :    I, J, JJ             ! counter variables

      REAL
     :    VALUE                ! current pixel value

      CHARACTER*(NCHLIN)
     :    BUFFER,              ! buffer to store output string
     :    OUTLIN*(NCHLIN-1)    ! output line for listing

      CHARACTER*10
     :    CURRX,               ! string to hold x index value
     :    CURRY,               !    "    "   "  y   "     "
     :    VALSTR               !    "    "   "  current pixel value

      CHARACTER*5
     :    CSTRIP               ! string to hold current strip number

*.
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( OPENF ) THEN

*       Attempt to obtain and open a file to output listing.  A null
*       input forces an exit, but not an error.

         CALL FIO_ASSOC( FILNAM, 'WRITE', 'FORTRAN', NCHLIN, FD,
     :                   STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
            GOTO 999
         END IF

*       Inform the user of the output filename.

         CALL MSG_SETC( 'FILNAM', FILNAM )
         CALL MSG_OUT( 'LOG', 'Listing to $^FILNAM', STATUS )
      ELSE
         FD = FDI
      END IF

*    Initialise the strip counter.

      NSTRIP  =  0

*    Loop round all the requested columns in strips of 10 numbers.

      DO  STRIPS  =  XLOW, XUPP, 10

*       Increment the strip counter by one.

         NSTRIP  =  NSTRIP + 1

*       Reset the output string.

         OUTLIN  =  ' '

*       Convert the strip number to a fixed-length string.

         CALL CHR_ITOC( NSTRIP, CSTRIP, NCHAR )

*       Stick this into the output line.

         OUTLIN( 1  : 18 )  =  '     Strip number '
         OUTLIN( 19 : 24 )  =  CSTRIP( 1 : 5 )

*       Clear the buffer.

         BUFFER = ' '

*       Output a line to the buffer indicating which strip we are on.

         WRITE( BUFFER, '(''1'', A)' ) OUTLIN
         CALL FIO_WRITE( FD, BUFFER, STATUS )

         BUFFER = ' '
         CALL FIO_WRITE( FD, BUFFER, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*          Given the x index of the strip start, work out where the
*          strip ends, it either being 10 numbers wide, or earlier
*          if the requested sub-array finishes first.

            STRIPE  =  MIN( STRIPS + 9, XUPP )

*          Reset the output line buffer.

            OUTLIN  =  ' '

*          Output the first line of the strip which is the running
*          x co-ordinate --- loop round all the x co-ordinates in the
*          current strip.

            DO  I  =  STRIPS, STRIPE

*             Convert the current index to a left-justified string of
*             fixed length 10 characters.

               CALL CHR_ITOC( I, CURRX, NCHAR )

*             Work out where in the line output string we are --- the
*             first ten characters are left blank.

               POINT  =  ( ( I - STRIPS ) * 11 ) + 11

*             Stick the current x index string in the right place in
*             the output string padding with one space to the right.

               OUTLIN( POINT : POINT + 9 )  =  CURRX( 1 : 10 )
               OUTLIN( POINT + 10 : POINT + 10 )  =  ' '
            END DO

*          Output the top line to the listing file with a blank line
*          following it.

            WRITE( BUFFER, '(A)' ) OUTLIN
            CALL FIO_WRITE( FD, BUFFER, STATUS )

            BUFFER = ' '
            CALL FIO_WRITE( FD, BUFFER, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*             Now we want to loop round all the selected lines in the
*             current strip.

               J = YLOW

               DO WHILE ( J .LE. YUPP .AND. STATUS .EQ. SAI__OK )

*                We really want to come from the top down, so work
*                out which line we are on from the top.

                  JJ  =  YUPP - J + YLOW

*                Reset the output line buffer.

                  OUTLIN  =  ' '

*                Convert the line number to a fixed-length string of
*                10 characters left justified.

                  CALL CHR_ITOC( JJ, CURRY, NCHAR )

*                Stick this into the output string at the beginning,
*                leaving three spaces --- if string started in first
*                character, then it would get interpreted as a page
*                throw if the first character was a 1.

                  OUTLIN( 3 : 10 )  =  CURRY( 1 : 7 )

*                Now loop round the pixels in this line.

                  DO  I  =  STRIPS, STRIPE

*                   Work out where in the output line we are, recalling
*                   that the output line starts with 10 characters
*                   containing the line index and padding spaces.

                     POINT  =  ( ( I - STRIPS ) * 11 ) + 11

*                   Check the current pixel is on the array.

                     IF ( I .LT. 1 .OR. I .GT. DIM1 .OR.
     :                    JJ .LT. 1 .OR. JJ .GT. DIM2 ) THEN

*                      Set up string to indicate this.

                        OUTLIN( POINT : POINT + 10 ) = '.......... '

*                   Check the pixel is invalid, and if it is then
*                   set up a string to indicate this.

                     ELSE IF ( ARRAY( I, JJ ) .EQ. VAL__BADR ) THEN
                        OUTLIN( POINT : POINT + 10 ) = '  INVALID  '

                     ELSE

*                      Get the value held in the array at the
*                      current pixel location.

                        VALUE  =  ARRAY( I, JJ )

*                      Convert this to a fixed length string of 10
*                      characters justified to the left with spaces.

                        CALL CHR_RTOC( VALUE, VALSTR, NCHAR )

*                      Stick the current pixel-value string in the
*                      right place in the output string padding
*                      with one space to the right.

                        OUTLIN( POINT : POINT + 9 )  =  VALSTR( 1 : 10 )
                        OUTLIN( POINT + 10 : POINT + 10 )  =  ' '

*                   End of if-pixel-not-on-array check.

                     END IF

*                End of loop round pixels in current line.

                  END DO

*                Output the current line line to the listing file.

                  WRITE( BUFFER, '(A)' ) OUTLIN

                  CALL FIO_WRITE( FD, BUFFER, STATUS )

                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_REP( 'ERR_LISTSB_WDATA',
     :                 'LISTSB: Error whilst writing data line',
     :                 STATUS )
                  END IF

*                Increment the loop counter.

                  J = J + 1

*             End of loop round lines in current strip.

               END DO

            ELSE
               CALL ERR_REP( 'ERR_LISTSB_WTOPL',
     :           'LISTSB: Error whilst writing the top line',
     :           STATUS )

            END IF

         ELSE
            CALL ERR_REP( 'ERR_LISTSB_WSTRIP',
     :        'LISTSB: Error whilst writing strip number', STATUS )

         END IF

*    End of loop round all the necessary strips of ten numbers.

      END DO

*    Close the file.

 999  CONTINUE
      IF ( OPENF ) CALL FIO_CANCL( FILNAM, STATUS )

*    return and end

      END
