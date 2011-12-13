      SUBROUTINE CAT5_DCANG (FNAME, TBLFMT, NANGLE, FANGLE, STATUS)
*+
*  Name:
*     CAT5_DCANG
*  Purpose:
*     Decode an angle in the TBLFMT specifier.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_DCANG (FNAME, TBLFMT; NANGLE; FANGLE; STATUS)
*  Description:
*     Decode an angle in the TBLFMT specifier.
*
*     Determine whether the specifier corresponds to an angle and
*     if so then decode its details.
*
*     The string containing the specifier should previously have
*     had leading blanks removed and been converted to upper case.
*  Arguments:
*     FNAME  =  CHARACTER*(*) (Given)
*        The name of the column (only used if an error message
*        is generated).
*     TBLFMT  =  CHARACTER*(*) (Given)
*        The table format specifier.
*     NANGLE  =  INTEGER (Given and Returned)
*        Running count of the columns of angles.
*     FANGLE  =  INTEGER (Returned)
*        Code indicating whether the format corresponds to an
*        angle or not (and if so, the units).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the table format corresponds to an angle then
*       Set the appropriate units.
*     else
*       Set the units to not an angle.
*     end if
*     If it is an angle then
*       Copy the remainder of the specifier to a work buffer.
*       Initialise the angle description.
*       Strip and enclosing '{' or '}'.
*       If the remaining string is not blank then
*         Try to read an integer number
*         If ok then (the angle has a simple format)
*           Set the angle format flag to 'simple'.
*           Set the width to the value decoded.
*         else (the angle has a complex format)
*           Replace the comma separators with spaces.
*           Determine the number of format elements (that is the
*           number of 'words' in the string).
*           Initialise the width to zero.
*           Initialise the number of specifiers to zero.
*           For each format element
*             If the format element is 'X' then
*               Determine the width.
*               Increment the total width.
*             else if the format element is 'A' then
*               If the separate sign flag is not yet set then
*                 Set the separate sign flag.
*                 Note the position.
*                 Note the format.
*                 Increment the total width.
*               else
*                 Set the bad format flag.
*               end if
*             else if the format element is 'I' then
*               If more sexagesimal subdivisions are available then
*                 Increment the number of sexagesimal subdivisions.
*                 Set the subdivision data type to 'I'.
*                 Copy the format.
*                 Determine the relative position.
*                 Increment the width.
*               else
*                 Set the bad format flag.
*               end if
*             else if the format element is 'F' then
*               If more sexagesimal subdivisions are available then
*                 Increment the number of sexagesimal subdivisions.
*                 Set the subdivision data type to 'I'.
*                 Copy the format.
*                 Determine the relative position.
*                 Increment the width.
*               else
*                 Set the bad format flag.
*               end if
*             else
*               Set the bad format flag.
*             end if
*           end for
*           If there were no subdivisions then
*             Set the bad format flag.
*           end if
*         end if
*       else
*         Set the flag to indicate a simple angle format.
*         Set the width to zero.
*       end if
*     end if
*     If the bad format flag is set then
*       Set the status.
*     end if
*     If the status is set then
*       Assemble and report an error message.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     28/7/98  (ACD): Original version.
*     2/8/98   (ACD): First stable version.
*     29/11/98 (ACD): Fixed omitted argument for CHR_LDBLK.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT5_ANG_CMN'      ! STL angles common block.
*  Arguments Given:
      CHARACTER
     :  FNAME*(*),
     :  TBLFMT*(*)
*  Arguments Given and Returned:
      INTEGER
     :  NANGLE
*  Arguments Returned:
      INTEGER
     :  FANGLE
*  Status:
      INTEGER
     :  STATUS             ! Global status.
*  Exernal References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER MXFELM
      PARAMETER (MXFELM = 10)
*  Local Variables:
      INTEGER
     :  ASTOP,    ! Stop position for angle type.
     :  LANGFM,   ! Length of ANGFMT (excl. trail. blanks).
     :  RSTAT,    ! Local status decoding INTEGER from string.
     :  WIDTH,    ! Width of angle.
     :  LOOP,     ! Loop index.
     :  LSTAT,    ! Local status decoding INTEGER from string.
     :  NFELM,    ! Number of format elements.
     :  START(MXFELM),  ! Start position of format elements.
     :  STOP(MXFELM),   ! Stop     "     "    "        "   .
     :  NSSUB     ! Number of sexagesimal subdivisions.
      INTEGER
     :  FWID,     ! Width of the current format element.
     :  LWRK,     ! Length of WRKBUF (excl. trail. blanks).
     :  DOTPOS,   ! Position of '.' in 'F' type format element.
     :  CURCHR,   ! Current character position.
     :  ERRLEN,   ! Length of ERRTXT (excl. trail. blanks).
     :  LFNAME,   !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LTBLFM    !   "    "  TBLFMT ( "  .   "  .   "   ).
      LOGICAL
     :  SSIGN,    ! Local separate sign flag.
     :  BADFMT    ! Bad table format flag.
      CHARACTER
     :  ANGFMT*(CAT__SZEXF),  ! Angular format specifier.
     :  FMTELM(MXFELM)*10,    ! List of format elements.
     :  WRKBUF*10,            ! Work buffer.
     :  ERRTXT*75             ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         BADFMT = .FALSE.

*
*       Check whether the format corresponds to an angle.

         IF (TBLFMT(1 : 7) .EQ. 'DEGREES') THEN
            FANGLE = CAT1__DEG
            ASTOP = 7
         ELSE IF (TBLFMT(1 : 5) .EQ. 'HOURS') THEN
            FANGLE = CAT1__HOUR
            ASTOP = 5

         ELSE IF (TBLFMT(1 : 5) .EQ. 'ANGLE') THEN
            FANGLE = CAT1__SDEG
            ASTOP = 5

         ELSE IF (TBLFMT(1 : 6) .EQ. 'ARCMIN') THEN
            FANGLE = CAT1__ARMIN
            ASTOP = 6

         ELSE IF (TBLFMT(1 : 6) .EQ. 'ARCSEC') THEN
            FANGLE = CAT1__ARSEC
            ASTOP = 6

         ELSE IF (TBLFMT(1 : 7) .EQ. 'TIMEMIN') THEN
            FANGLE = CAT1__TIMIN
            ASTOP = 7

         ELSE IF (TBLFMT(1 : 7) .EQ. 'TIMESEC') THEN
            FANGLE = CAT1__TISEC
            ASTOP = 7

         ELSE
            FANGLE = CAT1__NANGL

         END IF

*
*       Proceed if an angle has been found.

         IF (FANGLE .NE. CAT1__NANGL) THEN

*
*          Copy the remainder of the format specifier (that is,
*          the portion following the units) to a work buffer.
*          Then remove any leading spaces.

            ANGFMT = TBLFMT
            ANGFMT(1 : ASTOP) = ' '
            CALL CHR_LDBLK (ANGFMT)

*
*          Initialise the angle description.

            NANGLE = NANGLE + 1
            NANG__CAT5 = NANGLE

            ANCMX__CAT5(NANGLE) = .FALSE.
            ANUNI__CAT5(NANGLE) = FANGLE
            ANWID__CAT5(NANGLE) = 0

*
*          Strip any enclosing '{' or '}'.

            IF (ANGFMT(1 : 1) .EQ. '{') THEN
               ANGFMT(1 : 1) = ' '
            END IF

            IF (ANGFMT .NE. ' ') THEN
               LANGFM = CHR_LEN(ANGFMT)

               IF (ANGFMT(LANGFM : LANGFM) .EQ. '}') THEN
                  ANGFMT(LANGFM : LANGFM) = ' '
               END IF
            END IF

*
*          Check the resulting specifier is not blank.

            IF (ANGFMT .NE. ' ') THEN

*
*             Try to decode an INTEGER number from the specifier.
*             A simple INTEGER corresponds to the case where only
*             the width is specified.

               RSTAT = CAT__OK
               CALL CHR_CTOI (ANGFMT, WIDTH, RSTAT)
C              print3000, width, rstat
C3000          format(1x, 'Attempt to decode simple angle, ',
C    :           'width, rstat: ', i5 ,i10)
               IF (RSTAT .EQ. CAT__OK) THEN

*
*                An INTEGER was decoded successfully.  Consequently
*                the angle has a simple format; set the flag and the
*                width.

                  ANCMX__CAT5(NANGLE) = .FALSE.
                  ANWID__CAT5(NANGLE) = WIDTH

               ELSE

*
*                The format specifier must be complicated.  Attempt
*                to decode it.  First replace the comma (',')
*                separators with spaces.

                  LANGFM = CHR_LEN(ANGFMT)

                  DO LOOP = 1, LANGFM
                     IF (ANGFMT(LOOP : LOOP) .EQ. ',') THEN
                        ANGFMT(LOOP : LOOP) = ' '
                     END IF
                  END DO

*
*                Decompose the specifier into its constituent 'words',
*                that is, the separate format elements.

                  CALL CHR_DCWRD (ANGFMT, MXFELM, NFELM, START, STOP,
     :              FMTELM, LSTAT)
C                 print3001, nfelm, lstat
C3001             format(1x, 'attempt to decode into words ',
C    :              'nfelm, lstat: ', i5, i5)

*
*                Initialise the width and the number of sexagesimal
*                subdivisions.

                  WIDTH = 0
                  NSSUB = 0
                  SSIGN = .FALSE.

*
*                Examine each separate format element and determine
*                the corresponding sexagesimal subdivision details.

                  DO LOOP = 1, NFELM
C                    print7000, loop, fmtelm(loop)
C7000                format(3x, 'loop, fmtelm: ', i5, 1x, a )

*
*                   Check for an X specifier.  That is, an instruction
*                   to skip one or more spaces.

                     IF (ANGFMT(STOP(LOOP) : STOP(LOOP)) .EQ. 'X') THEN
                        IF (STOP(LOOP) .GT. START(LOOP)) THEN
                           WRKBUF = ' '
                           WRKBUF = ANGFMT(START(LOOP) : STOP(LOOP)-1)
                           RSTAT = CAT__OK
                           CALL CHR_CTOI (WRKBUF, FWID, RSTAT)
                           IF (RSTAT .EQ. CAT__OK) THEN
                              IF (FWID .GT. 0) THEN
                                 WIDTH = WIDTH + FWID
                              ELSE
                                 BADFMT = .TRUE.
                              END IF
                           ELSE
                              BADFMT = .TRUE.
                           END IF
                        ELSE

*
*                         A bad X specifier was given.

                           BADFMT = .TRUE.

                        END IF
C                       print2002, 'after X ', badfmt
C2002                   format(1x, a, 'badfmt: ', l5 )

*
*                   Check for a separate sign separator; that is, a
*                   Fortran 'A' format element.

                     ELSE IF (FMTELM(LOOP)(1 : 1) .EQ. 'A') THEN

*
*                      Check that the 'separate sign' flag has not
*                      already been set.  If not then extract the
*                      details.  If it has then set the bad format
*                      flag.

                        IF (.NOT. SSIGN) THEN
                           SSIGN = .TRUE.

                           ANSGN__CAT5(NANGLE) = .TRUE.
                           ANPSG__CAT5(NANGLE) = WIDTH + 1
                           ANFSG__CAT5(NANGLE) = FMTELM(LOOP)

                           WRKBUF = ' '
                           WRKBUF = FMTELM(LOOP)
                           WRKBUF(1 : 1) = ' '
                           CALL CHR_LDBLK (WRKBUF)
                           RSTAT = CAT__OK
                           CALL CHR_CTOI (WRKBUF, FWID, RSTAT)
                           IF (RSTAT .EQ. CAT__OK) THEN
                              IF (FWID .GT. 0) THEN
                                 WIDTH = WIDTH + FWID
                                 ANWSG__CAT5(NANGLE) = FWID
                              ELSE
                                 BADFMT = .TRUE.
                              END IF
                           ELSE
                              BADFMT = .TRUE.
                           END IF

                        ELSE

*
*                         The separate sign flag is already set.

                           BADFMT = .TRUE.

                        END IF
C                       print2002, 'after A ', badfmt

*
*                   Check for an 'I' format specifier.  Such a
*                   specifier corresponds to a sexagesimal subdivision.

                     ELSE IF (FMTELM(LOOP)(1 : 1) .EQ. 'I') THEN

*
*                      Check that all three possible subdivisions have
*                      not already been given.

                        IF (NSSUB .LE. 2) THEN

*
*                         Increment to the next subdivision.

                           NSSUB = NSSUB + 1

*
*                         Set the details.

                           ANTYP__CAT5(NANGLE, NSSUB) = CAT__TYPEI
                           ANFMT__CAT5(NANGLE, NSSUB) = FMTELM(LOOP)
                           ANPOS__CAT5(NANGLE, NSSUB) = WIDTH + 1

*
*                         Calculate the width.

                           WRKBUF = ' '
                           WRKBUF = FMTELM(LOOP)
                           WRKBUF(1 : 1) = ' '
                           CALL CHR_LDBLK (WRKBUF)

C                          print4011, wrkbuf
c4011                      format(1x, 'wrkbuf : ', a, ':' )

                           RSTAT = CAT__OK
                           CALL CHR_CTOI (WRKBUF, FWID, RSTAT)
                           IF (RSTAT .EQ. CAT__OK) THEN
                              IF (FWID .GT. 0) THEN
                                 ANSWD__CAT5(NANGLE, NSSUB) = FWID
                                 WIDTH = WIDTH + FWID
                              ELSE
                                 BADFMT = .TRUE.
                              END IF
                           ELSE
                              BADFMT = .TRUE.
                           END IF

                        ELSE

*
*                         Too many subdivisions (ie. more than units,
*                         minutes and seconds).

                           BADFMT = .TRUE.

                        END IF
C                       print2002, 'after I ', badfmt

*
*                   Check for an 'F' format specifier.  Again such a
*                   specifier corresponds to a sexagesimal subdivision.

                     ELSE IF (FMTELM(LOOP)(1 : 1) .EQ. 'F') THEN

*
*                      Check that all three possible subdivisions have
*                      not already been given.

                        IF (NSSUB .LE. 2) THEN

*
*                         Increment to the next subdivision.

                           NSSUB = NSSUB + 1

*
*                         Set the details.

                           ANTYP__CAT5(NANGLE, NSSUB) = CAT__TYPED
                           ANFMT__CAT5(NANGLE, NSSUB) = FMTELM(LOOP)
                           ANPOS__CAT5(NANGLE, NSSUB) = WIDTH + 1

*
*                         Calculate the width.  An 'F' format specifier
*                         will be something like 'F6.2'.  Remove the
*                         'F' and extract the number before the decimal
*                         point.

                           WRKBUF = ' '
                           WRKBUF = FMTELM(LOOP)
                           WRKBUF(1 : 1) = ' '
                           CALL CHR_LDBLK (WRKBUF)

                           IF (WRKBUF .NE. ' ') THEN
                              LWRK = CHR_LEN(WRKBUF)
                           ELSE
                              LWRK = 1
                           END IF

                           DOTPOS = 0

                           DO CURCHR = 1, LWRK
                              IF (WRKBUF(CURCHR : CURCHR) .EQ. '.') THEN
                                 DOTPOS = CURCHR
                              END IF
                           END DO

                           IF (DOTPOS .GT. 1) THEN
                              DOTPOS = DOTPOS - 1
                           ELSE
                              DOTPOS = LWRK
                           END IF

                           IF (DOTPOS .GT. 0) THEN
                              RSTAT = CAT__OK
                              CALL CHR_CTOI (WRKBUF(1 : DOTPOS),
     :                          FWID, RSTAT)
                              IF (RSTAT .EQ. CAT__OK) THEN
                                 IF (FWID .GT. 0) THEN
                                    ANSWD__CAT5(NANGLE, NSSUB) = FWID
                                    WIDTH = WIDTH + FWID
                                 ELSE
                                    BADFMT = .TRUE.
                                 END IF
                              ELSE
                                 BADFMT = .TRUE.
                              END IF

                           ELSE

*
*                            Bad format specifier; missing dot.

                              BADFMT = .TRUE.

                           END IF

                        ELSE

*
*                         Too many subdivisions (ie. more than units,
*                         minutes and seconds).

                           BADFMT = .TRUE.

                        END IF
C                       print2002, 'after F ', badfmt

                     ELSE

*
*                      Bad (ie. unrecognisable) specifier.

                        BADFMT = .TRUE.
C                       print2003
C2003                   format(1x, 'Bad (ie. unrecognisable) ',
C    :                    'specifier.')

                     END IF

                  END DO

*
*                Set the total width of the angle and the number
*                of sexagesimal  subdivisions.

                  ANCMX__CAT5(NANGLE) = .TRUE.
                  ANWID__CAT5(NANGLE) = WIDTH
                  ANNSG__CAT5(NANGLE) = NSSUB

*
*                Check that there was at least one sexagesimal
*                subdivision (corresponding to the primary units).

                  IF (NSSUB .LE. 0) THEN
                     BADFMT = .TRUE.
                  END IF

               END IF

            ELSE

*
*             The angle corresponds to a simple specifier for a
*             free-format table (that is, no width was specified).

               ANCMX__CAT5(NANGLE) = .FALSE.
               ANWID__CAT5(NANGLE) = 0

            END IF
         END IF

*
*       If the bad format flag is set then set the status.

         IF (BADFMT) THEN
            STATUS = CAT__INVDS
         END IF

*
*       If the status has been set then assemble and report an
*       error message.

         IF (STATUS .NE. CAT__OK) THEN
C           print6000, fname, tblfmt
C6000       format(3x, 'fname :', a, ':' /
C    :        3x, 'tblfmt :', a, ':' / )

            ERRLEN = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('The table format for column ', ERRTXT,
     :        ERRLEN)

            IF (FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN(FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), ERRTXT, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CHR_PUTC (' is illegal: ', ERRTXT, ERRLEN)

            IF (TBLFMT .NE. ' ') THEN
               LTBLFM = CHR_LEN(TBLFMT)
               CALL CHR_PUTC (TBLFMT(1 : LTBLFM), ERRTXT, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CAT1_ERREP ('CAT5_DCANG_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
