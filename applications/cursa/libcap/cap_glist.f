      SUBROUTINE CAP_GLIST (STATUS)
*+
*  Name:
*     CAP_GLIST
*  Purpose:
*     List the current selection to the screen.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GLIST (STATUS)
*  Description:
*     List the current selection to the screen, showing the currently
*     chosen components.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       Assemble the first line of the title.
*       Determine the number of components which fit on a single line.
*       Load the component names into the first line of the title.
*       Load the units of the components in the first line of the title.
*       Output the title.
*       Determine the number of rows in the selection.
*       Compute the first and last rows to be listed.
*       Initialise to the first row to be listed.
*       Do while more rows are to be listed.
*         If a sequence number is required then
*           Append a sequence number to the output buffer.
*         end if
*         Get the next row.
*         For each component to be listed
*           Get the formatted value.
*           Append the formatted value to the output line.
*         end for
*         Output the line.
*         If the last required line has been output then
*           Set the termination flag.
*         else
*           Increment the row number by +1 to get the next row.
*         end if
*         If the status is not ok then
*           Report an error.
*           Set the termination flag.
*         end if
*       end do
*       Update the current row.
*     else
*       Report message: catalogue not open.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/4/94  (ACD): Original version.
*     23/10/94 (ACD): First stable version.
*     6/3/95   (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     9/3/95   (ACD): Made the computation of the space allocated for
*        each column entirely local to this routine.
*     28/3/95  (ACD): Added check for whether any of the details of the
*        display line have changed prior to issuing message about the
*        line being truncated.
*     26/11/96 (ACD): Incorporated changes for Linux.
*     4/12/96  (ACD): Fix in calls to CAP_OUT to allow long lines to
*        be output.
*     1/7/99   (ACD): Explicitly defined the length of output lines to
*        correspond to the ADAM message system.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'SGZ_PAR'           ! catview parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! catview common block.
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CMPS,    ! Number of components which fit on a single line.
     :  CSTART(SGZ__MXCMP), ! Start position of each component.
     :  CWIDTH(SGZ__MXCMP), ! Width of each component.
     :  BUFPOS,  ! Current position in BUFFER.
     :  LOOP,    ! Loop index.
     :  LNAME,   ! Length of component name  (excl. trail. blanks).
     :  LUNIT,   !   "    "      "     units ( "  .   "  .   "   ).
     :  START,   ! Start position in BUFFER.
     :  STOP,    ! Stop     "     "    "   .
     :  ROWS,    ! Number of rows in the selection.
     :  ROW,     ! Current row number.
     :  FIRST,   ! First row to be listed.
     :  LAST,    ! Last row to be listed.
     :  SI       ! Selection identifier.
      INTEGER
     :  LFVAL,   ! Length of FVALUE     (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME__SGZ ( "  .   "  .   "   ).
     :  LCRIT    !   "    "  CRIT       ( "  .   "  .   "   ).
      LOGICAL
     :  MORE,    ! Flag; list more rows.
     :  NULFLG   ! Null value flag for the current field.
      CHARACTER
     :  UNITS(SGZ__MXCMP)*(CAT__SZUNI),  ! Units for each component.
     :  BUFFER*(SGZ__SZOMS),   ! Output buffer for the current row.
     :  TITLE1*(SGZ__SZOMS),   ! First  title for the listing.
     :  TITLE2*(SGZ__SZOMS),   ! Second   "    "   "     "   .
     :  TITLE3*(SGZ__SZOMS),   ! Third    "    "   "     "   .
     :  FVALUE*(CAT__SZVAL),   ! Formatted value for the current field.
     :  CRIT*(CAT__SZEXP),     ! Selection expression.
     :  RTFMT*80               ! Run-time format string.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Assemble the first line of the title.

            BUFFER = ' '
            BUFPOS = 0

            CALL CHR_PUTC ('Catalogue ', BUFFER, BUFPOS)

            IF (CNAME__SGZ .NE. ' ') THEN
               LCNAME = CHR_LEN(CNAME__SGZ)
               CALL CHR_PUTC (CNAME__SGZ(1 : LCNAME), BUFFER, BUFPOS)
            ELSE
               CALL CHR_PUTC ('<blank>', BUFFER, BUFPOS)
            END IF

            CALL CHR_PUTC (', selection ', BUFFER, BUFPOS)

            CALL CHR_PUTI (CSEL__SGZ, BUFFER, BUFPOS)
            CALL CHR_PUTC (': ', BUFFER, BUFPOS)

            CRIT = CRIT__SGZ(CSEL__SGZ)

            IF (CRIT .NE. ' ') THEN
               LCRIT = CHR_LEN(CRIT)
               CALL CHR_PUTC (CRIT(1 : LCRIT), BUFFER, BUFPOS)
            ELSE
               CALL CHR_PUTC ('<none>', BUFFER, BUFPOS)
            END IF

            TITLE1 = BUFFER

*
*          Determine the number of components which fit on a single line.

            CMPS = CMPS__SGZ

C           print3000, cmps, swid__sgz
C3000       format(1x, 'before CAP_GCDET, cmps, swid__sgz: ',
C    :        i5, i5)
            CALL CAP_GCDET (SWID__SGZ, RUN__SGZ, CMPS, CMPNM__SGZ,
     :        CMPID__SGZ, UNITS, CSTART, CWIDTH, STATUS)
C           print3001, CMPS
C3001       format(1x, 'after CAP_GCDET, cmps: ', i5)

            IF (CMPS .LT. CMPS__SGZ) THEN

*
*             Check whether any of the details of the list of columns
*             have changed prior to issuing the warning about it being
*             truncated, in order to ensure that the message is only
*             issued once.

               IF (CMPCG__SGZ) THEN
                  CALL MSG_SETI ('CMPS', CMPS)
                  CALL CAP_WARN (GUI__SGZ, ' ', 'There is sufficient '/
     :              /'space to list only the first ^CMPS columns.',
     :              STATUS)

                  CMPCG__SGZ = .FALSE.
               END IF
            END IF


*
*          Load the component names into the first line of the title.

            BUFFER = ' '
            BUFPOS = 0

            IF (RUN__SGZ .NE. 0) THEN
               BUFFER(RUN__SGZ-2 : RUN__SGZ) = 'Seq'
               BUFPOS = RUN__SGZ + SGZ__SPACE
            END IF

            DO LOOP = 1, CMPS
               IF (CMPNM__SGZ(LOOP) .NE. ' ') THEN
                  LNAME = CHR_LEN (CMPNM__SGZ(LOOP) )

                  STOP = CSTART(LOOP) + CWIDTH(LOOP) - 1
                  START = STOP + 1 - LNAME

                  BUFFER(START : STOP) = CMPNM__SGZ(LOOP)(1 : LNAME)
               END IF
            END DO

            TITLE2 = BUFFER

*
*          Load the units of the components in the first line of the
*          title.

            BUFFER = ' '
            BUFPOS = 0

            IF (RUN__SGZ .NE. 0) THEN
               BUFFER(RUN__SGZ-2 : RUN__SGZ) = 'num'
               BUFPOS = RUN__SGZ + SGZ__SPACE
            END IF

            DO LOOP = 1, CMPS
               IF (UNITS(LOOP) .NE. ' ') THEN
                  LUNIT = CHR_LEN (UNITS(LOOP) )

                  STOP = CSTART(LOOP) + CWIDTH(LOOP) - 1
                  START = STOP + 1 - LUNIT

                  BUFFER(START : STOP) = UNITS(LOOP)(1 : LUNIT)
               END IF
            END DO

            TITLE3 = BUFFER

*
*          Output the title.

            CALL CAP_OUT (GUI__SGZ, 'none', TITLE1, STATUS)
            CALL CAP_OUT (GUI__SGZ, 'none', TITLE2, STATUS)
            CALL CAP_OUT (GUI__SGZ, 'none', TITLE3, STATUS)

*
*          Determine the number of rows in the current selection.

            SI = SELID__SGZ(CSEL__SGZ)

            CALL CAT_TROWS (SI, ROWS, STATUS)

*
*          Compute the first and last rows to be listed.

            FIRST = CROW__SGZ

            IF (NLIST__SGZ  .NE.  SGZ__LALL) THEN
               LAST = FIRST + NLIST__SGZ - 1
               LAST = MIN(ROWS, LAST)

               IF (LAST .EQ. ROWS) THEN
                  FIRST = LAST + 1 - NLIST__SGZ
                  FIRST = MAX(1, FIRST)
               END IF
            ELSE
               LAST = ROWS
            END IF

*
*          Initialise so listing starts with the first row to be listed.

            ROW = FIRST

            MORE = .TRUE.

*
*          List the rows as required.

            DO WHILE (MORE)
               BUFFER = ' '
               BUFPOS = 0

*
*             If a sequence number is required then insert one.

               IF (RUN__SGZ .GT. 0) THEN
                  WRITE (RTFMT, '(''(I'', I4, '')'')' ) RUN__SGZ
                  WRITE (BUFFER(1 : RUN__SGZ), RTFMT ) ROW
C                 WRITE (BUFFER(1 : RUN__SGZ), '(I<RUN__SGZ>)' ) ROW
               END IF

*
*             Get the next row.

               CALL CAT_RGET (SI, ROW, STATUS)

*
*             For each component to be listed get the formatted value
*             and append it to the output line.

               DO LOOP = 1, CMPS
                  CALL CAT_EGT0F (CMPID__SGZ(LOOP), FVALUE, NULFLG,
     :              STATUS)

                  IF (FVALUE .NE. ' ') THEN
                     LFVAL = CHR_LEN (FVALUE)
                     LFVAL = MIN(LFVAL, CWIDTH(LOOP) )

                     STOP = CSTART(LOOP) + CWIDTH(LOOP) - 1
                     START = STOP + 1 - LFVAL

                     BUFFER(START : STOP) = FVALUE(1 : LFVAL)
                  END IF
               END DO

               IF (BUFFER .NE. ' ') THEN
                  BUFPOS = CHR_LEN (BUFFER)
               ELSE
                  BUFPOS = 1
               END IF

*
*             Output the line.

               CALL CAP_OUT (GUI__SGZ, 'none', BUFFER(1 : BUFPOS),
     :           STATUS)

*
*             If the last requested line has been output then set the
*             termination flag.  Otherwise increment to the next row.

               IF (ROW .GE. LAST) THEN
                  MORE = .FALSE.
               ELSE
                  ROW = ROW + 1
               END IF

*
*             If a bad status has been raised then report an error and
*             set the termination flag.

               IF (STATUS .NE. SAI__OK) THEN
                  CALL MSG_SETI ('ROW', ROW)
                  CALL ERR_REP ('CAP_GLIST_ERR', 'Error outputing row '/
     :              /'^ROW.', STATUS)

                  MORE = .FALSE.
               END IF
            END DO

*
*          Update the current row.

            CROW__SGZ = LAST

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
