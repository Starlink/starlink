      SUBROUTINE CAP_GSSEL (STATUS)
*+
*  Name:
*     CAP_GSSEL
*  Purpose:
*     Show details of all the selections which currently exist.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSSEL (STATUS)
*  Description:
*     Show details of all the selections which currently exist.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       Assemble the title.
*       Output the title.
*       Initialise the selections.
*       Do while more selections are to be listed.
*         Increment to the next selection.
*         Get the details of the selection and assemble an output line.
*         Output the line.
*         If all the selections have been output then
*           Set the termination flag.
*         end if
*         If the status is not ok then
*           Report an error.
*           Set the termination flag.
*         end if
*       end do
*     else
*       Report warning: catalogue not open.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/4/94 (ACD): Original version.
*     27/9/94 (ACD): First stable version.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     1/7/99  (ACD): Changed the output buffer lengths to correspond to
*       the ADAM message system.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'       ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'       ! External CAT constants.
      INCLUDE 'SGZ_PAR'       ! catview parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'       ! catview common block.
*  Status:
      INTEGER STATUS          ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  TITLE1*(SGZ__SZOMS),  ! First  title for listing.
     :  TITLE2*(SGZ__SZOMS),  ! Second   "    "     "   .
     :  BUFFER*(SGZ__SZOMS),  ! Output buffer.
     :  CRIT*(CAT__SZEXP)     ! Selection criteria for current selection.
      INTEGER
     :  BUFPOS,   ! Current position in BUFFER.
     :  CSEL,     ! Number of current selection.
     :  LSTAT,    ! Local status.
     :  ROWS,     ! Number of rows in the selection.
     :  LCRIT     ! Length of CRIT (excl. trail. blanks).
      LOGICAL
     :  MORE      ! Flag; list more selections.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Assemble the title.

            TITLE1 = '* - (on the left hand side) denotes the '/
     :        /'current selection.'

            BUFFER = ' '

            BUFFER(2 : 4) = 'Sel'
            BUFFER(6 : 9) = 'Base'
            BUFFER(15 : 18) = 'Rows'
            BUFFER(21 : 28) = 'Criteria'

            TITLE2 = BUFFER(1 : 28)

*
*          Output the title.

            CALL CAP_OUT (GUI__SGZ, ' ', TITLE1, STATUS)
            CALL CAP_OUT (GUI__SGZ, ' ', TITLE2, STATUS)

*
*          Initialise prior to listing the selections.

            CSEL = 0

*
*          List the selections.

            MORE = .TRUE.

            DO WHILE (MORE)

*
*             Increment to the next selection.

               CSEL = CSEL + 1

*
*            Get the details of the selection and assemble an output
*            line.

              BUFFER = ' '

              IF (CSEL .EQ. CSEL__SGZ) THEN
                 BUFFER(1 : 1) = '*'
              ELSE
                 BUFFER(1 : 1) = ' '
              END IF

              WRITE(BUFFER(2 : 4), '(I3)', IOSTAT=LSTAT) CSEL

              IF (CSEL .NE. 1) THEN
                 WRITE(BUFFER(6 : 9), '(I4)', IOSTAT=LSTAT)
     :             SELBS__SGZ(CSEL)
              ELSE
                 BUFFER(9 : 9) = '-'
              END IF

              CALL CAT_TROWS (SELID__SGZ(CSEL), ROWS, STATUS)

              WRITE(BUFFER(11 : 18), '(I8)', IOSTAT=LSTAT) ROWS

              BUFPOS = 20

              CRIT = CRIT__SGZ(CSEL)

              IF (CRIT .NE. ' ') THEN
                 LCRIT = CHR_LEN (CRIT)
                 CALL CHR_PUTC (CRIT(1 : LCRIT), BUFFER, BUFPOS)
              END IF

*
*            Trim the length of the line to the maximum permitted.

              BUFPOS = MIN(BUFPOS, SWID__SGZ)

*
*            Output the line.

              CALL CAP_OUT (GUI__SGZ, ' ', BUFFER(1 : BUFPOS), STATUS)

*
*            If all the selections have been output then set the
*            termination flag.

              IF (CSEL .GE. SELS__SGZ) THEN
                 MORE = .FALSE.
              END IF

*
*            If a bad status has been raised then report an error and
*            set the termination flag.

              IF (STATUS .NE. SAI__OK) THEN
                 CALL MSG_SETI ('CSEL', CSEL)
                 CALL ERR_REP ('CAP_GSSEL_SERR', 'Error listing '/
     :             /'selection ^CSEL.', STATUS)

                 MORE = .FALSE.
              END IF
           END DO

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
