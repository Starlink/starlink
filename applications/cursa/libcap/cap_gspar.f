      SUBROUTINE CAP_GSPAR (STATUS)
*+
*  Name:
*     CAP_GSPAR
*  Purpose:
*     Show summary details for all the parameters in the catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSPAR (STATUS)
*  Description:
*     Show summary details for all the parameters in the catalogue.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       Output the title.
*       Do while there are more parameters to be output and the user
*       wishes to continue
*         Attempt to get a new parameter identifier.
*         If the status is ok and the identifier is not null then
*           Get the details for the column and assemble it into an
*           output line.
*           Output the line.
*         else
*           Set the termination flag.
*         end if
*         If an error status was raised then
*           Set the termination flag.
*         end if
*       end do
*     else
*       Report warning: catalogue not open.
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/4/94  (ACD): Original version.
*     23/10/94 (ACD): First stable version.
*     11/4/95  (ACD): Changed the name of the null identifier.
*     1/7/99   (ACD): Changed the output buffer length to correspond to
*       the ADAM message system.
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
      INTEGER STATUS              ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CI,          ! Catalogue identifier.
     :  BUFPOS,      ! Current position in BUFFER.
     :  COUNT,       ! Number of the current column.
     :  QI,          ! Parameter identifier.
     :  LENGTH       ! Length of string (excl. trail. blanks).
      LOGICAL
     :  MORE         ! Flag; continue listing columns?
      CHARACTER
     :  BUFFER*(SGZ__SZOMS), ! Output buffer.
     :  NAME*(CAT__SZCMP),   ! Column: name.
     :  VALUE*(CAT__SZVAL),  !   "   : value.
     :  UNITS*(CAT__SZUNI),  !   "   : units.
     :  COMM*(CAT__SZCOM)    !   "   : comments.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Output the title.

            BUFPOS = 0
            BUFFER = ' '

            BUFFER(1 : 9) = 'Parameter'
            BUFFER(21 : 27) = 'Details'

            CALL CAP_OUT (GUI__SGZ, ' ', BUFFER(1 : 27), STATUS)

*
*          Output details for the columns, as required.

            CI = CI__SGZ
            COUNT = 0
            MORE = .TRUE.

            DO WHILE (MORE)

*
*             Attempt to get a new parameter identifier; proceed if all
*             is ok and the identifier is not null.

               COUNT = COUNT + 1

               CALL CAT_TNDNT (CI, CAT__QITYP, COUNT, QI, STATUS)

               IF (STATUS .EQ. SAI__OK  .AND.  QI .NE. CAT__NOID) THEN

*
*                Get the details for the parameter.

                  CALL CAT_TIQAC (QI, 'NAME', NAME, STATUS)
                  CALL CAT_TIQAC (QI, 'VALUE', VALUE, STATUS)
                  CALL CAT_TIQAC (QI, 'UNITS', UNITS, STATUS)
                  CALL CAT_TIQAC (QI, 'COMM', COMM, STATUS)

*
*                Assemble the output line.

                  BUFFER = ' '
                  BUFPOS = 0

                  IF (NAME .NE. ' ') THEN
                     LENGTH = CHR_LEN (NAME)
                     CALL CHR_PUTC (NAME(1 : LENGTH), BUFFER, BUFPOS)
                  END IF

                  CALL CHR_PUTC (' = ', BUFFER, BUFPOS)

                  IF (VALUE .NE. ' ') THEN
                     LENGTH = CHR_LEN (VALUE)
                     CALL CHR_PUTC (VALUE(1 : LENGTH), BUFFER, BUFPOS)
                  END IF

                  IF (UNITS .NE. ' ') THEN
                     CALL CHR_PUTC (' (', BUFFER, BUFPOS)

                     LENGTH = CHR_LEN (UNITS)
                     CALL CHR_PUTC (UNITS(1 : LENGTH), BUFFER, BUFPOS)

                     CALL CHR_PUTC (')', BUFFER, BUFPOS)
                  END IF

                  BUFPOS = MAX(BUFPOS, CAT__SZCMP + 2)

                  IF (COMM .NE. ' ') THEN
                     CALL CHR_PUTC (' :', BUFFER, BUFPOS)

                     LENGTH = CHR_LEN (COMM)
                     CALL CHR_PUTC (COMM(1 : LENGTH), BUFFER, BUFPOS)
                  END IF

*
*                Trim the length of the line to the maximum permitted.

                  BUFPOS = MIN(BUFPOS, SWID__SGZ)

*
*                Output the line.

                  CALL CAP_OUT (GUI__SGZ, ' ', BUFFER(1 : BUFPOS),
     :              STATUS)

               ELSE

*
*                Either a bad status was raised or the null identifier
*                was returned; set the termination flag.

                  MORE = .FALSE.
               END IF

*
*             Check if any error status has been raised and if so then
*             set the termination flag.

               IF (STATUS .NE. SAI__OK) THEN
                  MORE = .FALSE.
               END IF
            END DO

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETI ('COUNT', COUNT)
            CALL ERR_REP ('CAP_GSPAR_ERR', 'Error getting summary '/
     :        /'details for parameter number ^COUNT.', STATUS)
         END IF

      END IF

      END
