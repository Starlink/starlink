      SUBROUTINE CAP_GSCOL (STATUS)
*+
*  Name:
*     CAP_GSCOL
*  Purpose:
*     Show summary details for all the columns in the catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSCOL (STATUS)
*  Description:
*     Show summary details for all the columns in the catalogue.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       Output the title.
*       Do while there are more columns to be output and the user wishes
*       to continue
*         Attempt to get a new column identifier.
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
*     27/4/94 (ACD): Original version.
*     13/3/95 (ACD): First stable version.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     6/11/97 (ACD): Added details for sorted columns.
*     1/7/99  (ACD): Changed the output buffer length to correspond to
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
     :  FI,          ! Column identifier.
     :  DTYPE,       ! Column: data type code.
     :  CSIZE,       !   "   : character size.
     :  DIMS,        !   "   : scalar/vector flag.
     :  SIZEA,       !   "   : size if the column is a vector.
     :  ORDER,       !   "   : order flag.
     :  LENGTH       ! Length of string (excl. trail. blanks).
      LOGICAL
     :  MORE         ! Flag; continue listing columns?
      CHARACTER
     :  BUFFER*(SGZ__SZOMS), ! Output buffer.
     :  NAME*(CAT__SZCMP),   ! Column: name.
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

            CALL CHR_PUTC ('Column', BUFFER, BUFPOS)

            BUFPOS = CAT__SZCMP + 2
            CALL CHR_PUTC ('Details', BUFFER, BUFPOS)

            CALL CAP_OUT (GUI__SGZ, ' ', BUFFER(1 : BUFPOS), STATUS)

*
*          Output details for the columns, as required.

            CI = CI__SGZ
            COUNT = 0
            MORE = .TRUE.

            DO WHILE (MORE)

*
*             Attempt to get a new column identifier; proceed if all
*             is ok and the identifier is not null.

               COUNT = COUNT + 1

               CALL CAT_TNDNT (CI, CAT__FITYP, COUNT, FI, STATUS)

               IF (STATUS .EQ. SAI__OK  .AND.  FI .NE. CAT__NOID) THEN

*
*                Get the details for the column.

                  CALL CAT_TIQAC (FI, 'NAME', NAME, STATUS)
                  CALL CAT_TIQAI (FI, 'DTYPE', DTYPE, STATUS)
                  CALL CAT_TIQAI (FI, 'CSIZE', CSIZE, STATUS)
                  CALL CAT_TIQAI (FI, 'DIMS', DIMS, STATUS)
                  CALL CAT_TIQAI (FI, 'SIZE', SIZEA, STATUS)
                  CALL CAT_TIQAC (FI, 'UNITS', UNITS, STATUS)
                  CALL CAT_TIQAI (FI, 'ORDER', ORDER, STATUS)
                  CALL CAT_TIQAC (FI, 'COMM', COMM, STATUS)

*
*                Assemble the output line.

                  BUFFER = ' '
                  BUFPOS = 0

*                ... column name.

                  IF (NAME .NE. ' ') THEN
                     LENGTH = CHR_LEN (NAME)
                     CALL CHR_PUTC (NAME(1 : LENGTH), BUFFER, BUFPOS)
                  END IF

*                ... data type.

                  BUFPOS = CAT__SZCMP + 2

                  CALL CAT_TYFMT (DTYPE, CSIZE, BUFFER, BUFPOS,
     :              STATUS)

*                ... size of vectors.

                  IF (DIMS .EQ. CAT__VECTR) THEN
                     CALL CHR_PUTC ('[', BUFFER, BUFPOS)
                     CALL CHR_PUTI (SIZEA, BUFFER, BUFPOS)
                     CALL CHR_PUTC (']', BUFFER, BUFPOS)
                  END IF

                  BUFPOS = MAX(BUFPOS, CAT__SZCMP + 10)

*                ... units.

                  IF (UNITS .NE. ' ') THEN
                     BUFPOS = BUFPOS + 1

                     LENGTH = CHR_LEN (UNITS)
                     CALL CHR_PUTC (UNITS(1 : LENGTH), BUFFER, BUFPOS)
                  END IF

*                ... order for sorted columns.

                  IF (ORDER .EQ. CAT__ASCND) THEN
                     CALL CHR_PUTC (' (ascending)', BUFFER, BUFPOS)
                  ELSE IF (ORDER .EQ. CAT__DSCND) THEN
                     CALL CHR_PUTC (' (descending)', BUFFER, BUFPOS)
                  END IF

                  BUFPOS = MAX(BUFPOS, CAT__SZCMP + 16)

*                ... comments.

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
            CALL ERR_REP ('CAP_GSCOL_ERR', 'Error getting summary '/
     :        /'details for column number ^COUNT.', STATUS)
         END IF

      END IF

      END
