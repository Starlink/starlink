      SUBROUTINE CAP_GFCOL (FUNIT, STATUS)
*+
*  Name:
*     CAP_GFCOL
*  Purpose:
*     Write summary details for all the columns to the text file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GFCOL (FUNIT, STATUS)
*  Description:
*     Write summary details for all the columns to the text file.
*  Arguments:
*     FUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the text file.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Output the title.
*     Do while there are more columns to be output
*       Attempt to get a new column identifier.
*       If the status is ok and the identifier is not null then
*         Get the details for the column and assemble it into an
*         output line.
*         Output the line.
*       else
*         Set the termination flag.
*       end if
*       If an error status was raised then
*         Set the termination flag.
*       end if
*     end do
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     1/6/94  (ACD): Original version (based on CAP_GSCOL).
*     11/4/95 (ACD): Changed the name of the null identifier.
*     1/7/99  (ACD): Changed the output buffer length to correspond to
*       a print file.
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
*  Arguments Given:
      INTEGER
     :  FUNIT
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
     :  LENGTH       ! Length of string (excl. trail. blanks).
      LOGICAL
     :  MORE         ! Flag; continue listing columns?
      CHARACTER
     :  BUFFER*(SGZ__SZOPR), ! Output buffer.
     :  NAME*(CAT__SZCMP),   ! Column: name.
     :  UNITS*(CAT__SZUNI),  !   "   : units.
     :  COMM*(CAT__SZCOM)    !   "   : comments.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Output the title.

         BUFPOS = 0
         BUFFER = ' '

         CALL CHR_PUTC ('Column', BUFFER, BUFPOS)

         BUFPOS = CAT__SZCMP + 2
         CALL CHR_PUTC ('Description', BUFFER, BUFPOS)

         FTITL__SGZ(2) = BUFFER
         FNTTL__SGZ = 2
         FLNCT__SGZ = 0

*
*       Output details for the columns, as required.

         CI = CI__SGZ
         COUNT = 0
         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Attempt to get a new column identifier; proceed if all
*          is ok and the identifier is not null.

            COUNT = COUNT + 1

            CALL CAT_TNDNT (CI, CAT__FITYP, COUNT, FI, STATUS)

            IF (STATUS .EQ. SAI__OK  .AND.  FI .NE. CAT__NOID) THEN

*
*             Get the details for the column.

               CALL CAT_TIQAC (FI, 'NAME', NAME, STATUS)
               CALL CAT_TIQAI (FI, 'DTYPE', DTYPE, STATUS)
               CALL CAT_TIQAI (FI, 'CSIZE', CSIZE, STATUS)
               CALL CAT_TIQAI (FI, 'DIMS', DIMS, STATUS)
               CALL CAT_TIQAI (FI, 'SIZE', SIZEA, STATUS)
               CALL CAT_TIQAC (FI, 'UNITS', UNITS, STATUS)
               CALL CAT_TIQAC (FI, 'COMM', COMM, STATUS)

*
*             Assemble the output line.

               BUFFER = ' '
               BUFPOS = 0

               IF (NAME .NE. ' ') THEN
                  LENGTH = CHR_LEN (NAME)
                  CALL CHR_PUTC (NAME(1 : LENGTH), BUFFER, BUFPOS)
               END IF

               BUFPOS = CAT__SZCMP + 2

               CALL CAT_TYFMT (DTYPE, CSIZE, BUFFER, BUFPOS,
     :           STATUS)

               IF (DIMS .EQ. CAT__VECTR) THEN
                  CALL CHR_PUTC ('[', BUFFER, BUFPOS)
                  CALL CHR_PUTI (SIZEA, BUFFER, BUFPOS)
                  CALL CHR_PUTC (']', BUFFER, BUFPOS)
               END IF

               IF (UNITS .NE. ' ') THEN
                  BUFPOS = BUFPOS + 1

                  LENGTH = CHR_LEN (UNITS)
                  CALL CHR_PUTC (UNITS(1 : LENGTH), BUFFER, BUFPOS)
               END IF

               IF (COMM .NE. ' ') THEN
                  CALL CHR_PUTC (' :', BUFFER, BUFPOS)

                  LENGTH = CHR_LEN (COMM)
                  CALL CHR_PUTC (COMM(1 : LENGTH), BUFFER, BUFPOS)
               END IF

*
*             Trim the length of the line to the maximum permitted.

               BUFPOS = MIN(BUFPOS, FWID__SGZ)

*
*             Output the line.

               CALL CAP_GFOUT (FUNIT, BUFFER(1 : BUFPOS), STATUS)

            ELSE

*
*             Either a bad status was raised or the null identifier
*             was returned; set the termination flag.

               MORE = .FALSE.
            END IF

*
*          Check if any error status has been raised and if so then
*          set the termination flag.

            IF (STATUS .NE. SAI__OK) THEN
               MORE = .FALSE.
            END IF
         END DO

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETI ('COUNT', COUNT)
            CALL ERR_REP ('CAP_GFCOL_ERR', 'Error getting summary '/
     :        /'details for column number ^COUNT.', STATUS)
         END IF

      END IF

      END
