      SUBROUTINE CAP_GSCSH (STATUS)
*+
*  Name:
*     CAP_GSCSH
*  Purpose:
*     Show the range of a scatterplot.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSCSH (STATUS)
*  Description:
*     Show the range of a scatterplot.  Note that the values are
*     displayed without any annotation.  A semi-colon (`;') is
*     prepended to the start of the string to avoid confusing tcl
*     with negative numbers.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is a catalogue open then
*       If there is a plot open then
*         Assemble and write out the range.
*       else
*         Report warning; no plot open.
*       end if
*     else
*       Report warning; no open catalogue.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     19/11/98 (ACD): Original version.
*     16/9/99  (ACD): Minor aesthetic tweaks.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'           ! catview parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! catview common block.
      INCLUDE 'SPLOT_CMN'         ! catview scatterplot common block.
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  BUFFER*75  ! Output buffer.
      INTEGER
     :  BUFLEN,    ! Length of BUFFER (excl. trail. blanks).
     :  LSTAT      ! Local internal Fortran I/O status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Check that there is a plot open.

            IF (OPEN__SPLOT) THEN

*
*             Assemble and output the four extrema of the plote range.

               BUFFER = ' '
               WRITE(BUFFER, 2000, IOSTAT=LSTAT) XMIN__SPLOT
 2000          FORMAT(1PE12.4)
               IF (BUFFER .EQ. ' ') THEN
                  BUFFER = '0.0E0'
               END IF
               CALL CHR_LDBLK (BUFFER)
               BUFLEN = CHR_LEN(BUFFER)
               CALL CAP_OUT (GUI__SGZ, ' ', ';' // BUFFER(1 : BUFLEN),
     :           STATUS)

               BUFFER = ' '
               WRITE(BUFFER, 2000, IOSTAT=LSTAT) XMAX__SPLOT
               IF (BUFFER .EQ. ' ') THEN
                  BUFFER = '0.0E0'
               END IF
               CALL CHR_LDBLK (BUFFER)
               BUFLEN = CHR_LEN(BUFFER)
               CALL CAP_OUT (GUI__SGZ, ' ', ';' // BUFFER(1 : BUFLEN),
     :           STATUS)

               BUFFER = ' '
               WRITE(BUFFER, 2000, IOSTAT=LSTAT) YMIN__SPLOT
               IF (BUFFER .EQ. ' ') THEN
                  BUFFER = '0.0E0'
               END IF
               CALL CHR_LDBLK (BUFFER)
               BUFLEN = CHR_LEN(BUFFER)
               CALL CAP_OUT (GUI__SGZ, ' ', ';' // BUFFER(1 : BUFLEN),
     :           STATUS)

               BUFFER = ' '
               WRITE(BUFFER, 2000, IOSTAT=LSTAT) YMAX__SPLOT
               IF (BUFFER .EQ. ' ') THEN
                  BUFFER = '0.0E0'
               END IF
               CALL CHR_LDBLK (BUFFER)
               BUFLEN = CHR_LEN(BUFFER)
               CALL CAP_OUT (GUI__SGZ, ' ', ';' // BUFFER(1 : BUFLEN),
     :           STATUS)

            ELSE
               CALL CAP_WARN (GUI__SGZ, ' ', 'There is no plot '/
     :           /'open.', STATUS)

            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
