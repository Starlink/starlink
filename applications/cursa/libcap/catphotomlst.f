      SUBROUTINE CATPHOTOMLST (STATUS)
*+
*  Name:
*     CATPHOTOMLST
*  Purpose:
*     List a file of photometric transformation constants.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATPHOTOMLST (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     List the contents of a file of transformation coefficients for
*     converting instrumental magnitudes into calibrated or standard
*     magnitudes in some photometric system.  Such files are created
*     by application catphotomfit.
*  Usage:
*     catphotomlst
*  ADAM Parameters:
*     FILNME  =  CHARACTER (read)
*        The name of the file which contains the transformation
*        coefficients.
*     DECPL  =  INTEGER (read)
*        The number of decimal places for displaying the transformation
*        coefficients.  Note that this quantity controls only the
*        precision with which the coefficients are displayed; they
*        are stored in the file as DOUBLE PRECISION numbers.
*  Examples:
*     catphotomlst
*        The file of transformation coefficients will be prompted for
*        and listed.
*     catphotomlst  decpl=8
*        The file of transformation coefficients will be prompted for
*        and listed.  The coefficients will be displayed to a
*        precision of eight places of decimals.
*  Algorithm:
*     Get the name of the transformation coefficients file.
*     Read the transformation coefficients file.
*     Get the number of decimal places to which the coefficients are
*     to be displayed.
*     If all is ok then
*       Assemble the format for displaying the coefficients.
*       Assemble and display a line for each of the coefficients.
*     end if
*     Report any error.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     7/10/97  (ACD): Original version.
*     16/11/97 (ACD): First stable version.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'       ! Standard SAE constants.
*  Status:
      INTEGER STATUS          ! Global status.
*  Local Variables:
      CHARACTER
     :  FILNME*75,  ! Transformation coefficients file name.
     :  FMTBUF*20,  ! Buffer for holding format specification.
     :  CFFBUF*20,  ! Buffer for current coefficient.
     :  BUFFER*75   ! Output buffer.
      DOUBLE PRECISION
     :  ZEROP,      ! Fixed zero point.
     :  ATMOS,      !   "   atmospheric extinction.
     :  INSCON      ! Arbitrary constant applied to instrumental magnitudes.
      INTEGER
     :  DECPL,      ! Number of decimal places width.
     :  WIDTH,      ! Total width of field for coefficients.
     :  LFMTBF,     ! Length of FMTBUF (excl. trail. blanks).
     :  LSTAT       ! Local Fortran I/O status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Get the name of the transformation coefficients file and
*       attempt to read it.

         CALL PAR_GET0C ('FILNME', FILNME, STATUS)
         CALL PAR_CANCL ('FILNME', STATUS)

         CALL CAP_RFLPH (FILNME, INSCON, ZEROP, ATMOS, STATUS)

*
*       Get the number of decimal places to which the coefficients
*       are to be displayed.

         CALL PAR_GET0I ('DECPL', DECPL, STATUS)
         CALL PAR_CANCL ('DECPL', STATUS)

*
*       Proceed if all is ok.

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Assemble the format for displaying the coefficients.

            DECPL = MAX(DECPL, 0)
            DECPL = MIN(DECPL, 10)

            WIDTH = DECPL + 6

            FMTBUF = ' '
            LFMTBF = 0

            CALL CHR_PUTC ('(0PF', FMTBUF, LFMTBF)
            CALL CHR_PUTI (WIDTH, FMTBUF, LFMTBF)
            CALL CHR_PUTC ('.', FMTBUF, LFMTBF)
            CALL CHR_PUTI (DECPL, FMTBUF, LFMTBF)
            CALL CHR_PUTC (')', FMTBUF, LFMTBF)

C           print3000, fmtbuf
C3000       format(1x, 'fmtbuf: ', a )

*
*          Assemble and write the output line for each coefficient.

            CALL MSG_OUT (' ', ' ', STATUS)
            CALL MSG_OUT (' ', 'Photometric transformation '/
     :        /'coefficients:', STATUS)
            CALL MSG_OUT (' ', ' ', STATUS)

*          ... arbitrary constant.

            CFFBUF = ' '
            WRITE(CFFBUF, FMTBUF, IOSTAT=LSTAT) INSCON
            CALL CHR_LDBLK (CFFBUF)

            WRITE(BUFFER, 2000, IOSTAT=LSTAT)
     :        'Arbitrary constant:', CFFBUF
 2000       FORMAT(3X, A, T28, A)

            CALL MSG_OUT (' ', BUFFER, STATUS)

*          ... zero point.

            CFFBUF = ' '
            WRITE(CFFBUF, FMTBUF, IOSTAT=LSTAT) ZEROP
            CALL CHR_LDBLK (CFFBUF)

            WRITE(BUFFER, 2000, IOSTAT=LSTAT)
     :        'Zero point:', CFFBUF

            CALL MSG_OUT (' ', BUFFER, STATUS)

*          ... atmospheric extinction.

            CFFBUF = ' '
            WRITE(CFFBUF, FMTBUF, IOSTAT=LSTAT) ATMOS
            CALL CHR_LDBLK (CFFBUF)

            WRITE(BUFFER, 2000, IOSTAT=LSTAT)
     :        'Atmospheric extinction:', CFFBUF

            CALL MSG_OUT (' ', BUFFER, STATUS)

            CALL MSG_OUT (' ', ' ', STATUS)

         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CATPHOTOMTRN_ERR', 'Error listing the '/
     :        /'transformation coefficients file.', STATUS)
         END IF

      END IF

      END
