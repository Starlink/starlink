      SUBROUTINE CAP_HSDET (CSEL, CRIT, BINS, BINWD, COLOUR, NUMINC,
     :  NUMEXC, NUMNUL, STATUS)
*+
*  Name:
*     CAP_HSDET
*  Purpose:
*     Report histogram details.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_HSDET (CSEL, CRIT, BINS, BINWD, COLOUR, NUMINC,
*       NUMEXC, NUMNUL; STATUS)
*  Description:
*     Report histogram details.
*  Arguments:
*     CSEL  =  INTEGER (Given)
*        Number of the current selection.
*     CRIT  =  CHARACTER*(*) (Given)
*        Expression corresponding to the current selection.
*     BINS  =  INTEGER (Given)
*        Number of bins in the histogram.
*     BINWD  =  REAL (Given)
*        Width of each histogram bin.
*     COLOUR  =  CHARACTER*(*) (Given)
*        CURSA plotting colour.
*     NUMINC  =  INTEGER (Given)
*        Number of points used to construct the histogram.
*     NUMEXC  =  INTEGER (Given)
*        Number of points lying outside the histogram range.
*     NUMNUL  =  INTEGER (Given)
*        Number of null rows in the selection.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Report the details.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     16/9/99 (ACD): Original version.
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
*  Arguments Given:
      INTEGER
     :  CSEL,
     :  BINS,
     :  NUMINC,
     :  NUMEXC,
     :  NUMNUL
      CHARACTER
     :  CRIT*(*),
     :  COLOUR*(*)
      REAL
     :  BINWD
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  BUFFER*20, ! Work buffer.
     :  LOWCOL*20  ! Local copy of COLOUR, in lower case.
      INTEGER
     :  LBUFF,     ! Length of BUFFER (excl. trail. blanks).
     :  LSTAT      ! Local Fortran I/O status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Output the selection histogrammed.

         CALL MSG_SETI ('CSEL', CSEL)
         CALL MSG_SETC ('CRIT', CRIT)

         CALL CAP_INFO (GUI__SGZ, ' ', 'Selection ^CSEL: ^CRIT',
     :     STATUS)

*
*       Number of bins, bin width and colour.

         BUFFER = ' '

         IF (BINWD .GT. 1.0E5) THEN
            WRITE(BUFFER, '(1PE12.2)', IOSTAT=LSTAT) BINWD
         ELSE
            WRITE(BUFFER, '(0PF12.2)', IOSTAT=LSTAT) BINWD
         END IF

         CALL CHR_LDBLK (BUFFER)

         LBUFF = CHR_LEN(BUFFER)

         LOWCOL = COLOUR
         CALL CHR_LCASE (LOWCOL)

         CALL MSG_SETI ('BINS', BINS)
         CALL MSG_SETC ('BINWD', BUFFER)
         CALL MSG_SETC ('LOWCOL', LOWCOL)

         CALL CAP_INFO (GUI__SGZ, ' ', 'Number of bins: ^BINS,  '/
     :     /'bin width: ^BINWD,  line colour: ^LOWCOL.', STATUS)

*
*       Number of points in the histogram.

         CALL MSG_SETI ('NUMINC', NUMINC)
         CALL CAP_INFO (GUI__SGZ, ' ', 'Histogram constructed from '/
     :     /'^NUMINC rows.', STATUS)

*
*       Number of points outside the range.

         IF (NUMEXC .GT. 0) THEN
            CALL MSG_SETI ('NUMEXC', NUMEXC)
            CALL CAP_INFO (GUI__SGZ, ' ', '^NUMEXC rows fell outside '/
     :        /'the histogram limits.', STATUS)
         END IF

*
*       Number of null rows.

         IF (NUMNUL .GT. 0) THEN
            CALL MSG_SETI ('NUMNUL', NUMNUL)
            CALL CAP_INFO (GUI__SGZ, ' ', '^NUMNUL rows not '/
     :        /'histogrammed because they contained null values.',
     :        STATUS)
         END IF

      END IF

      END
