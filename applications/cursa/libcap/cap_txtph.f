      SUBROUTINE CAP_TXTPH (CI, CALMAG, INSCON, ZEROP, ATMOS, STATUS)
*+
*  Name:
*     CAP_TXTPH
*  Purpose:
*     Write textual information describing the transformation coeffs.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_TXTPH (CI, CALMAG, INSCON, ZEROP, ATMOS; STATUS)
*  Description:
*     Write textual information describing the transformation
*     coefficients used to compute the calibrated magnitude.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     CALMAG  =  CHARACTER*(*) (Given)
*        Name of the column holding the calibrated magnitudes.
*     INSCON  =  DOUBLE PRECISION (Given)
*        Arbitrary constant applied to the instrumental magnitudes.
*     ZEROP  =  DOUBLE PRECISION (Given)
*        Zero point.
*     ATMOS  =  DOUBLE PRECISION (Given)
*        Atmospheric extinction.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Write out the transformation coefficients as textual information.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     7/10/97  (ACD): Original version.
*     16/11/97 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  CALMAG*(*)
      DOUBLE PRECISION
     :  INSCON,
     :  ZEROP,
     :  ATMOS
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  BUFFER*75  ! Output buffer for the current comment.
      INTEGER
     :  LCALMG,    ! Length of CALMAG (excl. trail. blanks).
     :  BUFLEN,    !   "    "  BUFFER ( "  .   "  .   "   ).
     :  LSTAT      ! Local Fortran I/O status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Write the lines of textual information:

         CALL CAT_PUTXT (CI, 'COMMENT', ' ', STATUS)

*       ... the name of the column of calibrated magnitudes.

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Column ', BUFFER, BUFLEN)

         IF (CALMAG .NE. ' ') THEN
            LCALMG = CHR_LEN(CALMAG)
            CALL CHR_PUTC (CALMAG(1 : LCALMG), BUFFER, BUFLEN)
         ELSE
            CALL CHR_PUTC ('<none>', BUFFER, BUFLEN)
         END IF

         CALL CHR_PUTC (' was calculated using the following '/
     :     /'coefficients:', BUFFER, BUFLEN)

         CALL CAT_PUTXT (CI, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

*       ... arbitrary constant.

         BUFFER = ' '
         WRITE(BUFFER, 2000, IOSTAT=LSTAT) 'Arbitrary constant:', INSCON
 2000    FORMAT(3X, A, T28, 0PF10.4)
         BUFLEN = CHR_LEN(BUFFER)

         CALL CAT_PUTXT (CI, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

*       ... zero point.

         BUFFER = ' '
         WRITE(BUFFER, 2000, IOSTAT=LSTAT) 'Zero point:', ZEROP
         BUFLEN = CHR_LEN(BUFFER)

         CALL CAT_PUTXT (CI, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

*       ... atmospheric extinction.

         BUFFER = ' '
         WRITE(BUFFER, 2000, IOSTAT=LSTAT) 'Atmospheric extinction:',
     :     ATMOS
         BUFLEN = CHR_LEN(BUFFER)

         CALL CAT_PUTXT (CI, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

         CALL CAT_PUTXT (CI, 'COMMENT', ' ', STATUS)

      END IF

      END
