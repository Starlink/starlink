      SUBROUTINE CAP_GDCPL (DECPL, STATUS)
*+
*  Name:
*     CAP_GDCPL
*  Purpose:
*     Set the number of decimal places for displaying statistics.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GDCPL (DECPL; STATUS)
*  Description:
*     Set the number of decimal places for displaying statistics.
*  Arguments:
*     DECPL  =  INTEGER (Given)
*        The number of decimal places to be displayed.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the value is greater than zero and less than eleven then
*       Copy the value to the common block.
*     else
*       Report a warning.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     28/11/96 (ACD): Original version.
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
     :  DECPL
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      CHARACTER
     :  BUFFER*75    ! Buffer for warning message.
      INTEGER
     :   BUFLEN      ! Length of BUFFER (excl. trail. blanks).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       If the value lies in the range 1 to 10 then copy it to the
*       common block, otherwise report a warning.

         IF (DECPL .GT. 0  .AND.  DECPL .LE. 10) THEN
            SDCPL__SGZ = DECPL

         ELSE
            BUFFER = ' '
            BUFLEN = 0

            CALL CHR_PUTC ('Illegal value for number of decimal '/
     :        /'places: ', BUFFER, BUFLEN)
            CALL CHR_PUTI (DECPL, BUFFER, BUFLEN)
            CALL CHR_PUTC (' (not set).', BUFFER, BUFLEN)

            CALL CAP_WARN (GUI__SGZ, ' ', BUFFER(1 : BUFLEN), STATUS)

         END IF

      END IF

      END
