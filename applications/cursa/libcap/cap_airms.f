      SUBROUTINE CAP_AIRMS (ROWS, AIRMAS, STATUS)
*+
*  Name:
*     CAP_AIRMS
*  Purpose:
*     Calculate the air mass from the observed zenith distance.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_AIRMS (ROWS; AIRMAS; STATUS)
*  Description:
*     Calculate the air mass from the observed zenith distance.
*  Arguments:
*     ROWS  =  INTEGER
*        The number of rows for which the air mass is to be computed.
*     AIRMAS(ROWS)  =  DOUBLE PRECISION (Given and Returned)
*        On entry this array contains the observed zenith distance
*        in radians.  On exit it contains the air mass.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every row
*       Calculate the air mass from the observed zenith distance.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     5/10/97 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  ROWS
*  Arguments Given and Returned:
      DOUBLE PRECISION
     :  AIRMAS(ROWS)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      DOUBLE PRECISION SLA_AIRMAS
*  Local Variables:
      INTEGER
     :  CURROW  ! Current row.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Calculate the air mass for each row.

         DO CURROW = 1, ROWS
            AIRMAS(CURROW) = SLA_AIRMAS(AIRMAS(CURROW) )
         END DO

      END IF

      END
