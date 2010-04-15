      SUBROUTINE CAP_CRNSP (FREQ, NUMSEL, SELIST, STATUS)
*+
*  Name:
*     CAP_CRNSP
*  Purpose:
*     Create list of selected rows.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CRNSP (FREQ, NUMSEL; SELIST; STATUS)
*  Description:
*     Create list of selected rows.
*  Arguments:
*     FREQ  =  INTEGER (Given)
*        Frequency with which rows are to be selected.
*     NUMSEL  =  INTEGER (Given)
*        Number of rows to be selected.
*     SELIST(NUMSEL)  =  INTEGER (Returned)
*        List of rows to be selected.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Create the list of rows.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     14/6/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'      ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  FREQ,
     :  NUMSEL
*  Arguments Returned:
      INTEGER
     :  SELIST(NUMSEL)
*  Status:
      INTEGER STATUS         ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP                 ! Loop index.
*.

      IF (STATUS .EQ. SAI__OK) THEN

         DO LOOP = 1, NUMSEL
            SELIST(LOOP) = 1 + ((LOOP - 1) * FREQ)
         END DO

      END IF

      END
