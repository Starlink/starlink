      SUBROUTINE CAP_GCLSE (STATUS)
*+
*  Name:
*     CAP_GCLSE
*  Purpose:
*     Attempt to close any catalogue which is open.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCLSE (STATUS)
*  Description:
*     Attempt to close any catalogue which is open.  If there is no
*     catalogue the routine simply exists, without reporting a message
*     or error or setting the status.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is a catalogue open then
*       Attempt to close it.
*       Set the number of the current selection to 0.
*       Set the 'catalogue open' flag to .FALSE.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     2/6/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Status:
      INTEGER STATUS              ! Global status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.  If there is no cataogue
*       open then the routine simply exits.

         IF (COPEN__SGZ) THEN

*
*          Attempt to close it.

            CALL CAT_TRLSE (CI__SGZ, STATUS)

*
*          Set the number of the current selection to 0.

            SELS__SGZ = 0

*
*          Set the 'catalogue open' flag to .FALSE.

            COPEN__SGZ = .FALSE.
         END IF

      END IF

      END
