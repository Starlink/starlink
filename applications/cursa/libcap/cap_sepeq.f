      SUBROUTINE CAP_SEPEQ (CI, EPOCH, EQUINX, STATUS)
*+
*  Name:
*     CAP_SEPEQ
*  Purpose:
*     Set the epoch and equinox for a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_SEPEQ (CI, EPOCH, EQUINX; STATUS)
*  Description:
*     Set the epoch and equinox for a catalogue.  The catalogue may
*     or may not already contain these parameters.  If it does the
*     existing values are replaced with new ones, if it does the
*     parameters are created.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     EPOCH  =  CHARACTER*(*) (Given)
*        The new value for the epoch.
*     EQUINX  =  CHARACTER*(*) (Given)
*        The new value for the equinox.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For the epoch and then the equinox:-
*
*     Attempt to get an identifier for the parameter
*     If ok then
*       Set the value.
*     else
*       If the error was because the parameter did not exist then
*         Annul the error.
*         Create the parameter.
*       end if
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     12/6/97 (ACD): Original version.
*     7/10/97 (ACD): Removed unused INCLUDE file.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  EPOCH*(*),
     :  EQUINX*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  EPI,    ! Identifier for the epoch.
     :  EQI     !     "       "   "  equinox.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       First the epoch.  If the parameter already exists then replace
*       its value.  If it does not create it.

         CALL CAT_TIDNT (CI, 'EPOCH', EPI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TATTC (EPI, 'VALUE', EPOCH, STATUS)

         ELSE
            IF (STATUS .EQ. CAT__NOCMP) THEN
               CALL ERR_ANNUL (STATUS)

               CALL CAT_PPTSC (CI, 'EPOCH', EPOCH,
     :           'Epoch of the coordinates.', EPI, STATUS)

            END IF

         END IF

*
*       Repeat for equinox.

         CALL CAT_TIDNT (CI, 'EQUINOX', EQI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TATTC (EQI, 'VALUE', EQUINX, STATUS)

         ELSE
            IF (STATUS .EQ. CAT__NOCMP) THEN
               CALL ERR_ANNUL (STATUS)

               CALL CAT_PPTSC (CI, 'EQUINOX', EQUINX,
     :           'Equinox of the coordinates.', EQI, STATUS)

            END IF

         END IF

      END IF

      END
