      SUBROUTINE IRM_GETTD( PARAM, TIMDAT, MJD, STATUS )
*+
*  Name:
*     IRM_GETTD

*  Purpose:
*     Get a valid date and time string from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_GETTD( PARAM, TIMDAT, MJD, STATUS )

*  Description:
*     This routine gets a string from the environment using the
*     specified parameter, and checks that it is a valid date and time
*     string, as described in routine IRM_TD. If it is, the
*     corresponding Modified Julian Date is returned. If it is not a
*     valid date and time string, the parameter is cancelled and another
*     string is obtained.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter.
*     TIMDAT = CHARACTER * ( * ) (Returned)
*        The string obtained from the environment. The variable
*        specified for this argument should be at least 25 characters
*        long.
*     MJD = DOUBLE PRECISION (Returned)
*        The Modified Julian Date corresponding to the date and time
*        string. A blank string causes the current time to be returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUL-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      CHARACTER TIMDAT*(*)
      DOUBLE PRECISION MJD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ID                 ! Day of the month.
      INTEGER IHOUR              ! The hours.
      INTEGER IM                 ! Calendar month.
      INTEGER IMIN               ! The minutes.
      INTEGER IY                 ! Calendar year.
      LOGICAL MORE               ! False if a good date and time string
                                 ! has been obtained.
      REAL    SEC                ! The seconds.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop until a good time/date string is obtained.
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Get a string from the environment.
         CALL PAR_GET0C( PARAM, TIMDAT, STATUS )

*  Check it is a valid time/date string.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL IRM_TD( TIMDAT, IY, IM, ID, IHOUR, IMIN, SEC, MJD,
     :                   STATUS )

*  If the string was invalid, flush the error and cancel the parameter
*  value.
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( PARAM, STATUS )

*  Otherwise, indicate that a good value has been obtained.
            ELSE
               MORE = .FALSE.

            END IF

         END IF

      END DO

      END
