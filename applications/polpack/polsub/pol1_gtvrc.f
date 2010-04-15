      SUBROUTINE POL1_GTVRC( CI, VTEST, OK, STATUS )
*+
*  Name:
*     POL1_GTVRC

*  Purpose:
*     Check the POLPACK version number in a POLPACK catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_GTVRC( CI, VTEST, OK, STATUS )

*  Description:
*     This routine extracts the version number from the VERSION parameter
*     of the supplied POLPACK catalogue. It then compares this version
*     with the version supplied in VTEST. If the catalogue was created
*     by an earlier version of POLPACK than that indicated by VTEST, then
*     OK is returned false. Otherwise OK is returned true.

*  Arguments:
*     CI = INTEGER (Given)
*        A CAT identifier for the catalogue.
*     VTEST = CHARACTER*(*) (Given)
*        The earliest version of of POLPACK which created catalogues which
*        can be processed by the calling application. This should be in the
*        form "m.n-r" where m, n and r are integers. Missing trailing
*        fields default to zero.
*     OK = LOGICAL (Returned)
*        Can the catalogue be processed by the application?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-APR-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER CI
      CHARACTER VTEST*(*)

*  Arguments Returned:
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER VERS*20          ! Version string
      INTEGER GI                 ! CAT identifier for VERSION parameter
      INTEGER M                  ! Major version number from NDF
      INTEGER MT                 ! Smallest acceptable major version number
      INTEGER N                  ! Minor version number from NDF
      INTEGER NT                 ! Smallest acceptable minor version number
      INTEGER R                  ! Revision number from NDF
      INTEGER RT                 ! Smallest acceptable revision number
*.

*  Initialise.
      OK = .FALSE.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Split the supplied test version number up into its components.
      CALL POL1_PRSVR( VTEST, MT, NT, RT, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get a CAT identifier for the VERSION parameter in the catalogue.
      CALL CAT_TIDNT( CI, 'VERSION', GI, STATUS )

*  Version tagging was only introduced at version 2.0-2. If there is no
*  version number in the extension, annul the error and assume V1.0.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         VERS = '1'

*  Otherwise, get the parameters value, and release the identifier.
      ELSE
         CALL CAT_TIQAC( GI, 'VALUE', VERS, STATUS )
         CALL CAT_TRLSE( GI, STATUS )
      END IF

*  Split the version number up into its components.
      CALL POL1_PRSVR( VERS, M, N, R, STATUS )

*  Compare the results.
      IF( M .GT. MT ) THEN
         OK = .TRUE.

      ELSE IF( M .EQ. MT ) THEN

         IF( N .GT. NT ) THEN
            OK = .TRUE.

         ELSE IF( N .EQ. NT ) THEN
            IF( R .GE. RT ) OK = .TRUE.
         END IF

      END IF

 999  CONTINUE

*  Return .FALSE. if an error has occurred.
      IF( STATUS .NE. SAI__OK ) OK = .FALSE.

      END
