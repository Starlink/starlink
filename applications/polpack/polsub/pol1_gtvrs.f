      SUBROUTINE POL1_GTVRS( INDF, VTEST, OK, STATUS )
*+
*  Name:
*     POL1_GTVRS

*  Purpose:
*     Check that the POLPACK version number in a POLPACK NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_GTVRS( INDF, VTEST, OK, STATUS )

*  Description:
*     This routine extracts the version number from the VERSION component
*     of the POLPACK extension in the supplied NDF. It then compares this
*     version with the version supplied in VTEST. If the NDF was created
*     by an earlier version of POLPACK than that indicated by VTEST, then
*     OK is returned false. Otherwise OK is returned true.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF.
*     VTEST = CHARACTER*(*) (Given)
*        The earliest version of of POLPACK which created NDFs which can
*        be processed by the calling application. This should be in the
*        form "m.n-r" where m, n and r are integers. Missing trailing
*        fields default to zero.
*     OK = LOGICAL (Returned)
*        Can the NDF be processed by the application?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-APR-1999 (DSB):
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
      INTEGER INDF
      CHARACTER VTEST*(*)

*  Arguments Returned:
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER VERS*20          ! Version string
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

*  Version tagging was only introduced at version 2.0-2. If there is no
*  version number in the extension, assume V1.0.
      VERS = '1'

*  Get the VERSION item from the POLPACK extension, using the above
*  version as the default if VERSION is not present.
      CALL NDF_XGT0C( INDF, 'POLPACK', 'VERSION', VERS, STATUS )

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

*  Return .FALSE. if an error has occurred.
      IF( STATUS .NE. SAI__OK ) OK = .FALSE.

      END
