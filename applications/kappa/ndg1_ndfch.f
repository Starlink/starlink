      SUBROUTINE NDG1_NDFCH( IGRP1, START, IGRP2, STATUS )
*+
*  Name:
*     NDG1_NDFCH

*  Purpose:
*     Expand wild card templates and check all NDFs exist.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_NDFCH( IGRP1, START, IGRP2, STATUS )

*  Description:
*     Names stored in the supplied group with indices greater than or
*     equal to START are expanded into a list of explicit file names
*     which can be accessed as NDFs, either directly or using the
*     NDF on-the-fly conversion system. 
*
*     If any of the files in the group cannot be accessed, an error is
*     reported and STATUS is returned equal to NDG__NOFIL. If this
*     happens the name of each bad file is stoed in IGRP2.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        An identifier for the group containing the NDF names
*        (potentially containing wild cards).
*     START = INTEGER (Given)
*        The index of the first name to be expanded into a list of NDF
*        files.
*     IGRP2 = INTEGER (Given)
*        An identifier for the group containing the list of bad NDF 
*        names. This may be GRP__NOID in which case the names of bad
*        NDFs are not stored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*    -  This routine checks that the specified files exist, and also 
*    checks that any .sdf files contain legal native NDFs. However, it 
*    does not attempt to check the legality of foreign data format files.
*    -  The order of the names is preserved. 
*    -  If an input name contained an NDF slice specification, it is 
*    copied to all related output file names.
*    -  Native (.sdf) files are returned without a file type.
*    -  HDS component paths may be included for native (.sdf) files, and
*    will be appended to the returned file names if the component
*    identified by the path can be accessed as an NDF. Any wild-cards
*    within the component path are ignored.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1992 (DSB):
*        Original version.
*     29-AUG-1997 (DSB):
*        Modified to use NDF automatic data conversion facilities.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDG_CONST'        ! NDG private constants
      INCLUDE 'NDG_ERR'          ! NDG error constants
      INCLUDE 'PSX_ERR'          ! PSX error constants

*  Arguments Given:
      INTEGER IGRP1
      INTEGER START
      INTEGER IGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FMTIN*(NDG__SZFMT)! Current value of NDG_FORMATS_IN
      CHARACTER NAME*(GRP__SZNAM) ! Current file name or file name template.
      INTEGER I                  ! Name index.
      INTEGER IGRP3              ! ID. for temporary copy of input group.
      INTEGER NBAD               ! No. of inaccesable data sets
      INTEGER SIZE               ! Size of the input group.
      LOGICAL FOUND              ! Were any matching files found?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the current value of environment variable NDF_FORMATS_IN.
*  Annul the error and use a blank value if it is not defined.
      CALL PSX_GETENV( 'NDF_FORMATS_IN', FMTIN, STATUS )
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         FMTIN = ' '
      ELSE
         CALL CHR_RMBLK( FMTIN )
      END IF

*  Get the input group size.
      CALL GRP_GRPSZ( IGRP1, SIZE, STATUS )

*  Create a temporary group containing a copy of the input group.
      CALL GRP_COPY( IGRP1, 1, SIZE, .FALSE., IGRP3, STATUS )

*  Truncate the input group at the index of the first name to be
*  expanded. The expanded names will be appended to this truncated group.
      CALL GRP_SETSZ( IGRP1, MIN( SIZE, START - 1 ), STATUS )

*  Loop round all the required names, starting with the name specified
*  by argument START.
      DO I = MAX( 1, START ), SIZE

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the name to be expanded from the temporary group.
         CALL GRP_GET( IGRP3, I, 1, NAME, STATUS )

*  Expand the name into separate file names, appending them to IGRP1.
         CALL NDG1_EXPAN( NAME, IGRP1, FMTIN, FOUND, STATUS )

*  If no files were found matching the name, add the name to the group
*  containing bad NDF names.
         IF( .NOT. FOUND .AND. IGRP2 .NE. GRP__NOID ) THEN
            CALL GRP_PUT( IGRP2, 1, NAME, 0, STATUS )
         END IF

*  Get the next template from the input group.
      END DO

*  Delete the temporary group.
 999  CONTINUE
      CALL GRP_DELET( IGRP3, STATUS )

*  If some files were not found, report a general error.
      IF( IGRP2 .NE. GRP__NOID ) THEN
         CALL GRP_GRPSZ( IGRP2, NBAD, STATUS )
         IF( NBAD .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = NDG__NOFIL
            CALL ERR_REP( 'NDG1_NDFCH_ERR1',
     :           'NDG1_NDFCH: Unable to access all the specified '//
     :           'data sets.', STATUS )
         END IF
      END IF

      END
