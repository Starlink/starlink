      SUBROUTINE KPG1_NWILD( PNAME, NFILES, IGRP, STATUS )
*+
*  Name:
*     KPG1_NWILD

*  Purpose:
*     Forms a group of NDF and foreign-format files through wildcarded
*     lists supplied via a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NWILD( PNAME, NFILES, IGRP, STATUS )

*  Description:
*     This routine obtains lists of file names that may include
*     wildcards through a parameter.  The wildcards are expanded and
*     the resultant file names are tested whether they are NDFs or in a
*     recognised foreign data-format.  Those that pass the test are
*     stored in a group.  When this new group is completed, it is
*     purged of duplicates, and this group is returned.

*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        Parameter name through which the list of file names are
*        supplied.
*     NFILES = INTEGER (Returned)
*        The number of files that can be processed by the NDF system.
*     IGRP = INTEGER (Returned)
*        The identifier of the group containing the list of valid file
*        names.  If something goes wrong, this will be set to GRP__NOID.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 May 13 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'DAT_ERR'          ! Data-system error constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Arguments Given:
      CHARACTER * ( * ) PNAME

*  Arguments Returned:
      INTEGER NFILES
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ADDED              ! Number of items added to a group
      LOGICAL CFLAG              ! A group requires further input via
                                 ! continuation lines?
      LOGICAL FNF                ! Valid files were found
      INTEGER I                  ! Loop counter
      INTEGER NGLIST             ! Number of items in input list

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned arguments.
      IGRP = GRP__NOID
      NFILES = 0

*  Get file list and obtain the number of specifications.
*  ======================================================
*
*  Use GRP to get a list of wildcarded filenames.

*  Create a new group to contain the input file names.
      CALL GRP_NEW( 'Input files', IGRP, STATUS )

*  Allow for continuation lines.
      CFLAG = .TRUE.
      DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )

*  Get the list of file names from the environment.
         CALL GRP_GROUP( 'IN', GRP__NOID, IGRP, NGLIST, ADDED, 
     :                   CFLAG, STATUS )

*  Cancel the parameter association in order to get more group values
*  through the parameter, unless there are no more to obtain.
         IF ( CFLAG ) CALL PAR_CANCL( 'IN', STATUS )
      END DO

*  Tidy and exit if there has been an error.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP, STATUS )
         GOTO 999
      END IF

*  Inquire the number of file specifications.
      CALL GRP_GRPSZ( IGRP, NGLIST, STATUS )

*  Expand the wildcards.
*  =====================

*  Loop through the specifications.
      DO I = 1, NGLIST

*  Expand the list of files into valid NDFs and foreign file formats.
         CALL KPG1_NDFCH( I, IGRP, FNF, STATUS )
      END DO

*  Tidy up and exit if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP, STATUS )
         GOTO 999
      END IF

*  Inquire how many files are in the expanded list.
      CALL GRP_GRPSZ( IGRP, NFILES, STATUS )

  999 CONTINUE

      END
