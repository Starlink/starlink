      SUBROUTINE CTG1_CATCH( VERB, IGRP1, START, IGRP2, STATUS )
*+
*  Name:
*     CTG1_CATCH

*  Purpose:
*     Expand wild card templates, etc, and check all catalogues exist.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG1_CATCH( VERB, IGRP1, START, IGRP2, STATUS )

*  Description:
*     Names stored in the supplied group with indices greater than or
*     equal to START are expanded into a list of explicit file names
*     which can be accessed as catalogues using the CAT library. The
*     expanded set of names are appended to the end of the group, and
*     the original names are deleted.
*
*     If any of the files in the group cannot be accessed, an error is
*     reported and STATUS is returned equal to CAT__NOFIL. If this
*     happens the name of each bad file is stoed in IGRP2.

*  Arguments:
*     VERB = LOGICAL (Given)
*        If TRUE then errors which occur whilst accessing supplied catalogues
*        are flushed so that the user can see them before re-prompting for
*        a new catalogue ("verbose" mode). Otherwise, they are annulled and
*        a general "Cannot access file xyz" message is displayed before
*        re-prompting.
*     IGRP1 = INTEGER (Given)
*        An identifier for the group containing the catalogue names
*        (potentially containing wild cards).
*     START = INTEGER (Given)
*        The index of the first name to be expanded into a list of catalogue
*        files.
*     IGRP2 = INTEGER (Given)
*        An identifier for a group in which to store any supplied names
*        for which no corresponding catalogues could be found. This may be
*        GRP__NOID in which case the names of bad catalogues are discarded.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*    -  The order of the names is preserved.
*    -  If an input name contained a FITS extension number, it is copied to
*    all related output file names.

*  Copyright:
*     Copyright (C) 1999, 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1999 (DSB):
*        Original version.
*     10-APR-2000 (DSB):
*        Added argument VERB.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'CTG_CONST'        ! CAT private constants
      INCLUDE 'CTG_ERR'          ! CAT error constants

*  Arguments Given:
      LOGICAL VERB
      INTEGER IGRP1
      INTEGER START
      INTEGER IGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXTYP              ! Max. number of catalogue data formats
      PARAMETER ( MXTYP = 50 )
      INTEGER SZTYP              ! Max. length of a catalogue file type
      PARAMETER ( SZTYP = 15 )

*  Local Variables:
      CHARACTER FMT( MXTYP )*(SZTYP)! Known catalogue file types
      CHARACTER NAME*(GRP__SZNAM) ! Current file name or file name template
      INTEGER I                  ! Name index
      INTEGER IGRP3              ! ID. for temporary copy of input group
      INTEGER NBAD               ! No. of inaccesable data sets
      INTEGER NFMT               ! No. of known catalogue data formats
      INTEGER SIZE               ! Size of the input group
      LOGICAL FOUND              ! Were any matching files found?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract all the file types into the FMT array.
      CALL CTG1_GTYPS( MXTYP, CTG__FMTIN, NFMT, FMT, STATUS )

*  Get the input group size.
      CALL GRP_GRPSZ( IGRP1, SIZE, STATUS )

*  Create a temporary group containing a copy of the input group.
      CALL GRP_COPY( IGRP1, 1, SIZE, .FALSE., IGRP3, STATUS )

*  Truncate the input group to remove all the names which are to be
*  expanded. The expanded names will be appended to this truncated group.
      CALL CTG_SETSZ( IGRP1, MIN( SIZE, START - 1 ), STATUS )

*  Loop round all the required names, starting with the name specified
*  by argument START.
      DO I = MAX( 1, START ), SIZE

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the name to be expanded from the untruncated copy of the input group.
         CALL GRP_GET( IGRP3, I, 1, NAME, STATUS )

*  Expand the name into separate file names, appending them to the
*  truncated input group.
         CALL CTG1_EXPAN( VERB, NAME, IGRP1, NFMT, FMT, FOUND, STATUS )

*  If no files were found matching the name, add the name to the group
*  containing bad catalogue names.
         IF( .NOT. FOUND .AND. IGRP2 .NE. GRP__NOID ) THEN
            CALL GRP_PUT( IGRP2, 1, NAME, 0, STATUS )
         END IF

*  Get the next template from the input group.
      END DO

*  Delete the copy of the input group.
 999  CONTINUE
      CALL GRP_DELET( IGRP3, STATUS )

*  If some files were not found, report a general error.
      IF( IGRP2 .NE. GRP__NOID ) THEN
         CALL GRP_GRPSZ( IGRP2, NBAD, STATUS )
         IF( NBAD .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = CTG__NOFIL
            CALL ERR_REP( 'CTG1_CATCH_ERR1', 'Unable to access all '//
     :                    'the specified catalogues.', STATUS )
         END IF
      END IF

      END
