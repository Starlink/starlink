      SUBROUTINE CTG_CATAS( IGRP, INDEX, MODE, CI, STATUS )
*+
*  Name:
*     CTG_CATAS

*  Purpose:
*     Obtain a CAT identifier for an existing catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG_CATAS( IGRP, INDEX, MODE, CI, STATUS )

*  Description:
*     The routine returns a CAT identifier for an existing catalogue. The
*     name of the catalogue is held at a given index within a given group.
*     It is equivalent to CAT_ASSOC.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group holding the names of catalogues. This
*        will often be created using CTG_ASSOC, but groups created "by
*        hand" using GRP directly (i.e. without the supplemental groups
*        created by CTG_ASSOC) can also be used.
*     INDEX = INTEGER (Given)
*        The index within the group at which the name of the catalogue to be
*        accessed is stored.
*     MODE = CHARACTER * ( * ) (Given)
*        Type of catalogue access required: 'READ', or 'WRITE'.
*     CI = INTEGER (Returned)
*        catalogue identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then a value of
*     CAT__NOID will be returned for the CI argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The CAT__NOID
*     constant is defined in the include file CAT_PAR.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT_ public constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'CTG_CONST'        ! CTG constants.

*  Arguments Given:
      INTEGER IGRP
      INTEGER INDEX
      CHARACTER MODE*(*)

*  Arguments Returned:
      INTEGER CI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER LMODE*5          ! Local copy of MODE
      CHARACTER NAME*(GRP__SZNAM)! catalogue file specification
*.

*  Set an initial value for the CI argument.
      CI = CAT__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check supplied MODE (CAT seems not to do this).
      LMODE = MODE
      CALL CHR_UCASE( LMODE )
      IF( LMODE .NE. 'WRITE' .AND. LMODE .NE. 'READ' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'MODE', MODE )
         CALL ERR_REP( 'CTG_CATAS_ERR1', 'CTG_CATAS: Illegal MODE '//
     :                 'value (^MODE) suplied. Programming error.',
     :                 STATUS )
         GO TO 999
      END IF

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( CTG__UCASE ) CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  Get the required name.
      CALL GRP_GET( IGRP, INDEX, 1, NAME, STATUS )

*  If the name could not be obtained, set the name blank and abort.
      IF ( STATUS .NE. SAI__OK ) THEN
         NAME = ' '
         GO TO 999
      END IF

*  Open the catalogue.
      CALL CAT_TOPEN( NAME, 'OLD', MODE, CI, STATUS )

*  If an error occured, release the catalogue and add context information.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN

         CALL ERR_BEGIN( STATUS )
         CALL CAT_TRLSE( CI, STATUS )
         CALL ERR_END( STATUS )

         IF( NAME .NE. ' ' ) THEN
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'CTG_CATAS_ERR1', 'Unable to get a '//
     :                    'catalogue identifier for ''^NAME''',
     :                    STATUS )
         ELSE
            CALL ERR_REP( 'CTG_CATAS_ERR2', 'Unable to get a '//
     :                    'catalogue identifier for an existing '//
     :                    'data set.', STATUS )
         END IF

      END IF

      END
