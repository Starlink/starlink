      SUBROUTINE NDG_NDFPL( IGRP, INDEX, PLACE, STATUS )
*+
*  Name:
*     NDG_NDFPL

*  Purpose:
*     Obtain a placeholder for a new NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_NDFPL( IGRP, INDEX, PLACE, STATUS )

*  Description:
*     The routine returns a placeholder for a new NDF. The name of the new
*     NDF is held at a given index within a given group. It is equivalent
*     to NDF_CREPL.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group holding the names of NDFs. This
*        will often be created using NDG_CREAT, but groups created "by
*        hand" using GRP directly can also be used.
*     INDEX = INTEGER (Given)
*        The index within the group at which the name of the NDF to be
*        created is stored.
*     PLACE = INTEGER (Returned)
*        NDF placeholder.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-NOV-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDG_CONST'          ! NDG constants.

*  Arguments Given:
      INTEGER IGRP
      INTEGER INDEX

*  Arguments Returned:
      INTEGER PLACE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME*(GRP__SZNAM)! NDF file name (without file type).
      CHARACTER ENAME*(GRP__SZNAM)! Expanded NDF file name
      INTEGER CONTEXT            ! Context for shell expansion
      INTEGER SHELL              ! Original value of HDS SHELL tuning param
*.

*  Set an initial value for the PLACE argument.
      PLACE = NDF__NOPL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( NDG__UCASE ) CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  Get the required name.
      CALL GRP_GET( IGRP, INDEX, 1, NAME, STATUS )

*  If the name could not be obtained, set the name blank and abort.
      IF ( STATUS .NE. SAI__OK ) THEN
         NAME = ' '
         GO TO 999
      END IF

*  Expand any shell metacharacters in it. Having done this we can safely
*  switch off HDS metacharacter interpretation, since HDS has problems
*  with spaces in file names.
      CONTEXT = 0
      CALL PSX_WORDEXP( NAME, CONTEXT, ENAME, STATUS )
      IF (STATUS .EQ. SAI__OK .AND. CONTEXT .NE. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Received multiple results '//
     :      'from shell expansion', STATUS )
      END IF

      CALL HDS_GTUNE( 'SHELL', SHELL, STATUS )
      CALL HDS_TUNE( 'SHELL', -1, STATUS )

*  Create the NDF place holder.
      CALL NDG1_OPEN( ENAME, PLACE, STATUS )

*  Re-instate the original HDS SHELL value.
      CALL ERR_BEGIN( STATUS )
      CALL HDS_TUNE( 'SHELL', SHELL, STATUS )
      CALL ERR_END( STATUS )

*  If an error occured, add context information.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN

         IF( NAME .NE. ' ' ) THEN
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'NDG_NDFPL_ERR1', 'Unable to get an NDF '//
     :                    'placeholder for ''^NAME''.', STATUS )

         ELSE
            CALL ERR_REP( 'NDG_NDFPL_ERR2', 'Unable to get an NDF '//
     :                    'placeholder for a new data set.', STATUS )

         END IF

      END IF

      END
