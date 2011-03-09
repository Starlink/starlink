      SUBROUTINE NDG_NDFCO( INDF1, IGRP, INDEX, INDF2, STATUS )
*+
*  Name:
*     NDG_NDFCO

*  Purpose:
*     Obtain an NDF identifier for a new NDF created by copying
*     an existing NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_NDFCO( INDF1, IGRP, INDEX, INDF2, STATUS )

*  Description:
*     The routine returns an NDF identifier for a new NDF created by
*     copying an existing NDF. The name of the new NDF is held
*     at a given index within a given group. It is equivalent to NDF_PROP.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for an existing NDF (or NDF section) to act as a
*        template.
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group holding the names of NDFs. This
*        will often be created using NDG_CREAT, but groups created "by
*        hand" using GRP directly can also be used.
*     INDEX = INTEGER (Given)
*        The index within the group at which the name of the NDF to be
*        accessed is stored.
*     INDF2 = INTEGER (Returned)
*        Identifier for the new NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF2 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The NDF__NOID
*     constant is defined in the include file NDF_PAR.

*  Copyright:
*     Copyright (C) 2010-2011 Science & Technology Facilities Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-NOV-2004 (DSB):
*        Original version.
*     7-MAR-2006 (DSB):
*        Switch off interpretation of shell metacharacters by HDS.
*     2010-03-18 (TIMJ):
*        Use PSX_WORDEXP instead of ONE_SHELL_ECHO
*     2011-03-08 (TIMJ):
*        Use ONE_WORDEXP_NOGLOB
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDG_CONST'        ! NDG constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER IGRP
      INTEGER INDEX

*  Arguments Returned:
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME*(GRP__SZNAM)! NDF file name
      CHARACTER ENAME*(GRP__SZNAM)! Expanded NDF file name
      INTEGER PLACE              ! NDF placeholder.
      INTEGER SHELL              ! Original value of HDS SHELL tuning param
*.

*  Set an initial value for the INDF argument.
      INDF2 = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( NDG__UCASE ) CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  Get the required name for the new NDF.
      CALL GRP_GET( IGRP, INDEX, 1, NAME, STATUS )

*  If the name could not be obtained, set the name blank and abort.
      IF ( STATUS .NE. SAI__OK ) THEN
         NAME = ' '
         GO TO 999
      END IF

*  Expand any shell metacharacters in it. Having done this we can safely
*  switch off HDS metacharacter interpretation, since HDS has problems
*  with spaces in file names.
      CALL ONE_WORDEXP_NOGLOB( NAME, ENAME, STATUS )
      CALL HDS_GTUNE( 'SHELL', SHELL, STATUS )
      CALL HDS_TUNE( 'SHELL', -1, STATUS )

*  Create the NDF place holder.
      CALL NDG1_OPEN( ENAME, PLACE, STATUS )

* Copy the required components.
      CALL NDF_COPY( INDF1, PLACE, INDF2, STATUS )

*  Re-instate the original HDS SHELL value.
      CALL ERR_BEGIN( STATUS )
      CALL HDS_TUNE( 'SHELL', SHELL, STATUS )
      CALL ERR_END( STATUS )

*  If an error occured, add context information.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN

         IF( NAME .NE. ' ' ) THEN
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'NDG_NDFCO_ERR1', 'Unable to get an NDF '//
     :                    'identifier for ''^NAME''.', STATUS )

         ELSE
            CALL ERR_REP( 'NDG_NDFCO_ERR2', 'Unable to get an NDF '//
     :                    'identifier for a new data set.', STATUS )

         END IF

      END IF

      END
