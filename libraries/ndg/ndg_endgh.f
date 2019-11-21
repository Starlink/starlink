      SUBROUTINE NDG_ENDGH( STATUS )
*+
*  Name:
*     NDG_ENDGH

*  Purpose:
*     End a GRP NDF history block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_ENDGH( STATUS )

*  Description:
*     This routine should be called to mark the end of a GRP NDF
*     history block. The block should have been started by a
*     matching call to NDG_BEGGH. Note, GRP NDF history blocks must
*     not be nested.
*
*     During a GRP NDF history block, application code can register GRP
*     groups using routine NDG_ADDGH. When default history information is
*     written to any NDF, a handler routine is called that adds a new
*     history record to the NDF for each currently registered GRP group.
*     The new history record identifies the ADAM parameter with which the
*     group is associated, and lists the contents of the group (as it
*     existed at the time the group was registered - any subsequent changes
*     to the group contents are ignored).

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-OCT-2009 (DSB):
*        Original version.
*     21-JUL-2011 (DSB):
*        New scheme for preventing a group from being written out more
*        than once to a NDF - see NDG1_HWRGH.
*     6-AUG-2018 (DSB):
*        Fix bug introduced by previous change (2011!) that prevented
*        group history being written out to the second and subsequent
*        output NDFs created by an application. For instamce, this caused
*        no group history to be stored in "itermap" NDFs created by
*        smurf:makemap.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'GRP_PAR'

*  Global Variables:
      INCLUDE 'NDG_COM2'         ! Global GRP history information

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL NDG1_HNDLR
      INTEGER CHR_LEN

*  Local Constants:
      INTEGER MXLINE
      PARAMETER ( MXLINE = 20 )

*  Local Variables:
      CHARACTER KEY*30
      CHARACTER PATH*512
      INTEGER I
      INTEGER IGRP
      INTEGER INDF
      INTEGER IPATH
      INTEGER NGRP
      INTEGER NPATH
      INTEGER PLACE
      LOGICAL HASHIS
      LOGICAL UDHKMP
      LOGICAL UGHKMP
      LOGICAL OLD
*.

*  Begin a new error reporting context (we want to clean up even if an
*  error has occurred).
      CALL ERR_BEGIN( STATUS )

*  Get sole access to the NDG globals
      CALL NDG1_GLOCK( .TRUE. )

*  Lock the global objects so they can be use by this thread.
      CALL NDG1_ALOCK( .TRUE., DHKMP_COM2, UDHKMP, STATUS )
      CALL NDG1_ALOCK( .TRUE., GHKMP_COM2, UGHKMP, STATUS )

*  Remove the NDF event handlers needed to record the NDFs in which
*  GRP history should be stored.
      CALL NDG_HLTGH( .FALSE., OLD, STATUS )

*  Loop round each NDF to which default history has been written.
      NPATH = AST_MAPSIZE( DHKMP_COM2, STATUS )
      DO IPATH = 1, NPATH

*  Get the key with index 1 on each pass through this loop. Since the
*  key is immediately removed form the KeyMap, what was key 2 will become
*  key 1 on each pass.
         PATH = AST_MAPKEY( DHKMP_COM2, 1, STATUS )

*  Check no error has occurred.
         IF( STATUS .EQ. SAI__OK ) THEN

*  Get an NDF identifier for it.
            CALL NDF_OPEN( DAT__ROOT, PATH, 'UPDATE', 'OLD', INDF,
     :                     PLACE, STATUS )

*  If the NDF could not be opened (e.g. if it was a temporary NDF
*  that has since been deleted), annul the error and pass on.
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )

*  Otherwise, append a description of each registered GRP group to the
*  current History record of the NDF.
            ELSE
               CALL NDG1_HWRGH( INDF, STATUS )

*  If the NDF has a history component, set its history update mode to SKIP.
*  This means that no default history record will be added to the NDF when
*  it is closed below (one has already been written during the course of
*  the application that has just finished), but the history update mode
*  stored in the NDF structure on disk will not be changed.
               CALL NDF_STATE( INDF, 'History', HASHIS, STATUS )
               IF( HASHIS ) CALL NDF_HSMOD( 'SKIP', INDF, STATUS )

*  Annul the NDF identifier
               CALL NDF_ANNUL( INDF, STATUS )

            END IF
         END IF

*  Remove the current NDF path from the KeyMap. This is the key with
*  index 1, which means a new NDF path will then have index 1.
         CALL AST_MAPREMOVE( DHKMP_COM2, PATH, STATUS )

      END DO

*  Free resources. First delete the groups for which identifiers are held
*  in the keymap.
      NGRP = AST_MAPSIZE( GHKMP_COM2, STATUS )
      DO I = 1, NGRP
         KEY = AST_MAPKEY( GHKMP_COM2, I, STATUS )
         IF( AST_MAPGET0I( GHKMP_COM2, KEY, IGRP, STATUS ) ) THEN
            CALL GRP_DELET( IGRP, STATUS )
         END IF
      END DO

* Now free the KeyMaps.
      CALL AST_ANNUL( GHKMP_COM2, STATUS )
      CALL AST_ANNUL( DHKMP_COM2, STATUS )

*  Allow other threads to access the NDG globals
      CALL NDG1_GLOCK( .FALSE. )

*  End the error reporting context.
      CALL ERR_END( STATUS )

      END
