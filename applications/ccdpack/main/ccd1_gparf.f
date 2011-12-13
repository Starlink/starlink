      SUBROUTINE CCD1_GPARF( ACMODE, CREAT, GLOC, SLOC, STATUS )
*+
*  Name:
*     CCD1_GPARF

*  Purpose:
*     Get locators for the Global parameter file.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_GPARF( ACMODE, CREAT, GLOC, SLOC, STATUS )

*  Description:
*     This routine returns locators for the GLOBAL ADAM parameter
*     database file ($ADAM_USER/GLOBAL) and for the structure within
*     it which CCDPACK uses to store keyed global parameters.

*  Arguments:
*     ACMODE = CHARACTER * ( * ) (Given)
*        The access mode, 'READ', 'WRITE' or 'UPDATE'.
*     CREAT = LOGICAL (Given)
*        True if the GLOBAL file and the keyed parameter structure
*        should be created in the event of their non-existence.
*     GLOC = CHARACTER * ( * ) (Returned)
*        HDS locator for the GLOBAL file.  This is a primary locator and
*        should be annulled by the calling routine to close the file
*        and reclaim resources.  If CREAT is true and the file does
*        not already exist it will be created.  If CREAT is false
*        and the file does not exist GLOC will be returned as DAT__NOLOC
*        without error.
*     SLOC = CHARACTER * ( * ) (Returned)
*        HDS locator for the CCDPACK_KEYPARS structure within the
*        GLOBAL parameters file.  This is a secondary locator.
*        If CREAT is true and the structure does not already exist
*        then it will be created.  If CREAT is false and the
*        structure does not already exist SLOC will be returned as
*        DAT__NOLOC without error.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-MAY-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS system constants
      INCLUDE 'FIO_PAR'          ! FIO system constants
      INCLUDE 'DAT_ERR'          ! HDS system error values

*  Arguments Given:
      CHARACTER * ( * ) ACMODE
      LOGICAL CREAT

*  Arguments Returned:
      CHARACTER * ( * ) GLOC
      CHARACTER * ( * ) SLOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FILE ! File name of global parameter file
      INTEGER LENG               ! Length of filename
      LOGICAL THERE              ! Does HDS component exist?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise locators.
      GLOC = DAT__NOLOC
      SLOC = DAT__NOLOC

*  Get the name of the ADAM_USER directory.
      CALL SUBPAR_ADMUS( FILE, LENG, STATUS )

*  Get the name of the GLOBAL parameter file.
      CALL CHR_APPND( 'GLOBAL', FILE, LENG )

*  Try to get a locator for the GLOBAL parameter file.
      CALL HDS_OPEN( FILE, ACMODE, GLOC, STATUS )

*  If the file was not found, and we are in write mode, then create one.
      IF ( STATUS .EQ. DAT__FILNF ) THEN
         CALL ERR_ANNUL( STATUS )
         IF ( CREAT ) THEN
            CALL HDS_NEW( FILE, 'GLOBAL', 'STRUC', 0, 0, GLOC, STATUS )
         ELSE
            GLOC = DAT__NOLOC
         END IF
      END IF

*  Only proceed if we now have an open GLOBAL file.
      IF ( STATUS .EQ. SAI__OK .AND. GLOC .NE. DAT__NOLOC ) THEN

*  See if a keyed parameters structure exists in the GLOBAL file.
         CALL DAT_THERE( GLOC, 'CCDPACK_KEYPARS', THERE, STATUS )

*  If none exists and we are in write mode, then create one.
         IF ( .NOT. THERE .AND. CREAT ) THEN
            CALL DAT_NEW( GLOC, 'CCDPACK_KEYPARS', 'KEYED_PARAMS',
     :                    0, 0, STATUS )
            THERE = ( STATUS .EQ. SAI__OK )
         END IF

*  Get a locator to the structure if it now exists.
         IF ( THERE ) THEN
            CALL DAT_FIND( GLOC, 'CCDPACK_KEYPARS', SLOC, STATUS )
         END IF
      END IF

      END
* $Id$
