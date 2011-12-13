************************************************************************

      SUBROUTINE AGI_1ODB ( STATUS )

*+
*  Name:
*     AGI_1ODB

*  Purpose:
*     Open the database.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1ODB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Open the databas

*  Algorithm:
*     Check status on entry.
*     Open the database file if present.
*     If file is not found then
*        Create a new database file.
*     Endif.

*  Copyright:
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     Copyright (C) 1988, 1989, 1990, 1991, 1992 Science & Engineering
*     Research Council.
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
*     Nick Eaton  ( DUVAD::NE )
*     PWD: Peter W. Draper (JAC, Durham University)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1998-JUL-01 (NE):
*        Original version.
*     1989-JUL-01 (NE):
*        Read database locator from common block.
*     1990-JUL-01 (NE):
*        Call AGI_1FNAME and HDS_TUNE
*     1991-DEC-01 (NE):
*        Removed call to GDS_TUNE
*     1992-MAR-01 (NE):
*        Define file extension using AGI__ENAME parameter.
*     2007-NOV-12 (PWD):
*        Protect database name inquiry against bad status and
*        uninitialised length
*     2007-NOV-15 (TIMJ):
*        Fix initialisation of LNAME.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI1_PAR'
      INCLUDE 'agi_nam'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_locs'


*  Local Variables:
      LOGICAL FOUND

      CHARACTER FNAME * ( AGI1__MAXPATH )

      INTEGER LEXT, LNAME

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if database is already there
         CALL AGI_1FDB( FOUND, STATUS )

*   If there is no database then create it
         IF ( .NOT. FOUND ) THEN

*   Inquire the database file name
            LNAME = 0
            FNAME = ' '
            CALL AGI_1FNAME( FNAME, LNAME, STATUS )

*   Append the file extension
            IF ( STATUS .EQ. SAI__OK ) THEN
               LEXT = LEN( AGI__ENAME )
               FNAME( LNAME+1 : LNAME+LEXT ) = AGI__ENAME
            END IF

*   Set up the HDS tuning parameter to wait if the file is being
*   accessed by another task
C            CALL HDS_TUNE( 'WAIT', .TRUE., STATUS )

*   Create the new file
            DABLOC = ' '
            CALL HDS_NEW( FNAME( :LNAME+LEXT ), AGI__DBNAM, AGI__DBTYP,
     :                    0, 0, DABLOC, STATUS )

*   Set the database locator valid flag and the flush flag
            LOCVAL = .TRUE.
            FLUSH = .TRUE.

         ENDIF

      ENDIF

*      print*, '+++++ AGI_1ODB +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

