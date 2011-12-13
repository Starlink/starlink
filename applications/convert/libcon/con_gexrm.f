      SUBROUTINE CON_GEXRM( IGROUP, OGROUP, STATUS )
*+
*  Name:
*     CON_GEXRM

*  Purpose:
*     Creates a group of NDF names based upon a group of filenames.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_GEXRM( IGROUP, OGROUP, STATUS )

*  Description:
*     This routine takes a GRP group containing a list of filenames and
*     removes their file extensions.  The new names are appended to an
*     output group.  If there is no file extension, the new extension
*     is appended to the filenames in the GRP.  Any directory path to
*     the NDF is also removed.

*  Arguments:
*     IGROUP = CHARACTER * ( * ) (Given)
*        The GRP identifier containing the list of filenames.
*     OGROUP = CHARACTER * ( * ) (Given)
*        The GRP identifier containing the list of NDF names after the
*        removal of the file extension and any directory path.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Deficiencies:
*     -  This version uses the UNIX path delimiter explictely, until
*     there is PSX routine to do this.
*
*     [optional_subroutine_items]...

*  Prior Requirements:
*     The output group must already exist; it is not created by this
*     routine.

*  Copyright:
*     Copyright (C) 1996, 2000 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 January 25 (MJC):
*        Original version.
*     7-MAR-2000 (DSB):
*        Set STATUS to SAI__ERROR when reporting new errors.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IGROUP
      INTEGER OGROUP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string excluding trailing
                                 ! blanks

*  Local Variables:
      INTEGER CPOS               ! Column pointer
      CHARACTER * ( 256 ) FILNAM ! Name of the element in the group
      INTEGER I                  ! Loop counter
      INTEGER INEL               ! Number of elements in the input group
      INTEGER ONEL               ! Number of elements initially in the
                                 ! output group
      INTEGER POSPAT             ! Column position to the end of the
                                 ! path
      CHARACTER * ( 72 ) SLICE   ! Slice text extracted from the element

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the lengths of the groups.
*  =================================

*  Inquire the length of the input group.
      CALL GRP_GRPSZ( IGROUP, INEL, STATUS )

*  Check for an error.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CON_GEXRM_IGROUP',
     :     'Error with supplied input group identifier (probable '/
     :     /'programming error)', STATUS )
         GOTO 999

      ELSE IF ( INEL .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CON_GEXRM_EMPTY',
     :     'Group for modification contains no elements (probable '/
     :     /'programming error)', STATUS )
         GOTO 999
      END IF

*  Inquire the length of the output group.
      CALL GRP_GRPSZ( OGROUP, ONEL, STATUS )

*  Check for an error.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CON_GEXRM_OGROUP',
     :     'Error with supplied output group identifier (probable '/
     :     /'programming error)', STATUS )
         GOTO 999
      END IF

*  Loop for each input element.
      DO I = 1, INEL

*  Obtain the input element.
         CALL GRP_GET( IGROUP, I, 1, FILNAM, STATUS )

*  Remove any slice text from the element.
         CALL CON_GSLIC( FILNAM, SLICE, STATUS )

*  Remove any directory path. WARNING: this assumes UNIX, and that the
*  forward slash does not appear as part of the filename.  The search
*  starts from the end of the name and heads in a backwards direction.
*  Zero indicates that the search found no path delimiter.
         POSPAT = CHR_LEN( FILNAM )
         CALL CHR_FIND( FILNAM, '/', .FALSE., POSPAT )
         IF ( POSPAT .NE. 0 ) FILNAM = FILNAM( POSPAT+1: )

*  Find the start of the file extension.  This assumes that the first
*  fullstop is the start of the file extension.  This is reasonable as
*  as fullstop is used to indicate components within an HDS file.
         CPOS = INDEX( FILNAM, '.' )

*  When there is no file extension of the standard name, set the column
*  pointer to the immediately after the end of the string, so that
*  the extension can be appended.  Otherwise specify the last character
*  to include in the output name.
         IF ( CPOS .EQ. 0 ) THEN
            CPOS = CHR_LEN( FILNAM )
         ELSE
            CPOS = CPOS - 1
         END IF

*  Append the revised name to the output group.
         CALL GRP_PUT( OGROUP, 1, FILNAM( :CPOS ), ONEL + I, STATUS )
      END DO

  999 CONTINUE

      END
