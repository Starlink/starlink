      SUBROUTINE GRP1_GTIND( SLOT, INDEX, FILE, STATUS )
*+
*  Name:
*     GRP1_GTIND

*  Purpose:
*     Get an indirection file name from the list of indirection file
*     names used by the specified group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_GTIND( SLOT, INDEX, FILE, STATUS )

*  Description:
*     This routine retrieves the indirection file name stored at the
*     given index in the FILES array associated with the group
*     identified by SLOT. If an index of zero is supplied, a blank
*     string is returned. Any other out-of-bounds index causes an
*     error to be reported.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The GRP slot number for the group to which the indirection file
*        related.
*     INDEX = INTEGER (Given)
*        The index within the FILES array at which the required
*        indirection file name is stored. An error is reported if this
*        is outside the range of the array.
*     FILE = CHARACTER * ( * ) (Returned)
*        The file name stored at the requested index within the FILES
*        array. A blank value is returned if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'GRP_ERR'          ! GRP error constants.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_FLPNT( GRP__MAXG ) = INTEGER (Read and Write)
*           Pointers to the mapped FILES array of each group.
*        CMN_FLSIZ( GRP__MAXG ) = INTEGER (Read and Write)
*           The Ccurrent size of each groups FILES array.

*  Arguments Given:
      INTEGER SLOT
      INTEGER INDEX

*  Arguments Returned:
      CHARACTER FILE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*.

*  Set the returned value blank.
      FILE = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Return with a blank file name if an index of zero was supplied.
      IF( INDEX .EQ. 0 ) GO TO 999

*  Check that the array index is within range.
      IF( INDEX .LT. 0 .OR. INDEX .GT. CMN_FLSIZ( SLOT ) ) THEN
         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'I', INDEX )
         CALL MSG_SETI( 'J', CMN_FLSIZ( SLOT ) )
         CALL ERR_REP( 'GRP1_GTIND_ERR1',
     :         'GRP1_GTIND: Indirection file no. ^I is outside valid '//
     :         'range [1,^J]', STATUS )
         GO TO 999
      END IF

*  Retreive the file name from the FILES array. NB, the final argument
*  specifies the length of each character string in the mapped array,
*  and is required by UNIX. There is no corresponding dummy argument in
*  the code for GRP1_GETC.
      CALL GRP1_GETC( CMN_FLSIZ( SLOT ),
     :                %VAL( CNF_PVAL( CMN_FLPNT( SLOT ) ) ),
     :                INDEX, FILE, STATUS,
     :                %VAL( CNF_CVAL( GRP__SZNAM ) ) )

 999  CONTINUE

      END
