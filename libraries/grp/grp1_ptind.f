      SUBROUTINE GRP1_PTIND( SLOT, FILE, INDEX, STATUS )
*+
*  Name:
*     GRP1_PTIND

*  Purpose:
*     Put an indirection file name into the list of indirection file
*     names used by the specified group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_PTIND( SLOT, FILE, INDEX, STATUS )

*  Description:
*     This routine stores the given indirection file name in an element
*     of the FILES array associated with the group identified by SLOT.
*     The array is extended by one element and the file name is stored
*     at the end of the array (the array is created if necessary). The
*     index within the array at which the name is stored is returned in
*     argument INDEX.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The GRP slot number for the group to which the indirection file
*        related.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of a file used within an indirection element of a
*        group expression.
*     INDEX = INTEGER (Returned)
*        The index within the FILES array at which the supplied
*        indirection file name was stored.
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_FLPNT( GRP__MAXG ) = INTEGER (Read and Write)
*           Pointers to the mapped FILES array of each group.
*        CMN_FLSIZ( GRP__MAXG ) = INTEGER (Read and Write)
*           The Ccurrent size of each groups FILES array.

*  Arguments Given:
      INTEGER SLOT
      CHARACTER FILE*(*)

*  Arguments Returned:
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the new size of the character array, and return the index at
*  which the new name is stored.
      CMN_FLSIZ( SLOT ) = CMN_FLSIZ( SLOT ) + 1
      INDEX = CMN_FLSIZ( SLOT )

*  If necessary, create the FILES array.
      IF( INDEX .EQ. 1 ) THEN

*  Get a pointer to the character array.  Each element of the array
*  contains CMN_SZNAM characters.
         CALL PSX_CALLOC( GRP__SZNAM, '_CHAR', CMN_FLPNT( SLOT ),
     :                    STATUS )

         IF( STATUS .NE. SAI__OK ) GO TO 999

*  If the array already exists, increase its size by 1.
      ELSE

*  Extend the memory mapped to the character data.
         CALL PSX_REALLOC( INDEX*GRP__NBC*GRP__SZNAM,
     :                     CMN_FLPNT( SLOT ), STATUS )

      END IF

*  Store the supplied file name in the new element of the array.  NB,
*  the final argument specifies the length of each character string in
*  the mapped array, and is required by UNIX. There is no corresponding
*  dummy argument in the code for GRP1_SETC.
      CALL GRP1_SETC( INDEX, INDEX, INDEX,
     :                %VAL( CNF_PVAL( CMN_FLPNT( SLOT ) ) ),
     :                FILE, STATUS, %VAL( CNF_CVAL( GRP__SZNAM ) ) )

 999  CONTINUE

      END
