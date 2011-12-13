      SUBROUTINE GRP1_GTELM( SLOT, INDEX, NAME, LEVEL, IFILE, MODGP,
     :                       MODIN, STATUS )
*+
*  Name:
*     GRP1_GTELM

*  Purpose:
*     Get an element from a group, including supplementary information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_GTELM( SLOT, INDEX, NAME, LEVEL, IFILE, MODGP,
*                      MODIN, STATUS )

*  Description:
*     This routine retrieves the information describing a given element
*     from the group identified by SLOT. If the supplied index is
*     outside the bounds of the group, a blank value is returned for
*     NAME.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The GRP slot number for the group from which information is to
*        be retrieved.
*     INDEX = INTEGER (Given)
*        The index within the group of the element which is to be
*        retrieved.
*     NAME = CHARACTER * ( * ) (Returned)
*        The text retrieved from the NAMES array.
*     LEVEL = INTEGER (Returned)
*        The indirection depth at which the name was specified. Zero
*        is returned if the name was given directly, instead of by
*        an indirection element.
*     IFILE = INTEGER (Returned)
*        The index within the FILES array at which the name of the
*        indirection file in which the name was specified is stored. A
*        value of zero is returned if the name was given directly,
*        instead of by an indirection element.
*     MODGP = INTEGER (Returned)
*        A GRP identifier of the group used as a basis for the name if
*        it was created as a result of a modification element. A value
*        of GRP__NOID is returned if the name was not created as a
*        result of a modification element.
*     MODIN = INTEGER (Returned)
*        The index within the group specified by MODGP, of the name
*        used as a basis for the returned name. A value of zero is
*        returned if the name was not created as a result of a
*        modification element.
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
*        CMN_NMPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped NAMES array of each group.
*        CMN_MGPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped MOD_GROUP array of each group.
*        CMN_MIPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped MOD_INDEX array of each group.
*        CMN_LVPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped LEVEL array of each group.
*        CMN_INPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped FILE_INDEX array of each group.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER SLOT
      INTEGER INDEX

*  Arguments Returned:
      CHARACTER NAME*(*)
      INTEGER LEVEL
      INTEGER IFILE
      INTEGER MODGP
      INTEGER MODIN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the index is within the bounds of the group...
      IF( INDEX .GT. 0 .AND. INDEX .LE. CMN_GSIZE( SLOT ) ) THEN

*  Get the name from the NAMES array. NB, the final argument specifies
*  the length of each character string in the mapped NAMES array, and
*  is required by UNIX. There is no corresponding dummy argument in the
*  code for GRP1_GETC.
         CALL GRP1_GETC( CMN_GSIZE( SLOT ),
     :                   %VAL( CNF_PVAL( CMN_NMPNT( SLOT ) ) ),
     :                   INDEX, NAME, STATUS,
     :                   %VAL( CNF_CVAL( GRP__SZNAM ) ) )

*  Get the supplementary information.
         CALL GRP1_GSUPP( INDEX, CMN_GSIZE( SLOT ),
     :             %VAL( CNF_PVAL( CMN_LVPNT( SLOT ) ) ),
     :             %VAL( CNF_PVAL( CMN_INPNT( SLOT ) ) ),
     :             %VAL( CNF_PVAL( CMN_MGPNT( SLOT ) ) ),
     :             %VAL( CNF_PVAL( CMN_MIPNT( SLOT ) ) ),
     :             LEVEL, IFILE, MODGP, MODIN, STATUS )

*  If the index was out of bounds, return null information.
      ELSE
         NAME = ' '
         LEVEL = 0
         IFILE = 0
         MODGP = GRP__NOID
         MODIN = 0

      END IF

      END
