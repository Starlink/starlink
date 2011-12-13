      SUBROUTINE GRP1_IPUT( INDEX, SIZE, NAMES, LEVELS, IFILES, MODGPS,
     :                      MODINS, NAME, LEVEL, IFILE, MODGP, MODIN,
     :                      STATUS )
*+
*  Name:
*     GRP1_IPUT

*  Purpose:
*     Store group information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_IPUT( INDEX, SIZE, NAMES, LEVELS, IFILES, MODGPS,
*                     MODINS, NAME, LEVEL, IFILE, MODGP, MODIN, STATUS )

*  Description:
*     This routine stores the given information relating to a member of
*     a group, in the corresponding GROUP structure arrays.

*  Arguments:
*     INDEX = INTEGER (Given)
*        The index within the group at which the information is to be
*        stored.
*     SIZE = INTEGER (Given)
*        The size of the GROUP structure arrays.
*     NAMES(SIZE) = CHARACTER * ( * ) (Given and Returned)
*        The NAMES array.
*     LEVELS(SIZE) = INTEGER (Given and Returned)
*        The LEVEL array.
*     IFILES(SIZE) = INTEGER (Given and Returned)
*        The FILE_INDEX  array.
*     MODGPS(SIZE) = INTEGER (Given and Returned)
*        The MOD_GROUP  array.
*     MODINS(SIZE) = INTEGER (Given and Returned)
*        The MOD_INDEX  array.
*     NAME = CHARACTER * ( * ) (Given)
*        The text to be stored in the NAMES array.
*     LEVEL = INTEGER (Given)
*        The indirection depth at which the name was specified. Zero
*        should be given if the name was given directly, instead of by
*        an indirection element.
*     IFILE = INTEGER (Given)
*        The index within the FILES array (see routine GRP1_PTIND) at
*        which the name of the indirection file in which the name was
*        specified is stored. A value of zero should be given if the
*        name was given directly, instead of by an indirection element.
*     MODGP = INTEGER (Given)
*        The GRP identifier for the group used as a basis for the name
*        if it was created as a result of a modification element. A
*        value of GRP__NOID should be given if the name was not created
*        as a result of a modification element.
*     MODIN = INTEGER (Given)
*        The index within the group specified by MODGP, of the name
*        used as a basis for the name given by argument NAME.  If MODGP
*        is given as GRP__NOID, then MODIN is ignored.
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
      INCLUDE 'GRP_ERR'          ! GRP error values.
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants

*  Arguments Given:
      INTEGER INDEX
      CHARACTER NAME*(*)
      INTEGER LEVEL
      INTEGER IFILE
      INTEGER MODGP
      INTEGER MODIN
      INTEGER SIZE

*  Arguments Given and Returned:
      CHARACTER NAMES(SIZE)*(*)
      INTEGER LEVELS(SIZE)
      INTEGER IFILES(SIZE)
      INTEGER MODGPS(SIZE)
      INTEGER MODINS(SIZE)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the index is out of bounds, report an error.
      IF( INDEX .LE. 0 .OR. INDEX .GT. SIZE ) THEN
         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'IND', INDEX )
         CALL MSG_SETI( 'SIZ', SIZE )
         CALL ERR_REP( 'GRP1_IPUT_ERR1',
     :         'GRP1_IPUT: Array index (^IND) out of bounds [1,^SIZ].',
     :                 STATUS )

*  Otherwise, store the information.
      ELSE
         NAMES( INDEX ) = NAME
         LEVELS( INDEX ) = LEVEL
         IFILES( INDEX ) = IFILE
         MODGPS( INDEX ) = MODGP
         MODINS( INDEX ) = MODIN

      END IF

      END
