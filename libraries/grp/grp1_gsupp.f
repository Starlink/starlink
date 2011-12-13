      SUBROUTINE GRP1_GSUPP( INDEX, SIZE, LEVELS, IFILES, MODGPS,
     :                       MODINS, LEVEL, IFILE, MODGP, MODIN,
     :                       STATUS )
*+
*  Name:
*     GRP1_GSUPP

*  Purpose:
*     Retrieve group supplementary information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_GSUPP( INDEX, SIZE, LEVELS, IFILES, MODGPS, MODINS,
*                      LEVEL, IFILE, MODGP, MODIN, STATUS )

*  Description:
*     This routine retrieves the supplementary information relating to
*     a member of a group, from the corresponding GROUP arrays.

*  Arguments:
*     INDEX = INTEGER (Given)
*        The index within the group from which the information is to be
*        retrieved.
*     SIZE = INTEGER (Given)
*        The size of the GROUP arrays.
*     LEVELS(SIZE) = INTEGER (Given)
*        The LEVEL array.
*     IFILES(SIZE) = INTEGER (Given)
*        The FILE_INDEX array.
*     MODGPS(SIZE) = INTEGER (Given)
*        The MOD_GROUP  array.
*     MODINS(SIZE) = INTEGER (Given)
*        The MOD_INDEX  array.
*     LEVEL = INTEGER (Returned)
*        The indirection depth at which the name was specified. Zero
*        is returned if the name was given directly, instead of by
*        an indirection element.
*     IFILE = INTEGER (Returned)
*        The index within the FILES array (see routine GRP1_PTIND) at
*        which the name of the indirection file in which the name was
*        specified is stored. A value fo zero is returned if the name
*        was given directly, instead of by an indirection element.
*     MODGP = INTEGER (Returned)
*        The GRP slot number of the group used as a basis for the name
*        if it was created as a result of a modification element. A
*        value of zero is returned if the name was not created as a
*        result of a modification element.
*     MODIN = INTEGER (Returned)
*        The index within the group specified by MODGP, of the name
*        used as a basis for the name returned by argument NAME.
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

*  Arguments Given:
      INTEGER INDEX
      INTEGER SIZE
      INTEGER LEVELS(SIZE)
      INTEGER IFILES(SIZE)
      INTEGER MODGPS(SIZE)
      INTEGER MODINS(SIZE)

*  Arguments Returned:
      INTEGER LEVEL
      INTEGER IFILE
      INTEGER MODGP
      INTEGER MODIN

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the index is out of bounds, return null information.
      IF( INDEX .LE. 0 .OR. INDEX .GT. SIZE ) THEN
         LEVEL = 0
         IFILE = 0
         MODGP = 0
         MODIN = 0

*  Otherwise, retrieve the information from the GROUP arrays.
      ELSE
         LEVEL = LEVELS( INDEX )
         IFILE = IFILES( INDEX )
         MODGP = MODGPS( INDEX )
         MODIN = MODINS( INDEX )

      END IF

      END
