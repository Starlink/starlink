      SUBROUTINE GRP1_IPURG( UPPER, SIZE1, NAMES1, MODGP1, MODGI1,
     :                       LEVEL1, IFILE1, GSIZE1, SIZE2, NAMES2,
     :                       MODGP2, MODGI2, LEVEL2, IFILE2, GSIZE2,
     :                       STATUS )
*+
*  Name:
*     GRP1_IPURG

*  Purpose:
*     Purge duplicate names from a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_IPURG( UPPER, SIZE1, NAMES1, MODGP1, MODGI1, LEVEL1,
*                      IFILE1, GSIZE1, SIZE2, NAMES2, MODGP2, MODGI2,
*                      LEVEL2, IFILE2, GSIZE2, STATUS )

*  Description:
*     Names and supplementary information are copied from the input
*     group to the output group. A check is made before copying each
*     name that the same name has not already been included in the
*     output group. If it has, the name is not copied. The comparison
*     may be case sensitive or case insensitive, depending on the
*     argument UPPER.

*  Arguments:
*     UPPER = LOGICAL (Given)
*        If true then the string comparisons are case insensitive.
*     SIZE1 = INTEGER (Given)
*        The size of the arrays holding information about the input
*        group.
*     NAMES1( SIZE1 ) = CHARACTER * ( * ) (Given)
*        The names forming the input group.
*     MODGP1( SIZE1 ) = INTEGER (Given)
*        The GRP slot numbers of the groups used as a basis for all
*        names in the input group which were created as a result of a
*        modification element.
*     MODGI1( SIZE1 ) = INTEGER (Given)
*        The indices within the groups specified by MODGP1 of the names
*        used as a basis for all names in the input group which were
*        created as a result of a modification element.
*     LEVEL1( SIZE1 ) = INTEGER (Given)
*        The indirection depth at which each name in the input group
*        was specified. Zero should be given if the name was given
*        directly, instead of by an indirection element.
*     IFILE1( SIZE1 ) = INTEGER (Given)
*        The FILE_INDEX array for the input group.
*     GSIZE1 = INTEGER (Given)
*        The input group size. This can be smaller than the size of the
*        arrays used to hold the group.
*     SIZE2 = INTEGER (Given)
*        The size of the arrays holding information about the output
*        group.
*     NAMES2( SIZE2 ) = CHARACTER * ( * ) (Returned)
*        The names forming the output group.
*     MODGP2( SIZE2 ) = INTEGER (Returned)
*        The GRP slot numbers of the groups used as a basis for all
*        names in the output group which were created as a result of a
*        modification element.
*     MODGI2( SIZE2 ) = INTEGER (Returned)
*        The indices within the groups specified by MODGP2 of the names
*        used as a basis for all names in the output group which were
*        created as a result of a modification element.
*     LEVEL2( SIZE2 ) = INTEGER (Returned)
*        The indirection depth at which each name in the output group
*        was specified. Zero is returned if the name was given
*        directly, instead of by an indirection element.
*     IFILE2( SIZE2 ) = INTEGER (Given)
*        The FILE_INDEX array for the output group.
*     GSIZE2 = INTEGER (Returned)
*        The output group size. This can be smaller than the size of the
*        arrays used to hold the group.
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

*  Arguments Given:
      LOGICAL UPPER
      INTEGER SIZE1
      CHARACTER NAMES1( SIZE1 )*(*)
      INTEGER MODGP1( SIZE1 )
      INTEGER MODGI1( SIZE1 )
      INTEGER LEVEL1( SIZE1 )
      INTEGER IFILE1( SIZE1 )
      INTEGER GSIZE1
      INTEGER SIZE2

*  Arguments Returned:
      CHARACTER NAMES2( SIZE2 )*(*)
      INTEGER MODGP2( SIZE2 )
      INTEGER MODGI2( SIZE2 )
      INTEGER LEVEL2( SIZE2 )
      INTEGER IFILE2( SIZE2 )
      INTEGER GSIZE2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! True if strings are equal except for
                                 ! case.

*  Local Variables:
      LOGICAL FOUND              ! True if input name has already been
                                 ! included in the output group.
      INTEGER I                  ! Index into input group.
      INTEGER J                  ! Index into output group.
      CHARACTER NAME*(GRP__SZNAM)! Name from input group.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the size of the output group to zero.
      GSIZE2 = 0

*  Loop round each name in the input group.
      DO I = 1, GSIZE1
         NAME = NAMES1( I )

*  Search through the names already added to the output group, to see if
*  a match can be found for the current name.
         FOUND = .FALSE.
         DO J = 1, GSIZE2

*  Compare strings with or without case sensitivity, depending on the
*  argument UPPER.
            IF ( UPPER ) THEN
               IF( CHR_SIMLR( NAMES2( J ), NAME ) ) FOUND = .TRUE.
            ELSE
               IF ( NAMES2( J ) .EQ. NAME ) FOUND = .TRUE.
            END IF
         END DO

*  If the name has not already been included in the output group, add it
*  to the output group (together with supplementary information).
         IF( .NOT. FOUND ) THEN
            GSIZE2 = GSIZE2 + 1
            NAMES2( GSIZE2 ) = NAME
            MODGP2( GSIZE2 ) = MODGP1( I )
            MODGI2( GSIZE2 ) = MODGI1( I )
            LEVEL2( GSIZE2 ) = LEVEL1( I )
            IFILE2( GSIZE2 ) = IFILE1( I )
         END IF

      END DO

      END
