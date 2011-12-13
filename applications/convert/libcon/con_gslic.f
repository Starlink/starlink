      SUBROUTINE CON_GSLIC( NAME, SLICE, STATUS )
*+
*  Name:
*     CON_GSLIC

*  Purpose:
*     Find and remove any NDF slice specification from a name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_GSLIC( NAME, SLICE, STATUS )

*  Description:
*     A slice specification is taken to be anything between the first
*     opening parenthesis and the first closing parenthesis. No check is
*     made that these parenthesise occur at the same level of nesting.
*     That is, if NAME='(A,(B,C))' is supplied, the returned string
*     would be SLICE='(A,(B,C)'.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given and Returtned)
*        The name to be checked. On exit, any NDF slice specification
*        contained in the name on entry, any remaining text to the right
*        of the slice specifier is shifted to the left in order to
*        remove the slice specified.
*     SLICE = CHARACTER * ( * ) (Returned)
*        The slice specification including opening and closing
*        parenthesise. If the input name contains no slice
*        specification, then SLICE is returned blank.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1992 (DSB):
*        Original version.
*     1994 August 7 (MJC):
*        Renamed for use with CONVERT.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given and Returned:
      CHARACTER NAME*(*)

*  Arguments Returned:
      CHARACTER SLICE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.
*  Local Variables:
      INTEGER NAMLEN             ! Used length of original name.
      INTEGER START              ! Position of the first opening
                                 ! parenthesis in the original name.
      INTEGER STOP               ! Position of the first closing
                                 ! parenthesis in the original name.
      CHARACTER TAIL*(GRP__SZNAM)! Text found to the right of the slice
                                 ! specifier.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned slice specifier.
      SLICE = ' '

*  Store the used length of the input name.
      NAMLEN = CHR_LEN( NAME )

*  Search for the first opening parenthesis.
      START = INDEX( NAME, '(' )

*  If one was found, search for the first closing parenthesis following
*  the opening parenthesis just found.
      IF( START .NE. 0 ) THEN
         STOP = INDEX( NAME( START: ), ')' )

*  If one was found, calculate the offset of the closing parenthesis
*  from the start of the string, rather than from the opening
*  parenthesis.
         IF( STOP .NE. 0 ) THEN
            STOP = STOP + START - 1

*  Extract the slice specifier from the supplied name.
            SLICE = NAME( START:STOP )

*  Store any text  occuring to the right of the slice specifier.
            IF( STOP .LT. NAMLEN ) THEN
               TAIL = NAME( STOP + 1 : )
            ELSE
               TAIL = ' '
            END IF

*  Move it so that it starts where the slice specifier used to start.
            NAME( START : ) = TAIL

         END IF

      END IF

      END
