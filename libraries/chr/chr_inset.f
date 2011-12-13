      LOGICAL FUNCTION CHR_INSET( SET, STRING )
*+
*  Name:
*     CHR_INSET

*  Purpose:
*     Return whether a string is a member of a given set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_INSET( SET, STRING )

*  Description:
*     The character string is compared with each of the values
*     given in the given set.
*     The strings in the set can be any length and can differ
*     in length throughout the set. Each value is separated by a
*     comma. Upper and lowercase are treated as being equivalent and
*     trailing blanks are ignored.

*  Arguments:
*     SET = CHARACTER * ( * ) (Given)
*        The set of character values. It takes the form
*        'string1,string2,.......,stringN' where each of the
*        substring values from string1 to stringN can be of different
*        lengths.
*     STRING = CHARACTER * ( * ) (Given)
*        The character string to be checked for membership of the set.

*  Returned Value:
*     CHR_INSET = LOGICAL
*        Returns .TRUE. if the character string is a member of the
*        given set, returns .FALSE. otherwise.

*  Algorithm:
*     Set the default return value to .FALSE.
*     Find out the stripped lengths of STRING and SET.
*     Initialise the current string start position
*     Initialise the END logical to .FALSE.
*     Do until the end of the set or CHR_INSET = .TRUE.
*        Find position of next comma relative to current string
*        start position.
*        If no more commas then
*           Set comma position to point to the end of the set.
*           Set END to .TRUE.
*        endif
*        CHR_INSET = .TRUE. if the substring of SET between CURR and
*        COMMA-1 matches the given string, STRING.
*        Work out next CURR position from COMMA+1.
*     enddo

*  Copyright:
*     Copyright (C) 1983, 1984, 1985, 1986, 1994 Science & Engineering Research Council.
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
*     ASOC5: Dave Baines (ROE)
*     MJM: Mark McCaughrean (UoE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1983 (ASOC5):
*        Original version.
*     17-FEB-1984 (ASOC5):
*        Documentation brought up to standard.
*     11-DEC-1985 (MJM):
*        Modified considerably to allow the various members of SET
*        to be any and different lengths.
*     17-SEP-1986 (MJM):
*        Changed from INSET to CHR_INSET.
*     2-SEP-1986 (AJC):
*        Remove INCLUDE 'SAE_PAR'.
*        Re-format documentation.
*        Replace CHR_INDEX with INDEX.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER SET * ( * )
      CHARACTER STRING * ( * )

*  External References:
      LOGICAL CHR_SIMLR          ! Whether two strings match

      INTEGER CHR_LEN            ! Length of string (ignoring trailing blanks)

*  Local Variables:
      LOGICAL END                ! True if end of set has been reached

      INTEGER COMMA              ! Offset of next comma in SET
      INTEGER CURR               ! Position of next character in SET
      INTEGER SETLEN             ! Length of given set
      INTEGER STRLEN             ! Length of given string

*.

*  Set up default return value.
      CHR_INSET = .FALSE.

*  Get the length of the set, ignoring trailing blanks.
      SETLEN = CHR_LEN( SET )

*  Get the length of the string, ignoring trailing blanks.
      STRLEN = CHR_LEN( STRING )

*  Initialise the current position pointer - points to the first
*  character of the next string.
      CURR = 1

*  Set up the end-of-set marker.
      END = .FALSE.

*  Loop to search the given set - continue until end of set is reached,
*  or the string is matched.
*  DO WHILE loop.
 10   CONTINUE
      IF ( ( .NOT. END ) .AND. ( .NOT. CHR_INSET ) ) THEN

*     Find the position of the next comma relative to the position
*     of CURR, i.e. how many characters away it is from CURR.
         COMMA = INDEX( SET( CURR : SETLEN ), ',' )

*     If this value is zero, then there are no more commas in the
*     set, so set COMMA to be the number of characters left from
*     the current position of CURR to the end of the SET plus 2,
*     Also, set END to be .TRUE.
         IF( COMMA .EQ. 0 ) THEN
            COMMA = SETLEN - CURR + 2
            END = .TRUE.
         END IF

*     Now test to see whether the string between CURR and COMMA-1
*     matches up to the value of STRING - if so, INSET = .TRUE.
         CHR_INSET = CHR_SIMLR( SET( CURR : CURR+COMMA-2 ),
     :                          STRING( 1 : STRLEN ) )

*     Update the position of CURR for next pass through loop
         CURR = CURR + COMMA
      GO TO 10
      END IF

      END
