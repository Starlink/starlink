      SUBROUTINE DIMLST( NDIMS, DIMS, NCHDIM, DIMSTR, STATUS )
*+
*  Name:
*     DIMLST

*  Purpose:
*     Writes the dimensions of an array in a shorthand form.

*  Language:
*     Starlink

*  Invocation:
*     CALL DIMLST( NDIMS, DIMS, NCHDIM, DIMSTR, STATUS )

*  Description:
*     Returns a character variable containing the dimensions of
*     an array separated by commas and bounded by parentheses,
*     unless there is only one dimension when no parentheses and
*     comma are included. This enables the calling routine to require
*     only one message call.

*  Arguments:
*     NDIMS  =  INTEGER( READ )
*        Number of dimensions of array
*     DIMS ( NDIMS )  =  INTEGER( READ )
*        Dimensions of the array
*     NCHDIM  =  INTEGER( WRITE )
*        Number of characters in DIMSTR less trailing blanks
*     DIMSTR  =  CHARACTER*(*)( WRITE )
*        Character string of dimension list enclosed in parentheses
*     STATUS  = INTEGER( READ, WRITE )
*        Global status value

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Initialise character counter and output string to blanks
*     If only one dimension then
*        Convert integer to character
*     Else
*        For each dimension
*           Convert integer dimension to characters
*           If first dimension then
*              Put parenthesis in output string and increment counter
*           Endif
*           Append dimension to list; if last dimension append a
*             parenthesis otherwise a comma to separate each dimension
*           Increment character counter
*        Endfor
*     Endif
*     Return

*  Copyright:
*     Copyright (C) 1986, 1989 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*     {enter_new_authors_here}

*  History:
*     1986 Aug 8  : Original (RL.STAR::CUR).
*     1989 Mar 19 : Used CHR_APPND for in-situ concatenation
*        (RL.STAR::CUR).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! global SSE parameters


*  Status:
      INTEGER STATUS


*  Arguments Given:
      INTEGER
     :    NDIMS,
     :    DIMS( NDIMS )


*  Arguments Returned:
      INTEGER NCHDIM

      CHARACTER*(*) DIMSTR


*  Local Variables:
      INTEGER
     :    NCHAR,         ! Number of characters in a (decimal) dimension
     :    I              ! Loop counter

      CHARACTER*7
     :    CI             ! A dimension in characters


*.

*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Initialise string and character count

      DIMSTR = ' '
      NCHDIM = 0

*    If only one dimension just convert the dimension to a character
*    string

      IF ( NDIMS .EQ. 1 ) THEN

         CALL CHR_ITOC( DIMS( 1 ), DIMSTR, NCHDIM )

      ELSE

*       Loop for each dimension

         DO  I = 1, NDIMS

*       Convert dimension to a character string

            CALL CHR_ITOC( DIMS( I ), CI, NCHAR )

*       First dimension - precede by parenthesis

            IF ( I .EQ. 1 ) THEN
               DIMSTR( 1:2 ) = '( '
               NCHDIM = 2
            END IF

*          Append dimension to list. If last dimension append a
*          parenthesis otherwise a comma to separate each dimension.
*          CHR_APPND does not count trailing spaces so add one column
*          to the count for all but the last dimension.

            IF ( I .EQ. NDIMS ) THEN
               CALL CHR_APPND ( CI( 1:NCHAR )//' )', DIMSTR, NCHDIM )
            ELSE
               CALL CHR_APPND( CI( 1:NCHAR )//', ', DIMSTR, NCHDIM )
               NCHDIM = NCHDIM + 1
            END IF
         END DO
      END IF

 999  CONTINUE

      END
