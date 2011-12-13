      SUBROUTINE KPG_DIMLS( NDIMS, DIMS, NCHDIM, DIMSTR, STATUS )
*+
*  Name:
*     KPG_DIMLS

*  Purpose:
*     Writes the dimensions of an array in standard notation.

*  Language:
*     Starlink

*  Invocation:
*     CALL KPG_DIMLS( NDIMS, DIMS, NCHDIM, DIMSTR, STATUS )

*  Description:
*     This routine returns a character variable containing the
*     dimensions of an array in the usual notation, that is the
*     dimensions separated by commas and bounded by parentheses.
*     There is an exception; if there is only one dimension, no
*     parentheses and comma are included.  This enables calling
*     code to report the dimensions of an array using only one
*     message call.

*  Arguments:
*     NDIMS = INTEGER (Given)
*        Number of dimensions of the array.
*     DIMS( NDIMS ) = INTEGER (Given)
*        The dimensions of the array.
*     NCHDIM  =  INTEGER (Write)
*        The number of characters in DIMSTR, excluding trailing blanks.
*     DIMSTR  =  CHARACTER*(*) (Write)
*        A character string containing the dimension list enclosed in
*        parentheses.
*     STATUS  = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1986, 1989 Science & Engineering Research Council.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1986 August 8 (MJC): 
*        Original version.
*     1989 March 19 (MJC):
*        Used CHR_APPND for in-situ concatenation.
*     2010 August 25 (MJC):
*        Used modern coding and prologue style.  Renamed to include
*        package prefix.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE parameters

*  Status:
      INTEGER STATUS

*  Arguments Given:
      INTEGER NDIMS
      INTEGER DIMS( NDIMS )

*  Arguments Returned:
      INTEGER NCHDIM
      CHARACTER*(*) DIMSTR

*  Local Variables:
      CHARACTER*7 CI             ! A dimension in characters
      INTEGER I                  ! Loop counter
      INTEGER NCHAR              ! Number of characters in a (decimal)
                                 ! dimension

*.

*  Initialise returned string and character count.
      DIMSTR = ' '
      NCHDIM = 0

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  If only one dimension is present, just convert the dimension to a
*  character string.
      IF ( NDIMS .EQ. 1 ) THEN
         CALL CHR_ITOC( DIMS( 1 ), DIMSTR, NCHDIM )

      ELSE

*  Loop for each dimension.
         DO  I = 1, NDIMS

*  Convert the dimension to a character string.
            CALL CHR_ITOC( DIMS( I ), CI, NCHAR )

*  First dimension is preceded by a left parenthesis.
            IF ( I .EQ. 1 ) THEN
               DIMSTR( 1:2 ) = '( '
               NCHDIM = 2
            END IF

*  Append the dimension to list.  If it's last dimension, append a
*  a closing parenthesis; otherwise append a comma to separate each
*  dimension.  CHR_APPND does not count trailing spaces so add one
*  column to the count for all but the last dimension.
            IF ( I .EQ. NDIMS ) THEN
               CALL CHR_APPND( CI( 1:NCHAR )//' )', DIMSTR, NCHDIM )
            ELSE
               CALL CHR_APPND( CI( 1:NCHAR )//', ', DIMSTR, NCHDIM )
               NCHDIM = NCHDIM + 1
            END IF
         END DO
      END IF

 999  CONTINUE

      END
