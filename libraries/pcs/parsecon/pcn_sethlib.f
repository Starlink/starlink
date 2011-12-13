      SUBROUTINE PARSECON_SETHLIB ( ENTRY, STATUS )
*+
*  Name:
*     PARSECON_SETHLIB

*  Purpose:
*     Sets-up parameter help library specifier.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETHLIB( ENTRY, STATUS )

*  Description:
*     Loads the provided string into the HLBSTR common store.
*     Also sets LIBSET and HLBLEN.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        helplib string
*     STATUS=INTEGER

*  Algorithm:
*     Superfluous quotes are removed from the given string, its used
*     length is found. The results are entered in the common block.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     A. J. Chipperfield (RLVAD::AJC)
*     {enter_new_authors_here}

*  History:
*     04.07.1990:  Original (RLVAD::AJC)
*     17.10.1990:  define QUOTE portably (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      CHARACTER*(*) ENTRY             ! the help string


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'PARSECON2_CMN'


*  Local Constants:
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )

*    External Routines :
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   If the helplib text is a quoted string, process the quotes
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN
         CALL STRING_STRIPQUOT ( ENTRY, HLBSTR, STATUS )

      ELSE
         HLBSTR = ENTRY

      ENDIF

*   Find the used length
      HLBLEN = CHR_LEN( HLBSTR )

      END
