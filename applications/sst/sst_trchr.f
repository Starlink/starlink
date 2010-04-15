      SUBROUTINE SST_TRCHR( FROM, TO, STR )
*+
*  Name:
*     SST_TRCHR

*  Purpose:
*     Translate characters in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_TRCHR( FROM, TO, STR )

*  Description:
*     This routine uses a character translation table to convert
*     selected characters in a string into specified different
*     characters. Any characters not appearing in the translation table
*     are left unchanged.

*  Arguments:
*     FROM = CHARACTER * ( * ) (Given)
*        A string composed of the characters to be translated.
*     TO = CHARACTER * ( * ) (Given)
*        A string composed of the translation values for each of the
*        characters in the FROM argument. The lengths of the FROM and
*        TO arguments must be the same.
*     STR = CHARACTER * ( * ) (Given and Returned)
*        The string to be translated. Any character matching one of the
*        characters specified in the FROM argument is converted to the
*        corresponding character specified in the TO argument. All
*        other characters are left unchanged.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-AUG-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) FROM
      CHARACTER * ( * ) TO

*  Arguments Given and Returned:
      CHARACTER * ( * ) STR

*  Local Variables:
      INTEGER I                  ! Loop counter for string characters
      INTEGER IT                 ! Position in translation table

*.

      DO 1 I = 1, LEN( STR )
         IT = INDEX( FROM, STR( I : I ) )
         IF ( IT .NE. 0 ) STR( I : I ) = TO( IT : IT )
1     CONTINUE

      END
* @(#)sst_trchr.f   1.1   94/12/05 11:31:35   96/07/05 10:27:32
