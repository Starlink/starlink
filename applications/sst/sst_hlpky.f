      SUBROUTINE SST_HLPKY( STR, STATUS )
*+
*  Name:
*     SST_HLPKY

*  Purpose:
*     Make a help library key out of a supplied character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_HLPKY( STR, STATUS )

*  Description:
*     The routine modifies the character string supplied so that it
*     makes a suitable help library key. It is intended to be applied
*     to parameter names and section headings extracted from routine
*     prologues. It performs its task by removing any parenthesised
*     part of the string (e.g. a dimensionality specification on the
*     end of a parameter name) and then replaces all embedded blanks by
*     underscores '_'.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given and Returned)
*        The string to be modified.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     14-AUG-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given and Returned:
      CHARACTER * ( * ) STR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      INTEGER F                  ! Position of opening parenthesis
      INTEGER I                  ! Loop counter for characters
      INTEGER L                  ! Position of closing parenthesis
      INTEGER NC                 ! Number of characters

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop to identify any parenthesised expressions in the string,
*  replacing them with blanks until no more are found.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      CALL SST_FPARX( STR, F, L )
      IF ( F .LE. L ) THEN
         STR( F : L ) = ' '
         GO TO 1
      END IF

*  Find the remaining string length.
      NC = CHR_LEN( STR )

*  Loop to replace embedded blanks with underscores.
      DO 2 I = 1, NC
         IF ( STR( I : I ) .EQ. ' ' ) THEN
            STR( I : I ) = '_'
         END IF
2     CONTINUE

      END
* @(#)sst_hlpky.f   1.1   94/12/05 11:31:28   96/07/05 10:27:31
