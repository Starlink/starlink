      SUBROUTINE NDF1_PSNDB( STR, DEF, VALUE, ISPIX, STATUS )
*+
*  Name:
*     NDF1_PSNDB

*  Purpose:
*     Parse an NDF dimension bound.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PSNDB( STR, DEF, VALUE, ISPIX, STATUS )

*  Description:
*     The routine parses a string representing an upper or lower
*     dimension bound of an NDF section. If the string is blank, then a
*     default value is returned. Leading and trailing spaces are
*     ignored.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String to be parsed.
*     DEF = DOUBLE PRECISION (Given)
*        Default value to be returned if the string is blank.
*     VALUE = DOUBLE PRECISION (Returned)
*        Dimension bound value.
*     ISPIX = LOGICAL (Returned)
*        Whether the value returned is to be interpreted as a
*        pixel-index or an axis coordinate value; .TRUE. ==> pixel
*        index, .FALSE. ==> axis value. (The value is returned .TRUE.
*        if an integer format number is found and .TRUE. if floating
*        point. A value of .TRUE. is returned if the string is blank.)
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Find the first and last non-blank characters in the string.
*     -  If the input string is blank, then return the default value.
*     -  Otherwise, attempt to convert the string to a double precision
*     value.
*     -  If the attempt fails, then report an error message.
*     -  Otherwise, determine whether integer format was used.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1991 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      DOUBLE PRECISION DEF

*  Arguments Returned:
      DOUBLE PRECISION VALUE
      LOGICAL ISPIX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER L                  ! Position of last non-blank character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the string.
      CALL CHR_FANDL( STR, F, L )

*  If the input string is blank, then return the default value.
      IF ( F .GT. L ) THEN
         VALUE = DEF
         ISPIX = .TRUE.

*  Otherwise, attempt to convert the string to a double precision
*  value.
      ELSE
         CALL CHR_CTOD( STR( F : L ), VALUE, STATUS )

*  If the attempt fails, then report an error message.
         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = NDF__BNDIN
            CALL MSG_SETC( 'BADBOUND', STR( F : L ) )
            CALL ERR_REP( 'NDF1_PSNDB_SYN',
     :                    'Invalid NDF dimension bound ' //
     :                    '''^BADBOUND'' specified; bad syntax.',
     :                    STATUS )

*  Otherwise, determine whether integer format was used.
         ELSE
            ISPIX = ( ( INDEX( STR( F : L ), '.' ) .EQ. 0 ) .AND.
     :                ( INDEX( STR( F : L ), 'E' ) .EQ. 0 ) .AND.
     :                ( INDEX( STR( F : L ), 'e' ) .EQ. 0 ) .AND.
     :                ( INDEX( STR( F : L ), 'D' ) .EQ. 0 ) .AND.
     :                ( INDEX( STR( F : L ), 'd' ) .EQ. 0 ) )
         END IF
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PSNDB', STATUS )

      END
