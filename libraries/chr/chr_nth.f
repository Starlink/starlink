      CHARACTER * 2 FUNCTION CHR_NTH( IVALUE )
*+
*  Name:
*     CHR_NTH

*  Purpose:
*     Return the two-character ordinal abbreviation for a specified integer.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_NTH( IVALUE )

*  Description:
*     Return the two character ordinal abbreviation (i.e. st, nd, rd, th)
*     appropriate for the given integer value.

*  Arguments:
*     IVALUE = INTEGER (Given)
*        The integer for which the abbreviation is required.

*  Returned Value:
*     CHR_NTH = CHARACTER * 2
*        The appropriate two character abbreviation for the given
*        integer value.

*  Copyright:
*     Copyright (C) 1985, 1991, 1994 Science & Engineering Research Council.
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
*     KS: Keith Shortridge (AAO)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1985 (KS):
*        Original version.
*     6-FEB-1991 (PCTR):
*        Converted from Figaro to CHR_.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER IVALUE

*  Local Variables:
      INTEGER TENS               ! Factor of ten
      INTEGER UNITS              ! Digit: between 0 and 9

      CHARACTER * 2 TABLE( 0 : 3 ) ! Table of abbreviations

*  Local Data:
      DATA TABLE / 'th', 'st', 'nd', 'rd' /

*.

*  Determine the value of the least significant digit and the factor of
*  ten for the given integer.
      TENS = MOD( ABS( IVALUE ), 100 )
      UNITS = MOD( TENS, 10 )
      TENS = TENS - UNITS

*  Determine the appropriate abbreviation.
      IF ( ( TENS .EQ. 10 ) .OR. ( UNITS .GT. 3 ) ) THEN
         CHR_NTH = 'th'
      ELSE
         CHR_NTH = TABLE( UNITS )
      END IF

      END
