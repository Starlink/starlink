      SUBROUTINE CHR_ITOC( IVALUE, STRING, NCHAR )
*+
*  Name:
*     CHR_ITOC

*  Purpose:
*     Encode an INTEGER value as a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_ITOC( IVALUE, STRING, NCHAR )

*  Description:
*     Encode an integer value as a (decimal) character string, using as
*     concise a format as possible, and return the number of characters
*     used. In the event of an error, '*'s will be written into to the
*     string.

*  Arguments:
*     IVALUE = INTEGER (Given)
*        The value to be encoded.
*     STRING = CHARACTER * ( * ) (Returned)
*        The string into which the integer value is encoded.
*     NCHAR = INTEGER (Returned)
*        The field width used in encoding the value.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1989, 1994 Science & Engineering Research Council.
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
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     16-NOV-1984 (ACD):
*        Documentation improved.
*     1-SEP-1988 (AJC):
*        Use LEN instead of CHR_SIZE.
*     3-OCT-1988 (AJC):
*        Remove termination to declared length of STRING.
*        Improve documentation.
*     14-JUN-1989 (AJC):
*        Use field of minimum size/precision and CHR_LDBLK not RMBLK.
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

*  Arguments Returned:
      CHARACTER STRING * ( * )

      INTEGER NCHAR

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Constants:
      INTEGER MXPREC             ! Maximum number of digits in integer
      PARAMETER ( MXPREC = 11 )

*  Local Variables:
      INTEGER IOSTAT             ! Local status
      INTEGER FIELD              ! Character count
      INTEGER SIZE               ! Declared length of the returned string

      CHARACTER FORMAT * 10      ! Fortran 77 format string

*.

*  Get the declared length of the returned string.
      SIZE = LEN( STRING )

*  Calculate the field size for the internal write statement.
      FIELD = MIN( SIZE, MXPREC )

*  Construct the FORMAT statement for the internal WRITE.
      WRITE ( FORMAT, '(''(I'',I3,'')'')', IOSTAT=IOSTAT ) FIELD

*  Trap errors.
      IF ( IOSTAT .EQ. 0 ) THEN

*     Perform the internal WRITE.
         WRITE ( STRING, FORMAT, IOSTAT=IOSTAT ) IVALUE

*     Trap errors.
         IF ( IOSTAT .EQ. 0 ) THEN

*        Remove the leading spaces - the remainder was cleared by the
*        WRITE statement.
            CALL CHR_LDBLK( STRING( 1 : FIELD ) )
            NCHAR = CHR_LEN( STRING( 1 : FIELD ) )
         ELSE

*        On error, fill the returned string with '*'s.
            CALL CHR_FILL( '*', STRING )
            NCHAR = SIZE
         END IF
      ELSE

*     On error, fill the returned string with '*'s.
         CALL CHR_FILL( '*', STRING )
         NCHAR = SIZE
      END IF

      END
