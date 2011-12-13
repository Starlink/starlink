      SUBROUTINE CHR_CTOD( STRING, DVALUE, STATUS )
*+
*  Name:
*     CHR_CTOD

*  Purpose:
*     Read a DOUBLE PRECISION value from a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_CTOD( STRING, DVALUE, STATUS )

*  Description:
*     Read a DOUBLE PRECISION value from the given character string.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string from which a DOUBLE PRECISION value is to be read.
*     DVALUE = DOUBLE PRECISION (Returned)
*        The resulting DOUBLE PRECISION value.
*     STATUS = INTEGER (Given and Returned)
*        The status value: if this value is not SAI__OK on input,
*        the routine returns without action; if the routine does
*        not complete successfully, STATUS is returned set to
*        SAI__ERROR.

*  Algorithm:
*     Construct the G-format and decode the string using a Fortran 77
*     internal READ.

*  Copyright:
*     Copyright (C) 1982, 1984, 1988, 1989, 1990, 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     DLT: Dave Terrett (STARLINK)
*     PCTR: P.C.T. Ress (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1982 (JRG):
*        Original version.
*     2-OCT-1984 (ACD):
*        Documentation improved.
*     3-OCT-1988 (AJC):
*        Documentation improved.
*     16-AUG-1989 (AJC):
*        Use SAE_PAR.
*     22-JAN-1990 (DLT):
*        Eliminate overlapping substring assignment.
*     13-FEB-1991 (PCTR):
*        Added expicit BN editing for portability.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     30-MAY-1995 (AJC):
*        Trap blank string to avoid SunOS crash
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implict typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER STRING * ( * )

*  Arguments Returned:
      DOUBLE PRECISION DVALUE

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
      INTEGER IOSTAT             ! I/O status from READ/WRITE
      INTEGER NCHAR              ! Character count

      CHARACTER COUNT * 3        ! Character count
      CHARACTER FORMAT * 12      ! Fortran 77 format string

*.


*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Trap any commas in the given string or blank string.
      IF ( ( INDEX( STRING, ',' ) .NE. 0 )
     :.OR. ( STRING .EQ. ' ' ) ) THEN
         STATUS = SAI__ERROR
      ELSE

*     Construct the G-format string.
         NCHAR = CHR_LEN( STRING )
         WRITE( COUNT, '( I3 )', IOSTAT=IOSTAT ) NCHAR

         IF ( IOSTAT .NE. 0 ) THEN
            STATUS = SAI__ERROR
         ELSE

*        Read the DOUBLE PRECISION value.
            FORMAT = '(BN, G'//COUNT//'.0)'
            READ( STRING( 1 : NCHAR ), FORMAT, IOSTAT=IOSTAT ) DVALUE

            IF ( IOSTAT .NE. 0 ) STATUS = SAI__ERROR
         END IF
      END IF

*  Check the returned status value and set the returned DOUBLE PRECISION
*  value on error.
      IF ( STATUS .EQ. SAI__ERROR ) DVALUE = 0.0D+00

      END
