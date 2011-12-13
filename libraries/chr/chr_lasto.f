      SUBROUTINE CHR_LASTO( STRING, CVAL, IAT )
*+
*  Name:
*     CHR_LASTO

*  Purpose:
*     Locates the last occurence of CVAL in STRING.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_LASTO( STRING, CVAL, IAT )

*  Description:
*     The routine locates the last occurence of the single character
*     CVAL in STRING. If an occurence is not located then IAT is
*     returned as 0.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        String to be searched for occurences of CVAL.
*     CVAL = CHARACTER * ( 1 ) (Given)
*        Character whose last occurence is to be located.
*     IAT = INTEGER (Returned)
*        Position  within STRING at which last occurence of CVAL is
*        located. Set to 0 if the character is not found.

*  Copyright:
*     Copyright (C) 1992, 1997, 1999 STARLINK
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     DSB: David Berry (Starlink)
*     MJC: Malcolm J. Currie (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28-FEB-1992 (PDRAPER):
*        Original version.
*     19-MAY-1997 (MJC):
*        Rebadged and edited for KAPPA.
*     25-MAY-1999 (DSB):
*        Initialize IAT to zero before checking status.
*     27-DEC-2005 (TIMJ):
*        Move from CCDPACK, PISA, KAPLIBS and NDG to CHR.
*        Remove STATUS argument so as to be consistent with other
*        CHR routines.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:

*  Arguments Given:
      CHARACTER * ( * ) STRING
      CHARACTER * ( 1 ) CVAL

*  Arguments Returned:
      INTEGER IAT

*  Local Variables:
      INTEGER STRLEN             ! Length of STRING
      INTEGER WASAT              ! Previous position within STRING
      INTEGER NOWAT              ! Current position within STRING
      LOGICAL MORE               ! Flag for more loops required

*.

*  Initialise.
      IAT = 0

*  Get the length of the string.
      STRLEN = LEN ( STRING )

*  Initialise the string positions
      NOWAT = 0
      WASAT = 1

*  Initialise the loop flag.
      MORE = .TRUE.

*  Loop while occurences are still located and the string length is not
*  exceeded.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( MORE ) THEN
         NOWAT = INDEX( STRING( WASAT : ) , CVAL )
         IF ( NOWAT .EQ. 0 ) THEN

*  There are no more occurrences.
            MORE = .FALSE.
            IAT = WASAT - 1
         ELSE

*  There is more to do. Increment position within STRING.
            WASAT = NOWAT + WASAT

*  If WASAT now exceeds the string length, the last occurrence was at
*  the end of string.
            IF ( WASAT .GT. STRLEN ) THEN
               MORE = .FALSE.
               IAT = STRLEN
            END IF
         END IF
         GO TO 1
      END IF

      END
