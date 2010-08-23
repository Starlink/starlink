      SUBROUTINE KPG1_CNLIM( NOS, FIRST, LAST, STATUS )

*+
*  Name:
*     KPG1_CNLIM

*  Purpose:
*     Parses a character string into integer bounds.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CNLIM( NOS, FIRST, LAST, STATUS )

*  Description:
*     This routine is used to parse a character specification which
*     defines a range of integers.  The specification is of the form:

*       Number[-number]

*     If the second number is specified then it is assumed that a
*     sequence of numbers are to be parsed, e.g. 4-6 (or 6-4) specifies
*     integers 4, 5, 6; and bounds 4 and 6 are returned.  If only one
*     number is specified, then both the returned bounds are set equal
*     to it.

*     An asterisk may be used as a wildcard.  Thus 5-* specifies all
*     numbers upwards from 5 inclusive, the upper bound being given by
*     the largest integer.  * indicates all integers, i.e. the bounds
*     would be the smallest (non-bad) and largest integers. *-4
*     would specify a range from the smallest integer to 4 inclusive.

*  Arguments:
*      NOS = CHARACTER * ( * ) (Given)
*          Number specification.
*      FIRST = INTEGER (Returned)
*          First integer in the sequence.
*      LAST  = INTEGER (Returned)
*          Last integer in the sequence.
*      STATUS = INTEGER (Given and Returned)
*          Global status value.

*  Algorithm:
*     - Find character position of the number separator and wildcard
*     if present. Report an error if the first character is a
*     separator.
*     - If there is no separator and no wildcard convert from
*     character to integer the single value and use for the upper
*     and lower limits.
*     - If there is a separator and the wildcard comes before it set
*     the lower limit to the minimum integer and convert the upper
*     limit.
*     - If there is a separator and the wildcard comes after it set the
*     upper limit to the largest positive integer and convert the lower
*     limit.
*     - If there is a separator and no wildcard determine the first and
*     last numbers either side of the separator by type conversion.  If
*     the first is greater than the last then swap them.

*  Copyright:
*     Copyright (C) 1980, 1988, 1990 Science & Engineering Research
*                   Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*      DP: Dave Pearce (STARLINK)
*      MJC: Malcolm J. Currie (STARLINK)
*      DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*      1980 Sep 20 (DJP):
*        Second version.
*      1988 Aug 2 (MJC):
*        Tidied to KAPPA style, adding comments and error reports.
*      1990 Nov 25 (MJC):
*        Renamed from CFILIM and added wildcards.
*      2-OCT-1998 (DSB):
*        Made error reporting more secure.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! no default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'         ! SSE global definitions
      INCLUDE  'PRM_PAR'         ! Maximum-constant definitions

*  Arguments Given:
      CHARACTER*( * ) NOS

*  Arguments Returned:
      INTEGER FIRST
      INTEGER LAST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_INDEX          ! Index of substring in string
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      INTEGER DUMMY              ! Dummy position for swapping
      INTEGER NSEP               ! Character position of separator
      INTEGER WILD               ! Character position of wildcard

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find character position of number separator ('-').
      NSEP = CHR_INDEX( NOS, '-' )

*  Is there a wildcard?
      WILD = CHR_INDEX( NOS, '*' )

*  Check if the delimiter is the first or last character.
      IF ( NSEP .EQ. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ERR_KPG1_CNLIM_INPINV',
     :     'KPG1_CNLIM: First character is a delimiter', STATUS )

      ELSE IF( NSEP .EQ. CHR_LEN( NOS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ERR_KPG1_CNLIM_INPINV',
     :     'KPG1_CNLIM: Last character is a delimiter', STATUS )

      ELSE

*  If there is no separator, then only one number specified unless
*  there is a wildcard.
         IF ( NSEP .EQ. 0 ) THEN
            IF ( WILD .EQ. 0 ) THEN

*  Convert the string to an integer.  Upper and lower limits are
*  identical.
               CALL CHR_CTOI( NOS, FIRST, STATUS )
               IF ( STATUS .EQ. SAI__OK ) LAST = FIRST
            ELSE

*  All numbers are required so set the limits to minimum and maximum
*  integers.
               FIRST = VAL__MINI
               LAST = VAL__MAXI
            END IF

*  The specification has a delimiter.
         ELSE

*  Does it have a wildcard before the delimiter?
            IF ( WILD .GE. 1 .AND. WILD .LE. NSEP - 1 ) THEN

*  Start from the smallest non-bad integer.
               FIRST = VAL__MINI

*  Extract the last number.
               CALL CHR_CTOI( NOS( NSEP+1: ), LAST, STATUS )

*  Does it have a wildcard after the delimiter?
            ELSE IF ( WILD .GE. NSEP + 1 ) THEN

*  Extract the first number.
               CALL CHR_CTOI( NOS( :NSEP-1 ), FIRST, STATUS )

*  Go to the end because of the wildcard delimiter.
               LAST = VAL__MAXI
            ELSE

*  Just a simple a-b format.  So extract the first and last numbers.
               CALL CHR_CTOI( NOS( :NSEP-1 ), FIRST, STATUS )
               CALL CHR_CTOI( NOS( NSEP+1: ), LAST, STATUS )

*  If the values are misordered, swap them around.
               IF ( FIRST .GT. LAST ) THEN
                  DUMMY = FIRST
                  FIRST = LAST
                  LAST = DUMMY
               END IF

*  End of check for wildcard and a delimiter.
            END IF

*  End of no-separator check.
         END IF

      END IF

*  Report a context message if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'NOS', NOS )
         CALL ERR_REP( 'KPG1_CNLIM_ERR', 'Cannot interpret range of '/
     :                 /'integers ''^NOS''.', STATUS )
      END IF

      END
