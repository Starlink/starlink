      SUBROUTINE CHR_COPY( STR1, TRUNC, STR2, LSTAT )
*+
*  Name:
*     CHR_COPY

*  Purpose:
*     Copy one string to another, checking for truncation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_COPY( STR1, TRUNC, STR2, LSTAT )

*  Description:
*     This routine copies one character string to another, checking
*     for truncation caused by the returned string being too short to
*     accommodate the entire given string. As much of the given string
*     as possible is copied to the returned string, ignoring any
*     trailing blanks. If truncation is found, it is indicated by the
*     returned status.
*     Optionally, the last character of the returned string may also
*     be set to '#' if truncation occurs.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The given string.
*     TRUNC = LOGICAL (Given)
*        A logical flag indicating the action to be taken if
*        truncation occurs: if TRUNC is .TRUE., a '#' will be
*        written into the last element of the returned string on
*        truncation; if TRUNC is .FALSE., no '#' is written to the
*        returned string.
*     STR2 = CHARACTER * ( * ) (Returned)
*        The returned string. This will contain the given string,
*        possibly truncated.
*     LSTAT = INTEGER (Returned)
*        The status: 0 for success, 1 if truncation occurs.

*  Algorithm:
*     Find the length of the given string (without trailing blanks).
*     Find the declared length of the returned string.
*     Returned string = given string.
*     If the given string is longer than the returned string then
*       If TRUNC is set then
*         set the last element of the output to '#'.
*       end if
*       Set the local status.
*     end if

*  Implementation Deficiencies:
*     This routine may be rather slow, and probably should not be
*     used where it is going to be called many times inside a loop.
*     It is really for cases where it will not be called very often,
*     but where it is imperative to check if truncation is occurring.

*  Copyright:
*     Copyright (C) 1984, 1989, 1991, 1994 Science & Engineering Research Council.
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
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JUN-1984 (ACD):
*        Original version.
*     27-FEB-1989 (AJC):
*        Modified prologue.
*     12-FEB-1991 (PCTR):
*        Optimisation changes.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STR1 * ( * )

      LOGICAL TRUNC

*  Arguments Returned:
      CHARACTER STR2 * ( * )

*  Status:
      INTEGER LSTAT

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
      INTEGER LEN1               ! Length of input string, no trailing blanks
      INTEGER LEN2               ! Declared length of output string

*.

*  Initialise the local status.
      LSTAT = 0

*  Determine the lengths of the string arguments.
      LEN1 = CHR_LEN( STR1 )
      LEN2 = LEN( STR2 )

*  Perform the string copy.
      STR2 = STR1

      IF ( LEN1 .GT. LEN2 ) THEN
         IF ( TRUNC ) STR2( LEN2 : LEN2 ) = '#'
         LSTAT = 1
      END IF

      END
