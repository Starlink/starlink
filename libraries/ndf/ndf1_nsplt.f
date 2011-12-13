      SUBROUTINE NDF1_NSPLT( NAME, REL, N1, N2, S1, S2, STATUS )
*+
*  Name:
*     NDF1_NSPLT

*  Purpose:
*     Split an NDF name into an HDS name and a section specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_NSPLT( NAME, REL, N1, N2, S1, S2, STATUS )

*  Description:
*     This routine analyses an absolute or relative NDF name and splits
*     it into the (absolute or relative) name of the HDS object and a
*     section specification. Both components are optional (unless it is
*     an absolute name, in which case the HDS name component must
*     exist).

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The NDF name to be analysed.
*     REL = LOGICAL (Given)
*        If a .TRUE. value is given, then NAME will be interpreted as a
*        relative NDF name, lacking a container file specification.
*        Otherwise an absolute name, including a container file
*        specification, will be expected.
*     N1 = INTEGER (Returned)
*        Character position in NAME of the start of the HDS object name.
*     N2 = INTEGER (Returned)
*        Character position in NAME of the end of the HDS object name.
*     S1 = INTEGER (Returned)
*        Character position of the start of the section specification.
*     S2 = INTEGER (Returned)
*        Character position of the end of the section specification.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Any blanks which surround either component will be omitted
*     from the character positions returned.
*     -  This routine will perform some simple checks on the validity
*     of the NDF name. However, these are not complete and complete
*     validity can only be checked by actually locating the object
*     which the name describes.
*     -  If the HDS name and/or section spec. components do not exist,
*     then N1 will be returned greater than N2 and/or S1 will be
*     returned greater than S2.
*     -  The component returned as a section spec. may, in some cases,
*     actually be an HDS cell or slice specification applied to a
*     non-scalar HDS object. This ambiguity can only be resolved by
*     locating the object and checking its dimensionality.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-MAR-1991 (RFWS):
*        Original version.
*     17-FEB-1992 (RFWS):
*        Corrected bug in finding file and path name positions which
*        prevented leading blanks on an NDF name being handled
*        correctly.
*     12-AUG-1993 (RFWS):
*        Changed to handle absolute and relative names and to return
*        the HDS name as a whole, rather than decomposing it further.
*     27-DEC-2005 (TIMJ):
*        Replace call to NDF1_HSPLT with call to HDS_SPLIT
*     {enter_further_changes_here}

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
      CHARACTER * ( * ) NAME
      LOGICAL REL

*  Arguments Returned:
      INTEGER N1
      INTEGER N2
      INTEGER S1
      INTEGER S2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      INTEGER F1                 ! Start of container file spec. (junk)
      INTEGER F2                 ! End of container file spec. (junk)
      INTEGER I                  ! Loop counter for path name characters
      INTEGER LPAR               ! Parenthesis level
      INTEGER LZERO              ! Last zero-level character position
      INTEGER P1                 ! Start of HDS path name
      INTEGER P2                 ! End of HDS path name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the name.
      CALL CHR_FANDL( NAME, N1, N2 )

*  If this is an absolute HDS object name (including a container file
*  specification), then check it is not entirely blank and report an
*  error if it is.
      IF ( .NOT. REL ) THEN
         IF ( N1 .GT. N2 ) THEN
            STATUS = NDF__NAMIN
            CALL ERR_REP( 'NDF1_NSPLT_BLK',
     :                    'Blank NDF name supplied.', STATUS )

*  Otherwise, split the name into an HDS container file name and a path
*  name.
         ELSE
            CALL HDS_SPLIT( NAME( N1 : N2 ), F1, F2, P1, P2, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Correct the path name extent returned for the starting position of
*  the search.
               P1 = P1 + N1 - 1
               P2 = P2 + N1 - 1
            END IF
         END IF

*  If it is a relative NDF name, then use the entire name as the HDS
*  path name.
      ELSE
         P1 = N1
         P2 = N2
      END IF

*  Search through the path name counting nested levels of parenthesis.
      IF ( STATUS .EQ. SAI__OK ) THEN
         LPAR = 0
         LZERO = P2
         DO 1 I = P1, P2

*  Note the last character which occurs at a nesting level of zero.
            IF ( LPAR .EQ. 0 ) LZERO = I
            IF ( NAME( I : I ) .EQ. '(' ) THEN
               LPAR = LPAR + 1
            ELSE IF ( NAME( I : I ) .EQ. ')' ) THEN
               LPAR = LPAR - 1
            END IF

*  If too many right parentheses are detected, then report an error and
*  quit the loop.
            IF ( LPAR .LT. 0 ) THEN
               STATUS = NDF__NAMIN
               CALL MSG_SETC( 'NAME', NAME( N1 : N2 ) )
               CALL MSG_SETC( 'THE', 'the' )
               IF ( REL ) CALL MSG_SETC( 'THE', ' relative' )
               CALL ERR_REP( 'NDF1_NSPLT_LP',
     :                       'Missing left parenthesis in ^THE NDF ' //
     :                       'name ''^NAME''.', STATUS )
               GO TO 2
            END IF
 1       CONTINUE
 2       CONTINUE

*  If a missing right parenthesis is detected, then report an error.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( LPAR .GT. 0 ) THEN
               STATUS = NDF__NAMIN
               CALL MSG_SETC( 'NAME', NAME( N1 : N2 ) )
               CALL MSG_SETC( 'THE', 'the' )
               IF ( REL ) CALL MSG_SETC( 'THE', ' relative' )
               CALL ERR_REP( 'NDF1_NSPLT_RP',
     :                       'Missing right parenthesis in ^THE NDF ' //
     :                       'name ''^NAME''.', STATUS )

*  If the last non-parenthesised character is not the final character
*  in the path name, then there is a trailing section specification and
*  this character marks the opening parenthesis. Record the location of
*  the section specification.
            ELSE IF ( LZERO .NE. P2 ) THEN
               S1 = LZERO
               S2 = P2

*  Update the object name end position, removing any new trailing
*  blanks.
               N2 = S1 - 1
               IF ( N1 .LE. N2 ) N2 = N1 +
     :                           CHR_LEN( NAME( N1 : N2 ) ) - 1

*  Note if no trailing section specification was found.
            ELSE
               S1 = 1
               S2 = 0
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_NSPLT', STATUS )

      END
