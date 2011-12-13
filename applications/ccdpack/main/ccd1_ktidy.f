      SUBROUTINE CCD1_KTIDY( TOTRN, TEXT, INDXY, STATUS )
*+
*  Name:
*     CCD1_KTIDY

*  Purpose:
*     Make suitable substitutions in a FITS-like keyword.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_KTIDY( TOTRN, TEXT, INDXY, STATUS )

*  Description:
*     This routine turns text containing FITS-header-like strings of
*     the form which may be found in the translation tables used
*     by the IMPORT utility into a string suitable for use as a token
*     in TRANSFORM routines.
*
*     In fact it makes two related sets of substitutions: if it finds
*     either a character which is legal in a FITS header but not a
*     TRANSFORM token ('.'), or one of the special strings
*     '<X1>', '<X2>', '<Y1>' or '<Y2>', then it replaces them by
*     something identifiable but palatable to TRANSFORM.
*     Additionally, if exactly one substitution of the X1,X2,Y1,Y2 type
*     has been made, the returned INDXY argument will indicate which
*     it was.
*
*     If TOTRN is FALSE, then X1-type strings will be removed rather
*     than substituted for, and illegal character substitutions will
*     not be made at all.
*
*     The routine is designed to be used with TOTRN = TRUE to generate
*     a token for use in TRANSFORM expressions, and with TOTRN = FALSE
*     to generate a genuine FITS header.  In either case, the INDXY
*     argument can be examined to determine whether an X1-type
*     string was present.

*  Arguments:
*     TOTRN = LOGICAL (Given)
*        If TRUE, then the strings '<X1>', '<X2>', '<Y1>' and '<Y2>'
*        will be turned into something identifiable and the returned
*        string will be legal as a TRANSFORM token.  If FALSE, then
*        the strings '<X1>', '<X2>', '<Y1>' and '<Y2>' will be
*        removed, and the returned string will be legal as a FITS
*        header keyword.
*     TEXT = CHARACTER * ( * ) (Given and Returned)
*        On entry a FITS header-like keyword, which may contain
*        substrings of the form "<X1>" etc.  On exit, such strings
*        will be removed or converted as described above.  The string
*        must be long enough to accommodate such substitutions.
*     INDXY = INTEGER (Returned)
*        Indicates which of '<X1>', '<X2>', '<Y1>' or '<Y2>' has
*        been found in TEXT.  If exactly one of these has been found,
*        INDXY will be 1, 2, 3 or 4 respectively.  If more than one
*        was found it will be -1, and if none has been found it
*        will be 0.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The form of the substituted phrases could be harmlessly altered to
*     something else, with the following constraints: they should be
*     at least eight characters long, so that a FITS header with these
*     appended cannot be mistaken for a normal FITS header, and they
*     are alphanumeric so that they can be used as part of a TRN token.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-2000 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     This routine does not cope with '-', which is a legal character
*     in a FITS header keyword, but not in a TRANSFORM token.
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP system constants

*  Arguments Given:
      LOGICAL TOTRN

*  Arguments Given and Returned:
      CHARACTER * ( * ) TEXT

*  Arguments Returned:
      INTEGER INDXY

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing blanks
      EXTERNAL CHR_ISALF
      LOGICAL CHR_ISALF          ! Whether character is alphabetic

*  Local Constants:
      INTEGER NTOK
      PARAMETER ( NTOK = 4 )     ! Number of possible token substitutions

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in input string
      INTEGER NREP( NTOK )       ! Number of replacements made per token
      INTEGER OAT                ! Position in output string
      INTEGER RLENG( NTOK )      ! Length of strings in REPLC
      INTEGER SLENG              ! Length of TEXT string
      INTEGER TOTREP             ! Number of replacements altogether
      INTEGER TLENG( NTOK )      ! Length of strings in TOKEN
      LOGICAL FOUND              ! Have we found a replacement token?
      CHARACTER * 8 DOTREP       ! Replacement for '.' character
      CHARACTER * 8 REPLC( NTOK ) ! Replacements for tokens
      CHARACTER * 4 TOKEN( NTOK ) ! Tokens to substitute for
      CHARACTER * ( GRP__SZNAM ) WORK ! Output string

*  Local Data:
      DATA TOKEN / '<X1>', '<X2>', '<Y1>', '<Y2>' /
      DATA REPLC / '___X1___', '___X2___', '___Y1___', '___Y2___' /
      DATA DOTREP / '___DT___' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set some initial values.
      DO I = 1, NTOK
         NREP( I ) = 0
         TLENG( I ) = CHR_LEN( TOKEN( I ) )
         RLENG( I ) = CHR_LEN( REPLC( I ) )
      END DO
      SLENG = CHR_LEN( TEXT )
      WORK = ' '

*  Work through the text string a character at a time, replacing tokens
*  as they are encountered.
      IAT = 1
      OAT = 1
 1    CONTINUE

*  Check for the replacement tokens and handle them if we find them.
         FOUND = .FALSE.
         DO I = 1, NTOK

*  If this is one of the tokens, copy the replacement string to the
*  output string.
            IF ( TEXT( IAT : IAT + TLENG( I ) - 1 ) .EQ. TOKEN( I ) )
     :         THEN
               FOUND = .TRUE.
               IAT = IAT + TLENG( I )
               NREP( I ) = NREP( I ) + 1
               IF ( TOTRN ) THEN
                  WORK( OAT : OAT + RLENG( I ) - 1 ) = REPLC( I )
                  OAT = OAT + RLENG( I  )
               END IF
            END IF
         END DO

*  If it's a '.' character between alphabetic characters it should
*  get translated (in other contexts it is probably a decimal point).
         IF ( .NOT. FOUND ) THEN
            IF ( TOTRN .AND. TEXT( IAT : IAT ) .EQ. '.' .AND.
     :                IAT .GT. 1 .AND.
     :                CHR_ISALF( TEXT( IAT - 1 : IAT - 1 ) ) .AND.
     :                CHR_ISALF( TEXT( IAT + 1 : IAT + 1 ) ) ) THEN
               WORK( OAT : OAT + CHR_LEN( DOTREP ) - 1 ) = DOTREP
               OAT = OAT + CHR_LEN( DOTREP )
               IAT = IAT + 1

*  Otherwise, copy the character unchanged.
            ELSE
               WORK( OAT : OAT ) = TEXT( IAT : IAT )
               OAT = OAT + 1
               IAT = IAT + 1
            END IF
         END IF

*  Check that we haven't overflowed the string length.
         IF ( OAT .GT. LEN( TEXT ) .OR. OAT .GT. LEN( WORK ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ',
     :      '  CCD1_KTIDY: Keyword string too short for substitutions',
     :                    STATUS )
            GO TO 99
         END IF

*  Go to the next character if there is one.
      IF ( IAT .LE. SLENG ) GO TO 1

*  Set INDXY so that if there has been a single substitution of one of
*  the first four tokens it is set to the index of that token, otherwise
*  it is 0 or -1.
      TOTREP = 0
      DO I = 1, NTOK
         IF ( NREP( I ) .EQ. 1 ) INDXY = I
         TOTREP = TOTREP + NREP( I )
      END DO
      IF ( TOTREP .EQ. 0 ) THEN
         INDXY = 0
      ELSE IF ( TOTREP .GT. 1 ) THEN
         INDXY = -1
      END IF

*  Finally write the output string back to the input one for return.
      TEXT = WORK

*  Error exit label.
 99   CONTINUE

      END
* $Id$
