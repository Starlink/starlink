      SUBROUTINE CCD1_FTSEC( SUBST, TEXT, INDXY, STATUS )
*+
*  Name:
*     CCD1_FTSEC

*  Purpose:
*     Substitute for TRIMSEC-like subscript in FITS-like keyword.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_FTSEC( SUBST, TEXT, INDXY, STATUS )

*  Description:
*     This routine substitutes for a subscript in a FITS-header-like
*     keyword indicating an index into a CHARACTER value of the form
*     "[A:B,C:D]", as used in header cards for IRAF-compatible keywords 
*     like TRIMSEC.
*
*     If the string TEXT contains one of the sequences "<X1>", "<X2>",
*     "<Y1>" or "<Y2>" respectively, then it will be replaced by
*     "___X1___", "___X2___", "___Y1___" or "___Y2___" respectively
*     if SUBST is TRUE, and simply removed otherwise.
*     Additionally the value of INDXY will be returned as 1, 2, 3, or 4 
*     respectively.  If none of these is present, INDXY will be returned
*     as zero.  If more than one is present, INDXY will be returned 
*     as -1.

*  Notes:
*     The form of the substituted phrases could be harmlessly altered to
*     something else, with the following constraints: they should be 
*     at least eight characters long, so that a FITS header with these
*     appended cannot be mistaken for a normal FITS header, and they
*     are alphanumeric so that they can be used as part of a TRN token.

*  Arguments:
*     SUBST = LOGICAL (Given)
*        If TRUE, then each occurrence of one of the tokens will be 
*        replaced by a different string as described above.  If FALSE,
*        then each such occurrence will simply be removed.
*     TEXT = CHARACTER * ( * ) (Given and Returned)
*        On entry a FITS header-like keyword, which may contain 
*        substrings of the form "<X1>" etc.  On exit, any such occurrence
*        will be converted to "___X1___" etc or removed.  The string 
*        must be long enough to accommodate such substitutions.
*     INDXY = INTEGER (Returned)
*        Value of 1, 2, 3 or 4 for X1, X2, Y1 or Y2 substitution.  Value
*        of 0 if no substitution.  Value of -1 for more than one 
*        substitution.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-2000 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     If SUBST is FALSE, the token is actually replaced by ' ' rather
*     than removed.  This is equivalent to advertised behaviour only 
*     when the token is at the end of the TEXT string.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL SUBST

*  Arguments Given and Returned:
      CHARACTER * ( * ) TEXT
      
*  Arguments Returned:
      INTEGER INDXY
      
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing blanks
      
*  Local Constants:
      INTEGER NTOK
      PARAMETER ( NTOK = 4 )     ! Number of possible token substitutions
      
*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER NREP               ! Number of replacements made altogether
      INTEGER NT                 ! Number of replacements for current token
      CHARACTER * 2 TOKEN( NTOK ) ! Tokens to substitute for
      CHARACTER * 8 REPLC( NTOK ) ! Replacements for tokens
      
*  Local Data:
      DATA TOKEN / 'X1', 'X2', 'Y1', 'Y2' /
      DATA REPLC / '___X1___', '___X2___', '___Y1___', '___Y2___' /
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise counter.
      NREP = 0

*  Loop through tokens which might get substituted for.
      DO I = 1, NTOK

*  Attempt substitutions on the current token.
         IF ( SUBST ) THEN
            CALL TRN_STOK( TOKEN( I ), REPLC( I ), TEXT, NT, STATUS )
         ELSE
            CALL TRN_STOK( TOKEN( I ), ' ', TEXT, NT, STATUS )
         END IF

*  Check for probable string overflow.
         IF ( CHR_LEN( TEXT ) .EQ. LEN( TEXT ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ',
     :      '  CCD1_FTSEC: Keyword string too short for substitutions',
     :                    STATUS )
            GO TO 99
         END IF

*  If a single substitution has been made, record it.
         IF ( NT .EQ. 1 ) INDXY = I 

*  Keep track of the total number of substitutions.
         NREP = NREP + NT
      END DO

*  Adjust the value of the return parameter.
      IF ( NREP .EQ. 0 ) THEN
         INDXY = 0
      ELSE IF ( NREP .GT. 1 ) THEN
         INDXY = -1
      END IF

*  Error exit label.
 99   CONTINUE

      END
* $Id$
