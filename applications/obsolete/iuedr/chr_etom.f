      SUBROUTINE CHR_ETOM( STR1, STR2 )
*+
*  Name:
*     CHR_ETOM

*  Purpose:
*     Translate a string from EBCDIC to machine internal characters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_ATOM( STR1, STR2 )

*  Description:
*     The string STR1, which has been written on a machine with an
*     internal character representation given by the EBCDIC collating
*     sequence and subsequently read on a machine which may not
*     use the EBCDIC collating sequence to represent characters in
*     Fortran, is returned in STR2 translated into the correct
*     Fortran 77 CHARACTER string.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The character string written on a machine with an EBCDIC
*        collating sequence and read on a machine which may not use the
*        EBCDIC collating sequence to represent characters in Fortran.
*     STR2 = CHARACTER * ( * ) (Returned)
*        The translated EBCDIC character string. If STR2 is shorter
*        than STR1, the translated string will be truncated; if
*        STR2 is longer than STR1, STR2 will be padded with blanks
*        beyond the translated string.

*  System-specific:
*     This subroutine has been implemented for machines which use
*     the ASCII collating sequence for the internal representation of
*     characters.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1991 (PCTR):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STR1 * ( * )

*  Arguments Returned:
      CHARACTER STR2 * ( * )

*  Local Constants:
      INTEGER EBCMAX             ! Maximum EBCDIC character code
      PARAMETER ( EBCMAX = 255 )

      INTEGER MCHSPC             ! Machine internal SPACE
      PARAMETER ( MCHSPC = 32 )

*  Local Variables:
      INTEGER ICHR               ! Character loop index
      INTEGER IEBC               ! EBCDIC character code
      INTEGER LENGTH             ! Maximum loop index.
      INTEGER LEN1               ! Declared length of STR1
      INTEGER LEN2               ! Declared length of STR2
      INTEGER TABLE( 0 : EBCMAX ) ! Translation table

*  Local Data:
      DATA TABLE /   0,   1,   2,   3,  32,   9,  32, 127, 3*32, 11,
     :              12,  13,  14,  15,  16,  17,  18,  19, 2*32,  8,
     :              32,  24,  25, 2*32, 28,  29,  30,  31, 5*32, 10,
     :              23,  27, 2*32, 92, 2*32,  5,   6,   7, 2*32, 22,
     :            4*32,   4, 4*32, 20,  21,  32,  26, 10*32, 91, 46,
     :              60,  40,  43,  33,  38, 9*32, 93,  36,  42,  41,
     :              59,  94,  45,  47, 8*32, 124, 44,  37,  95,  62,
     :              63, 9*32, 96,  58,  35,  64,  39,  61,  34,  32,
     :              97,  98,  99, 100, 101, 102, 103, 104, 105, 7*32,
     :             106, 107, 108, 109, 110, 111, 112, 113, 114, 7*32,
     :             126, 115, 116, 117, 118, 119, 120, 121, 122, 22*32,
     :             123,  65,  66,  67,  68,  69,  70,  71,  72,  73,
     :            6*32, 125,  74,  75,  76,  77,  78,  79,  80,  81,
     :              82, 8*32, 83,  84,  85,  86,  87,  88,  89,  90,
     :            6*32,  48,  49,  50,  51,  52,  53,  54,  55,  56,
     :              57, 6*32 /

*.

*  Get the declared length of the two strings.
      LEN1 = LEN(STR1)
      LEN2 = LEN(STR2)

*  Get the maximum loop index.
      LENGTH = MIN(LEN1, LEN2)

*  If the string length is non-zero, loop to translate it from ASCII to
*  EBCDIC.
      IF ( LENGTH .GT. 0 ) THEN

*     Initialise STR2.
         STR2 = ' '

*     Perform loop.
         DO ICHR = 1, LENGTH

*        Get the machine internal value.
            IEBC = ICHAR(STR1(ICHR:ICHR))

*        If the machine internal value is legitimate, translate it;
*        if not, translate it to an EBCDIC SPACE.
            IF ( IEBC .GT. EBCMAX ) THEN
               STR2(ICHR:ICHR) = CHAR(MCHSPC)

            ELSE
               STR2(ICHR:ICHR) = CHAR(TABLE(IEBC))
            END IF
         END DO
      END IF

      END
