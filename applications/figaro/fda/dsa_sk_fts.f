      SUBROUTINE DSA_SEEK_FITS( DSAREF, KEY, EXIST, TYPE,
     :                          NELM, STRLEN, STATUS )
*+
*  Name:
*     DSA_SEEK_FITS

*  Purpose:
*     Check for the existence of a specified FITS item in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SEEK_FITS( DSAREF, KEY,
*        EXIST, TYPE, NELM, STRLEN, STATUS )

*  Description:
*     This routine looks to see if the FITS extension of an NDF contains
*     an item of given keyword. If so, it returns the type and size of
*     the item. Most FITS items are scalar and show as having one
*     element. Only those items that can appear a number of times, such
*     as the comments `COMMENT', `HISTORY' and items with blank
*     keywords, will show as having a number of elements. If the item is
*     not present, this routine returns indicating that, but does not
*     treat this as an error and will not generate an error message.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     KEY = CHARACTER * ( * ) (Given)
*        The FITS keyword of the item in question, case-insensitive.
*     EXIST = LOGICAL (Returned)
*        Whether such an item exists.
*     TYPE = CHARACTER * ( * ) (Returned)
*        A single character that indicates the routine to be used to
*        access the item in its 'natural' form. This will be one of 'L',
*        'I', 'C', 'F', and 'D'.
*     NELM = INTEGER (Returned)
*        The number of elements fouond for the given keyword. This will
*        be one for all bar the comment items that may occur several
*        times.
*     STRLEN = INTEGER (Returned)
*        If the item is a character string, this returns the number of
*        characters in it. In fact, this is always set to 80.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     01 Dec 1988 (ks):
*        Original version.
*     12 Feb 1990 (ks):
*        Now uses the now-renamed DSA__FITS_DETAILS which supports NDF
*        formats.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     28 Feb 1996 (hme):
*        FDA library.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) KEY

*  Arguments Returned:
      LOGICAL EXIST
      CHARACTER * ( * ) TYPE
      INTEGER NELM
      INTEGER STRLEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL CMMT               ! Whether keyword is comment/history
      LOGICAL ISSTR              ! Whether value is quoted string
      LOGICAL LVALUE             ! A logical value
      INTEGER I, ILEN            ! Loop variables
      INTEGER NDIGIT             ! Number of digits in number
      INTEGER ISTR               ! Index into item array
      INTEGER SLOT               ! The reference slot
      DOUBLE PRECISION VALUE     ! A numeric value
      CHARACTER * ( 80 ) STRING  ! The value string (quotes stripped)
      CHARACTER * ( 80 ) BUFFER  ! A temporary string
      CHARACTER * ( 8 ) KEYUC    ! Upper case version of keyword
      CHARACTER * ( 1 ) CHAR1    ! First character in keyword

*  Internal References:
      LOGICAL CHR_ISDIG          ! Whether a character is a digit
      INTEGER CHR_LEN            ! Used length of string

*.

*  Set string length and default size.
      STRLEN = 80
      NELM   = 1

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Make sure the FITS extension has been mapped.
      CALL DSA1_ORFITS( SLOT, STATUS )

*  Local copy of keyword.
      KEYUC = KEY
      CALL CHR_UCASE( KEYUC )

*  Find the first occurrence of the keyword. Also returns the value
*  string.
      CALL DSA1_KEYVAL( DSA__REFFNE(SLOT),
     :                  %VAL( CNF_PVAL(DSA__REFFPT(SLOT)) ),
     :                  KEYUC, 1, STRING, BUFFER, ISTR, ISSTR,
     :                  STATUS, %VAL(CNF_CVAL(80)) )

*  If failure, annul error and return indicating absence of item
*  (or absence of FITS extension.)
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         EXIST = .FALSE.
         GO TO 500
      END IF
      EXIST = .TRUE.

*  If COMMENT, HISTORY, or blank keyword.
      CHAR1 = KEYUC(1:1)
      CMMT  = .FALSE.
      IF ( CHAR1 .EQ. 'H' ) THEN
         CMMT = KEYUC .EQ. 'HISTORY'
      ELSE IF ( CHAR1 .EQ. 'C' ) THEN
         CMMT = KEYUC .EQ. 'COMMENT'
      ELSE IF ( CHAR1 .EQ. ' ' ) THEN
         CMMT = KEYUC .EQ. ' '
      END IF
      IF ( CMMT ) THEN

*     Count items in the array that have this keyword.
         CALL DSA1_KEYCNT( DSA__REFFNE(SLOT),
     :                     %VAL( CNF_PVAL(DSA__REFFPT(SLOT)) ),
     :                     KEYUC, NELM, STATUS, %VAL(CNF_CVAL(80)) )

*     Type is CHARACTER.
         TYPE = 'C'

*  Else if quoted string.
      ELSE IF ( ISSTR ) THEN

*     Type is CHARACTER.
         TYPE = 'C'

*  Else (neither comment'ish nor quoted string).
      ELSE

*     If number (can be read by CHR_CTOD).
         CALL CHR_CTOD( STRING, VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*        Work out number of significant digits.
*        (This counts leading and trailing zeros even if they are not
*        significant.)
            NDIGIT = 0
            BUFFER = STRING
            CALL CHR_LDBLK( BUFFER )
            ILEN = CHR_LEN( BUFFER )
            DO 1 I = 1, ILEN
               IF ( CHR_ISDIG(BUFFER(I:I)) ) THEN
                  NDIGIT = NDIGIT + 1
               ELSE IF ( 0 .NE. INDEX( '+-.', BUFFER(I:I) ) ) THEN
                  CONTINUE
               ELSE
                  GO TO 2
               END IF
 1          CONTINUE
 2          CONTINUE

*        If less than 10 million and truncatable. Type is INTEGER.
*        Else if six or more digits. Type is DOUBLE PRECISION.
*        Else. Type is REAL.
            TYPE = 'F'
            IF ( DABS(VALUE) .LT. 1D7 ) THEN
               IF ( DBLE(INT(VALUE)) .EQ. VALUE ) TYPE = 'I'
            END IF
            IF ( TYPE .EQ. 'F' .AND. NDIGIT .GE. 6 ) THEN
               TYPE = 'D'
            END IF

*     Else (not a number).
         ELSE

*        Annul error.
            CALL ERR_ANNUL( STATUS )

*        If logical (can be read by CHR_CTOL). Type is LOGICAL.
*        Else. Annul error. Type is (possibly treatable as) CHARACTER.
            CALL CHR_CTOL( STRING, LVALUE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               TYPE = 'L'
            ELSE
               CALL ERR_ANNUL( STATUS )
               TYPE = 'C'
            END IF

         END IF

      END IF

*  Tidy up.
 500  CONTINUE

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END



      SUBROUTINE DSA1_KEYCNT( NITEMS, ITEMS, KEYUC, NELM, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER NITEMS
      CHARACTER * ( 80 ) ITEMS( 1 )
      CHARACTER * ( 8 ) KEYUC

      INTEGER NELM

      INTEGER STATUS

      INTEGER I                  ! Loop index
      CHARACTER * ( 1 ) CHAR1    ! First character in keyword

      IF ( STATUS .NE. SAI__OK ) RETURN

      CHAR1 = KEYUC(1:1)
      NELM = 0
      DO 1 I = 1, NITEMS
         IF ( ITEMS(I)(1:1) .EQ. CHAR1 ) THEN
            IF ( ITEMS(I)(1:8) .EQ. KEYUC ) NELM = NELM + 1
         END IF
 1    CONTINUE

      IF ( NELM .LT. 1 ) NELM = 1

      END
