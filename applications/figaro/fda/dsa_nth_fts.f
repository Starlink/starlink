      SUBROUTINE DSA_NTH_FITS_ITEM( DSAREF, NTH,
     :   EXIST, KEY, TYPE, NELM, STRLEN, STATUS )
*+
*  Name:
*     DSA_NTH_FITS_ITEM

*  Purpose:
*     Return details about the N-th FITS item in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_NTH_FITS_ITEM( DSAREF, NTH,
*        EXIST, KEY, TYPE, NELM, STRLEN, STATUS )

*  Description:
*     This routine looks at the FITS extension in an NDF, and in
*     particluar at the N-th item. If there is such an item, this
*     routine returns the keyword, type and size of the item. The type
*     is returned as a single character indicating which of the various
*     DSA_GET_FITS_{x} routines should be used to access the item. Most
*     FITS items are scalar and show as having one element (in other
*     words there is only one item with the keyword in question). Only
*     those keywords that can appear a number of times, such as the
*     comments `COMMENT', `HISTORY' and blank keywords, will show as
*     having a number of elements. If the item is not present, this
*     routine returns indicating that but does not treat it as an error
*     and will not generate an error message.  This routine may be used
*     to search completely through the FITS items in an NDF, by starting
*     at NTH=1 and continuing until EXIST is returned as false.
*
*     The way this routine counts the items is a bit odd: For keywords
*     that can appear more than once their first occurrence is counted
*     as an item, but further occurrences are not counted, e.g.:
*
*        COMMENT    1st item
*        KEY1 = 1 / 2nd item
*        COMMENT    not counted
*        KEY2 = 2 / 3rd item

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     NTH = INTEGER (Given)
*        The number of the item in question.
*     EXIST = LOGICAL (Returned)
*        Whether that many items exist.
*     KEY = CHARACTER * ( * ) (Returned)
*        The FITS keyword of the item in question, case-insensitive.
*     TYPE = CHARACTER * ( * ) (Returned)
*        A single character that indicates the routine to be used to
*        access the item in its 'natural' form. This will be one of 'L',
*        'I', 'C', 'F', and 'D'.
*     NELM = INTEGER (Returned)
*        The number of elements found for the given keyword. This will
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
*     acd: Clive Davenhall (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     01 Dec 1988 (ks):
*        Original version.
*     12 Feb 1990 (ks):
*        Most work now relegated to DSA__FITS_DETAILS, to support
*        different data formats.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     29 Feb 1996 (hme):
*        FDA library.
*     18 Dec 2000 (acd):
*        Corrected data type of arguments to CHR_CTOL.
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
      INTEGER NTH

*  Arguments Returned:
      LOGICAL EXIST
      CHARACTER * ( * ) KEY
      CHARACTER * ( * ) TYPE
      INTEGER NELM
      INTEGER STRLEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL CMMT               ! Whether keyword is comment/history
      LOGICAL ISSTR              ! Whether value is quoted string
      INTEGER I, ILEN            ! Loop variables
      INTEGER NDIGIT             ! Number of digits in number
      INTEGER ISTR               ! Index into item array
      INTEGER SLOT               ! The reference slot
      DOUBLE PRECISION VALUE     ! A numeric value
      LOGICAL LVALUE             ! A logical value
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

*  Translate the given index into the keyword.
*  Find the first occurrence of the keyword. Also returns the value
*  string.
      CALL DSA1_NTHKEY( DSA__REFFNE(SLOT),
     :                  %VAL( CNF_PVAL(DSA__REFFPT(SLOT)) ),
     :                  NTH, KEYUC, STATUS, %VAL(CNF_CVAL(80)) )
      CALL CHR_UCASE( KEYUC )
      KEY = KEYUC
      CALL DSA1_KEYVAL( DSA__REFFNE(SLOT),
     :                  %VAL( CNF_PVAL(DSA__REFFPT(SLOT)) ),
     :                  KEYUC, 1, STRING, BUFFER, ISTR, ISSTR,
     :                  STATUS, %VAL(CNF_CVAL(80)) )

*  If failure, annul error and return indicating absence of item
*  (which may be due to absence of the FITS extension).
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



      SUBROUTINE DSA1_NTHKEY( NELM, ITEMS, NTH, KEY, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER NELM
      CHARACTER * ( 80 ) ITEMS( 1 )
      INTEGER NTH

      CHARACTER * ( 8 ) KEY

      INTEGER STATUS

      LOGICAL MORE               ! Controls loop through FITS arrays
      INTEGER BLANK              ! Number of blank keyword comments so far
      INTEGER COMENT             ! Number of COMMENTs found so far
      INTEGER HISTRY             ! Number of HISTORY keyword entries so far
      INTEGER COUNT              ! Number of distinct keywords found so far
      INTEGER INDX               ! Index into item array
      INTEGER ISTR               ! Final index into item array
      CHARACTER * ( 1 ) CHAR1    ! First character in keyword

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the FITS extension is brand-new, the keyword wasn't found.
      IF ( NELM .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T022', NTH )
         CALL ERR_REP( 'FDA_E073', 'DSA1_NTHKEY: Failed to find ' //
     :      'the FITS keyword no. ^FDA_T022.', STATUS )
         GO TO 500
      END IF

*  Start looking through the items in the FITS array. The code assumes
*  that it is much more efficient to test for a single character match
*  than for a string match - hence the test on the first character
*  (CHAR1).
      BLANK  = 0
      COMENT = 0
      HISTRY = 0
      ISTR   = 0
      COUNT  = 0
      INDX   = 1
      MORE   = .TRUE.
      DO WHILE ( MORE )
         CHAR1 = ITEMS(INDX)(1:1)
         IF ( CHAR1 .EQ. 'C' ) THEN
            IF ( ITEMS(INDX)(1:8) .EQ. 'COMMENT' ) THEN
               IF ( COMENT .EQ. 0 ) COUNT = COUNT + 1
               COMENT = COMENT + 1
            ELSE
               COUNT  = COUNT  + 1
            END IF
         ELSE IF ( CHAR1 .EQ. 'H' ) THEN
            IF ( ITEMS(INDX)(1:8) .EQ. 'HISTORY' ) THEN
               IF ( HISTRY .EQ. 0 ) COUNT = COUNT + 1
               HISTRY = HISTRY + 1
            ELSE
               COUNT  = COUNT  + 1
            END IF
         ELSE IF ( CHAR1 .EQ. ' ' ) THEN
            IF ( ITEMS(INDX)(1:8) .EQ. ' ' ) THEN
               IF ( BLANK  .EQ. 0 ) COUNT = COUNT + 1
               BLANK  = BLANK  + 1
            ELSE
               COUNT  = COUNT  + 1
            END IF
         ELSE
            COUNT = COUNT + 1
         END IF
         IF ( COUNT .GE. NTH ) THEN
            MORE = .FALSE.
            ISTR = INDX
         END IF
         INDX = INDX + 1
         IF ( INDX .GT. NELM ) MORE = .FALSE.
      END DO

*  If the item was found.
      IF ( ISTR .GT. 0 ) THEN
         KEY = ITEMS(ISTR)(1:8)
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T022', NTH )
         CALL ERR_REP( 'FDA_E073', 'DSA1_NTHKEY: Failed to find ' //
     :      'the FITS keyword no. ^FDA_T022.', STATUS )
         GO TO 500
      END IF

*  Return.
 500  CONTINUE
      END
