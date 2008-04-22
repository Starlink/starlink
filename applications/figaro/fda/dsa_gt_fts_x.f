*+
*  Name:
*     DSA_GET_FITS_{CDFIL}

*  Purpose:
*     Get an item from the FITS extension of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     DSA_GET_FITS_<T>( DSAREF, KEY, ELEM, VALUE, COMENT, STATUS )

*  Description:
*     An NDF may contain a FITS extension used to transfer information
*     from and to the FITS format. The items of such an extension must
*     generally be simple items: single character strings, or single
*     numeric values. Each item is named by a keyword and can have a
*     comment associated with it. Normally, each keyword has only a
*     single item, but there are some keywords such as `HISTORY' and
*     `COMMENT' where the FITS extension may contain a number of items
*     all with the same keyword. These are treated as being multiple
*     elements of the same named item.
*
*     These routines (one for each data type) will return the value of
*     an element of a keyword. If the routine used is not the one that
*     matches the actual type of the item, it will be converted to the
*     type requested, but it is usually best to use the correct routine.
*     These routines assume that the specified item (and element) exist,
*     and will generate error messages if this is not the case.
*     DSA_SEEK_FITS can be used to see if an element does exist, and to
*     find out its type. DSA_NTH_FITS_ITEM can be used to discover one
*     by one all the items in a FITS extension.
*
*     Contrary to earlier implementations there is no routine for short
*     (2-byte) integers.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     KEY = CHARACTER * ( * ) (Given)
*        The FITS keyword of the item in question, case-insensitive.
*     ELEM = INTEGER (Given)
*        The element number to be read. This is only needed for
*        multi-valued keywords, and should be 1 or 0 (either will do)
*        for single-valued keywords.
*     VALUE = <TYPE> (Returned)
*        The value of the item in question.
*     COMENT = CHARACTER * ( * ) (Returned)
*        The comment that the item contained behind the value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02 Dec 1988 (ks):
*        Original version.  KS / AAO.
*     07 Feb 1990 (ks):
*        Now uses DSA__ routines to handle different data structures.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     13 Mar 1996 (hme):
*        FDA library.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      SUBROUTINE DSA_GET_FITS_C( DSAREF, KEY, ELEM,
     :   VALUE, COMENT, STATUS )
      
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
      INTEGER ELEM

*  Arguments Returned:
      CHARACTER * ( * ) VALUE
      CHARACTER * ( * ) COMENT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ISSTR              ! Ignored
      INTEGER ISTR               ! Ignored
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( 80 ) STRING  ! The value string
      CHARACTER * ( 80 ) LCOMNT  ! Local copy of comment
      CHARACTER * ( 8 ) KEYUC    ! Upper case version of keyword

*.

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

*  Local copies of keyword and comment.
      KEYUC = KEY
      CALL CHR_UCASE( KEYUC )

*  Given the keyword find a value string and the comment.
      CALL DSA1_KEYVAL( DSA__REFFNE(SLOT), 
     :                  %VAL( CNF_PVAL(DSA__REFFPT(SLOT)) ),
     :                  KEYUC, ELEM, STRING, LCOMNT, ISTR,
     :                  ISSTR, STATUS, %VAL(CNF_CVAL(80)) )
      COMENT = LCOMNT

*  Read the value from the value string.
      VALUE = STRING

*  Translate status and end error context.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END



      SUBROUTINE DSA_GET_FITS_D( DSAREF, KEY, ELEM,
     :   VALUE, COMENT, STATUS )
      
      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DSA_COMMON'       ! DSA global variables
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) KEY
      INTEGER ELEM
      DOUBLE PRECISION VALUE
      CHARACTER * ( * ) COMENT
      INTEGER STATUS             ! Global status

      LOGICAL ISSTR              ! Ignored
      INTEGER ISTR               ! Ignored
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( 80 ) STRING  ! The value string
      CHARACTER * ( 80 ) LCOMNT  ! Local copy of comment
      CHARACTER * ( 8 ) KEYUC    ! Upper case version of keyword

      IF ( STATUS .NE. 0 ) RETURN

      CALL ERR_MARK
      STATUS = SAI__OK

      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL DSA1_ORFITS( SLOT, STATUS )
      KEYUC = KEY
      CALL CHR_UCASE( KEYUC )
      CALL DSA1_KEYVAL( DSA__REFFNE(SLOT),
     :                  %VAL( CNF_PVAL(DSA__REFFPT(SLOT) ) ),
     :                  KEYUC, ELEM, STRING, LCOMNT, ISTR, ISSTR,
     :                  STATUS, %VAL( CNF_CVAL(80) ) )
      COMENT = LCOMNT
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_CTOD( STRING, VALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FDA_T020', STRING )
            CALL ERR_REP( 'FDA_E068', 'DSA_GET_FITS_D: Error ' //
     :         'reading a double precision value from the string ' //
     :         '"^FDA_T020".', STATUS )
         END IF
      END IF

 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END



      SUBROUTINE DSA_GET_FITS_F( DSAREF, KEY, ELEM,
     :   VALUE, COMENT, STATUS )
      
      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DSA_COMMON'       ! DSA global variables
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) KEY
      INTEGER ELEM
      REAL VALUE
      CHARACTER * ( * ) COMENT
      INTEGER STATUS             ! Global status

      LOGICAL ISSTR              ! Ignored
      INTEGER ISTR               ! Ignored
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( 80 ) STRING  ! The value string
      CHARACTER * ( 80 ) LCOMNT  ! Local copy of comment
      CHARACTER * ( 8 ) KEYUC    ! Upper case version of keyword

      IF ( STATUS .NE. 0 ) RETURN

      CALL ERR_MARK
      STATUS = SAI__OK

      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL DSA1_ORFITS( SLOT, STATUS )
      KEYUC = KEY
      CALL CHR_UCASE( KEYUC )
      CALL DSA1_KEYVAL( DSA__REFFNE(SLOT),
     :                  %VAL( CNF_PVAL(DSA__REFFPT(SLOT)) ),
     :                  KEYUC, ELEM, STRING, LCOMNT, ISTR, ISSTR,
     :                  STATUS, %VAL(CNF_CVAL(80)) )
      COMENT = LCOMNT
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_CTOR( STRING, VALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FDA_T020', STRING )
            CALL ERR_REP( 'FDA_E069', 'DSA_GET_FITS_F: Error ' //
     :         'reading a single precision value from the string ' //
     :         '"^FDA_T020".', STATUS )
         END IF
      END IF

 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END



      SUBROUTINE DSA_GET_FITS_I( DSAREF, KEY, ELEM,
     :   VALUE, COMENT, STATUS )
      
      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DSA_COMMON'       ! DSA global variables
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) KEY
      INTEGER ELEM
      INTEGER VALUE
      CHARACTER * ( * ) COMENT
      INTEGER STATUS             ! Global status

      DOUBLE PRECISION DVALUE    ! Value
      LOGICAL ISSTR              ! Ignored
      INTEGER ISTR               ! Ignored
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( 80 ) STRING  ! The value string
      CHARACTER * ( 80 ) LCOMNT  ! Local copy of comment
      CHARACTER * ( 8 ) KEYUC    ! Upper case version of keyword

      IF ( STATUS .NE. 0 ) RETURN

      CALL ERR_MARK
      STATUS = SAI__OK

      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL DSA1_ORFITS( SLOT, STATUS )
      KEYUC = KEY
      CALL CHR_UCASE( KEYUC )
      CALL DSA1_KEYVAL( DSA__REFFNE(SLOT),
     :                  %VAL( CNF_PVAL(DSA__REFFPT(SLOT)) ),
     :                  KEYUC, ELEM, STRING, LCOMNT, ISTR,
     :                  ISSTR, STATUS, %VAL(CNF_CVAL(80)) )
      COMENT = LCOMNT
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_CTOD( STRING, DVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FDA_T020', STRING )
            CALL ERR_REP( 'FDA_E070', 'DSA_GET_FITS_I: Error ' //
     :         'reading an integer value from the string ' //
     :         '"^FDA_T020".', STATUS )
         ELSE
            VALUE = NINT( DVALUE )
         END IF
      END IF

 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END



      SUBROUTINE DSA_GET_FITS_L( DSAREF, KEY, ELEM,
     :   VALUE, COMENT, STATUS )
      
      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DSA_COMMON'       ! DSA global variables
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) KEY
      INTEGER ELEM
      LOGICAL VALUE
      CHARACTER * ( * ) COMENT
      INTEGER STATUS             ! Global status

      LOGICAL ISSTR              ! Ignored
      INTEGER ISTR               ! Ignored
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( 80 ) STRING  ! The value string
      CHARACTER * ( 80 ) LCOMNT  ! Local copy of comment
      CHARACTER * ( 8 ) KEYUC    ! Upper case version of keyword

      IF ( STATUS .NE. 0 ) RETURN

      CALL ERR_MARK
      STATUS = SAI__OK

      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL DSA1_ORFITS( SLOT, STATUS )
      KEYUC = KEY
      CALL CHR_UCASE( KEYUC )
      CALL DSA1_KEYVAL( DSA__REFFNE(SLOT),
     :                  %VAL( CNF_PVAL(DSA__REFFPT(SLOT)) ),
     :                  KEYUC, ELEM, STRING, LCOMNT, ISTR,
     :                  ISSTR, STATUS, %VAL(CNF_CVAL(80) ) )
      COMENT = LCOMNT
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_CTOL( STRING, VALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FDA_T020', STRING )
            CALL ERR_REP( 'FDA_E071', 'DSA_GET_FITS_L: Error ' //
     :         'reading a logical value from the string ' //
     :         '"^FDA_T020".', STATUS )
         END IF
      END IF

 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END



      SUBROUTINE DSA1_KEYVAL( NELM, ITEMS, KEYUC, ELEM,
     :   STRING, COMENT, ISTR, ISSTR, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER NELM
      CHARACTER * ( 80 ) ITEMS( 1 )
      CHARACTER * ( 8 ) KEYUC
      INTEGER ELEM

      CHARACTER * ( 80 ) STRING
      CHARACTER * ( 80 ) COMENT
      INTEGER ISTR
      LOGICAL ISSTR

      INTEGER STATUS

      LOGICAL MORE             ! Controls loop through FITS arrays
      LOGICAL CMMT             ! Whether keyword is comment/history
      INTEGER COUNT            ! Number of matches to keyword found so far
      INTEGER INDX             ! Index into item array
      INTEGER IPTR, IEND       ! Pointers into string
      CHARACTER * ( 1 ) CHAR1  ! First character in keyword
      CHARACTER * ( 80 ) BUFF  ! Temporary string

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the FITS extension is brand-new, the keyword wasn't found.
      IF ( NELM .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T021', KEYUC )
         CALL ERR_REP( 'FDA_E072', 'DSA1_KEYVAL: Failed to find ' //
     :      '(an element of) the FITS keyword ^FDA_T021.', STATUS )
         GO TO 500
      END IF

*  Start looking through the items in the FITS array. The code assumes
*  that it is much more efficient to test for a single character match
*  than for a string match - hence the test on the first character
*  (CHAR1).
      COUNT = 0
      ISTR  = 0
      INDX  = 1
      MORE  = .TRUE.
      CHAR1 = KEYUC(1:1)
      DO WHILE ( MORE )
         IF ( ITEMS(INDX)(1:1) .EQ. CHAR1 ) THEN
            IF ( ITEMS(INDX)(1:8) .EQ. KEYUC ) THEN
               COUNT = COUNT + 1
               IF ( COUNT .GE. ELEM ) THEN
                  MORE = .FALSE.
                  ISTR = INDX
               END IF
            END IF
         END IF
         INDX = INDX + 1
         IF ( INDX .GT. NELM ) MORE = .FALSE.
      END DO

*  If the final index is still zero, the keyword wasn't found.
      IF ( ISTR .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T021', KEYUC )
         CALL ERR_REP( 'FDA_E072', 'DSA1_KEYVAL: Failed to find ' //
     :      '(an element of) the FITS keyword ^FDA_T021.', STATUS )
         GO TO 500
      END IF

*  ITEM(ISTR) is the item string. Parse it.
*  HISTORY, COMMENT and blank keywords have values that start in column
*  9, other keywords have an = sign in that column.
      ISSTR = .FALSE.
      CMMT  = .FALSE.
      CHAR1 = ITEMS(ISTR)(1:1)
      IF ( CHAR1 .EQ. 'H' ) THEN
         CMMT = ITEMS(ISTR)(1:8) .EQ. 'HISTORY'
      ELSE IF ( CHAR1 .EQ. 'C' ) THEN
         CMMT = ITEMS(ISTR)(1:8) .EQ. 'COMMENT'
      ELSE IF ( CHAR1 .EQ. ' ' ) THEN
         CMMT = ITEMS(ISTR)(1:8) .EQ. ' '
      END IF

      IF ( CMMT ) THEN
         COMENT = ' '
         STRING = ITEMS(ISTR)(9:)
      ELSE

*     Not a comment-type keyword, so look for a character string -
*     i.e. look for a quotation mark as the first non-blank character.
         BUFF = ITEMS(ISTR)(10:)
         CALL CHR_LDBLK( BUFF )
         ISSTR = BUFF(1:1) .EQ. ''''
         IPTR = 9 + INDEX( ITEMS(ISTR)(10:), BUFF(1:1) )
         IF ( ISSTR ) THEN

*        It is a quoted string. Locate the opening quote in the item,
*        then look for the closing quote.
            IF ( IPTR .GE. 80 ) THEN
               STRING = ' '
               COMENT = ' '
            ELSE
               IEND = IPTR + INDEX( ITEMS(ISTR)(IPTR+1:), '''' )
               IF ( IEND .LE. IPTR ) IEND = 81
               STRING = ITEMS(ISTR)(IPTR+1:IEND-1)
               IF ( IEND .GE. 80 ) THEN
                  COMENT = ' '
               ELSE
                  IPTR = IEND + INDEX( ITEMS(ISTR)(IEND+1:), '/' )
                  IF ( IPTR .LE. IEND .OR. IPTR .GE. 80 ) THEN
                     COMENT = ' '
                  ELSE
                     IF ( ITEMS(ISTR)(IPTR+1:IPTR+1) .EQ. ' ' ) THEN
                        COMENT = ITEMS(ISTR)(IPTR+2:)
                     ELSE
                        COMENT = ITEMS(ISTR)(IPTR+1:)
                     END IF
                  END IF
               END IF
            END IF

         ELSE

*        It is not a quoted string. Look for a comment separator.
*        Anything between the first non-blank character and either the
*        comment separator or the end of the string is the value.
*        (Somewhat arbitrarily, we ignore a single leading blank in the
*        comment. It is usually put there - see the FITS papers - but
*        doesn't appear to be obligatory. Doing this at least makes us
*        match the way DSA__WRITE_FITS_x writes comments.)
            IEND = IPTR - 1 + INDEX( ITEMS(ISTR)(IPTR:), '/' )
            IF ( IEND .LT. IPTR ) THEN
               COMENT = ' '
               STRING = ITEMS(ISTR)(IPTR:)
            ELSE
               STRING = ITEMS(ISTR)(IPTR:IEND-1)
               IF ( IEND .GE. 80 ) THEN
                  COMENT = ' '
               ELSE
                  IF ( ITEMS(ISTR)(IEND+1:IEND+1) .EQ. ' ' ) THEN
                     COMENT = ITEMS(ISTR)(IEND+2:)
                  ELSE
                     COMENT = ITEMS(ISTR)(IEND+1:)
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Return.
 500  CONTINUE
      END



      SUBROUTINE DSA1_ORFITS( SLOT, STATUS )

      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DSA_COMMON'       ! DSA global variables

      INTEGER SLOT               ! The reference slot
      INTEGER STATUS             ! Global status

      LOGICAL ISWRIT             ! Whether NDF is writable

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( DSA__REFFLO(SLOT) .EQ. DAT__NOLOC ) THEN
         CALL NDF_ISACC( DSA__REFID1(SLOT), 'WRITE', ISWRIT, STATUS )
         IF ( ISWRIT ) THEN
            CALL NDF_XLOC( DSA__REFID1(SLOT), 'FITS', 'UPDATE',
     :         DSA__REFFLO(SLOT), STATUS )
            CALL DAT_MAPV( DSA__REFFLO(SLOT), '_CHAR*80', 'UPDATE',
     :         DSA__REFFPT(SLOT), DSA__REFFNE(SLOT), STATUS )
         ELSE
            CALL NDF_XLOC( DSA__REFID1(SLOT), 'FITS', 'READ',
     :         DSA__REFFLO(SLOT), STATUS )
            CALL DAT_MAPV( DSA__REFFLO(SLOT), '_CHAR*80', 'READ',
     :         DSA__REFFPT(SLOT), DSA__REFFNE(SLOT), STATUS )
         END IF
      END IF

      END
