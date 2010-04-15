*+
*  Name:
*     DSA_PUT_FITS_{CDFIL}

*  Purpose:
*     Put an item into the FITS extension of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     DSA_GET_FITS_<T>( DSAREF, KEY, VALUE, COMENT, STATUS )

*  Description:
*     An NDF may contain a FITS extension used to transfer information
*     from and to the FITS format. The items of such an extension must
*     generally be simple items: single character strings, or single
*     numeric values. Each item is named by a keyword (the keyword must
*     be no more than eight characters long) and can have a comment
*     associated with it. Normally, there can only be one item with a
*     given keyword, but there can be multiple instances of keywords
*     `COMMENT', `HISTORY' or with blank keywords. These are all treated
*     as comments.
*
*     These routines are given an item in three parts: its keyword, its
*     value (whose type depends on the routine being called), and any
*     associated comment. If the name is `COMMENT', `HISTORY' or blank,
*     the value is always a string to be used as the comment, and the
*     comment argument will normally be left blank (although it does not
*     have to be - the two will be concatenated). This routine does not
*     guarantee that the order in which the items appear in the file
*     will match the order in which they are passed to this routine.
*
*     Contrary to earlier implementations there is no routine for short
*     (2-byte) integers.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     KEY = CHARACTER * ( * ) (Given)
*        The FITS keyword of the item in question, case-insensitive.
*     VALUE = <TYPE> (Given)
*        The value of the item in question.
*     COMENT = CHARACTER * ( * ) (Given)
*        The comment that the item contained behind the value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25 Nov 1988 (ks):
*        Original version.
*     13 Feb 1990 (ks):
*        Modified to support different data formats, through use of
*        DSA__ routines.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     23 Feb 1996 (hme):
*        FDA library.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      SUBROUTINE DSA_PUT_FITS_C( DSAREF, KEY, VALUE, COMENT, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) KEY
      CHARACTER * ( * ) VALUE
      CHARACTER * ( * ) COMENT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL CMMT               ! Whether keyword is comment/history
      INTEGER SLOT               ! The reference slot
      INTEGER STRLEN             ! A string length
      CHARACTER * ( 80 ) STRING  ! The item string
      CHARACTER * ( 8 ) KEYUC    ! Upper case keyword
      CHARACTER * ( 1 ) CHAR1    ! First character in keyword

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*  Formats:
*101  FORMAT( A8, '= ', <T>20, ' / ', A )

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
      CALL DSA1_OWFITS( SLOT, STATUS )

*  Convert keyword, value, comment into an item string.
*  (For strings, namely comments and history items an internal write is
*  not suitable.)
      KEYUC = KEY
      CALL CHR_UCASE( KEYUC )
      CMMT  = .FALSE.
      CHAR1 = KEYUC(1:1)
      IF ( CHAR1 .EQ. 'H' ) THEN
         CMMT = KEYUC .EQ. 'HISTORY'
      ELSE IF ( CHAR1 .EQ. 'C' ) THEN
         CMMT = KEYUC .EQ. 'COMMENT'
      ELSE IF ( CHAR1 .EQ. ' ' ) THEN
         CMMT = KEYUC .EQ. ' '
      END IF
      IF ( CMMT ) THEN
         STRING = KEYUC // VALUE // COMENT
      ELSE
         STRING = KEYUC // '= ''' // VALUE
         STRLEN = CHR_LEN(VALUE)
         STRLEN = MAX( 8, STRLEN )
         STRLEN = MIN( 68, STRLEN )
         STRING(STRLEN+12:) = ''''
         STRLEN = MAX( 30, CHR_LEN(STRING) )
         STRING(STRLEN+1:) = ' / ' // COMENT
      END IF

*  Given the item, assemble it and stick it into the FITS extension.
      CALL DSA1_VALITM( SLOT, STRING, STATUS )

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



      SUBROUTINE DSA_PUT_FITS_D( DSAREF, KEY, VALUE, COMENT, STATUS )

      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DSA_COMMON'       ! DSA global variables
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) KEY
      DOUBLE PRECISION VALUE
      CHARACTER * ( * ) COMENT

      INTEGER STATUS             ! Global status

      INTEGER I                  ! Pointer to D/E
      INTEGER SLOT               ! The reference slot
      INTEGER STRLEN             ! Length of number string
      CHARACTER * ( 80 ) STRING  ! The item string
      CHARACTER * ( 8 ) KEYUC    ! Upper case keyword
      CHARACTER * ( 20 ) VALSTR  ! The value string

      INTEGER CHR_LEN            ! Used length of string

 101  FORMAT( A8, '= ', A20, ' / ', A )

      IF ( STATUS .NE. 0 ) RETURN

      CALL ERR_MARK
      STATUS = SAI__OK

      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL DSA1_OWFITS( SLOT, STATUS )
      KEYUC = KEY
      CALL CHR_UCASE( KEYUC )
      CALL CHR_DTOC( VALUE, VALSTR, STRLEN )
      I = INDEX( VALSTR, 'D' )
      IF ( I .GT. 0 ) VALSTR(I:I) = 'E'
      WRITE( STRING, 101 ) KEYUC, VALSTR(:STRLEN),
     :   COMENT(:CHR_LEN(COMENT))
      CALL DSA1_VALITM( SLOT, STRING, STATUS )

 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END



      SUBROUTINE DSA_PUT_FITS_F( DSAREF, KEY, VALUE, COMENT, STATUS )

      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DSA_COMMON'       ! DSA global variables
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) KEY
      REAL VALUE
      CHARACTER * ( * ) COMENT

      INTEGER STATUS             ! Global status

      INTEGER SLOT               ! The reference slot
      INTEGER STRLEN             ! Length of number string
      CHARACTER * ( 80 ) STRING  ! The item string
      CHARACTER * ( 8 ) KEYUC    ! Upper case keyword
      CHARACTER * ( 20 ) VALSTR  ! The value string

      INTEGER CHR_LEN            ! Used length of string

 101  FORMAT( A8, '= ', A20, ' / ', A )

      IF ( STATUS .NE. 0 ) RETURN

      CALL ERR_MARK
      STATUS = SAI__OK

      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL DSA1_OWFITS( SLOT, STATUS )
      KEYUC = KEY
      CALL CHR_UCASE( KEYUC )
      CALL CHR_RTOC( VALUE, VALSTR, STRLEN )
      WRITE( STRING, 101 ) KEYUC, VALSTR(:STRLEN),
     :   COMENT(:CHR_LEN(COMENT))
      CALL DSA1_VALITM( SLOT, STRING, STATUS )

 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END



      SUBROUTINE DSA_PUT_FITS_I( DSAREF, KEY, VALUE, COMENT, STATUS )

      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DSA_COMMON'       ! DSA global variables
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) KEY
      INTEGER VALUE
      CHARACTER * ( * ) COMENT

      INTEGER STATUS             ! Global status

      INTEGER SLOT               ! The reference slot
      CHARACTER * ( 80 ) STRING  ! The item string
      CHARACTER * ( 8 ) KEYUC    ! Upper case keyword

      INTEGER CHR_LEN            ! Used length of string

 101  FORMAT( A8, '= ', I20, ' / ', A )

      IF ( STATUS .NE. 0 ) RETURN

      CALL ERR_MARK
      STATUS = SAI__OK

      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL DSA1_OWFITS( SLOT, STATUS )
      KEYUC = KEY
      CALL CHR_UCASE( KEYUC )
      WRITE( STRING, 101 ) KEYUC, VALUE, COMENT(:CHR_LEN(COMENT))
      CALL DSA1_VALITM( SLOT, STRING, STATUS )

 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END



      SUBROUTINE DSA_PUT_FITS_L( DSAREF, KEY, VALUE, COMENT, STATUS )

      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DSA_COMMON'       ! DSA global variables
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) KEY
      LOGICAL VALUE
      CHARACTER * ( * ) COMENT

      INTEGER STATUS             ! Global status

      INTEGER SLOT               ! The reference slot
      CHARACTER * ( 80 ) STRING  ! The item string
      CHARACTER * ( 8 ) KEYUC    ! Upper case keyword

      INTEGER CHR_LEN            ! Used length of string

 101  FORMAT( A8, '= ', L20, ' / ', A )

      IF ( STATUS .NE. 0 ) RETURN

      CALL ERR_MARK
      STATUS = SAI__OK

      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL DSA1_OWFITS( SLOT, STATUS )
      KEYUC = KEY
      CALL CHR_UCASE( KEYUC )
      WRITE( STRING, 101 ) KEYUC, VALUE, COMENT(:CHR_LEN(COMENT))
      CALL DSA1_VALITM( SLOT, STRING, STATUS )

 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END



      SUBROUTINE DSA1_VALITM( SLOT, STRING, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DSA_COMMON'       ! DSA global variables
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      INTEGER SLOT
      CHARACTER * ( 80 ) STRING

      INTEGER STATUS

      CHARACTER * ( 80 ) ENDSTR  ! 'END' string of correct length
      PARAMETER ( ENDSTR = 'END' )

      LOGICAL CMMT               ! Whether keyword is comment/history
      LOGICAL ISSTR              ! Ignored
      INTEGER ISTR               ! Final index into item array
      CHARACTER * ( 8 ) KEYUC    ! Upper case version of keyword
      CHARACTER * ( 1 ) CHAR1    ! First character in keyword
      CHARACTER * ( 80 ) IGNOR1  ! Ignored
      CHARACTER * ( 80 ) IGNOR2  ! Ignored

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Upper-case keyword.
      KEYUC = STRING(:8)
      CALL CHR_UCASE( KEYUC )

*  If the FITS extension is brandnew.
      IF ( DSA__REFFNE(SLOT) .EQ. 0 ) THEN

*     The new item goes into element 1.
*     And 'END' goes into element 2.
         ISTR = 1
         DSA__REFFNE(SLOT) = 2

*  Else.
      ELSE

*     If the keyword is COMMENT, HISTORY or blank.
         CMMT  = .FALSE.
         CHAR1 = KEYUC(1:1)
         IF ( CHAR1 .EQ. 'H' ) THEN
            CMMT = KEYUC .EQ. 'HISTORY'
         ELSE IF ( CHAR1 .EQ. 'C' ) THEN
            CMMT = KEYUC .EQ. 'COMMENT'
         ELSE IF ( CHAR1 .EQ. ' ' ) THEN
            CMMT = KEYUC .EQ. ' '
         END IF
         IF ( CMMT ) THEN

*        The new item replaces 'END' in the last element.
*        Unmap, alter, and re-map the extension.
            ISTR = DSA__REFFNE(SLOT)
            DSA__REFFNE(SLOT) = DSA__REFFNE(SLOT) + 1
            CALL DAT_UNMAP( DSA__REFFLO(SLOT), STATUS )
            CALL DAT_ALTER( DSA__REFFLO(SLOT),
     :         1, DSA__REFFNE(SLOT), STATUS )
            CALL DAT_MAPV( DSA__REFFLO(SLOT), '_CHAR*80', 'UPDATE',
     :         DSA__REFFPT(SLOT), DSA__REFFNE(SLOT), STATUS )

*     Else (single-item keyword and existing FITS extension).
         ELSE

*        Try to find the old item for the keyword.
*        If this succeeds, it tells where the item is to go.
            CALL DSA1_KEYVAL( DSA__REFFNE(SLOT),
     :         %VAL( CNF_PVAL(DSA__REFFPT(SLOT)) ), KEYUC, 1,
     :         IGNOR1, IGNOR2, ISTR, ISSTR, STATUS, %VAL(CNF_CVAL(80)) )

*        If no old item for keyword.
            IF ( STATUS .NE. SAI__OK ) THEN

*           Anull the error.
*           The new item replaces 'END' in the last element.
*           Unmap, alter, and re-map the extension.
               CALL ERR_ANNUL( STATUS )
               ISTR = DSA__REFFNE(SLOT)
               DSA__REFFNE(SLOT) = DSA__REFFNE(SLOT) + 1
               CALL DAT_UNMAP( DSA__REFFLO(SLOT), STATUS )
               CALL DAT_ALTER( DSA__REFFLO(SLOT),
     :            1, DSA__REFFNE(SLOT), STATUS )
               CALL DAT_MAPV( DSA__REFFLO(SLOT), '_CHAR*80', 'UPDATE',
     :            DSA__REFFPT(SLOT), DSA__REFFNE(SLOT), STATUS )

            END IF

         END IF

      END IF

*  We now know that the item is to go into element no. ISTR. Put it
*  there. Also put 'END' into the last item, in case we altered the
*  the length of the extension and are overwriting the current 'END'.
      CALL DSA1_ITMPUT( DSA__REFFNE(SLOT),
     :                  %VAL( CNF_PVAL(DSA__REFFPT(SLOT)) ),
     :                  STRING, ISTR, STATUS, %VAL(CNF_CVAL(80)) )
      CALL DSA1_ITMPUT( DSA__REFFNE(SLOT),
     :                  %VAL( CNF_PVAL(DSA__REFFPT(SLOT)) ),
     :                  ENDSTR, DSA__REFFNE(SLOT), STATUS,
     :                  %VAL(CNF_CVAL(80)) )

*  Return.
      END



      SUBROUTINE DSA1_ITMPUT( NELM, ARRAY, STRING, ISTR, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER NELM
      CHARACTER * ( 80 ) ARRAY( NELM )
      CHARACTER * ( 80 ) STRING
      INTEGER ISTR

      INTEGER STATUS

      IF ( STATUS .NE. SAI__OK ) RETURN

      ARRAY(ISTR) = STRING

      END



      SUBROUTINE DSA1_OWFITS( SLOT, STATUS )

      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DSA_COMMON'       ! DSA global variables
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      INTEGER SLOT               ! The reference slot
      INTEGER STATUS             ! Global status

      LOGICAL THERE              ! Whether FITS extension is there

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( DSA__REFFLO(SLOT) .EQ. DAT__NOLOC ) THEN
         CALL NDF_XSTAT( DSA__REFID1(SLOT), 'FITS', THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_XLOC( DSA__REFID1(SLOT), 'FITS', 'UPDATE',
     :         DSA__REFFLO(SLOT), STATUS )
            CALL DAT_MAPV( DSA__REFFLO(SLOT), '_CHAR*80', 'UPDATE',
     :         DSA__REFFPT(SLOT), DSA__REFFNE(SLOT), STATUS )
         ELSE
            CALL NDF_XNEW( DSA__REFID1(SLOT), 'FITS', '_CHAR*80',
     :         1, 2, DSA__REFFLO(SLOT), STATUS )
            CALL DAT_MAPV( DSA__REFFLO(SLOT), '_CHAR*80', 'WRITE',
     :         DSA__REFFPT(SLOT), DSA__REFFNE(SLOT), STATUS )
            DSA__REFFNE(SLOT) = 0
         END IF
      END IF

      END
