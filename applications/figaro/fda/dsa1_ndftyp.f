      SUBROUTINE DSA1_NDFTYP( TYPE, NDFTYP, STATUS )
*+
*  Name:
*     DSA1_NDFTYP

*  Purpose:
*     Convert Figaro data type specification to NDF speak.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_NDFTYP( TYPE, NDFTYP, STATUS )

*  Description:
*     This routine takes a Figaro data type such as 'FLOAT' or 'SHORT'
*     and returns the corresponding NDF data type such as '_REAL' or
*     '_WORD'.
*
*     The translations are:
*        FLOAT      _REAL
*        INT        _INTEGER
*        DOUBLE     _DOUBLE
*        SHORT      _WORD
*        BYTE       _BYTE
*        USHORT     _UWORD

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The type specification as used in a Figaro application. The
*        case is not important.
*     NDFTYP = CHARACTER * ( * ) (Returned)
*        The type specification to be used for NDF and HDS. This will
*        be in upper case. The string should have a declared length of
*        at least NDF__SZTYP as declared in the include file NDF_PAR.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     'BYTE' is converted to '_BYTE', even though the most
*     common use of 'BYTE' is for quality arrays and hence should be
*     the NDF type '_UBYTE'. This is not a problem, since the actual
*     access to quality by the DSA system will always specify '_UBYTE'
*     to the NDF system. It would get difficult only when quality was
*     checked against bad values: DSA_GET_FLAG_VALUE returns for
*     'BYTE' the number VAL__BADB (-128), while bad quality values in
*     the file would be mapped to the value VAL__BADUB (255). But
*     currently NDF does not support the concept of bad quality values
*     and Figaro applications test only whether quality is zero or not.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Nov 1995 (hme):
*        Original version.
*     12 Dec 1995 (hme):
*        Correct DATA statement.
*     30 Jul 1996 (mjcl):
*        Moved NDF_TYPES as it contains DATA statements.
*        Temporary hardwire of number of TYPES as 7.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Arguments Given:
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      CHARACTER * ( * ) NDFTYP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      CHARACTER * ( 6 ) TYPEUC   ! Given type in upper case
      CHARACTER * ( NDF__SZTYP ) NDF_TYPES( 7 )

*  Includes with DATA statements:
      INCLUDE 'DSA_TYPES'        ! DSA data types and their sizes

*  Local Data:
      DATA NDF_TYPES
     :   / '_REAL', '_INTEGER', '_DOUBLE', '_WORD',
     :     '_CHAR', '_BYTE',    '_UWORD' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fold given type to upper case.
      TYPEUC = TYPE
      CALL CHR_UCASE( TYPEUC )

*  Check each type, first match bails out.
      DO 1 I = 1, MAX_TYPES
         IF ( TYPEUC .EQ. TYPE_NAMES(I) .AND. I .NE. CHAR_TYPE ) THEN
            NDFTYP = NDF_TYPES(I)
            GO TO 2
         END IF
 1    CONTINUE
         NDFTYP = NDF_TYPES(1)
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T004', TYPEUC )
         CALL ERR_REP( 'FDA_E006', 'DSA1_NDFTYP: ' //
     :      'Cannot translate type ^FDA_T004.', STATUS )
 2    CONTINUE

*  Return.
      END



      SUBROUTINE DSA1_DSATYP( NDFTYP, TYPE, STATUS )

      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

      CHARACTER * ( * ) NDFTYP
      CHARACTER * ( * ) TYPE
      INTEGER STATUS             ! Global status

      INTEGER I                  ! Loop index
      CHARACTER * ( 6 ) TYPEUC   ! Given type in upper case
      CHARACTER * ( NDF__SZTYP ) NDF_TYPES( 7 )

      INCLUDE 'DSA_TYPES'        ! DSA data types and their sizes

      DATA NDF_TYPES
     :   / '_REAL', '_INTEGER', '_DOUBLE', '_WORD',
     :     '_CHAR', '_BYTE',    '_UWORD' /

      IF ( STATUS .NE. SAI__OK ) RETURN
      TYPEUC = NDFTYP
      CALL CHR_UCASE( TYPEUC )
      DO 1 I = 1, MAX_TYPES
         IF ( TYPEUC .EQ. NDF_TYPES(I) .AND. I .NE. CHAR_TYPE ) THEN
            TYPE = TYPE_NAMES(I)
            GO TO 2
         END IF
 1    CONTINUE
         TYPE = TYPE_NAMES(1)
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T003', TYPEUC )
         CALL ERR_REP( 'FDA_E007', 'DSA1_DSATYP: ' //
     :      'Cannot translate type ^FDA_T003.', STATUS )
 2    CONTINUE

      END
