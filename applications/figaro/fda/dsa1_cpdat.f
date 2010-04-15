      SUBROUTINE DSA1_CPDAT( TYPE1, TYPE2, NELM,
     :   ARRAY1, ARRAY2, STATUS )
*+
*  Name:
*     DSA1_CPDAT

*  Purpose:
*     Copy between arrays with data conversion.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_CPDAT( TYPE1, TYPE2, NELM, ARRAY1, ARRAY2, STATUS )

*  Description:
*     This routine is very similar to DSA_FMTCON, but used only
*     internally. The data types are specified to this routine as
*     strings containing an HDS primitive type, e.g. '_REAL'.

*  Arguments:
*     TYPE1 = CHARACTER * ( * ) (Given)
*        The HDS data type of ARRAY1.
*     TYPE2 = CHARACTER * ( * ) (Given)
*        The HDS data type of ARRAY2.
*     NELM = INTEGER (Given)
*        The size of the arrays. In practice this is often the size of
*        the smaller of the two arrays.
*     ARRAY1( NELM ) = TYPE1 (Given)
*        The input array.
*     ARRAY2( NELM ) = TYPE2 (Returned)
*        The output array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Feb 1996 (hme):
*        Original version.
*     05 Feb 1996 (hme):
*        Add _UBYTE and use upper-case conversions.
*     07 Feb 1996 (hme):
*        Convert bad values to avoid overflows.
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
      CHARACTER * ( * ) TYPE1
      CHARACTER * ( * ) TYPE2
      INTEGER NELM
      BYTE ARRAY1( 1 )

*  Arguments Returned:
      BYTE ARRAY2( 1 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IERR, NBAD
      CHARACTER * ( NDF__SZTYP ) TYPE1U
      CHARACTER * ( NDF__SZTYP ) TYPE2U

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      TYPE1U = TYPE1
      TYPE2U = TYPE2
      CALL CHR_UCASE(TYPE1U)
      CALL CHR_UCASE(TYPE2U)

      IF ( TYPE1U .EQ. '_BYTE' ) THEN
         IF ( TYPE2U .EQ. '_BYTE' ) THEN
            CALL VEC_BTOB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_WORD' ) THEN
            CALL VEC_BTOW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_INTEGER' ) THEN
            CALL VEC_BTOI( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_REAL' ) THEN
            CALL VEC_BTOR( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_DOUBLE' ) THEN
            CALL VEC_BTOD( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UWORD' ) THEN
            CALL VEC_BTOUW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UBYTE' ) THEN
            CALL VEC_BTOUB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T003', TYPE2 )
            CALL ERR_REP( 'FDA_E002', 'DSA1_CPDAT: Error, no copy ' //
     :         'routine for NDF type ^FDA_T003 available.', STATUS )
         END IF
      ELSE IF ( TYPE1U .EQ. '_WORD' ) THEN
         IF ( TYPE2U .EQ. '_BYTE' ) THEN
            CALL VEC_WTOB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_WORD' ) THEN
            CALL VEC_WTOW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_INTEGER' ) THEN
            CALL VEC_WTOI( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_REAL' ) THEN
            CALL VEC_WTOR( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_DOUBLE' ) THEN
            CALL VEC_WTOD( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UWORD' ) THEN
            CALL VEC_WTOUW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UBYTE' ) THEN
            CALL VEC_WTOUB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T003', TYPE2 )
            CALL ERR_REP( 'FDA_E002', 'DSA1_CPDAT: Error, no copy ' //
     :         'routine for NDF type ^FDA_T003 available.', STATUS )
         END IF
      ELSE IF ( TYPE1U .EQ. '_INTEGER' ) THEN
         IF ( TYPE2U .EQ. '_BYTE' ) THEN
            CALL VEC_ITOB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_WORD' ) THEN
            CALL VEC_ITOW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_INTEGER' ) THEN
            CALL VEC_ITOI( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_REAL' ) THEN
            CALL VEC_ITOR( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_DOUBLE' ) THEN
            CALL VEC_ITOD( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UWORD' ) THEN
            CALL VEC_ITOUW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UBYTE' ) THEN
            CALL VEC_ITOUB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T003', TYPE2 )
            CALL ERR_REP( 'FDA_E002', 'DSA1_CPDAT: Error, no copy ' //
     :         'routine for NDF type ^FDA_T003 available.', STATUS )
         END IF
      ELSE IF ( TYPE1U .EQ. '_REAL' ) THEN
         IF ( TYPE2U .EQ. '_BYTE' ) THEN
            CALL VEC_RTOB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_WORD' ) THEN
            CALL VEC_RTOW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_INTEGER' ) THEN
            CALL VEC_RTOI( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_REAL' ) THEN
            CALL VEC_RTOR( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_DOUBLE' ) THEN
            CALL VEC_RTOD( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UWORD' ) THEN
            CALL VEC_RTOUW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UBYTE' ) THEN
            CALL VEC_RTOUB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T003', TYPE2 )
            CALL ERR_REP( 'FDA_E002', 'DSA1_CPDAT: Error, no copy ' //
     :         'routine for NDF type ^FDA_T003 available.', STATUS )
         END IF
      ELSE IF ( TYPE1U .EQ. '_DOUBLE' ) THEN
         IF ( TYPE2U .EQ. '_BYTE' ) THEN
            CALL VEC_DTOB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_WORD' ) THEN
            CALL VEC_DTOW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_INTEGER' ) THEN
            CALL VEC_DTOI( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_REAL' ) THEN
            CALL VEC_DTOR( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_DOUBLE' ) THEN
            CALL VEC_DTOD( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UWORD' ) THEN
            CALL VEC_DTOUW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UBYTE' ) THEN
            CALL VEC_DTOUB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T003', TYPE2 )
            CALL ERR_REP( 'FDA_E002', 'DSA1_CPDAT: Error, no copy ' //
     :         'routine for NDF type ^FDA_T003 available.', STATUS )
         END IF
      ELSE IF ( TYPE1U .EQ. '_UWORD' ) THEN
         IF ( TYPE2U .EQ. '_BYTE' ) THEN
            CALL VEC_UWTOB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_WORD' ) THEN
            CALL VEC_UWTOW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_INTEGER' ) THEN
            CALL VEC_UWTOI( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_REAL' ) THEN
            CALL VEC_UWTOR( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_DOUBLE' ) THEN
            CALL VEC_UWTOD( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UWORD' ) THEN
            CALL VEC_UWTOUW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UBYTE' ) THEN
            CALL VEC_UWTOUB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T003', TYPE2 )
            CALL ERR_REP( 'FDA_E002', 'DSA1_CPDAT: Error, no copy ' //
     :         'routine for NDF type ^FDA_T003 available.', STATUS )
         END IF
      ELSE IF ( TYPE1U .EQ. '_UBYTE' ) THEN
         IF ( TYPE2U .EQ. '_BYTE' ) THEN
            CALL VEC_UBTOB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_WORD' ) THEN
            CALL VEC_UBTOW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_INTEGER' ) THEN
            CALL VEC_UBTOI( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_REAL' ) THEN
            CALL VEC_UBTOR( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_DOUBLE' ) THEN
            CALL VEC_UBTOD( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UWORD' ) THEN
            CALL VEC_UBTOUW( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE IF ( TYPE2U .EQ. '_UBYTE' ) THEN
            CALL VEC_UBTOUB( .TRUE., NELM, ARRAY1, ARRAY2,
     :         IERR, NBAD, STATUS )
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T003', TYPE2 )
            CALL ERR_REP( 'FDA_E002', 'DSA1_CPDAT: Error, no copy ' //
     :         'routine for NDF type ^FDA_T003 available.', STATUS )
         END IF
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T003', TYPE1 )
         CALL ERR_REP( 'FDA_E002', 'DSA1_CPDAT: Error, no copy ' //
     :      'routine for NDF type ^FDA_T003 available.', STATUS )
      END IF

      END




      SUBROUTINE DSA1_BFILL( TYPE, NELM, ARRAY, STATUS )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      CHARACTER * ( * ) TYPE
      INTEGER NELM
      BYTE ARRAY( 1 )
      INTEGER STATUS             ! Global status

      CHARACTER * ( NDF__SZTYP ) TYPEU

      IF ( STATUS .NE. SAI__OK ) RETURN

      TYPEU = TYPE
      CALL CHR_UCASE(TYPEU)

      IF ( TYPEU .EQ. '_BYTE' ) THEN
         CALL DSA2_BFILLB( NELM, ARRAY, STATUS )
      ELSE IF ( TYPEU .EQ. '_WORD' ) THEN
         CALL DSA2_BFILLW( NELM, ARRAY, STATUS )
      ELSE IF ( TYPEU .EQ. '_INTEGER' ) THEN
         CALL DSA2_BFILLI( NELM, ARRAY, STATUS )
      ELSE IF ( TYPEU .EQ. '_REAL' ) THEN
         CALL DSA2_BFILLR( NELM, ARRAY, STATUS )
      ELSE IF ( TYPEU .EQ. '_DOUBLE' ) THEN
         CALL DSA2_BFILLD( NELM, ARRAY, STATUS )
      ELSE IF ( TYPEU .EQ. '_UWORD' ) THEN
         CALL DSA2_BFILLUW( NELM, ARRAY, STATUS )
      ELSE IF ( TYPEU .EQ. '_UBYTE' ) THEN
         CALL DSA2_BFILLUB( NELM, ARRAY, STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T003', TYPE )
         CALL ERR_REP( 'FDA_E003', 'DSA1_BFILL: Error, no fill ' //
     :      'routine for NDF type ^FDA_T003 available.', STATUS )
      END IF

      END

      SUBROUTINE DSA2_BFILLB( NELM, ARRAY, STATUS )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INTEGER NELM
      BYTE ARRAY( NELM )
      INTEGER STATUS             ! Global status

      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         ARRAY(I) = VAL__BADB
 1    CONTINUE

      END

      SUBROUTINE DSA2_BFILLW( NELM, ARRAY, STATUS )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INTEGER NELM
      INTEGER * 2 ARRAY( NELM )
      INTEGER STATUS             ! Global status

      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         ARRAY(I) = VAL__BADW
 1    CONTINUE

      END

      SUBROUTINE DSA2_BFILLI( NELM, ARRAY, STATUS )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INTEGER NELM
      INTEGER ARRAY( NELM )
      INTEGER STATUS             ! Global status

      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         ARRAY(I) = VAL__BADI
 1    CONTINUE

      END

      SUBROUTINE DSA2_BFILLR( NELM, ARRAY, STATUS )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INTEGER NELM
      REAL ARRAY( NELM )
      INTEGER STATUS             ! Global status

      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         ARRAY(I) = VAL__BADR
 1    CONTINUE

      END

      SUBROUTINE DSA2_BFILLD( NELM, ARRAY, STATUS )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INTEGER NELM
      DOUBLE PRECISION ARRAY( NELM )
      INTEGER STATUS             ! Global status

      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         ARRAY(I) = VAL__BADD
 1    CONTINUE

      END

      SUBROUTINE DSA2_BFILLUW( NELM, ARRAY, STATUS )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INTEGER NELM
      INTEGER * 2 ARRAY( NELM )
      INTEGER STATUS             ! Global status

      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         ARRAY(I) = VAL__BADUW
 1    CONTINUE

      END

      SUBROUTINE DSA2_BFILLUB( NELM, ARRAY, STATUS )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INTEGER NELM
      BYTE ARRAY( NELM )
      INTEGER STATUS             ! Global status

      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         ARRAY(I) = VAL__BADUB
 1    CONTINUE

      END
