      SUBROUTINE DSA1_MRGFQ( NDFTYP, NELM, DATA, QUAL, STATUS )
*+
*  Name:
*     DSA1_MRGFQ

*  Purpose:
*     Merge bad values from data into quality array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_MRGFQ( NDFTYP, NELM, DATA, QUAL, STATUS )

*  Description:
*     This routine combines the bad-pixel information from a data array
*     and the corresponding quality array. Afterwards the data will
*     have a bad value where there was one before or where quality was
*     not zero. And afterwards the quality will be 255 where it was
*     non-zero before or where the data contained a bad value.
*
*     Hence this routine makes bad value information and quality
*     information identical. Afterwards the caller can either clean the
*     data and keep the quality, or keep the data and discard the quality.

*  Arguments:
*     NDFTYP = CHARACTER * ( * ) (Given)
*        The data type of the data array in NDF speak. Recognised are
*        _REAL, _DOUBLE, _INTEGER, _BYTE, _WORD, _UWORD.
*     NELM = INTEGER (Given)
*        The size of the arrays DATA and QUAL.
*     DATA( NELM ) = NDFTYP (Given and Returned)
*        The data array to be searched for bad values.
*     QUAL( NELM ) = BYTE (Given and Returned)
*        The quality array to be searched for non-zero values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26 Nov 1995 (hme):
*        Original version.
*     13 Dec 1995 (hme):
*        Use a quality value of +1 to mark bad values.
*     14 Dec 1995 (hme):
*        Review again. The processing should not follow the DSA example,
*        but fewer, simpler, firmer principles.
*        Basically, first merge quality into bad values, then merge bad
*        values into quality. The principle involved here is that of
*        equivalence of bad values and quality.
*        Also use a quality value of 255 so that any bit in the bad-bits
*        mask will flag the pixel as bad.
*     06 Mar 1996 (hme):
*        Correct documentation.
*     15 Aug 2005 (timj):
*        Use NUM__MAXUB to initialise BADBIT
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) NDFTYP
      INTEGER NELM
      BYTE DATA( 1 )

*  Arguments Given and Returned:
      BYTE QUAL( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Depending on the given type, just call the correct routine.
      IF ( NDFTYP .EQ. '_REAL' ) THEN
         CALL DSA2_MRGFQF( NELM, DATA, QUAL, STATUS )
      ELSE IF ( NDFTYP .EQ. '_DOUBLE' ) THEN
         CALL DSA2_MRGFQD( NELM, DATA, QUAL, STATUS )
      ELSE IF ( NDFTYP .EQ. '_INTEGER' ) THEN
         CALL DSA2_MRGFQI( NELM, DATA, QUAL, STATUS )
      ELSE IF ( NDFTYP .EQ. '_BYTE' ) THEN
         CALL DSA2_MRGFQB( NELM, DATA, QUAL, STATUS )
      ELSE IF ( NDFTYP .EQ. '_WORD' ) THEN
         CALL DSA2_MRGFQS( NELM, DATA, QUAL, STATUS )
      ELSE IF ( NDFTYP .EQ. '_UWORD' ) THEN
         CALL DSA2_MRGFQU( NELM, DATA, QUAL, STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T003', NDFTYP )
         CALL ERR_REP( 'FDA_E004', 'DSA1_MRGFQ: ' //
     :      'Cannot check an array of type ^FDA_T003 ' //
     :      'for bad values.', STATUS )
      END IF

*  Return.
      END


      SUBROUTINE DSA2_MRGFQF( NELM, DATA, QUAL, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      REAL DATA( NELM )
      BYTE QUAL( NELM )
      INTEGER STATUS

      BYTE BADBIT
      PARAMETER ( BADBIT = NUM__MAXUB )
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADR .OR. QUAL(I) .NE. 0 ) THEN
            DATA(I) = VAL__BADR
            QUAL(I) = BADBIT
         END IF
 1    CONTINUE

      END


      SUBROUTINE DSA2_MRGFQD( NELM, DATA, QUAL, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      DOUBLE PRECISION DATA( NELM )
      BYTE QUAL( NELM )
      INTEGER STATUS

      BYTE BADBIT
      PARAMETER ( BADBIT = NUM__MAXUB )
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADD .OR. QUAL(I) .NE. 0 ) THEN
            DATA(I) = VAL__BADD
            QUAL(I) = BADBIT
         END IF
 1    CONTINUE

      END


      SUBROUTINE DSA2_MRGFQI( NELM, DATA, QUAL, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      INTEGER DATA( NELM )
      BYTE QUAL( NELM )
      INTEGER STATUS

      BYTE BADBIT
      PARAMETER ( BADBIT = NUM__MAXUB )
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADI .OR. QUAL(I) .NE. 0 ) THEN
            DATA(I) = VAL__BADI
            QUAL(I) = BADBIT
         END IF
 1    CONTINUE

      END


      SUBROUTINE DSA2_MRGFQB( NELM, DATA, QUAL, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      BYTE DATA( NELM )
      BYTE QUAL( NELM )
      INTEGER STATUS

      BYTE BADBIT
      PARAMETER ( BADBIT = NUM__MAXUB )
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADB .OR. QUAL(I) .NE. 0 ) THEN
            DATA(I) = VAL__BADB
            QUAL(I) = BADBIT
         END IF
 1    CONTINUE

      END


      SUBROUTINE DSA2_MRGFQS( NELM, DATA, QUAL, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      INTEGER * 2 DATA( NELM )
      BYTE QUAL( NELM )
      INTEGER STATUS

      BYTE BADBIT
      PARAMETER ( BADBIT = NUM__MAXUB )
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADW .OR. QUAL(I) .NE. 0 ) THEN
            DATA(I) = VAL__BADW
            QUAL(I) = BADBIT
         END IF
 1    CONTINUE

      END


      SUBROUTINE DSA2_MRGFQU( NELM, DATA, QUAL, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      INTEGER * 2 DATA( NELM )
      BYTE QUAL( NELM )
      INTEGER STATUS

      BYTE BADBIT
      PARAMETER ( BADBIT = NUM__MAXUB )
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADUW .OR. QUAL(I) .NE. 0 ) THEN
            DATA(I) = VAL__BADUW
            QUAL(I) = BADBIT
         END IF
 1    CONTINUE

      END
