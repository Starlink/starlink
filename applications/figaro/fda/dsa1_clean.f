      SUBROUTINE DSA1_CLEAN( SLOT, NDFTYP, NELM, DATA, STATUS )
*+
*  Name:
*     DSA1_CLEAN

*  Purpose:
*     Remove bad values from an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_CLEAN( SLOT, NDFTYP, NELM, DATA, STATUS )

*  Description:
*     This routine replaces bad values in an array with wrong but good
*     values. Currently the replacement value is taken from the
*     previous element of the array. If the first element is bad, then
*     zero is used to replace it. This is the traditional Figaro
*     mechanism, although it leaves a lot to be desired. To use zero
*     at the beginning of the array is arbitrary. Also for data of
*     true dimension of 2 or more the start of a row is filled
*     in from the end of the previous row, which is plain wrong.
*
*     The hope of course is that either
*     -  data have only few bad values that have little influence on
*        what's going on, or
*     -  the operation is on a pixel-by-pixel basis and the bad pixel
*        information is propagated by more subtle means, or
*     -  that applications handle bad values properly and this routine
*        isn't called.
*
*     This routine is given the reference slot number so that in a
*     future implementation it might find out about the shape and
*     size of the array via its NDF identifier. It could then fill in
*     the bad values with more sensitive interpolation or extrapolation.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The reference slot number that can be used to look up its
*        NDF shape. Assuming that DATA is the data or variance array
*        of the NDF associated with that reference slot, this routine
*        could in future find out a number of things about DATA that
*        might help it perform better.
*     NDFTYP = CHARACTER * ( * ) (Given)
*        The data type of the array in NDF speak. Recognised are
*        _REAL, _DOUBLE, _INTEGER, _BYTE, _WORD, _UWORD.
*     NELM = INTEGER (Given)
*        The size of the array DATA.
*     DATA( NELM ) = NDFTYP (Given and Returned)
*        The array to be decontaminated of bad values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Nov 1995 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER SLOT
      CHARACTER * ( * ) NDFTYP
      INTEGER NELM

*  Arguments Given and Returned:
      BYTE DATA( 1 )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Depending on the given type, just call the correct routine.
*  The interface to these routines is a matter between this routine
*  and them. It may change in future. E.g. this routine might use
*  the SLOT to get more information and it would pass it on to the
*  routines called here.
      IF ( NDFTYP .EQ. '_REAL' ) THEN
         CALL DSA2_CLEANF( NELM, DATA, STATUS )
      ELSE IF ( NDFTYP .EQ. '_DOUBLE' ) THEN
         CALL DSA2_CLEAND( NELM, DATA, STATUS )
      ELSE IF ( NDFTYP .EQ. '_INTEGER' ) THEN
         CALL DSA2_CLEANI( NELM, DATA, STATUS )
      ELSE IF ( NDFTYP .EQ. '_BYTE' ) THEN
         CALL DSA2_CLEANB( NELM, DATA, STATUS )
      ELSE IF ( NDFTYP .EQ. '_WORD' ) THEN
         CALL DSA2_CLEANS( NELM, DATA, STATUS )
      ELSE IF ( NDFTYP .EQ. '_UWORD' ) THEN
         CALL DSA2_CLEANU( NELM, DATA, STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T003', NDFTYP )
         CALL ERR_REP( 'FDA_E001', 'DSA1_CLEAN: ' //
     :      'Cannot clean an array of type ^FDA_T003.', STATUS )
      END IF

*  Return.
      END



      SUBROUTINE DSA2_CLEANF( NELM, DATA, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      REAL DATA( NELM )
      INTEGER STATUS

      INTEGER I
      REAL FILLER

      IF ( STATUS .NE. SAI__OK ) RETURN

      FILLER = 0.
      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADR ) THEN
            DATA(I) = FILLER
         ELSE
            FILLER = DATA(I)
         END IF
 1    CONTINUE

      END

      SUBROUTINE DSA2_CLEAND( NELM, DATA, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      DOUBLE PRECISION DATA( NELM )
      INTEGER STATUS

      INTEGER I
      DOUBLE PRECISION FILLER

      IF ( STATUS .NE. SAI__OK ) RETURN

      FILLER = 0D0
      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADD ) THEN
            DATA(I) = FILLER
         ELSE
            FILLER = DATA(I)
         END IF
 1    CONTINUE

      END

      SUBROUTINE DSA2_CLEANI( NELM, DATA, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      INTEGER DATA( NELM )
      INTEGER STATUS

      INTEGER I
      INTEGER FILLER

      IF ( STATUS .NE. SAI__OK ) RETURN

      FILLER = 0
      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADI ) THEN
            DATA(I) = FILLER
         ELSE
            FILLER = DATA(I)
         END IF
 1    CONTINUE

      END

      SUBROUTINE DSA2_CLEANB( NELM, DATA, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      BYTE DATA( NELM )
      INTEGER STATUS

      INTEGER I
      BYTE FILLER

      IF ( STATUS .NE. SAI__OK ) RETURN

      FILLER = 0
      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADB ) THEN
            DATA(I) = FILLER
         ELSE
            FILLER = DATA(I)
         END IF
 1    CONTINUE

      END

      SUBROUTINE DSA2_CLEANS( NELM, DATA, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      INTEGER * 2 DATA( NELM )
      INTEGER STATUS

      INTEGER I
      INTEGER * 2 FILLER

      IF ( STATUS .NE. SAI__OK ) RETURN

      FILLER = 0
      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADW ) THEN
            DATA(I) = FILLER
         ELSE
            FILLER = DATA(I)
         END IF
 1    CONTINUE

      END

      SUBROUTINE DSA2_CLEANU( NELM, DATA, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NELM
      INTEGER * 2 DATA( NELM )
      INTEGER STATUS

      INTEGER I
      INTEGER * 2 FILLER

      IF ( STATUS .NE. SAI__OK ) RETURN

      FILLER = 0
      DO 1 I = 1, NELM
         IF ( DATA(I) .EQ. VAL__BADUW ) THEN
            DATA(I) = FILLER
         ELSE
            FILLER = DATA(I)
         END IF
 1    CONTINUE

      END
