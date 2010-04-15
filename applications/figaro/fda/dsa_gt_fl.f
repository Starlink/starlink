      SUBROUTINE DSA_GET_FLAG_VALUE( TYPE, VALUE, STATUS )
*+
*  Name:
*     DSA_GET_FLAG_VALUE

*  Purpose:
*     Get the bad value for a specified data type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_GET_FLAG_VALUE( TYPE, VALUE, STATUS )

*  Description:
*     This routine returns the the value being used as the `flag'
*     value (`bad' value, `magic number', or whatever) for a specified
*     data type such as 'FLOAT' or 'DOUBLE'.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type for which the bad value is required.
*     VALUE = TYPE (Returned)
*        The bad value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     13 Jul 1988 (ks):
*        Original version.
*     25 Aug 1992 (hme):
*        Return the appropriate PRIMDAT constant.
*     31 Aug 1992 (ks):
*        Replaced use of '/N' with call to DSA_WRFLUSH.
*     15 Jun 1993 (hme):
*        Fold given type before comparing with upper case constants.
*     25 Nov 1995 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      BYTE VALUE                 ! at this level, TYPE above and below

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 6 ) TYPEUC   ! folded type string

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Upper case given type.
      TYPEUC=TYPE
      CALL CHR_UCASE( TYPEUC )

*  Call the type-specific routine.
      IF ( TYPEUC .EQ. 'BYTE' ) THEN
         CALL DSA2_FLAG_B( VALUE )
      ELSE IF ( TYPEUC .EQ. 'WORD'  .OR. TYPEUC .EQ. 'SHORT' ) THEN
         CALL DSA2_FLAG_S( VALUE )
      ELSE IF ( TYPEUC .EQ. 'INT' ) THEN
         CALL DSA2_FLAG_I( VALUE )
      ELSE IF ( TYPEUC .EQ. 'REAL'  .OR. TYPEUC .EQ. 'FLOAT' ) THEN
         CALL DSA2_FLAG_F( VALUE )
      ELSE IF ( TYPEUC .EQ. 'DOUBLE' ) THEN
         CALL DSA2_FLAG_D( VALUE )
      ELSE IF ( TYPEUC .EQ. 'UWORD' .OR. TYPEUC .EQ. 'USHORT' ) THEN
         CALL DSA2_FLAG_U( VALUE )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T004', TYPEUC )
         CALL ERR_REP( 'FDA_E016', 'DSA_GET_FLAG_VALUE: ' //
     :      'Invalid type ^FDA_T004.', STATUS )
      END IF

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




      SUBROUTINE DSA2_FLAG_B( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      BYTE VALUE

      VALUE = VAL__BADB

      END

      SUBROUTINE DSA2_FLAG_S( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INTEGER * 2 VALUE

      VALUE = VAL__BADW

      END

      SUBROUTINE DSA2_FLAG_I( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INTEGER VALUE

      VALUE = VAL__BADI

      END

      SUBROUTINE DSA2_FLAG_F( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      REAL VALUE

      VALUE = VAL__BADR

      END

      SUBROUTINE DSA2_FLAG_D( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      DOUBLE PRECISION VALUE

      VALUE = VAL__BADD

      END

      SUBROUTINE DSA2_FLAG_U( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INTEGER * 2 VALUE

      VALUE = VAL__BADUW

      END
