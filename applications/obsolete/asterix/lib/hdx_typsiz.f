*+  HDX_TYPSIZ - Return size in bytes of one element of type TYPE
      SUBROUTINE HDX_TYPSIZ( TYPE, SIZE, STATUS )
*
*    Description :
*
*     Tests the input string against each of the valid HDS types. If found
*     the size of one element of the type is returned, otherwise an error
*     is generated.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 Aug 91 : Original (DJA)
*      6 Jan 94 : Use PRM constants instead (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
*
*    Import :
*
      CHARACTER*(*)                 TYPE          ! Type description
*
*    Export :
*
      INTEGER                       SIZE          ! Size of type in bytes
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Check different types
        IF ( TYPE(1:5) .EQ. '_REAL' ) THEN
          SIZE = VAL__NBR

        ELSE IF ( ( TYPE(1:8) .EQ. '_INTEGER' ) .OR.
     :            ( TYPE(1:8) .EQ. '_LOGICAL' ) ) THEN
          SIZE = VAL__NBI

        ELSE IF ( TYPE(1:7) .EQ. '_DOUBLE' ) THEN
          SIZE = VAL__NBD

        ELSE IF ( ( TYPE(1:5) .EQ. '_WORD' ) .OR.
     :            ( TYPE(1:6) .EQ. '_UWORD' ) ) THEN
          SIZE = VAL__NBW

        ELSE IF ( ( TYPE(1:5) .EQ. '_BYTE' ) .OR.
     :            ( TYPE(1:6) .EQ. '_UBYTE' ) ) THEN
          SIZE = VAL__NBB

        ELSE IF ( TYPE(1:5) .EQ. '_CHAR' ) THEN
          CALL CHR_CTOI( TYPE(7:), SIZE, STATUS )

        ELSE
          STATUS = SAI__ERROR
          CALL MSG_PRNT( '! Unrecognized type '//TYPE )

        END IF

*      Report errors
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL AST_REXIT( 'HDX_TYPSIZ', STATUS )
        END IF

      END IF

      END
