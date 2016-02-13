      SUBROUTINE ECH_ACCESS_WORKSPACE(
     :           REQUIRED_OBJECT,
     :           TYPE,
     :           ADDRESS,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_ACCESS_WORKSPACE

*  Purpose:
*     Interfaces to explicit memory alloc/dealloc calls.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables and Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_REPORT.INC'

*  Arguments:
      CHARACTER*( * ) REQUIRED_OBJECT
      CHARACTER*( * ) TYPE
      INTEGER ADDRESS

*  Global Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER AUNIT
      INTEGER I
      INTEGER ILEN
      INTEGER SIZE
      INTEGER STATIC_INDICES
      INTEGER INDEX
      INTEGER VM_SIZE
      INTEGER TYPE_CODE
      INTEGER NCHAR1
      INTEGER NCHAR2

      LOGICAL ALREADY_ACTIVE

      CHARACTER*255 FULL_OBJECT_PATH
      CHARACTER*80 PATH_NAME
      CHARACTER*32 REF_STR1
      CHARACTER*32 REF_STR2

*  External Functions:
      INTEGER ECH_WORD_LEN
      INTEGER CHR_LEN
*.

      STATIC_INDICES = 0
      CALL ECH_GET_OBJECT_PATH( REQUIRED_OBJECT, FULL_OBJECT_PATH,
     :     PATH_NAME, STATIC_INDICES, STATUS )

*  Check if object has already got an access active.
      ALREADY_ACTIVE = .FALSE.
      I = 1
      INDEX = 0
      DO WHILE ( .NOT. ALREADY_ACTIVE .AND. I .LE. WS_ACCESS_COUNT )
         IF ( .NOT. WS_ACCESS_ACTIVE( I ) ) THEN
            IF ( INDEX .EQ. 0 ) INDEX = I

         ELSE IF ( FULL_OBJECT_PATH .EQ. WS_OBJECT_NAME( I ) )  THEN
            ALREADY_ACTIVE = .TRUE.
            INDEX = I
            ADDRESS = WS_OBJECT_ADDRESS( I )
            SIZE = WS_OBJECT_SIZE( I )
            GO TO 100
         END IF
         I = I + 1
      END DO

  100 STATUS = SAI__OK

*  Map the object onto virtual memory.
      IF ( .NOT. ALREADY_ACTIVE ) THEN
         IF ( INDEX .EQ. 0 ) INDEX = WS_ACCESS_COUNT + 1
         SIZE = 1
         IF ( STATIC_INDICES .GT. 0 ) THEN
            DO I = 1, STATIC_INDICES
               SIZE = SIZE * DIMEN_VALUE( I )
            END DO
         END IF
         CALL ECH_TYPEINFO( TYPE, TYPE_CODE, AUNIT )
         VM_SIZE = SIZE * AUNIT
         CALL PSX_MALLOC( VM_SIZE, ADDRESS, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ECH_SET_CONTEXT( 'PROBLEM', 'VMem exhausted' )
            STATUS = ECH__BAD_VMEMORY

         ELSE
            IF ( INDEX .GT. WS_ACCESS_COUNT )
     :         WS_ACCESS_COUNT = WS_ACCESS_COUNT + 1
            WS_OBJECT_NAME( INDEX ) = FULL_OBJECT_PATH
            WS_OBJECT_TYPE( INDEX ) = TYPE
            WS_OBJECT_SIZE( INDEX ) = SIZE
            WS_OBJECT_ADDRESS( INDEX ) = ADDRESS
            WS_ACCESS_ACTIVE( INDEX ) = .TRUE.
            CALL ECH_ZERO_INIT( SIZE, AUNIT, %VAL( ADDRESS ) )
         END IF

*  Active workspace will be released automatically upon
*  a re-access request. ie the first access to a named
*  workspace object creates it; the next access deletes it.
      ELSE
         CALL PSX_FREE( ADDRESS, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ECH_SET_CONTEXT( 'PROBLEM', 'VMem release' )
            STATUS = ECH__BAD_VMEMORY
         END IF
         WS_ACCESS_ACTIVE( INDEX ) = .FALSE.
         WS_OBJECT_ADDRESS( INDEX ) = 0
         WS_OBJECT_SIZE( INDEX ) = 0
         WS_OBJECT_NAME( INDEX ) = ' '
         WS_OBJECT_TYPE( INDEX ) = ' '
         IF ( WS_ACCESS_COUNT .EQ. INDEX ) THEN
            I = INDEX
            DO WHILE ( .NOT. WS_ACCESS_ACTIVE( I ) .AND. I .GT. 0 )
               WS_ACCESS_COUNT = WS_ACCESS_COUNT - 1
               I = I - 1
            END DO
         END IF
      END IF

*  Diagnostics.
      IF ( DIAGNOSTICS_ACTIVE ) THEN
         ILEN = ECH_WORD_LEN( FULL_OBJECT_PATH )
         CALL CHR_ITOC( SIZE, REF_STR1, NCHAR1 )
         CALL CHR_ITOC( ADDRESS, REF_STR2, NCHAR2 )
         REPORT_STRING = ' Workspace'
         IF ( ALREADY_ACTIVE ) THEN
            REPORT_STRING = REPORT_STRING(
     :            : CHR_LEN( REPORT_STRING ) ) // ' freed:'

         ELSE
            REPORT_STRING = REPORT_STRING(
     :            : CHR_LEN( REPORT_STRING ) ) // ' allocated:'
         END IF
         REPORT_STRING = REPORT_STRING(
     :         : CHR_LEN( REPORT_STRING ) ) //  ' ' //
     :         FULL_OBJECT_PATH( :ILEN ) // ' ' //
     :         TYPE( :ECH_WORD_LEN( TYPE ) ) // ' ' //
     :         REF_STR1( :NCHAR1 ) // ' units @' //
     :         REF_STR2( :NCHAR2 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

      END
