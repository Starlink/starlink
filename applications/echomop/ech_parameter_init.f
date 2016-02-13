      SUBROUTINE ECH_PARAMETER_INIT( PARAMETER_NAME, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_PARAMETER_INIT

*  Purpose:
*     Setup a user parameter with a value.

*  Description :
*     This routine is called by every processing module to ensure that any
*     required data/reduction database mappings are made.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_REPORT.INC'

*  Arguments:
      CHARACTER*( * ) PARAMETER_NAME
      INTEGER STATUS

*  Local Variables:
      REAL VALUE

      INTEGER P_NUMBER
      INTEGER MAPPED_ADDRESS
      INTEGER MAPPED_QADDRESS
      INTEGER MAPPED_EADDRESS
      INTEGER DEFAULT_INDEX
      INTEGER I
      INTEGER ECH_OBJ_IND

      LOGICAL BOOLEAN_VALUE
      LOGICAL OBJECT_ACTIVE

      CHARACTER*80 STRING
      CHARACTER*80 REF_NAME
      CHARACTER*32 TYPE
*.

C
C       Check if object has already got an access active
C
         object_active = .FALSE.
         i = 1
         DO WHILE ( .NOT. object_active .AND. i .LE. access_count )
            IF ( parameter_name .EQ. object_name ( i ) )  THEN
               object_active = .TRUE.
            ENDIF
            i = i + 1
         END DO
C
C        Map the object onto virtual memory
C
         IF ( .NOT. object_active ) THEN
C
C  Locate parameter by searching the table for its name as passed by the caller
        p_number = ECH_OBJ_IND ( parameter_name )
        IF ( p_number .EQ. max_required_objects ) THEN
          CONTINUE

        ELSE
           status = 0
           ref_name = required_objects ( p_number )
           type = required_type ( p_number )
           default_index = defaults_indices ( p_number )

           CALL ECH_GET_PARAMETER( ref_name, type, value,
     :          boolean_value, string, default_index, status )
           IF ( status .EQ. 0 ) THEN
                  access_count = access_count + 1
                  object_name ( access_count ) = ref_name
                  object_type ( access_count ) = type
                  object_size ( access_count ) = 0
                  object_address ( access_count ) = 0
                  object_index ( access_count ) = p_number
           ENDIF

C          If required object has been located then set up the
C                relevant address/variable in common.
C
           IF ( status .EQ. 0 .OR.
     :                status .EQ. ECH__IS_ACCESSED ) THEN
              CALL ECH_SETUP_OBJECT_REF( ref_name,
     :             mapped_address,
     :             mapped_qaddress,
     :             mapped_eaddress,
     :             string,
     :             value,
     :             boolean_value,
     :             status )
           ENDIF
        ENDIF

        IF ( status .EQ. ECH__IS_ACCESSED ) status =  0

        ENDIF

        END
