      SUBROUTINE ECH_DATAFILE_TIDYUP( STATUS )
*+
*  Name:
*     ECHOMOP - ECH_DATAFILE_TIDYUP

*-

*  Type Definitions:
      IMPLICIT NONE

*  Globals:
      INCLUDE 'ECH_USE_RDCTN.INC'

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL DUMMY_VALUE

      INTEGER DUMDIM( MAX_DIMENSIONS )
      INTEGER I
      INTEGER IDUMMY
      INTEGER ICLOSE
      INTEGER STATIC_INDICES
      INTEGER OBJECT_NUMBER
      INTEGER TO_BE_CLOSED

      CHARACTER*255 FULL_OBJECT_PATH
      CHARACTER*80 PENDING_CLOSURE( 10 )
      CHARACTER*80 LAST_NAME
*.

      TO_BE_CLOSED = 0
      I = 1
*  Loop through all known objects.
      DO WHILE ( I .LE. ACCESS_COUNT )

*     If object is of type IMAGE and active then
*        Retrieve actual filename the active image references
*        Unmap data quality/flags if they are active
*        Unmap data error array if active
*        Unmap data array
         IF ( OBJECT_TYPE( I ) .EQ. 'IMAGE' .AND.
     :        OBJECT_ADDRESS( I ) .NE. 0 ) THEN
            OBJECT_NUMBER = I
            CALL ECH_UPDATE_OBJECT_REF( OBJECT_NAME( I ), IDUMMY,
     :           IDUMMY, IDUMMY, LAST_NAME, DUMMY_VALUE, .FALSE.,
     :           STATUS )
            CALL ECH_GET_DATA_PATH( OBJECT_NAME( I ), FULL_OBJECT_PATH,
     :           OBJECT_TYPE( I ), STATIC_INDICES,  STATUS )
            IF ( OBJECT_QADDRESS( OBJECT_NUMBER ) .NE. 0 ) THEN
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'UNMAP',
     :              'IMAGE-QUALITY', 0, 0,
     :              OBJECT_QHANDLE( OBJECT_NUMBER ), DUMDIM,
     :              MAX_DIMENSIONS, 0, ' ', STATUS )

            ELSE
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'UNMAP',
     :              'IMAGE-FLAGS', 0, 0, 0, DUMDIM, MAX_DIMENSIONS, 0,
     :              ' ', STATUS )
            END IF

            CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'UNMAP',
     :           'IMAGE-DATA', 0, 0, OBJECT_HANDLE( OBJECT_NUMBER ),
     :           DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )

            OBJECT_HANDLE( OBJECT_NUMBER ) = 0
            OBJECT_QHANDLE( OBJECT_NUMBER ) = 0
            IF ( OBJECT_EHANDLE( OBJECT_NUMBER ) .NE. 0 ) THEN
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'UNMAP',
     :              'IMAGE-ERRORS', 0, 0,
     :              OBJECT_EHANDLE( OBJECT_NUMBER ), DUMDIM,
     :              MAX_DIMENSIONS, 0, ' ', STATUS )
               OBJECT_EHANDLE( OBJECT_NUMBER ) = 0
            END IF

*        Clear objects table entries and restore name of associated file
*        Save name of file in `pending closure' table.
*
*        We have to do this because the same file may be opened under
*        other reference names and until we have UNMAPped these
*        references the data-access routines will refuse to release
*        the file. This is filing system dependent, and will need
*        changing if moved to a filing system where the first UNMAP
*        automatically releases all outstanding accesses to the same file.
            OBJECT_ADDRESS( OBJECT_NUMBER ) = 0
            OBJECT_QADDRESS( OBJECT_NUMBER ) = 0
            OBJECT_EADDRESS( OBJECT_NUMBER ) = 0
            CALL ECH_SETUP_OBJECT_REF( OBJECT_NAME( I ), 0, 0, 0,
     :           LAST_NAME, 0.0, .FALSE., STATUS )
            TO_BE_CLOSED = TO_BE_CLOSED + 1
            PENDING_CLOSURE( TO_BE_CLOSED ) = OBJECT_NAME( I )
         END IF
         I = I + 1
      END DO

*  If any files are in the `pending closure' list then loop through
*  list performing final UNMAP of the container file.
      IF ( TO_BE_CLOSED .GT. 0 ) THEN
         DO ICLOSE = 1, TO_BE_CLOSED
            CALL ECH_ACCESS_OBJECT( PENDING_CLOSURE( ICLOSE ),
     :           'UNMAP', 'STRUCTURE', 0, 0, 0, DUMDIM, MAX_DIMENSIONS,
     :           0, ' ', STATUS )
            PENDING_CLOSURE( ICLOSE ) = ' '
         END DO
         TO_BE_CLOSED = 0
      END IF

      END
