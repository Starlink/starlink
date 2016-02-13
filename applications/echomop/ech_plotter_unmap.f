      SUBROUTINE ECH_PLOTTER_UNMAP( OBJECT, TYPE, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_PLOTTER_UNMAP
*-

*  Type definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_USE_RDCTN.INC'

*  Arguments Given:
      CHARACTER*( * ) OBJECT

*  Arguments Returned:
      CHARACTER*( * ) TYPE

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER NUM_ABBREV
      PARAMETER ( NUM_ABBREV = 17 )

*  Local Variables:
      INTEGER DDIM( MAX_DIMENSIONS )
      INTEGER I
      INTEGER INDEX
      INTEGER PLEN
      INTEGER STATIC_INDICES

      CHARACTER*128 PATH_NAME
      CHARACTER*128 FINAL_PATH
      CHARACTER*16 EXPANDED( NUM_ABBREV )
      CHARACTER*80 CINDICES
      CHARACTER*80 STRING
      CHARACTER*6 ABBREV( NUM_ABBREV )

*  Functions Called:
      INTEGER ECH_WORD_LEN

*  Data Statements:
      DATA EXPANDED / 'SKY_SPECTRUM',  'SKY_VARIANCE',  'EXTRACTED_OBJ',
     :                'EXTR_OBJ_VAR',  'SCRNCHD_OBJ',   'SCRNCHD_OBJV',
     :                'EXTRACTED_ARC', 'SCRNCHD_ARC',   'FITTED_WAVES',
     :                'FITTED_FLAT',   'FITTED_SKY',    'FSKY_ERRORS',
     :                'BLAZE_SPECT',   'SCRNCHD_WAVES', '1D_SPECTRUM',
     :                'ERR_SPECTRUM',  'WAVELENGTH' /
      DATA ABBREV / 'SKY',   'SKYV',  'OBJ',   'OBJV',  'SOBJ',
     :              'SOBJV', 'ARC',   'SARC',  'FWAV',  'FFLT',
     :              'FSKY',  'FSKYV', 'BLZ',   'SWAV',  '1D',
     :              '1DV',   'WAV' /
*.

*  Get the object name part and indices part from the given string.
      CINDICES = ' '
      INDEX = 0
      DO I = 1, LEN( OBJECT )
         IF ( OBJECT( I : I ) .EQ. '[' ) THEN
            IF ( INDEX .EQ. 0 ) THEN
               INDEX = I - 1
               CINDICES = OBJECT( I : ) // ' '
               OBJECT = OBJECT( : INDEX ) // ' '
               GO TO 100
            END IF
         END IF
      END DO
  100 CONTINUE

*  Check for an abbreviated object name.
      DO I = 1, NUM_ABBREV
         IF ( OBJECT .EQ. ABBREV( I ) ) THEN
            OBJECT = EXPANDED( I )
            GO TO 200
         END IF
      END DO
  200 CONTINUE

      STATIC_INDICES = -1
      CALL ECH_GET_OBJECT_PATH( OBJECT, FINAL_PATH,
     :     PATH_NAME, STATIC_INDICES, STATUS )
      PLEN = ECH_WORD_LEN( FINAL_PATH )
      IF ( FINAL_PATH( PLEN : PLEN ) .EQ. '.' ) FINAL_PATH = OBJECT
      STRING = ' Path syntax: ' // FINAL_PATH( :PLEN )
      CALL ECH_REPORT( 0, STRING )
      OBJECT = PATH_NAME
      PLEN = ECH_WORD_LEN( PATH_NAME )
      IF ( INDEX .GT. 0 )
     :   OBJECT = PATH_NAME( :PLEN ) // CINDICES // ' '

*  Unmap the object if it is already mapped.
      PLEN = ECH_WORD_LEN( PATH_NAME )
      DO I = 1, ACCESS_COUNT
         IF ( OBJECT_NAME( I )( :PLEN ) .EQ. PATH_NAME( :PLEN ) ) THEN
            CALL ECH_ACCESS_OBJECT( OBJECT_NAME( I ),
     :           'UNMAP', OBJECT_TYPE( I ), OBJECT_SIZE( I ),
     :           OBJECT_ADDRESS( I ),
     :           0, DDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
            OBJECT_ADDRESS( I ) = 0
            OBJECT_NAME( I ) = ' '
            OBJECT_SIZE( I ) = 0
            OBJECT_ADDRESS( I ) = 0
            OBJECT_INDEX( I ) = 0
            IF ( I .EQ. ACCESS_COUNT ) ACCESS_COUNT = ACCESS_COUNT - 1
            GO TO 300
         END IF
      END DO
  300 CONTINUE

      CALL ECH_ACCESS_OBJECT( OBJECT, 'READ-TYPE', TYPE, 0,
     :     0, 0, DDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
      IF ( STATUS .NE. 0 ) THEN
         STRING = ' Cannot find ' // OBJECT // '.'
         CALL ECH_REPORT( 0, STRING )
      END IF

      END
