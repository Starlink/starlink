*+COM_DBS_BYTES    Include file, holds field lengths, location in record
 
      INTEGER LENTH(MAX_FIELD_COUNT, MAX_FILE_COUNT)
      INTEGER START(MAX_FIELD_COUNT, MAX_FILE_COUNT)
 
      COMMON /DBS_BYTES/ START, LENTH
