*+COM_DBS_CHARS    Include file, holds character iterms, each file, field
 
      CHARACTER*(CRITERION_LENGTH) CRITERION(MAX_FILE_COUNT)	! Join criterion
 
      CHARACTER*128 FILENAME(MAX_FILE_COUNT)			! File name
      CHARACTER*17 NULVALUE(MAX_FIELD_COUNT, MAX_FILE_COUNT)	! Default vals
      CHARACTER*10 UNIT(MAX_FIELD_COUNT, MAX_FILE_COUNT)	! Field units
      CHARACTER*1  ORDER(MAX_FILE_COUNT)			! File order
 
      COMMON /DBS_CHARS/ CRITERION, ORDER, FILENAME, UNIT,
     &                   NULVALUE
