*+COM_DBS_REC     Include file, holds current record for each file
 
      CHARACTER*(MAX_REC_SIZE)   RECORD(MAX_FILE_COUNT)
      COMMON / DBS_RECRD / RECORD
*- 

