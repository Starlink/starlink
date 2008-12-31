*+COM_DBS_IOF     Include file, holds common Input, Array, Nrecs
 
      INTEGER ARRAY(MAX_FILE_COUNT)		! Pointers
      CHARACTER*1 OPEN_TYPE(MAX_FILE_COUNT)	! Shows how file was opened
      COMMON / DBS_ARRAY / ARRAY , OPEN_TYPE
 
      INTEGER NRECORDS(MAX_FILE_COUNT)		! No. recs in file
      COMMON / DBS_NRECS / NRECORDS
 
      INTEGER LNDB(MAX_FILE_COUNT)		! Logical unit nos.
      INTEGER RECSIZE(MAX_FILE_COUNT)		! No. bytes in record
      COMMON / DBS_INPUT / LNDB, RECSIZE
