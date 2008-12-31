*+AAA_DBS_PARAMS    Include file holds DBS Array size parameters
 
      INTEGER MAX_REC_SIZE		! Max size of byte array, each record
      PARAMETER (MAX_REC_SIZE=3300)
      INTEGER MAX_FILE_COUNT		! Max number of files open concurrently
      PARAMETER (MAX_FILE_COUNT=2)
      INTEGER MAX_FIELD_COUNT		! Max number of fields per record
      PARAMETER (MAX_FIELD_COUNT=120)
      INTEGER CRITERION_LENGTH		! Character string size, join criterion
      PARAMETER (CRITERION_LENGTH=32)
      INTEGER MAX_KEYFLD_COUNT		! Max no. keyfields allowed per record
      PARAMETER (MAX_KEYFLD_COUNT=10)
      INTEGER FILE_BLOCK_SIZE		! Parameter in DBS_OPEN
      PARAMETER (FILE_BLOCK_SIZE=4)
*- 
