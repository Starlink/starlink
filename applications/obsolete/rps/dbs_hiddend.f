*+DBS_HIDDEND Gets R*8 from a hidden field that follows a given field.
*  June 1992	M DUESTERHAUS		modify for port to unix
**************************************************************************
      SUBROUTINE DBS_HIDDEND(REF_NO,FIELD,DPVAL)

*  ---------
*  Variables
*  ---------

      IMPLICIT NONE

*Input:

      INTEGER REF_NO					!Reference number of files.
      DOUBLE PRECISION DPVAL				!Hidden double precision value.
      CHARACTER*(*) FIELD				!Field after which double precision value is to be inserted.

*  Common blocks
*  -------------

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_rec.inc'

*FUNCTIONS
      DOUBLE PRECISION  MDH_CTOD
*Local:

      CHARACTER*10  BA					!Byte array.
      DOUBLE PRECISION STORE				!Location to store the passed double precision value in.

      INTEGER FIELD_LENGTH				!Length of field which precedes insertion.
      INTEGER BYTENO					!Byte number of preceding record.
      CHARACTER*6 FORMAT				!Format of field.
      INTEGER FIELDNO					!Field number
      INTEGER START					!Byte number at which double precision value is to be inserted.

*  Functions
*  ---------

      INTEGER DBS_FIELDNO				!Gets field number
      INTEGER DBS_INFOI					!Used to get byte number of field
      CHARACTER*6 DBS_INFOC				!Used to get length of field.


*- M.L.Bush ( RAL )                             Oct. 1988

      FIELDNO=DBS_FIELDNO(REF_NO,FIELD)
      FORMAT=DBS_INFOC(REF_NO,FIELDNO,'FORMAT')
      READ(FORMAT,'(2X,I2)')FIELD_LENGTH
      BYTENO=DBS_INFOI(REF_NO,FIELDNO,'START')
      START=BYTENO+FIELD_LENGTH

      BA=RECORD(REF_NO)(START:START+11)
      STORE = MDH_CTOD (BA)
      DPVAL=STORE

      END
