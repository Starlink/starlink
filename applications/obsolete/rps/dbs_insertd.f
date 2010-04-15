*+DBS_INSERTD    Puts R*8 into a record starting at the end of a given field.
*- M.L.Bush ( RAL )                             Oct. 1988
*  M. Duesterhaus	June 1992		Port to UNIX
****************************************************************************
      SUBROUTINE DBS_INSERTD(REF_NO,DPVAL,FIELD)
      IMPLICIT NONE

*Input:

      INTEGER REF_NO					!Reference number of files.
      DOUBLE PRECISION DPVAL				!Double precision value to be inserted.
      CHARACTER*(*) FIELD				!Field after which double precision value is to be inserted.


*Local:

      DOUBLE PRECISION STORE				!Location to store the passed double precision value in.
      CHARACTER*25 TEMP

      INTEGER FIELD_LENGTH				!Length of field which precedes insertion.
      INTEGER BYTENO					!Byte number of preceding record.
      CHARACTER*6 FORMAT				!Format of field.
      INTEGER FIELDNO					!Field number
      INTEGER START					!Byte number at which double precision value is to be inserted.

*  ---------
*  Functions
*  ---------

      INTEGER DBS_FIELDNO				!Gets field number
      INTEGER DBS_INFOI					!Used to get byte number of field
      CHARACTER*6 DBS_INFOC				!Used to get length of field.
      CHARACTER*25 MDH_DTOC

*  -------------
*  Common blocks
*  -------------

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_rec.inc'


      FIELDNO=DBS_FIELDNO(REF_NO,FIELD)
      FORMAT=DBS_INFOC(REF_NO,FIELDNO,'FORMAT')
      READ(FORMAT,'(2X,I2)')FIELD_LENGTH
      BYTENO=DBS_INFOI(REF_NO,FIELDNO,'START')
      START=BYTENO+FIELD_LENGTH
      STORE=DPVAL

      TEMP =MDH_DTOC ( STORE )
      RECORD(REF_NO)(START:START+11)=TEMP(1:12)

      END
