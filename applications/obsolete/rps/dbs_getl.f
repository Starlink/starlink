*+DBS_GETL       gets a Logical value from d/b giver file, field nos.
*-  Author M.D.C.Harris ( R.A.L )                    17th February 1987.
* 	8 Apr 1992	M. Duesterhaus (GSFC)	port to UNIX
*************************************************************************
      LOGICAL FUNCTION DBS_GETL( REF_NO , FIELDNO )

*  ---------
*  VARIABLES
*  ---------

*INPUT:

      INTEGER      FIELDNO   	! Number of field.
     & ,           REF_NO    	! Reference number of database.

*LOCAL:

      CHARACTER*1        LBYT	! Equivalence variable.
      LOGICAL      LOLD /.FALSE./  ! Temporary equivalence variable.


*  -------------
*  COMMON BLOCKS
*  -------------

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_dbs_rec.inc'


      LBYT = RECORD(REF_NO)(START(FIELDNO,REF_NO):START(FIELDNO,REF_NO))
      IF (LBYT.EQ.'Y') THEN
        LOLD = .TRUE.
      ELSE
        LOLD = .FALSE.
      END IF
      DBS_GETL = LOLD								! Transfer value to return variable.

      END									! End.
