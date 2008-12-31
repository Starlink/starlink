*+DBS_GETI       gets Integer value from d/b given file, field nos.
* History
*   Author M.D.C.Harris ( R.A.L )                    17th February 1987.
*   Modified by M.Bush  ( R.A.L )                         October  1988
*     To handle I*2, I*1
*	9 Apr 1992	M. Duesterhaus (GSFC)		port to unix
***************************************************************************
      INTEGER FUNCTION DBS_GETI( REF_NO , FIELDNO )

*INPUT:

      INTEGER      FIELDNO	! Number of field.
     & ,           REF_NO    	! Reference number of database.

*FUNCTION

      CHARACTER*3  DBS_INFOC	! Gets integer type.
      CHARACTER*10 DBS_GETC     ! Gets characters from record
      INTEGER      MDH_CTOI

*LOCAL:

      CHARACTER*3  TYPE		! Type if integer.
      CHARACTER*10 IBYT	! Equivalence variable.

*  -------------
*  COMMON BLOCKS
*  -------------

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_dbs_rec.inc'



      IBYT = DBS_GETC( REF_NO, FIELDNO )
      TYPE= DBS_INFOC(REF_NO, FIELDNO, 'FORMAT')
      DBS_GETI = MDH_CTOI(IBYT)

      END
