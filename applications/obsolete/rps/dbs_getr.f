*+DBS_GETR       Get Real value from d/b given file, field nos.
*-  Author M.D.C.Harris ( R.A.L )                    	17th February 1987.
*	9 Apr 1992	M. Duesterhaus		port to unix
*****************************************************************************	
      REAL FUNCTION DBS_GETR( REF_NO , FIELDNO )

*INPUT:

      INTEGER FIELDNO	! Number of field.
     & ,      REF_NO	! Reference number of data set.

*FUNCTION:

      CHARACTER*15    DBS_GETC
      REAL            MDH_CTOR

*LOCAL:

      CHARACTER*15    RBYT	! Equivalence variable.

*  -------------
*  COMMON BLOCKS
*  -------------

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_dbs_rec.inc'


      RBYT = DBS_GETC(REF_NO, FIELDNO)
      DBS_GETR = MDH_CTOR(RBYT)		

      END
