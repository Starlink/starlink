*+DBS_PUTR       Puts a Real in d/b given file, field nos.
*-  Author M.D.C.Harris ( R.A.L )                    20th March 1987.
*	9 Apr 1992	M. Duesterhaus	Port to Unix
*************************************************************************
      SUBROUTINE DBS_PUTR( REF_NO , FIELDNO , VALUE )
 
*INPUT:

      INTEGER FIELDNO	! Number of field.
     & ,      REF_NO	! Reference number of data set.
      REAL    VALUE	! Input value.

*LOCAL:

      CHARACTER*4    RBYT	! Equivalence variable.
      INTEGER I		! Loop variable.
      REAL    ROLD	! Temporary equivalence variable.

*  ----------------------
*  EQUIVALENCE STATEMENTS
*  ----------------------

      EQUIVALENCE   ( ROLD , RBYT )

*  -------------
*  COMMON BLOCKS
*  -------------

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_dbs_rec.inc'

      ROLD = VALUE								! Transfer value to equivalence variable.

      I =  START(FIELDNO, REF_NO) + LENTH( FIELDNO , REF_NO ) - 1					! Do for each byte in field.

        RECORD( REF_NO) ( START( FIELDNO , REF_NO ) : I ) = RBYT		!  Transfer byte from equivalence variable.


      END



