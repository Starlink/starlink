*+DBS_PUTL       Puts a value into a database given the file, field nos.
*-  Author M.D.C.Harris ( R.A.L )                    20th March 1987.
*	9 apr 1992	M. Duesterhaus (GSFC)	port to unix
*	7/29/93		P. Brisco		Fixed incorrect if block.
*************************************************************************
      SUBROUTINE DBS_PUTL( REF_NO , FIELDNO , VALUE )

*INPUT:

      INTEGER FIELDNO	! Number of field.
     & ,      REF_NO	! Reference number of data set.
      LOGICAL VALUE	! Input value.

*LOCAL:

      CHARACTER*1    LBYT /'N'/	! Equivalence variable.
      INTEGER I		! Loop variable.
      LOGICAL LOLD	! Temporary equivalence variable.

*  ----------------------
*  EQUIVALENCE STATEMENTS
*  ----------------------


*  -------------
*  COMMON BLOCKS
*  -------------

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_dbs_rec.inc'

	LOLD = VALUE								! Transfer value to equivalence variable.
      IF (LOLD) THEN
	LBYT = 'Y'
      ELSE
	LBYT ='N' 
      ENDIF

      I = START(FIELDNO, REF_NO)					! Do for each byte in field.

      RECORD(REF_NO)(I:I) = LBYT		!  Transfer byte from equivalence variable.

      END									! End.
