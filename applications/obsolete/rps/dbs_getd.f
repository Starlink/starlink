*+DBS_GETD       Gets R*8 value from d/b given file, field nos.
*-  Author M.D.C.Harris ( R.A.L )                    	20th March 1987.
*	9 Apr 1992	M. Duesterhaus		port to unix
*	7/29/93		P. Brisco		Declared MDH_CTOD
****************************************************************************
      DOUBLE PRECISION FUNCTION DBS_GETD( REF_NO , FIELDNO )

*  -----------
*  DESCRIPTION
*  -----------

*  This function gets a value from a database given the reference and field numbers.

*  ---------
*  VARIABLES
*  ---------

*INPUT:

      INTEGER FIELDNO	! Number of field.
     & ,      REF_NO	! Reference number of data set.

*LOCAL:

      INTEGER I		! Loop variable.
      REAL MDH_CTOD
      DOUBLE PRECISION ROLD	! Temporary equivalence variable.

*  ----------------------
*  EQUIVALENCE STATEMENTS
*  ----------------------


*  -------------
*  COMMON BLOCKS
*  -------------

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_dbs_rec.inc'


      I = START(FIELDNO, REF_NO) + LENTH( FIELDNO , REF_NO ) -1					! Do for each byte in field.

        ROLD = MDH_CTOD (RECORD(REF_NO)( START( FIELDNO , REF_NO ):I ))		!  Transfer byte to equivalence variable.


      DBS_GETD = ROLD								! Transfer value to return variable.

      END									! End.
