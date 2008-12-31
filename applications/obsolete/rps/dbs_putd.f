*+DBS_PUTD       Puts a value into a database given the file, field numbers
*-  Author M.D.C.Harris ( R.A.L )                    20th March 1987.
*	9 Apr 1992	M. Duesterhaus (GSFC)	port to unix
***************************************************************************

      SUBROUTINE DBS_PUTD( REF_NO , FIELDNO , VALUE )

*INPUT:

      INTEGER FIELDNO	! Number of field.
     & ,      REF_NO	! Reference number of data set.
      DOUBLE PRECISION VALUE	! Input value.

*LOCAL:

      CHARACTER*8    RBYT	! Equivalence variable.
      INTEGER I		! Loop variable.
      DOUBLE PRECISION ROLD	! Temporary equivalence variable.

*  ----------------------
*  EQUIVALENCE STATEMENTS
*  ----------------------
	EQUIVALENCE ( ROLD, RBYT)

*  Global Variables
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_dbs_rec.inc'


      ROLD = VALUE								! Transfer value to equivalence variable.

      I = START(FIELDNO,REF_NO) + LENTH( FIELDNO , REF_NO ) -1					! Do for each byte in field.

        RECORD(REF_NO)(START( FIELDNO , REF_NO ):I) = RBYT		!  Transfer byte from equivalence variable.


      END									! End.
