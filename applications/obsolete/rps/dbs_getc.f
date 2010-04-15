*+DBS_GETC       Gets Char value from d/b given file, field nos.
*-  Author M.D.C.Harris ( R.A.L )                    20th March 1987.
*	9 Apr 1992	M. Duesterhaus	(gsfc)	port to unix
      CHARACTER*(*) FUNCTION DBS_GETC( REF_NO , FIELDNO )

*  -----------
*  DESCRIPTION
*  -----------

*  This function gets a value from a database given the reference and field numbers.

*  ---------
*  VARIABLES
*  ---------

*INPUT:

      INTEGER      FIELDNO	! Number of field.
     & ,           REF_NO    	! Reference number of database.

*LOCAL:

      CHARACTER*60 COLD		! Temporary equivalence variable.
      CHARACTER*1  TYPE         ! Type of storage.
      INTEGER      I		! Loop variable.

*  ------------------------------------
*  FUNCTIONS AND SUBROUTINES REFERENCED
*  ------------------------------------

*  -------------
*  COMMON BLOCKS
*  -------------

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_field.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_dbs_rec.inc'


      COLD = ' '								! Wipe temporary storage.
      TYPE = FORMAT( FIELDNO, REF_NO )( :1 )					! Get type of variable.


        I = START(FIELDNO,REF_NO)+ LENTH( FIELDNO , REF_NO ) - 1				!  For each byte of field.

          COLD = RECORD( REF_NO) (START( FIELDNO, REF_NO ) : I)		!   Transfer each byte to equivalence variable.




      DBS_GETC = COLD								! Move from temporary storage to return value.

      END									! Value returned.
