*+DBS_PUTI       Puts an Integer Value into a record
*-  Author M.D.C.Harris ( R.A.L )                    20th March 1987.
*	9 Apr 1992 	M. Duesterhaus (GSFC)	port to unix
*	7/29/93		P. Brisco		Changed field_no to fieldno
***********************************************************************
      SUBROUTINE DBS_PUTI( REF_NO , FIELDNO , VALUE )

*INPUT:

      INTEGER FIELDNO	! Number of field.
     & ,      REF_NO	! Reference number of data set.
     & ,      VALUE	! Input value.

*OUTPUT:

      INTEGER  IERR

*FUNCTIONS:

      CHARACTER*6    MDH_ITOC

*LOCAL:

      CHARACTER*6    IBYT	! Equivalence variable.

*  -------------
*  COMMON BLOCKS
*  -------------

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_dbs_rec.inc'

      IBYT = MDH_ITOC(VALUE)
      CALL DBS_PUTC ( REF_NO, FIELDNO, IBYT, IERR)


      END									! End.
