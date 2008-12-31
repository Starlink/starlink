*+FORM_GECHEK      Gets checksum from current record of given file
*  July 20 1992	M. Duesterhaus		Modify for portability
*******************************************************************
      INTEGER FUNCTION FORM_GECHEK(REF_NO)
      IMPLICIT NONE
 
*  Calling Argument
      INTEGER REF_NO
 
*  Global Variables
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_rec.inc'
      INCLUDE 'com_dbs_iof.inc'
 
*  Local Variables
      CHARACTER*6 BSUM
      INTEGER QBYTE

*  Functions
      INTEGER    MDH_CTOI
 
*  Executable Code
 
      QBYTE =   RECSIZE(REF_NO) - 5	! Checksum in last 2 bytes
      BSUM = RECORD( REF_NO)(QBYTE:QBYTE+5)
      FORM_GECHEK = MDH_CTOI( BSUM )
      END
