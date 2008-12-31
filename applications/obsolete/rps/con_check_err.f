*+CON_CHECK_ERR    Puts out Error message and gets reply
*-	DATE		AUTHOR		DESCRIPTION
*-	???		RAL		original
*-	8 APR 1992	M. Duesterhaus	remove VAX RTL calls
**************************************************************************
      CHARACTER*1 FUNCTION CON_CHECK_ERR(MESID,MESSAGE)
      IMPLICIT NONE
 
*  Input
      INTEGER MESID
      CHARACTER*(*) MESSAGE
 
*  Global Variables
      LOGICAL SMG
      COMMON /SMG_KEEP/ SMG

      CHARACTER*48 MESF1
      CHARACTER*48 MESF2
      INTEGER MESTART		! MEssage start point across SMG
      COMMON/MES_FIELD/ MESF1,MESF2,MESTART
*-
*  Functions
      CHARACTER*1 MDH_GETC
      INTEGER MDH_ENDWORD
 
*  Local Variables
      CHARACTER*9 M1/'Error in '/
      CHARACTER*29 MESE2/'R(eturn) to form, else E(xit)'/
      CHARACTER*80 MESE3
      INTEGER NCHAR
 
* _____________________________ Executable Code _______________________________
  
      NCHAR = MDH_ENDWORD(MESSAGE)
 
      MESE3=M1//MESSAGE(:NCHAR)//', R(eturn) to form, else E(xit)'
      CON_CHECK_ERR = MDH_GETC(MESE3,'R')

      END
