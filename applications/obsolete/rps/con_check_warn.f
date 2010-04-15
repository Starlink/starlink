*+CON_CHECK_WARN   Puts out warning message and gets reply
      CHARACTER*1 FUNCTION CON_CHECK_WARN(MESID,MESSAGE)
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

      CHARACTER*29 MESE2/'R(eturn) to form, or A(ccept)'/
      INTEGER NCHAR

* _____________________________ Executable Code _______________________________

      NCHAR = MDH_ENDWORD(MESSAGE)

         CON_CHECK_WARN = MDH_GETC('Warning: '//MESSAGE(:NCHAR)//', R(eturn) to form, or A(ccept)','R')

      END
