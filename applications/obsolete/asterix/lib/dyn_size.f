*+  DYN_SIZE - returns size of dynamic array
      SUBROUTINE DYN_SIZE(PTR,NVAL,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER PTR			! pointer to dynamic memory
*    Import/Export :
*    Export :
      INTEGER NVAL
*    Global variables :
      INCLUDE 'ASTLIB(DYN_CMN)'
*    Local Constants :
*    Local variables :
      INTEGER IPTR
      LOGICAL FOUND
*-
      NVAL=0

      IF (STATUS.EQ.SAI__OK) THEN

        FOUND=.FALSE.
        IPTR=0

*  find pointer in list
        DO WHILE (.NOT.FOUND.AND.IPTR.LT.NPTR)
          IPTR=IPTR+1
          FOUND=(PTR.EQ.LIST(IPTR).PTR)
        ENDDO

        IF (FOUND) THEN
          NVAL=LIST(IPTR).NITEM
        ENDIF

      ENDIF
      END
