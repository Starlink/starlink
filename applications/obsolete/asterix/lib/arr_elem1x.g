*+  ARR_ELEM1<T> - Returns element of <TYPE> array given pointer and index
      SUBROUTINE ARR_ELEM1<T>( PTR, DIM, INDEX, VAL, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER PTR		!
      INTEGER DIM		!
      INTEGER INDEX		!
*    Import-Export :
*    Export :
      <TYPE> VAL			!
*    Status :
      INTEGER STATUS
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (INDEX.GT.0.AND.INDEX.LE.DIM) THEN
          CALL ARR_ELEM1<T>_INT(%VAL(PTR),INDEX,VAL)
        ELSEIF (INDEX.LE.0) THEN
          STATUS=SAI__ERROR
          CALL ERR_REP(' ','AST_ERR: zero or negative array index',
     :                 STATUS )
        ELSEIF (INDEX.GT.DIM) THEN
          STATUS=SAI__ERROR
          CALL ERR_REP(' ','AST_ERR: array index exceeds size of array',
     :                 STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP( ' ', '...from ARR_ELEM1<T>',STATUS)
        ENDIF

      ENDIF


      END

      SUBROUTINE ARR_ELEM1<T>_INT(ARRAY,INDEX,VAL)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      <TYPE> ARRAY(*)		!
      INTEGER INDEX		!
*    Import-Export :
*    Export :
      <TYPE> VAL			!
*    Status :
*    Local variables :
*-
      VAL=ARRAY(INDEX)

      END
