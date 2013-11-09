*+  HDX_NUMERIC - returns .TRUE. if object numeric
      LOGICAL FUNCTION HDX_NUMERIC(LOC)

*    Description :
*     Checks that object is of any numeric type.  Returns false if
*     locator is invalid.
*    Author :
*            (BHVAD::RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) LOC       ! locators to data object
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*(DAT__SZTYP) TYPE       ! type of object
      LOGICAL VALID                     ! whether locator valid
      LOGICAL NUMERIC
*-
      STATUS=SAI__OK
      NUMERIC=.FALSE.
*  first check validity of locator
      CALL DAT_VALID(LOC,VALID,STATUS)
      IF (VALID) THEN
*  get type of object
        CALL DAT_TYPE(LOC,TYPE,STATUS)
        NUMERIC=(TYPE.EQ.'_BYTE'.OR.TYPE.EQ.'_UBYTE'.OR.
     &         TYPE.EQ.'_WORD'.OR.TYPE.EQ.'_UWORD'.OR.
     &         TYPE.EQ.'_INTEGER'.OR.TYPE.EQ.'_REAL'.OR.
     &         TYPE.EQ.'_DOUBLE')
      ENDIF
      HDX_NUMERIC=NUMERIC
      END
