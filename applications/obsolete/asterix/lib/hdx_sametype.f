*+  HDX_SAMETYPE - returns .TRUE. if objects are same type
      LOGICAL FUNCTION HDX_SAMETYPE(ALOC,BLOC)

*    Description :
*     Returns a true value if two objects are of same type. .FALSE.
*     returned if either locator invalid
*    Author :
*            (BHVAD::RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) ALOC,BLOC      ! locators to data object
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZTYP) ATYPE,BTYPE    ! type of object
      LOGICAL AVALID,BVALID		    ! whether locators valid
*-
      STATUS=SAI__OK
*  check validity of locators
      CALL DAT_VALID(ALOC,AVALID,STATUS)
      CALL DAT_VALID(BLOC,BVALID,STATUS)
      IF (AVALID.AND.BVALID) THEN

*  get types of objects
        CALL DAT_TYPE(ALOC,ATYPE,STATUS)
        CALL DAT_TYPE(BLOC,BTYPE,STATUS)

*  disregard lengths of string types
        IF (ATYPE(:5).EQ.'_CHAR') THEN
          ATYPE=ATYPE(:5)
        ENDIF
        IF (BTYPE(:5).EQ.'_CHAR') THEN
          BTYPE=BTYPE(:5)
        ENDIF
        HDX_SAMETYPE=(ATYPE.EQ.BTYPE)
      ELSE
        HDX_SAMETYPE=.FALSE.
      ENDIF
      END
