*+   RED4_GET_DATA_OBJECT read object in structure
      SUBROUTINE RED4_GET_DATA_OBJECT (REF_NAME,OBJECT_NAME,TYPE,NITEM,
     :   IVALUE,FVALUE,CVALUE,STATUS)
*    Description :
*     subroutine to read an object in structure opened
*     by DSA_INPUT or DSA_OUTPUT
*    Invocation :
*     CALL DSA_GET_DATA_OBJECT (REF_NAME,OBJECT_NAME,TYPE,NITEM,IVALUE,FVALUE,
*    :   CVALUE,STATUS)
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     1989:        Original version.                       (JFL)
*     24-Apr-1990: History and description added. Dynamic
*                  memory functions were declared but not
*                  used. Removed.                          (SMB)
*     19-Feb-1993: Conform to error strategy               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Import:
      CHARACTER*(*) REF_NAME    !The reference name used to identify the
*                               !structure.
      CHARACTER*(*) OBJECT_NAME !The name of the object in the structure,
*                               !it is assumed to alrerady exist.
      CHARACTER*(*) TYPE        !The type of the object, INT, FLOAT or CHAR
      INTEGER NITEM             !Number of items to be transferred
*    Export:
      INTEGER IVALUE(NITEM)     !Integer value.
      REAL FVALUE(NITEM)        !Real value.
      CHARACTER*(*) CVALUE      !Character value.
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'DSA_ERRORS'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER ICH_LEN, ICH_FOLD
*    Global variables:
*    Local variables :
      CHARACTER*80 OBJ_NAME                 ! DTA_ name for structure
      CHARACTER*32 REF_NAME_UC              ! Upper case version of REF_NAME
      CHARACTER*80 NAME
      CHARACTER*80 ERROR
      CHARACTER*80 STRUCTURE
      INTEGER   REF_SLOT                    ! Reference table slot #
      INTEGER INVOKE
      INTEGER DTA_STATUS
      INTEGER DTA_CODE
      INTEGER LENGTH
      INTEGER IGNORE
*-

      IF ( STATUS .NE. ADAM__OK ) RETURN
*
*     Look up the reference name in the tables.
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      IF (STATUS.NE.0) GO TO 500          ! Error exit
*
*     Generate the name of the object string
      NAME=OBJ_NAME(:LENGTH)//OBJECT_NAME(:ICH_LEN(OBJECT_NAME))
*
*     Read it
      IF (TYPE .EQ. 'INT')THEN
         CALL DTA_RDVARI (NAME, NITEM, IVALUE, DTA_STATUS)
      ELSE IF (TYPE .EQ. 'FLOAT') THEN
         CALL DTA_RDVARF (NAME, NITEM, FVALUE, DTA_STATUS)
      ELSE IF (TYPE .EQ. 'CHAR') THEN
         CALL DTA_RDVARC (NAME, NITEM, CVALUE, DTA_STATUS)
      ENDIF

      IF (DTA_STATUS.NE.0) THEN
         CALL DTA_ERROR (DTA_STATUS,ERROR)
         CALL DSA_WRUSER('Error trying to read object ')
         CALL DSA_WRUSER(OBJECT_NAME(:ICH_LEN(OBJECT_NAME))//' in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER('. ')
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER('.\N')
         STATUS=DSA__DTAERR
         DTA_CODE=DTA_STATUS
         GO TO 500      ! Error exit
      END IF
*
*     Exit
  500 CONTINUE
*
      END
