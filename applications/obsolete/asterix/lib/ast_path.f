*+  AST_PATH - resolve path to file
      SUBROUTINE AST_PATH(DEF,ALT,REL,PATH,L,STATUS)
*    Description :
*  Creates a path name from an environment variable and relative path.
*  It allows two possibilities for the environment variable - a default
*  and an alternative.  The relative path may be blank.  There is also
*  an overall default of AST_ETC
*    History :
*     27/2/98 : Original (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      CHARACTER*(*) DEF,ALT,REL
*    Export :
      CHARACTER*(*) PATH
      INTEGER L
*    Functions :
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      CHARACTER*132 BUFF
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL PSX_GETENV(ALT,BUFF,STATUS)
        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_ANNUL(STATUS)
          CALL PSX_GETENV(DEF,BUFF,STATUS)
          IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            CALL PSX_GETENV('AST_ETC',BUFF,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
              CALL MSG_PRNT('! Unable to resolve data path')
            ENDIF
          ENDIF
        ENDIF

        IF (STATUS.EQ.SAI__OK) THEN

          L=CHR_LEN(BUFF)
          IF (CHR_LEN(REL).GT.0) THEN
            IF (BUFF(L:L).NE.'/'.AND.REL(1:1).NE.'/') THEN
              BUFF=BUFF(1:L)//'/'
              L=L+1
            ENDIF
            PATH=BUFF(1:L)//REL
          ENDIF
          L=CHR_LEN(PATH)

        ENDIF


      ENDIF

      END
