*+  GCB_ATTACH - Grafix Control Block with given context
      SUBROUTINE GCB_ATTACH(CONTEXT,STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
      CHARACTER*(*) CONTEXT
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      LOGICAL GCB_ATTACH_VALID
      LOGICAL GCB_ATTACH_ACTIVE
*    Local constants :
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  read description file if necessary
        IF (.NOT.G_INITIALISED) THEN
          CALL GCB_RDDSCF(STATUS)
        ENDIF

        IF (GCB_ATTACH_VALID(CONTEXT)) THEN

*  local context - attach to dynamic memory
          IF (CONTEXT.EQ.'LOCAL') THEN
            CALL GCB_ATTACH_CACHE(STATUS)
            CALL GCB_ATTACH_LOCAL(STATUS)
            CALL GCB_ATTACH_SETCONTXT(CONTEXT,STATUS)
            G_MEMPTR=G_DMEMPTR

*  existing, current context - confirm attachment to global memory
          ELSEIF (CONTEXT.EQ.'CURRENT'.OR.CONTEXT.EQ.G_CONTEXT) THEN
            G_MEMPTR=G_GMEMPTR


*  no existing context - create global memory and attach
          ELSEIF (G_NCONTEXT.EQ.0) THEN
            CALL GCB_ATTACH_GLOBAL(STATUS)
            G_MEMPTR=G_GMEMPTR
            CALL GCB_ATTACH_SETCONTXT(CONTEXT,STATUS)

*  context active but not current - cache current then uncache
          ELSEIF (GCB_ATTACH_ACTIVE(CONTEXT)) THEN
            G_MEMPTR=G_GMEMPTR
            CALL GCB_ATTACH_CACHE(STATUS)
            CALL GCB_ATTACH_UNCACHE(CONTEXT,STATUS)
            CALL GCB_ATTACH_SETCONTXT(CONTEXT,STATUS)

*  new context - cache current and clear
          ELSE
            G_MEMPTR=G_GMEMPTR
            CALL GCB_ATTACH_CACHE(STATUS)
            CALL GCB_CLEAR(STATUS)
            CALL GCB_ATTACH_SETCONTXT(CONTEXT,STATUS)

          ENDIF

        ELSE
          CALL MSG_PRNT('AST_ERR: invalid graphics context -'//CONTEXT)
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_ATTACH',STATUS)
        ENDIF


      ENDIF

      END



*+  GCB_ATTACH_LOCAL - attach local GCB in dynamic memory
      SUBROUTINE GCB_ATTACH_LOCAL(STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  allocate memory
        CALL DYN_MAPB(1,G_SIZE,G_DMEMPTR,STATUS)

        G_MEMPTR=G_DMEMPTR
        CALL GCB_PUTPTR(G_SIZE,G_END,STATUS)
        CALL GCB_PUTRELPTR(G_STARTLISTS,G_ENDSTRUC,STATUS)
        CALL GCB_CLEAR(STATUS)

        G_LOCAL=.TRUE.

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_ATTACH_LOCAL',STATUS)
        ENDIF

      ENDIF

      END


*+  GCB_ATTACH_SETCONTXT - set graphics context (eg GRAFIX, IMAGE etc)
      SUBROUTINE GCB_ATTACH_SETCONTXT(CONTEXT,STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
      CHARACTER*(*) CONTEXT
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (CONTEXT.EQ.'GRAFIX') THEN
          G_CONTEXT='GRAFIX'
          IF (.NOT.G_GRAFIX) THEN
            G_GRAFIX=.TRUE.
            G_NCONTEXT=G_NCONTEXT+1
          ENDIF

        ELSEIF (CONTEXT.EQ.'IMAGE') THEN
          G_CONTEXT='IMAGE'
          IF (.NOT.G_IMAGE) THEN
            G_IMAGE=.TRUE.
            G_NCONTEXT=G_NCONTEXT+1
          ENDIF

        ELSEIF (CONTEXT.EQ.'SPECTRUM') THEN
          G_CONTEXT='SPECTRUM'
          IF (.NOT.G_SPECTRUM) THEN
            G_SPECTRUM=.TRUE.
            G_NCONTEXT=G_NCONTEXT+1
          ENDIF

        ELSEIF (CONTEXT.EQ.'TIME') THEN
          G_CONTEXT='TIME'
          IF (.NOT.G_TIME) THEN
            G_TIME=.TRUE.
            G_NCONTEXT=G_NCONTEXT+1
          ENDIF

        ELSEIF (CONTEXT.EQ.'LOCAL') THEN
          G_CONTEXT=CHAR(0)

        ENDIF

      ENDIF

      END

*+  GCB_ATTACH_ACTIVE - determine if context active
      LOGICAL FUNCTION GCB_ATTACH_ACTIVE(CONTEXT)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
      CHARACTER*(*) CONTEXT
*    Import-Export :
*    Export :
*    Status :
*    Function declarations :
*    Local constants :
*    Local variables :
      LOGICAL YESNO
*-
      YESNO=.FALSE.

      IF (CONTEXT.EQ.'GRAFIX'.AND.G_GRAFIX) THEN
          YESNO=.TRUE.

      ELSEIF (CONTEXT.EQ.'IMAGE'.AND.G_IMAGE) THEN
          YESNO=.TRUE.

      ELSEIF (CONTEXT.EQ.'SPECTRUM'.AND.G_SPECTRUM) THEN
          YESNO=.TRUE.

      ELSEIF (CONTEXT.EQ.'TIME'.AND.G_TIME) THEN
          YESNO=.TRUE.

      ENDIF

      GCB_ATTACH_ACTIVE=YESNO

      END


*+  GCB_ATTACH_CACHE - cache current context
      SUBROUTINE GCB_ATTACH_CACHE(STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (G_CONTEXT.EQ.'GRAFIX') THEN
          IF (G_GRAFIX_PTR.EQ.0) THEN
            CALL GCB_CRECACHE(G_GRAFIX_PTR,STATUS)
          ENDIF
          CALL GCB_CACHE(G_GRAFIX_PTR,STATUS)

        ELSEIF (G_CONTEXT.EQ.'IMAGE') THEN
          IF (G_IMAGE_PTR.EQ.0) THEN
            CALL GCB_CRECACHE(G_IMAGE_PTR,STATUS)
          ENDIF
          CALL GCB_CACHE(G_IMAGE_PTR,STATUS)

        ELSEIF (G_CONTEXT.EQ.'SPECTRUM') THEN
          IF (G_SPECTRUM_PTR.EQ.0) THEN
            CALL GCB_CRECACHE(G_SPECTRUM_PTR,STATUS)
          ENDIF
          CALL GCB_CACHE(G_SPECTRUM_PTR,STATUS)

        ELSEIF (G_CONTEXT.EQ.'TIME') THEN
          IF (G_TIME_PTR.EQ.0) THEN
            CALL GCB_CRECACHE(G_TIME_PTR,STATUS)
          ENDIF
          CALL GCB_CACHE(G_TIME_PTR,STATUS)

        ENDIF

      ENDIF

      END


*+  GCB_ATTACH_UNCACHE - uncache given context
      SUBROUTINE GCB_ATTACH_UNCACHE(CONTEXT,STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
      CHARACTER*(*) CONTEXT
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (CONTEXT.EQ.'GRAFIX') THEN
          CALL GCB_UNCACHE(G_GRAFIX_PTR,STATUS)

        ELSEIF (CONTEXT.EQ.'IMAGE') THEN
          CALL GCB_UNCACHE(G_IMAGE_PTR,STATUS)

        ELSEIF (CONTEXT.EQ.'SPECTRUM') THEN
          CALL GCB_UNCACHE(G_SPECTRUM_PTR,STATUS)

        ELSEIF (CONTEXT.EQ.'TIME') THEN
          CALL GCB_UNCACHE(G_TIME_PTR,STATUS)

        ENDIF

      ENDIF

      END



      LOGICAL FUNCTION GCB_ATTACH_VALID(CONTEXT)

      CHARACTER*(*) CONTEXT

      GCB_ATTACH_VALID=(CONTEXT.EQ.'LOCAL'.OR.
     :                  CONTEXT.EQ.'IMAGE'.OR.
     :                  CONTEXT.EQ.'GRAFIX'.OR.
     :                  CONTEXT.EQ.'TIME'.OR.
     :                  CONTEXT.EQ.'SPECTRUM'.OR.
     :                  CONTEXT.EQ.'CURRENT')

      END



*+  GCB_ATTACH_GLOBAL - create global GCB
      SUBROUTINE GCB_ATTACH_GLOBAL(STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
C      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  if it doesn't exist then create
        IF (G_GMEMPTR.EQ.0) THEN
          CALL DYN_MAPB(1,G_SIZE,G_GMEMPTR,STATUS)

          G_MEMPTR=G_GMEMPTR
          CALL GCB_PUTPTR(G_SIZE,G_END,STATUS)
          CALL GCB_PUTRELPTR(G_STARTLISTS,G_ENDSTRUC,STATUS)
          CALL GCB_CLEAR(STATUS)

        ELSE

          G_MEMPTR=G_GMEMPTR

        ENDIF

*  look for noticeboard
C        CALL GCB_CHKNB(OK,STATUS)
*  create and clear if not there
C        IF (.NOT.OK) THEN
C          CALL GCB_CRENB(STATUS)
C          CALL GCB_CLEAR(STATUS)
C        ENDIF

      ENDIF

      END



*+  GCB_CHKNB - check if Grafix Control Block notice board exists
      SUBROUTINE GCB_CHKNB(OK,STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'NBS_PAR'
      INCLUDE 'GCB_PAR'
*    Import :
*    Import-Export :
*    Export :
      LOGICAL OK
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(NBS_K_MAXNAME) NAME
      INTEGER NBID,ID
*-

      OK=.FALSE.
      IF (STATUS.EQ.SAI__OK) THEN

*  already a pointer
        IF (G_GMEMPTR.NE.0) THEN
          OK=.TRUE.

        ELSE
*  othewise get name and look for noticeboard
          CALL GCB_NBNAME(NAME,STATUS)
          CALL NBS_FIND_NOTICEBOARD(NAME,NBID,STATUS)

*  not there
          IF (STATUS.NE.SAI__OK) THEN
            OK=.FALSE.
            CALL ERR_ANNUL(STATUS)
*  there, so get id of primitive part
          ELSE
            OK=.TRUE.
            CALL NBS_FIND_ITEM(NBID,'GCB',ID,STATUS)
            CALL NBS_GET_POINTER(ID,G_GMEMPTR,STATUS)
            G_MEMPTR=G_GMEMPTR
          ENDIF

        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_CHKNB',STATUS)
        ENDIF

      ENDIF

      END



*+  GCB_CRENB - creates noticeboard for Graphix Control Block
      SUBROUTINE GCB_CRENB(STATUS)
*    Description :
*    Method :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'NBS_PAR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(NBS_K_MAXNAME) NAME
      INTEGER PID
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  read description file if necessary
        IF (.NOT.G_INITIALISED) THEN
          CALL GCB_RDDSCF(STATUS)
        ENDIF

*  define noticeboard
        CALL NBS_BEGIN_DEFINITION(G_ID,STATUS)
        CALL NBS_DEFINE_PRIMITIVE(G_ID,'GCB','GCB',0,G_SIZE,PID,STATUS)

*  get name from pid
        CALL GCB_NBNAME(NAME,STATUS)

*  create and map
        CALL NBS_END_DEFINITION(NAME,'CREATE_NOTICEBOARD',STATUS)
        CALL NBS_FIND_NOTICEBOARD(NAME,G_ID,STATUS)
        CALL NBS_FIND_ITEM(G_ID,'GCB',PID,STATUS)
        CALL NBS_GET_POINTER(PID,G_GMEMPTR,STATUS)
        G_MEMPTR=G_GMEMPTR

*  set end pointer
        CALL GCB_PUTPTR(G_SIZE,G_END,STATUS)
*  and pointer to free space for linked lists
        CALL GCB_PUTRELPTR(G_STARTLISTS,G_ENDSTRUC,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_CRENB',STATUS)
        ENDIF

      ENDIF

      END


*+  GCB_NBNAME - get name of Grafix ATTribute notice board
      SUBROUTINE GCB_NBNAME(NAME,STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-Export :
*    Export :
      CHARACTER*(*) NAME
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      INTEGER        LOW16                        ! Lowest 16 bits set mask
        PARAMETER    ( LOW16='0000FFFF'X )

*    Local variables :
      INTEGER   PID
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get process id for first part of name
        CALL PSX_GETPID(PID,STATUS)

*  Ignores bits 16-31 and converts to unsigned by ignoring sign bit
        PID = IAND(PID,LOW16)
        NAME(1:1) = CHAR(PID/256)
        NAME(2:2) = CHAR(MOD(PID,256))
        NAME(3:) = 'CONTROL'			! For [C]ontrol block

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_NBNAME',STATUS)
        ENDIF

      ENDIF

      END
