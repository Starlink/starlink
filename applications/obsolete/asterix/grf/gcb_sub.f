*+  GCB_CSIZE - returns size of compressed Grafix Control Block
      SUBROUTINE GCB_CSIZE(NBYTE,NSCAL,NSTRUC,STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
      INTEGER NBYTE,NSCAL,NSTRUC
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  header
        NBYTE=GCB__NHDBLK*GCB__SZPTR
*  scalar part
        NSCAL=G_TOPSCAL-GCB__SZPTR
        NBYTE=NBYTE+NSCAL
*  structured part
        CALL GCB_GETRELPTR(G_ENDSTRUC,NSTRUC,STATUS)
        NBYTE=NBYTE+NSTRUC

      ENDIF

      END

*+  GCB_NULL - check if given location in GCB is empty
      LOGICAL FUNCTION GCB_NULL(GCB,DISP,SIZ,STATUS)
*    Description :
*    Method :
*    Authors :
*    History :
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
      BYTE GCB(*)
      INTEGER DISP,SIZ
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      BYTE ZERO
      PARAMETER (ZERO='00'X)
*    Local variables :
      INTEGER I,J
      LOGICAL NULL
*-
      IF (STATUS.EQ.SAI__OK) THEN

        NULL=.TRUE.
        I=DISP
        J=DISP+SIZ-1
*  every byte must be zero
        DO WHILE (NULL.AND.I.LE.J)
          NULL=(GCB(I).EQ.ZERO)
          I=I+1
        ENDDO

        GCB_NULL=NULL

      ELSE
        GCB_NULL=.TRUE.
      ENDIF

      END


*+  GCB_ZERO - zero given location in GCB
      SUBROUTINE GCB_ZERO(GCB,DISP,SIZ,STATUS)
*    Description :
*    Method :
*    Authors :
*    History :
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
      BYTE GCB(*)
      INTEGER DISP,SIZ
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      BYTE ZERO
      PARAMETER (ZERO='00'X)
*    Local variables :
      INTEGER I,J,IJ
*-
      IF (STATUS.EQ.SAI__OK) THEN

        I=DISP
        J=DISP+SIZ-1
        DO IJ=I,J
          GCB(IJ)=ZERO
        ENDDO

      ENDIF

      END


*+  GCB_CAN_SUB - zero given location in GCB
      SUBROUTINE GCB_CAN_SUB(GCB,DISP,SIZ,STATUS)
*    Description :
*    Method :
*    Authors :
*    History :
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
      BYTE GCB(*)
      INTEGER DISP,SIZ
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      BYTE ZERO
      PARAMETER (ZERO='00'X)
*    Local variables :
      INTEGER I,J,IJ
*-
      IF (STATUS.EQ.SAI__OK) THEN

        I=DISP
        J=DISP+SIZ-1
        DO IJ=I,J
          GCB(IJ)=ZERO
        ENDDO

      ENDIF

      END




*+  GCB_LOCSCAL - find scalar attribute name
      SUBROUTINE GCB_LOCSCAL(NAME,DISP,SIZ,FMT,TYPE,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      CHARACTER*(*) NAME
*    Import-Export :
*    Export :
      INTEGER DISP,SIZ
      CHARACTER*(*) FMT,TYPE
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*15 LNAME
      INTEGER I
      LOGICAL FOUND
*-

      IF (STATUS.EQ.SAI__OK) THEN

        LNAME=NAME
        CALL CHR_UCASE(LNAME)

        FOUND=.FALSE.
        I=1
*  search list for name
        DO WHILE (.NOT.FOUND.AND.I.LE.GCB__MXSCAL)
          IF (LNAME.EQ.G_SCNAME(I)) THEN
            FOUND=.TRUE.
*  extract properties
            DISP=G_SCDISP(I)
            SIZ=G_SCSIZ(I)
            FMT=G_SCFMT(I)
*  determine type from format
            IF (INDEX(FMT,'F').NE.0.OR.INDEX(FMT,'E').NE.0) THEN
              TYPE='_REAL'
            ELSEIF (INDEX(FMT,'I').NE.0) THEN
              TYPE='_INTEGER'
            ELSEIF (INDEX(FMT,'L').NE.0) THEN
              TYPE='_LOGICAL'
            ELSEIF (INDEX(FMT,'A').NE.0) THEN
              TYPE='_CHAR'
            ELSE
              TYPE='UNKNOWN'
            ENDIF
          ENDIF
          I=I+1
        ENDDO

        IF (.NOT.FOUND) THEN
          CALL MSG_PRNT('AST_ERR: graphics control attribute '//
     :                    LNAME//' not found')
          STATUS=SAI__ERROR
        ENDIF

      ENDIF

      END



*+  GCB_LOCCOMP - locate component within structure
      SUBROUTINE GCB_LOCCOMP(NAME,INDX,DISP,SIZ,FMT,TYPE,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      CHARACTER*(*) NAME
      INTEGER INDX
*    Import-Export :
*    Export :
      INTEGER DISP,SIZ
      CHARACTER*(*) FMT,TYPE
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER GCB_REL2ABS
*    Local constants :
*    Local variables :
      CHARACTER*15 LNAME
      INTEGER I
      INTEGER IROOT
      INTEGER OFFSET
      LOGICAL FOUND
*-

      IF (STATUS.EQ.SAI__OK) THEN

        LNAME=NAME
        CALL CHR_UCASE(LNAME)

*  locate structure with given index
        CALL GCB_LOCSTRUC(LNAME,INDX,IROOT,OFFSET,STATUS)

        IF (STATUS.EQ.SAI__OK) THEN

          FOUND=.FALSE.
          I=1
*  look for individual component name
          DO WHILE (.NOT.FOUND.AND.I.LE.GCB__MXCOMP)
            IF (LNAME.EQ.G_CNAME(IROOT,I)) THEN
              FOUND=.TRUE.
*  extract properties
              DISP=GCB_REL2ABS(OFFSET-G_CDISP(IROOT,I))
              SIZ=G_CSIZ(IROOT,I)
              FMT=G_CFMT(IROOT,I)
*  determine type from format
              IF (INDEX(FMT,'F').NE.0.OR.INDEX(FMT,'E').NE.0) THEN
                TYPE='_REAL'
              ELSEIF (INDEX(FMT,'I').NE.0) THEN
                TYPE='_INTEGER'
              ELSEIF (INDEX(FMT,'L').NE.0) THEN
                TYPE='_LOGICAL'
              ELSEIF (INDEX(FMT,'A').NE.0) THEN
                TYPE='_CHAR'
              ELSE
                TYPE='UNKNOWN'
              ENDIF
            ELSE
              I=I+1
            ENDIF
          ENDDO

          IF (.NOT.FOUND) THEN
            CALL MSG_PRNT('AST_ERR: graphics control attribute '//
     :                    LNAME//' not found')
            STATUS=SAI__ERROR
          ENDIF

        ENDIF

      ENDIF

      END


*+  GCB_LOCSTRUC - locate start of structure with given index
      SUBROUTINE GCB_LOCSTRUC(NAME,INDX,IROOT,DISP,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      CHARACTER*(*) NAME
      INTEGER INDX
*    Import-Export :
*    Export :
      INTEGER IROOT
      INTEGER DISP
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*15 LNAME,ROOT
      INTEGER LOCPTR,PTR
      INTEGER I,L
      INTEGER ICURR
      LOGICAL FOUND
*-

      IF (STATUS.EQ.SAI__OK) THEN

        LNAME=NAME
        CALL CHR_UCASE(LNAME)
        L=INDEX(LNAME,'_')
        ROOT=LNAME(:L)

*  find root in list
        FOUND=.FALSE.
        IROOT=1
        DO WHILE (.NOT.FOUND.AND.IROOT.LE.GCB__MXSTRUC)
          IF (ROOT.EQ.G_STNAME(IROOT)) THEN
            FOUND=.TRUE.
          ELSE
            IROOT=IROOT+1
          ENDIF
        ENDDO

        IF (.NOT.FOUND) THEN
          CALL MSG_PRNT('AST_ERR: graphics control attribute '//
     :                    LNAME//' not found')
          STATUS=SAI__ERROR
        ELSE

*  get current index
          CALL GCB_GETRELPTR(G_CURR(IROOT),ICURR,STATUS)
*  if this is greater than one requested, then go back to start
          IF (ICURR.GT.INDX.OR.ICURR.EQ.0) THEN
            I=1
            LOCPTR=G_PTR_START(IROOT)
*  otherwise use this as starting point in linked list
          ELSE
            I=ICURR
            LOCPTR=G_PTR_CURR(IROOT)
          ENDIF

*  step through linked list to requested structure
          DO WHILE (I.LE.INDX)
            CALL GCB_GETRELPTR(LOCPTR,PTR,STATUS)
*  create new link if necessary
            IF (PTR.EQ.0) THEN
              CALL GCB_ADDSTRUC(IROOT,LOCPTR,STATUS)
              CALL GCB_GETRELPTR(LOCPTR,PTR,STATUS)
            ENDIF
            LOCPTR=PTR
            I=I+1
          ENDDO

*  update current index
          CALL GCB_PUTRELPTR(INDX,G_CURR(IROOT),STATUS)
          CALL GCB_PUTRELPTR(PTR,G_PTR_CURR(IROOT),STATUS)

          DISP=PTR

        ENDIF

      ENDIF

      END


*+  GCB_ADDSTRUC - add a structure to linked list
      SUBROUTINE GCB_ADDSTRUC(IROOT,LOCPTR,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      INTEGER IROOT
      INTEGER LOCPTR
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER GCB_REL2ABS
      INTEGER GCB_SIZE
*    Local constants :
*    Local variables :
      INTEGER NEXTFREE
      INTEGER BASE,LIMIT
      INTEGER ABSPTR
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get pointer to next free space
        CALL GCB_GETRELPTR(G_ENDSTRUC,NEXTFREE,STATUS)
*  get limit of free space
        LIMIT=GCB_SIZE()-G_TOPSCAL
        IF (NEXTFREE.EQ.0) THEN
          NEXTFREE=G_STARTLISTS
        ENDIF
*  estimate where link would start
        BASE=NEXTFREE+G_STSIZ(IROOT)-1
*  check against limit
        IF (BASE.GE.LIMIT) THEN
          CALL MSG_PRNT('AST_ERR: Graphics Control Block full')
          STATUS=SAI__ERROR
*  enough space
        ELSE
*  update pointer to next free space
          CALL GCB_PUTRELPTR(BASE+1,G_ENDSTRUC,STATUS)
*  write location into previous link
          CALL GCB_PUTRELPTR(BASE,LOCPTR,STATUS)
          ABSPTR=GCB_REL2ABS(BASE)
*  clear this piece of memory
          CALL GCB_ZERO(%val(G_MEMPTR),ABSPTR,G_STSIZ(IROOT),STATUS)
        ENDIF

      ENDIF

      END



*+  GCB_GETPTR - read pointer value from given location
      SUBROUTINE GCB_GETPTR(LOCPTR,PTR,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      INTEGER LOCPTR
*    Import-Export :
*    Export :
      INTEGER PTR
*    Status :
      INTEGER STATUS
*    Function declarations :
      LOGICAL GCB_NULL
*    Local constants :
*    Local variables :
      CHARACTER*(GCB__SZPTR) STR
      INTEGER ISTAT
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  first check for null pointer
        IF (GCB_NULL(%val(G_MEMPTR),LOCPTR,GCB__SZPTR,STATUS)) THEN
          PTR=0
        ELSE
*  extract string and get pointer value
          CALL GCB_GETSTR(%val(G_MEMPTR),LOCPTR,GCB__SZPTR,STR,STATUS)
          READ(STR,*,IOSTAT=ISTAT) PTR
          IF (ISTAT.NE.0) THEN
            PTR=0
          ENDIF
        ENDIF

      ENDIF

      END



*+  GCB_PUTPTR - write pointer value to given location
      SUBROUTINE GCB_PUTPTR(PTR,LOCPTR,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      INTEGER LOCPTR
*    Import-Export :
*    Export :
      INTEGER PTR
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(GCB__SZPTR) STR
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  write numeric value into string
        WRITE(STR,GCB__PTRFMT) PTR
*  and copy to memory
        CALL GCB_PUTSTR(STR,LOCPTR,GCB__SZPTR,%val(G_MEMPTR),STATUS)

      ENDIF

      END



*+  GCB_GETRELPTR - read pointer value from location relative to top of block
      SUBROUTINE GCB_GETRELPTR(RELPTR,PTR,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      INTEGER RELPTR
*    Import-Export :
*    Export :
      INTEGER PTR
*    Status :
      INTEGER STATUS
*    Function declarations :
      LOGICAL GCB_NULL
      INTEGER GCB_SIZE
*    Local constants :
*    Local variables :
      CHARACTER*(GCB__SZPTR) STR
      INTEGER ABSPTR
      INTEGER ISTAT
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  convert from pointer relative to top of block to absolute from bottom
        ABSPTR=GCB_SIZE()-RELPTR+1

*  and get value
        IF (GCB_NULL(%val(G_MEMPTR),ABSPTR,GCB__SZPTR,STATUS)) THEN
          PTR=0
        ELSE
          CALL GCB_GETSTR(%val(G_MEMPTR),ABSPTR,GCB__SZPTR,STR,STATUS)
          READ(STR,*,IOSTAT=ISTAT) PTR
          IF (ISTAT.NE.0) THEN
            PTR=0
          ENDIF
        ENDIF

      ENDIF

      END



*+  GCB_PUTRELPTR - write pointer value to location relative to top of block
      SUBROUTINE GCB_PUTRELPTR(PTR,RELPTR,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      INTEGER RELPTR
*    Import-Export :
*    Export :
      INTEGER PTR
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER GCB_SIZE
*    Local constants :
*    Local variables :
      CHARACTER*(GCB__SZPTR) STR
      INTEGER ABSPTR
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  convert to absolute pointer
        ABSPTR=GCB_SIZE()-RELPTR+1

*  write to memory
        WRITE(STR,GCB__PTRFMT) PTR
        CALL GCB_PUTSTR(STR,ABSPTR,GCB__SZPTR,%val(G_MEMPTR),STATUS)

      ENDIF

      END



*+  GCB_GETR_SUB - get a real value from specified location in GCB
      SUBROUTINE GCB_GETR_SUB(GCB,DISP,SIZ,FMT,RVAL,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      BYTE GCB(*)
      INTEGER DISP
      INTEGER SIZ
      CHARACTER*(*) FMT
*    Import-Export :
*    Export :
      REAL RVAL
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ISTAT
      CHARACTER*20 STR
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get appropriate length string
        CALL GCB_GETSTR(GCB,DISP,SIZ,STR,STATUS)
*  extract value
        READ(STR(:SIZ),FMT,IOSTAT=ISTAT) RVAL
        IF (ISTAT.NE.0) THEN
          CALL MSG_PRNT('AST_ERR: error reading value')
          STATUS=SAI__ERROR
          RVAL=0.0
        ENDIF
      ENDIF

      END



*+  GCB_GETI_SUB - get a integer value from specified location in GCB
      SUBROUTINE GCB_GETI_SUB(GCB,DISP,SIZ,FMT,IVAL,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      BYTE GCB(*)
      INTEGER DISP
      INTEGER SIZ
      CHARACTER*(*) FMT
*    Import-Export :
*    Export :
      INTEGER IVAL
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ISTAT
      CHARACTER*20 STR
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETSTR(GCB,DISP,SIZ,STR,STATUS)
        READ(STR(:SIZ),FMT,IOSTAT=ISTAT) IVAL
        IF (ISTAT.NE.0) THEN
          CALL MSG_PRNT('AST_ERR: error reading value')
          STATUS=SAI__ERROR
          IVAL=0
        ENDIF

      ENDIF

      END




*+  GCB_GETL_SUB - get a logical value from specified location in GCB
      SUBROUTINE GCB_GETL_SUB(GCB,DISP,SIZ,FMT,LVAL,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      BYTE GCB(*)
      INTEGER DISP
      INTEGER SIZ
      CHARACTER*(*) FMT
*    Import-Export :
*    Export :
      LOGICAL LVAL
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ISTAT
      CHARACTER*1 STR
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETSTR(GCB,DISP,SIZ,STR,STATUS)
        READ(STR(:SIZ),FMT,IOSTAT=ISTAT) LVAL
        IF (ISTAT.NE.0) THEN
          CALL MSG_PRNT('AST_ERR: error reading value')
          STATUS=SAI__ERROR
          LVAL=.FALSE.
        ENDIF

      ENDIF

      END




*+  GCB_GETC_SUB - get a character string from specified location in GCB
      SUBROUTINE GCB_GETC_SUB(GCB,DISP,SIZ,STR,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      BYTE GCB(*)
      INTEGER DISP
      INTEGER SIZ
*    Import-Export :
*    Export :
      CHARACTER*(*) STR
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETSTR(GCB,DISP,SIZ,STR,STATUS)

      ENDIF

      END





*+  GCB_SETR_SUB - set a real value at specified location in GCB
      SUBROUTINE GCB_SETR_SUB(RVAL,DISP,SIZ,FMT,GCB,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      REAL RVAL
      INTEGER DISP
      INTEGER SIZ
      CHARACTER*(*) FMT
*    Import-Export :
*    Export :
      BYTE GCB(*)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*20 STR
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  write value into string
        WRITE(STR(:SIZ),FMT) RVAL
*  copy to appropriate place in memory
        CALL GCB_PUTSTR(STR(:SIZ),DISP,SIZ,GCB,STATUS)

      ENDIF

      END




*+  GCB_SETI_SUB - set an integer value at specified location in GCB
      SUBROUTINE GCB_SETI_SUB(IVAL,DISP,SIZ,FMT,GCB,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      INTEGER IVAL
      INTEGER DISP
      INTEGER SIZ
      CHARACTER*(*) FMT
*    Import-Export :
*    Export :
      BYTE GCB(*)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*20 STR
*-

      IF (STATUS.EQ.SAI__OK) THEN

        WRITE(STR(:SIZ),FMT) IVAL
        CALL GCB_PUTSTR(STR(:SIZ),DISP,SIZ,GCB,STATUS)

      ENDIF

      END




*+  GCB_SETL_SUB - set a logical value at specified location in GCB
      SUBROUTINE GCB_SETL_SUB(LVAL,DISP,SIZ,FMT,GCB,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      LOGICAL LVAL
      INTEGER DISP
      INTEGER SIZ
      CHARACTER*(*) FMT
*    Import-Export :
*    Export :
      BYTE GCB(*)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*20 STR
*-

      IF (STATUS.EQ.SAI__OK) THEN

        WRITE(STR(:SIZ),FMT) LVAL
        CALL GCB_PUTSTR(STR(:SIZ),DISP,SIZ,GCB,STATUS)

      ENDIF

      END




*+  GCB_SETC_SUB - set a real value at specified location in GCB
      SUBROUTINE GCB_SETC_SUB(CVAL,DISP,SIZ,GCB,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      CHARACTER*(*) CVAL
      INTEGER DISP
      INTEGER SIZ
*    Import-Export :
*    Export :
      BYTE GCB(*)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_PUTSTR(CVAL,DISP,SIZ,GCB,STATUS)

      ENDIF

      END



*+  GCB_RDDSCF - read in GCB description file
      SUBROUTINE GCB_RDDSCF(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      CHARACTER*80 BUFFER,FILE
      CHARACTER*15 NAME
      CHARACTER*8 FMT
      INTEGER FD
      INTEGER US
      INTEGER SCPTR,STPTR
      INTEGER NSCAL,NSTRUC,NCOMP
      INTEGER DISP,CSIZE
      LOGICAL FINI
      LOGICAL SCALAR,STRUCTURE
*-
      IF (STATUS.NE.SAI__OK) RETURN

      CALL PSX_GETENV('AST_GCB_DSCF',FILE,STATUS)
      CALL FIO_OPEN(FILE,'READ','LIST',80,FD,STATUS)

*  reserve first part of memory for end pointer
      G_END=1
      SCPTR=GCB__SZPTR+1
      NSCAL=0
      NSTRUC=0
      FINI=.FALSE.
      DO WHILE (.NOT.FINI.AND.STATUS.EQ.SAI__OK)

        CALL FIO_READF(FD,BUFFER,STATUS)
*  ignore lines beginning with '*'
        IF (BUFFER(1:1).NE.'*') THEN
*  control keyword
          IF (BUFFER(1:1).EQ.'%') THEN
*  definition version number
            IF (BUFFER(2:8).EQ.'version') THEN
              READ(BUFFER(9:),*) G_VERSION
*  default size of control block
            ELSEIF (BUFFER(2:5).EQ.'size') THEN
              READ(BUFFER(6:),*) G_SIZE
*  top of block contains pointer to end of linked lists
              G_ENDSTRUC=GCB__SZPTR
              STPTR=2*GCB__SZPTR
*  address for first location in block available to store lists
              G_STARTLISTS=(GCB__MXSTRUC*3+1)*GCB__SZPTR +1
*  start of scalar definition
            ELSEIF (BUFFER(2:).EQ.'scalar') THEN
              SCALAR=.TRUE.
*  end of scalar definition
            ELSEIF (BUFFER(2:).EQ.'endscalar') THEN
              SCALAR=.FALSE.
*  start of structure definition
            ELSEIF (BUFFER(2:).EQ.'structure') THEN
              STRUCTURE=.TRUE.
              NSTRUC=NSTRUC+1
              NCOMP=0
*  first part of structure is pointer to next link
              DISP=GCB__SZPTR
*  end of structure definition
            ELSEIF (BUFFER(2:).EQ.'endstructure') THEN
*  record total size of each group for that structure
              G_STSIZ(NSTRUC)=DISP
              STRUCTURE=.FALSE.
*  end of description file
            ELSEIF (BUFFER(2:).EQ.'enddescription') THEN
              FINI=.TRUE.


            ENDIF

          ELSE

*  reading a scalar definition
            IF (SCALAR) THEN
              NSCAL=NSCAL+1
*  name
              G_SCNAME(NSCAL)=BUFFER(:16)
*  length and format
              READ(BUFFER(17:),*) G_SCSIZ(NSCAL),G_SCFMT(NSCAL)
*  displacement within block
              G_SCDISP(NSCAL)=SCPTR
              SCPTR=SCPTR+G_SCSIZ(NSCAL)

*  reading a structure definition
            ELSEIF (STRUCTURE) THEN
              NAME=BUFFER(:16)
              READ(BUFFER(17:),*) CSIZE,FMT
              US=INDEX(NAME,'_')
*  each structure must have NAME_N component
              IF (NAME(US:).EQ.'_N') THEN
*  take root name from this
                G_STNAME(NSTRUC)=NAME(:US)
*  allocate space for pointers to linked lists at top of block
                G_PTR_START(NSTRUC)=STPTR
                G_CURR(NSTRUC)=STPTR+GCB__SZPTR
                G_PTR_CURR(NSTRUC)=STPTR+2*GCB__SZPTR
                STPTR=STPTR+3*GCB__SZPTR
*  add _N component to scalar definitions
                NSCAL=NSCAL+1
                G_SCNAME(NSCAL)=NAME
                G_SCSIZ(NSCAL)=CSIZE
                G_SCFMT(NSCAL)=FMT
                G_SCDISP(NSCAL)=SCPTR
                SCPTR=SCPTR+CSIZE
*  otherwise add to list of component names for this structure
              ELSE
                NCOMP=NCOMP+1
                G_CNAME(NSTRUC,NCOMP)=NAME
                G_CSIZ(NSTRUC,NCOMP)=CSIZE
                G_CFMT(NSTRUC,NCOMP)=FMT
                G_CDISP(NSTRUC,NCOMP)=DISP
                DISP=DISP+CSIZE

              ENDIF
            ENDIF


          ENDIF
        ENDIF
      ENDDO

      G_TOPSCAL=SCPTR

      G_INITIALISED=(STATUS.EQ.SAI__OK)

      CALL FIO_CLOSE(FD,STATUS)

      END


*+  GCB_PUTSTR - write string into specified location in GCB
      SUBROUTINE GCB_PUTSTR(STR,DISP,SIZ,GCB,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      CHARACTER*(*) STR
      INTEGER DISP,SIZ
*    Import-Export :
*    Export :
      BYTE GCB(*)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER I,J
*-

      IF (STATUS.EQ.SAI__OK) THEN


        J=DISP
        DO I=1,SIZ
          GCB(J)=ICHAR(STR(I:I))
          J=J+1
        ENDDO

      ENDIF

      END

*+  GCB_GETSTR - reads string from specified location in GCB
      SUBROUTINE GCB_GETSTR(GCB,DISP,SIZ,STR,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
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
      BYTE GCB(*)
      INTEGER DISP,SIZ
*    Import-Export :
*    Export :
      CHARACTER*(*) STR
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER I,J
      INTEGER IB
*-

      IF (STATUS.EQ.SAI__OK) THEN

        STR=' '

        J=DISP
        DO I=1,SIZ
          IB=GCB(J)
          STR(I:I)=CHAR(IB)
          J=J+1
        ENDDO

      ENDIF

      END


*+  GCB_SIZE - returns full current size of GCB
      INTEGER FUNCTION GCB_SIZE()
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ISTAT,SIZ

*-
      ISTAT=0
      CALL GCB_GETPTR(G_END,SIZ,ISTAT)
      IF (ISTAT.EQ.0) THEN
        GCB_SIZE=SIZ
      ELSE
        GCB_SIZE=0
      ENDIF



      END


*+  GCB_REL2ABS - converts relative pointer to absolute
      INTEGER FUNCTION GCB_REL2ABS(RELPTR)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
      INTEGER RELPTR
*    Import-Export :
*    Export :
*    Status :
*    Function declarations :
      INTEGER GCB_SIZE
*    Local constants :
*    Local variables :

*-
      GCB_REL2ABS=GCB_SIZE()-RELPTR+1

      END



*+
      SUBROUTINE GCB_SAVE_SUB(NSCAL,NSTRUC,MEM,FL,STATUS)
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
      INTEGER NSCAL,NSTRUC
      BYTE MEM(*)
*    Import-Export :
*    Export :
      BYTE FL(*)
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER GCB_SIZE
*    Local constants :
*    Local variables :
      INTEGER DISP
      INTEGER I,J
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  write header
        DISP=1
        CALL GCB_SETR_SUB(G_VERSION,DISP,GCB__SZPTR,GCB__VERFMT,FL,
     :                                                       STATUS)
        DISP=DISP+GCB__SZPTR
        CALL GCB_SETI_SUB(GCB_SIZE(),DISP,GCB__SZPTR,GCB__PTRFMT,FL,
     :                                                       STATUS)
        DISP=DISP+GCB__SZPTR
        CALL GCB_SETI_SUB(NSCAL,DISP,GCB__SZPTR,GCB__PTRFMT,FL,STATUS)
        DISP=DISP+GCB__SZPTR
        CALL GCB_SETI_SUB(NSTRUC,DISP,GCB__SZPTR,GCB__PTRFMT,FL,STATUS)
        DISP=GCB__NHDBLK*GCB__SZPTR+1

*  write scalar components
        J=GCB__SZPTR+1
        DO I=1,NSCAL
          FL(DISP)=MEM(J)
          J=J+1
          DISP=DISP+1
        ENDDO

*  write structured components
        J=GCB_SIZE()
        DO I=1,NSTRUC
          FL(DISP)=MEM(J)
          J=J-1
          DISP=DISP+1
        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_SAVE_SUB',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE GCB_LOAD_SUB(FL,MEM,STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
      BYTE FL(*)
*    Import-Export :
*    Export :
      BYTE MEM(*)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      REAL VERSION
      INTEGER NSCAL,NSTRUC
      INTEGER DISP,SIZ
      INTEGER GCBSIZ
      INTEGER I,J,K
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  check version number
        DISP=1
        SIZ=GCB__SZPTR
        CALL GCB_GETR_SUB(FL,DISP,SIZ,GCB__VERFMT,VERSION,STATUS)
        IF (VERSION.NE.G_VERSION) THEN
          CALL MSG_PRNT('GCB_ERR: incompatible graphics control data')
          CALL MSG_PRNT('         * existing data will be cleared *')
          IF (STATUS.EQ.SAI__ERROR) THEN
            CALL ERR_ANNUL(STATUS)
          ENDIF
          CALL GCB_CLEAR(STATUS)
        ELSE

          DISP=DISP+GCB__SZPTR
          CALL GCB_GETI_SUB(FL,DISP,SIZ,GCB__PTRFMT,GCBSIZ,STATUS)
          DISP=DISP+GCB__SZPTR
          CALL GCB_GETI_SUB(FL,DISP,SIZ,GCB__PTRFMT,NSCAL,STATUS)
          DISP=DISP+GCB__SZPTR
          CALL GCB_GETI_SUB(FL,DISP,SIZ,GCB__PTRFMT,NSTRUC,STATUS)

          CALL GCB_PUTPTR(GCBSIZ,G_END,STATUS)
          J=GCB__NHDBLK*GCB__SZPTR+1
          K=GCB__SZPTR+1
          DO I=1,NSCAL
            MEM(K)=FL(J)
            J=J+1
            K=K+1
          ENDDO

          K=GCBSIZ
          DO I=1,NSTRUC
            MEM(K)=FL(J)
            K=K-1
            J=J+1
          ENDDO

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_LOAD_SUB',STATUS)
        ENDIF

      ENDIF

      END
