*+  GDV_OPEN - open a graphics device
      SUBROUTINE GDV_OPEN(NAME,NX,NY,STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'GDV_CMN'
*    Structure definitions :
*    Import :
      CHARACTER*(*) NAME
      INTEGER NX,NY
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER PGBEGIN
      LOGICAL CHR_SIMLR
*    Local constants :
*    Local variables :
      CHARACTER YN*3,LNAME*40
      INTEGER L
*-

      IF (STATUS.EQ.SAI__OK) THEN

        LNAME=NAME
        CALL CHR_LCASE(LNAME)


*  deal with X-window case
        IF (CHR_SIMLR(LNAME(:3),'380')) THEN
          IF (LNAME(4:4).EQ.'0') THEN
            LNAME='xwindows'
          ELSE
            LNAME='x'//LNAME(4:4)//'windows'
          ENDIF
        ENDIF

        IF (PGBEGIN(0,LNAME,NX,NY).EQ.1) THEN
*  switch off prompting between pages
          CALL PGASK(.FALSE.)
*  page to first zone
          CALL PGPAGE()
*  put 'F' in last char. to indicate freshly opened
          LNAME(40:40)='F'
*  store device name
          G_DEVICE=LNAME
*  hardcopy device?
          CALL PGQINF('HARDCOPY',YN,L)
          G_HARDCOPY=(YN(:L).EQ.'YES')
*  colours?
          CALL PGQCOL(G_BGCOL,G_NCOL)
*  plotting zones
          G_XZONES=NX
          G_YZONES=NY
*  cursor?
          CALL PGQINF('CURSOR',YN,L)
          G_CURSOR=(YN(:L).EQ.'YES')


        ELSE
          STATUS=SAI__ERROR
          CALL ERR_REP(' ','AST_ERR: unable to open device '//LNAME,
     :                 STATUS )
          G_DEVICE=' '
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDV_OPEN',STATUS)
        ENDIF

      ENDIF

      END
