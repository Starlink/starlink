*+  GFX_TITLE - plots titles over plot
      SUBROUTINE GFX_TITLE(DEFAULT,STATUS)

*    Description :
*        Interogates the Grafix Control Block for titles to
*        put over plot - if none found the default is used
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'GCB_PAR'
*    Import :
      CHARACTER*(*) DEFAULT
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*80 TEXT
      CHARACTER*1 JUST
      INTEGER FONT
      INTEGER NLINE
      INTEGER ILINE
      INTEGER BOLD
      REAL SIZE
      REAL ALIGN
      REAL DISP
      REAL OFFSET
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_SETDEF(STATUS)

*  see how many lines
        CALL GCB_GETI('TITLE_N',OK,NLINE,STATUS)

*  if none then use default
        IF (.NOT.OK) THEN
          DISP=0.5
          ALIGN=0.5
          CALL PGMTEXT('T',DISP,ALIGN,ALIGN,DEFAULT)

        ELSE


          DO ILINE=NLINE,1,-1

*  get text and attributes for each line
            CALL GCB_GET1I('TITLE_FONT',ILINE,1,OK,FONT,STATUS)
            IF (OK) THEN
              CALL PGSCF(FONT)
            ENDIF
            CALL GCB_GET1I('TITLE_BOLD',ILINE,1,OK,BOLD,STATUS)
            IF (OK) THEN
              CALL PGSLW(BOLD)
            ENDIF
            CALL GCB_GET1R('TITLE_SIZE',ILINE,1,OK,SIZE,STATUS)
            IF (OK) THEN
              CALL PGSCH(SIZE)
            ELSE
              CALL PGQCH(SIZE)
            ENDIF
            CALL GCB_GET1C('TITLE_JUST',ILINE,1,OK,JUST,STATUS)
            IF (OK) THEN
              IF (JUST.EQ.'L') THEN
                ALIGN=0.0
              ELSEIF (JUST.EQ.'C') THEN
                ALIGN=0.5
              ELSEIF (JUST.EQ.'R') THEN
                ALIGN=1.0
              ELSE
                ALIGN=0.5
              ENDIF
            ELSE
              ALIGN=0.5
            ENDIF
            CALL GCB_GET1C('TITLE_TEXT',ILINE,1,OK,TEXT,STATUS)
            IF (.NOT.OK) THEN
              TEXT=' '
            ENDIF

*  set displacement from edge of plot taking character size into account
            IF (ILINE.EQ.NLINE) THEN
              OFFSET=0.5*SIZE
            ELSE
              OFFSET=OFFSET+SIZE
            ENDIF
            DISP=OFFSET/SIZE

            CALL PGMTEXT('T',DISP,ALIGN,ALIGN,TEXT)

            CALL GCB_SETDEF(STATUS)

          ENDDO

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_TITLE',STATUS)
        ENDIF

      ENDIF

      END
