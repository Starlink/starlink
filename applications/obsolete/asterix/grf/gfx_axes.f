*+  GFX_AXES - plot axes
	SUBROUTINE GFX_AXES(STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
*  Local constants :
*  Local variables :
      CHARACTER*15 XOPTB,YOPTB
      CHARACTER*5 XOPTN,YOPTN
      REAL XTICK,YTICK,SIZE
      INTEGER I
      INTEGER FONT,WIDTH,XDIV,YDIV,BOLD,COLOUR
      LOGICAL OK
      LOGICAL LOK,LOG
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_SETDEF(STATUS)

*  get control parameters
        CALL GCB_GETC('XAXIS_OPT',OK,XOPTB,STATUS)
        IF (.NOT.OK) THEN
          XOPTB='BCTS'
          XOPTN='N'
        ELSE
*  move codes to do with numbers into separate string
          XOPTN=' '
          I=INDEX(XOPTB,'N')
          IF (I.GT.0) THEN
            XOPTB(I:I)=' '
            XOPTN(1:1)='N'
          ENDIF
          I=INDEX(XOPTB,'M')
          IF (I.GT.0) THEN
            XOPTB(I:I)=' '
            XOPTN(2:2)='M'
          ENDIF
          I=INDEX(XOPTB,'V')
          IF (I.GT.0) THEN
            XOPTB(I:I)=' '
            XOPTN(3:3)='V'
          ENDIF
        ENDIF
        CALL GCB_GETL('XAXIS_LOG',LOK,LOG,STATUS)
        IF (LOK.AND.LOG) THEN
          XOPTB(15:15)='L'
          XOPTN(5:5)='L'
        ENDIF

        CALL GCB_GETC('YAXIS_OPT',OK,YOPTB,STATUS)
        IF (.NOT.OK) THEN
          YOPTB='BCTS'
          YOPTN='N'
        ELSE
          YOPTN=' '
          I=INDEX(YOPTB,'N')
          IF (I.GT.0) THEN
            YOPTB(I:I)=' '
            YOPTN(1:1)='N'
          ENDIF
          I=INDEX(YOPTB,'M')
          IF (I.GT.0) THEN
            YOPTB(I:I)=' '
            YOPTN(2:2)='M'
          ENDIF
          I=INDEX(YOPTB,'V')
          IF (I.GT.0) THEN
            YOPTB(I:I)=' '
            YOPTN(2:2)='V'
          ENDIF
        ENDIF
        CALL GCB_GETL('YAXIS_LOG',LOK,LOG,STATUS)
        IF (LOK.AND.LOG) THEN
          YOPTB(15:15)='L'
          YOPTN(5:5)='L'
        ENDIF

        CALL GCB_GETR('XAXIS_TICK',OK,XTICK,STATUS)
        IF (.NOT.OK) THEN
          XTICK=0.0
        ENDIF
        CALL GCB_GETR('YAXIS_TICK',OK,YTICK,STATUS)
        IF (.NOT.OK) THEN
          YTICK=0.0
        ENDIF
        CALL GCB_GETI('XAXIS_DIV',OK,XDIV,STATUS)
        IF (.NOT.OK) THEN
          XDIV=0
        ENDIF
        CALL GCB_GETI('YAXIS_DIV',OK,YDIV,STATUS)
        IF (.NOT.OK) THEN
          YDIV=0
        ENDIF
        CALL GCB_GETI('AXES_WIDTH',OK,WIDTH,STATUS)
        IF (OK) THEN
          CALL PGSLW(WIDTH)
        ENDIF
        CALL GCB_GETI('AXES_COLOUR',OK,COLOUR,STATUS)
        IF (OK) THEN
          CALL PGSCI(COLOUR)
        ENDIF

*  draw box
        CALL PGBOX(XOPTB,XTICK,XDIV,YOPTB,YTICK,YDIV)
        CALL GCB_SETDEF(STATUS)

        CALL GCB_GETI('AXES_FONT',OK,FONT,STATUS)
        IF (OK) THEN
          CALL PGSCF(FONT)
        ENDIF

        CALL GCB_GETI('AXES_BOLD',OK,BOLD,STATUS)
        IF (OK) THEN
          CALL PGSLW(BOLD)
        ENDIF

        CALL GCB_GETR('AXES_SIZE',OK,SIZE,STATUS)
        IF (OK) THEN
          CALL PGSCH(SIZE)
        ENDIF

        CALL GCB_GETI('AXES_COLOUR',OK,COLOUR,STATUS)
        IF (OK) THEN
          CALL PGSCI(COLOUR)
        ENDIF

*  draw numeric labels
        CALL PGBOX(XOPTN,XTICK,XDIV,YOPTN,YTICK,YDIV)

        CALL GCB_SETDEF(STATUS)

      ENDIF

      END
