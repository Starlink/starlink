*+  GFX_LABELS - puts axis labels on plot
      SUBROUTINE GFX_LABELS(XDEF,YDEF,STATUS)

*    Description :
*        Interogates the Grafix Control Block for labels to
*        put on plot - if none found the defaults are used
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
      CHARACTER*(*) XDEF,YDEF
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*80 TEXT
      INTEGER FONT
      INTEGER BOLD
      INTEGER COLOUR
      REAL NSIZE,LSIZE
      REAL OFFSET
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_SETDEF(STATUS)

*  get size of numeric labels for estimating offset
        CALL GCB_GETR('AXES_SIZE',OK,NSIZE,STATUS)
        IF (.NOT.OK) THEN
          CALL PGQCH(NSIZE)
        ENDIF

*  x label
        CALL GCB_SETDEF(STATUS)
        CALL GCB_GETC('XLABEL_TEXT',OK,TEXT,STATUS)
*  if none then use default
        IF (.NOT.OK) THEN
          TEXT=XDEF
        ENDIF
*  get attributes
        CALL GCB_GETI('XLABEL_FONT',OK,FONT,STATUS)
        IF (OK) THEN
          CALL PGSCF(FONT)
        ENDIF
        CALL GCB_GETI('XLABEL_BOLD',OK,BOLD,STATUS)
        IF (OK) THEN
          CALL PGSLW(BOLD)
        ENDIF
        CALL GCB_GETR('XLABEL_SIZE',OK,LSIZE,STATUS)
        IF (OK) THEN
          CALL PGSCH(LSIZE)
        ELSE
          CALL PGQCH(LSIZE)
        ENDIF
        CALL GCB_GETI('XLABEL_COLOUR',OK,COLOUR,STATUS)
        IF (OK) THEN
          IF (COLOUR.LT.0) THEN
            COLOUR=0
          ENDIF
          CALL PGSCI(COLOUR)
        ENDIF
        CALL GCB_GETR('XLABEL_OFFSET',OK,OFFSET,STATUS)
        IF (.NOT.OK) THEN
          OFFSET=(0.8*NSIZE+1.6*LSIZE)/LSIZE
        ENDIF

        CALL PGMTEXT('B',OFFSET,0.5,0.5,TEXT)


*  y label
        CALL GCB_SETDEF(STATUS)

        CALL GCB_GETC('YLABEL_TEXT',OK,TEXT,STATUS)
        IF (.NOT.OK) THEN
          TEXT=YDEF
        ENDIF

        CALL GCB_GETI('YLABEL_FONT',OK,FONT,STATUS)
        IF (OK) THEN
          CALL PGSCF(FONT)
        ENDIF
        CALL GCB_GETI('YLABEL_BOLD',OK,BOLD,STATUS)
        IF (OK) THEN
          CALL PGSLW(BOLD)
        ENDIF
        CALL GCB_GETR('YLABEL_SIZE',OK,LSIZE,STATUS)
        IF (OK) THEN
          CALL PGSCH(LSIZE)
        ELSE
          CALL PGQCH(LSIZE)
        ENDIF
        CALL GCB_GETI('YLABEL_COLOUR',OK,COLOUR,STATUS)
        IF (OK) THEN
          IF (COLOUR.LT.0) THEN
            COLOUR=0
          ENDIF
          CALL PGSCI(COLOUR)
        ENDIF
        CALL GCB_GETR('YLABEL_OFFSET',OK,OFFSET,STATUS)
        IF (.NOT.OK) THEN
          OFFSET=(0.5*NSIZE+1.6*LSIZE)/LSIZE
        ENDIF

        CALL PGMTEXT('L',OFFSET,0.5,0.5,TEXT)


        CALL GCB_SETDEF(STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_LABELS',STATUS)
        ENDIF

      ENDIF

      END
