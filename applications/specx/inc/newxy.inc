C-------------------------------------------------
C    NEWXY.INC
C   Common block for plot scales determined interactively
C-------------------------------------------------

      REAL*4 XXST
      REAL*4 XXEND
      REAL*4 YYST
      REAL*4 YYEND
      REAL*4 XRANGE(2)
      REAL*4 YRANGE(2)
      INTEGER*4 NX
      INTEGER*4 NY
      LOGICAL*4 CHANGE_SCALES

      COMMON /NEWXY/   XXST, XXEND, YYST, YYEND, NX, NY, CHANGE_SCALES,
     &                 XRANGE, YRANGE

      SAVE   /NEWXY/
