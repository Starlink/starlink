*     Command line buffer

      INTEGER*4           ILB, IBPTR
      INTEGER*4           MAXLB
      PARAMETER          (MAXLB=512)
      CHARACTER           BUFFER*(MAXLB)
      COMMON /SCL_BUFFER/ BUFFER, ILB, IBPTR

      SAVE   /SCL_BUFFER/
