C-------------------------------------------------------------------------
C
C                       HEADER.INC
C                       ----------

C   This include file describes the scan headers as stored in SPECX data
C   files. 
C   Contains 64 integer*4 words

C   NOTE: 
C        This file is NOT identical to STACKCOMM.SAV as all integer variables
C        in the stack are I*4 whereas most of those in the header are I*2.
C        READSCAN and WRITESCAN therefore have to convert these.

      REAL*4    R4DATA
      INTEGER*2 I2DATA
      CHARACTER CDATA*44

      COMMON /HEADER/ R4DATA(35),
     &                I2DATA(36),
     &                CDATA
      SAVE   /HEADER/
C-------------------------------------------------------------------------
