C----------------------------------------------
C
C     GSD_FILE.INC
C    Include file to hold details of currently
C    open GSD SCAN_nnnn file
C
C----------------------------------------------

      INTEGER*4 IFD
      INTEGER*4 NITEM   
      REAL*4    VERSION
      LOGICAL   GSD_OPEN
      CHARACTER FILENAME*80
      CHARACTER LABEL*40

      COMMON /GSD_FILE/ IFD, NITEM, VERSION, GSD_OPEN, FILENAME, LABEL
      SAVE   /GSD_FILE/

