*----6-----------------------------------------------------------------*-------
    
      INTEGER*4 COL_NUM,COLDM

      REAL*4    COL_RED(0:256),COL_GREEN(0:256),COL_BLUE(0:256)

      REAL*4    NWRAP

      INTEGER*4 COLMIN
      INTEGER*4 COLMAX

      COMMON/ANM_COLS/COL_NUM, COL_RED, COL_GREEN, COL_BLUE,
     &                NWRAP, COLMIN, COLMAX

      SAVE  /ANM_COLS/
*----6-----------------------------------------------------------------*-------
