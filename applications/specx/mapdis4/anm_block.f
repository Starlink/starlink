      BLOCK DATA ANM_BLOCK

      INCLUDE 'ANM_COL.INC'

      DATA NWRAP   /1.0/
      DATA COLMIN  /16/
      DATA COLMAX  /255/
      DATA COL_NUM /1/

      DATA (COL_RED(COLDM),COLDM=0,15)
     &           /0,1,1,0,0,0,1,1,1,0.5,0,0,0.5,1,0.33,0.66/
      DATA (COL_GREEN(COLDM),COLDM=0,15)
     &           /0,1,0,1,0,1,0,1,0.5,1,1,0.5,0,0,0.33,0.66/
      DATA (COL_BLUE(COLDM),COLDM=0,15)
     &           /0,1,0,0,1,1,1,0,0,0,0.5,1,1,0.5,0.33,0.66/

      END
