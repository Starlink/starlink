
      REAL         RED(1:256), GREEN(1:256), BLUE(1:256)

      REAL         FG_RD, FG_GR, FG_BL
      REAL         BG_RD, BG_GR, BG_BL

      COMMON /COLOURS/ 
     :  RED, GREEN, BLUE, FG_RD, FG_GR, FG_BL, BG_RD, BG_GR, BG_BL
      SAVE /COLOURS/
