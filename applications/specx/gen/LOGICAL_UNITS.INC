      INTEGER*4 LUN_IN,    LUN_IN_SET
      INTEGER*4 LUN_OUT,   LUN_OUT_SET
      INTEGER*4 LUN_ERROR, LUN_ERROR_SET

      COMMON /FORLUNS/ LUN_IN,    LUN_IN_SET,
     &                 LUN_OUT,   LUN_OUT_SET,
     &                 LUN_ERROR, LUN_ERROR_SET

      SAVE   /FORLUNS/

