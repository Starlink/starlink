*.......................................................................
*   Common block for storing look-up tables

*   Look up table 0
      REAL CLUT0( 3, MAXCOL )

*   Look up table 1
      REAL CLUT1( 3, MAXCOL )

      COMMON / IKN_COMLUT / CLUT0, CLUT1

      SAVE / IKN_COMLUT /

