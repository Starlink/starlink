*.......................................................................
*   Common block for storing the timer flag

*   Length of elapse time in seconds
      REAL TDELAY

*   Flag to indicate state of timer
*   0 = time still progressing
*   1 = time has reached target
      INTEGER TONOFF

      COMMON / IKN_COMTIM / TDELAY, TONOFF

      SAVE / IKN_COMTIM /

