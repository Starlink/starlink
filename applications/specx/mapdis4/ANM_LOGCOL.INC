*--------------------------------------------------
*    anm_logcol.inc
*    - common blocks for log grey scales
*--------------------------------------------------

      real     vref /1.e-4/

      real     vmin
      real     vmax
      logical  logcol

      common /logcol/ vmin, vmax, logcol

      save   /logcol/
