*--------------------------------------------------
*    anm_logcol.inc
*    - common blocks for log grey scales
*--------------------------------------------------

      real     vref
      parameter ( vref = 1.0e-4 )

      real     vmin
      real     vmax
      logical  logcol

      common /logcol/ vmin, vmax, logcol

      save   /logcol/
