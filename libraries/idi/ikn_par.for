*.......................................................................
*   Parameters for IDI

*   Maximum buffer size for I/O
      INTEGER MAXBUF
      PARAMETER ( MAXBUF = 1024 )

*   Maximum number of cursors
      INTEGER MAXCUR
      PARAMETER ( MAXCUR= 2 )

*   Maximum number of devices available
      INTEGER MAXID
      PARAMETER ( MAXID = 4 )

*   Maximum number of interactors available
      INTEGER MAXINT
      PARAMETER ( MAXINT = 8 )

*   Maximum number of memories
      INTEGER MAXMEM
      PARAMETER ( MAXMEM = 4 )

*   Maximum number of regions of interest ( ROI )
      INTEGER MAXROI
      PARAMETER ( MAXROI = 2 )

*   Maximum number of colours
      INTEGER MAXCOL
      PARAMETER ( MAXCOL = 256 )

*   Shift to allow full use of the Ikon screen = ( 1024 - 780 )
      INTEGER IKON_SCROLL
      PARAMETER ( IKON_SCROLL = 244 )

