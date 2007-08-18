CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  GRAPH.INC
C
C   alan penny                    ral             1990 Apr

      logical       gd_dispop       !Is device open?
      character*256 gd_dev	    !Device type
      integer       gd_devn         !Device type name length
      integer       gd_bad          !=0 for good device, 1= bad
      real          gd_xs           ! X start posn of graph
      real          gd_xe           ! X end posn of graph
      real          gd_ys           ! Y start posn of graph
      real          gd_ye           ! Y end posn of graph
      integer       gd_linecol      ! Colour of written line (1=black;2=white)

      logical       gd_dum1
      logical       gd_dum2
      logical       gd_dum3

      common /deva/ gd_xs, gd_xe, gd_ys, gd_ye, gd_bad, gd_devn,
     +              gd_linecol

      common /devb/ gd_dispop, gd_dum1, gd_dum2, gd_dum3

      common /devc/ gd_dev
