CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPLOT.INC for TBPLOT.F

      logical    gotplims	!Got table posn limits?
      logical    gothlims	!Got hisogram limits?
      logical    gotdlims	!Got table ranges?
      logical    gotaspect	!Got plot aspect?
      logical    gottable 	!Got table open?
      logical    gotfile2 	!Got a second table open?
      logical    file2ok 	!Got the equivalent of a second table?
      logical    gotim	 	!Got image open?
      logical    gotpsize	!Got plotting size?
      logical    doneaxis	!Plot device axis plotted?
      logical    ishist         !Plot is histogram?

      integer    nxs            !X start of image area of interest
      integer    nxe            !X end of image area of interest
      integer    nys            !Y start of image area of interest
      integer    nye            !Y end of imagearea of interest
      integer    iptab1		!Input table 1 pointer
      integer    iptab2		!Input table 2 pointer
      real       prx(2)		!Table x plot limits
      real       pry(2)		!Table y plot limits
      real       psx(2)		!X plot limits
      real       psy(2)		!Y plot limits
      real       phx(2)		!Histogram plot limits
      real       xmin		!'X' data minimum
      real       xmax		!'X' data maximum
      real       ymin		!'Y' data minimum
      real       ymax		!'Y' data maximum
      integer    tbvx1		!Input table 1 X size
      integer    tby1		!Input table 1 Y size
      integer    tbx1		!Input table 1 X size - 5
      integer    tbvx2		!Input table 2X size
      integer    tby2		!Input table 2Y size
      integer    tbx2		!Input table 2X size - 5
      integer    ncol1		!Column number in table 1
      integer    ncol2		!Column number in table 2
      integer    nbin		!Number of bins in histogram
      integer    kaspect   	!Flag for graph aspect ratio same(1) or variable(0)
      integer    kmode		!Flag for mode (1=interactve:2=auto points:
				!               3=auto line:4=auto histogram)

      logical    tbc_dum1	!Dummy for 4*n logicals
CX      logical    tbc_dum2	!Dummy for 4*n logicals
CX      logical    tbc_dum3	!Dummy for 4*n logicals

      character*20  xhead	!Header for X axis
      character*20  yhead	!Header for Y axis

      common / tbplota / iptab1,  iptab2,    prx,    pry,   phx,
     +                      psx,     psy,  tbvx1,   tby1,  tbx1,
     +                    tbvx2,    tby2,   tbx2,   xmin,  xmax,
     +                     ymin,    ymax,  ncol1,  ncol2,  nbin,
     +                   kaspect,  kmode,    nxs,    nxe,   nys,
     +                       nye

      common / tbplotb / gotplims,  gottable,   gotpsize,  gotdlims, 
     +                   doneaxis,  gothlims,   gotfile2,    ishist,
     +                    file2ok,     gotim,  gotaspect,  tbc_dum1

      common / tbplotc / xhead, yhead
