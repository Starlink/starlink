CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSTAT.INC for TBSTAT.F

      logical    istwo		!Is one (F) or two (T) columns?

      integer    iptab1		!Input table 1 pointer
      integer    iptab2		!Input table 2 pointer
      integer    tbvx1		!Input table 1 X size
      integer    tby1		!Input table 1 Y size
      integer    tbx1		!Input table 1 X size - 5
      integer    tbvx2		!Input table 2X size
      integer    tby2		!Input table 2Y size
      integer    tbx2		!Input table 2X size - 5
      integer    ncol1		!Column number in table 1
      integer    ncol2		!Column number in table 2


      logical    tb_dum1	!Dummy for 4*n logicals
      logical    tb_dum2	!Dummy for 4*n logicals
      logical    tb_dum3	!Dummy for 4*n logicals

      common / tbstata / iptab1,  iptab2,    tbvx1,   tby1,  tbx1,
     +                    tbvx2,    tby2,     tbx2,  ncol1, ncol2

      common / tbstatb / istwo, tb_dum1, tb_dum2, tb_dum3


