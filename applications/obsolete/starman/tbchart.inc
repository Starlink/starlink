CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C C_TBCHART.INC for TBCHART.F

      logical    gotmlims	!Got table mag limits?
      logical    gotplims	!Got table posn limits?
      logical    gotwork 	!Got work space?
      logical    newwork	!Need new work space?
      logical    gotdata 	!Got table open?
      logical    gotpsize	!Got plotting size?
      logical    gotmscale 	!Got mag scaling?
      logical    gropen		!Plot device open?
      logical    doneaxis	!Plot device axis plotted?
      logical    donecircle	!Written circles round star?
      logical    in_mag         !Magnitides input, rather than heights?

      integer    iptab		!Input table pointer
      integer    ipwk		!Dummy array pointer
      real       lsize		!Text size
      real       prx(2)		!Table x plot limits
      real       pry(2)		!Table y plot limits
      real       dml(2)		!Table mag plot limits
      real       psx(2)		!X plot limits
      real       psy(2)		!Y plot limits
      real       magmin		!Mag min for mag scaling
      real       magsc	 	!Scale for mag scaling
      integer    tbvx		!Input table X size
      integer    tby		!Input table Y size
      integer    nx		!Dummy array X size
      integer    ny		!Dummy array Y size

      logical    tbc_dum1	!Dummy for 4*n logicals
CX      logical    tbc_dum2	!Dummy for 4*n logicals
CX      logical    tbc_dum3	!Dummy for 4*n logicals

      common / tbcharta / iptab,  ipwk,  lsize,     prx,    pry,
     +                    dml,   psx,    psy,  magmin,  magsc,
     +                    tbvx,   tby,     nx,      ny

      common / tbchartb / gotmlims,  gotplims,    gotwork,   newwork,
     +                    gotdata,   gotpsize,  gotmscale,   gropen,
     +                    doneaxis,  donecircle,  in_mag,    tbc_dum1


