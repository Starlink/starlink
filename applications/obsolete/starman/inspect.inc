CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INSPECT.INC
C
C  alan penny                    ral                   1990-06-16

      integer	NXS		!X start of area of interest
      integer	NXE		!X   end of area of interest
      integer	NXSTEP		!X type-out sampling step in area of interest
      integer	NYS		!Y start of area of interest
      integer	NYE		!Y   end of area of interest
      integer	NYSTEP		!Y type-out sampling step in area of interest
      real	BOT		!Lower limit on typed-out values
      real	TOP		!Lower limit on typed-out values
      real	SCALE		!Scaling applied to typed-out values
      real	BASE		!Zero-point applied to typed-out values
      integer	WIDTH		!Total width of character string of typed-out
				! values
      integer	DECPL		!No of decimal places in typed-out values
      integer	ORIENT		!(degrees in azimuth) of point of view for 
				! 3-D solid plot

      logical	GOTHRANGE	!Got histogram range, step?

      logical	ins_dum1	!Dummy for 4*n logicals
      logical	ins_dum2	!
      logical	ins_dum3	!

      common / incoma / nxs, nxe, nxstep, nys, nye, nystep, bot, top, 
     +                  scale, base, width, decpl, orient

      common / incomb / gothrange, ins_dum1, ins_dum2, ins_dum3

