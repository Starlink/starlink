CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI.INC
C
C alan penny          RAL           1991 May

      real	xcur(10000)	!
      real	ycur(10000)	!
      real	plmin		!
      real	odata(2,100)	!Output table result
      real	slope		!
      real	zero		!
      real	zmag		!
      integer	numcur		!

      integer	ipt		!

      real	dlim		!Limit to distance from input posn
      real	chilim		!Limit to no of std devs of chi
      integer	itlim		!Limit to number of iterations
      integer	invlim		!Limit to number of invalid points

      logical	reduce		!
      logical	dograph		!
      logical	ch_dum1		!
      logical	ch_dum2		!
      
      character*50 title	!Input table title
      character*6  mode		!Automatic or interactive (auto:inter)

      common / chcoma /   xcur,    ycur,   odata,  slope,   ipt, 
     +                    zero,    zmag,  numcur,  plmin,  dlim,
     +                   itlim,  invlim,  chilim

      common / chcomb / reduce, dograph, ch_dum1, ch_dum2 

      common /chcomc / title, mode
