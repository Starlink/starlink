CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   SCRUT.INC
C
C  alan penny                    ral              1990-06-09

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Extra stars put in by user

      integer	nxee			!x;y;code of an extra star
      integer	nyee			!Max no of allowed extra stars
      parameter ( nxee=3 )
      parameter ( nyee=2000 )
      integer	knxee
      integer	knyee
      parameter ( knxee=3 )
      parameter ( knyee=2000 )
      real	extra(knxee,knyee)	!Code of extra(3,k) : 0=not there
					! 1=extra star ; 2=remove from main
					! list by number in extra(1,k)
      integer	nextra			!No of extra stars

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      integer	lpos(2)		!Start/end of list section to use
      real	larea(2,2)	!X,Y start and end area that list 
				!  stars have to fall in to be used
      real	prof(9)		!Star Profile from star list
      integer	ipin		!Pointer to list
      integer	tbvxs		!No of cols (inc 5 for name ) in list
      integer	tbxs		!No of data cols in list
      integer	tbys		!No of rows in list
      integer   ipmap		!Pointer for profigle map
      integer   mx		!X size of 3-D profile map array
      integer   my		!Y size of 3-D profile map array
      integer   mz		!Z size of 3-D profile map array
      integer   mapxk		!X size of Z plane of 3-D profile map array
      integer   mapyk		!Y size of Z plane of 3-D profile map array
      integer   magnifk		!Magnification of Z plane of 3-D profile map 
      integer	lx		!Load box X size
      integer	ly		!Load box Y size
      integer	lxold		!Last load box X size used for calc/display
      integer	lyold		!Last load box Y size used for calc/display
      integer	lxolda		!Last load box X size for replace display
      integer	lyolda		!Last load box Y size for replace display
      integer	lxs		!B.l.h. corner X location of load box
      integer	lys		!B.l.h. corner Y location of load box
      integer	lxsold		!Last b.l.h. corner X location of load box
      integer	lysold		!Last b.l.h. corner Y location of load box
      integer   ipp		!Pointer for work area to store profile map
      integer	dsize		!Image display device pixel size

      integer	numid		!No of characters in name to output
      integer	numbef(100)	!No of decimal places before dec points
      integer	numaft(100)	!No of decimal places after dec points
      integer	numcol(100)	!Column numbers to use
      integer	numcoltot	!No of columns to output

      logical	gotlist		!Got star list?
      logical	gotprof		!Got profile?
      logical	dremove		!`Removed' an area?
      logical	dreplace	!`Replaced' an area?
      logical	domap		!Use the profile map?
      logical	docont		!Load area with high contrast display?
      logical	doslope		!Subtract fit slope background?
      logical	dopref		!Load prefixes in names of `extra' stars?
      logical	doform		!Format typed-out values line?
      logical	doxy		!Have X,Y posns in list of star results?
      logical	bylist		!Choose stars by list or X,Y posn?
      logical   opscrut		!Opened 'scrutiny' s/r before?
      
CX      logical	sc_dum1		! booleans to make up 4*n for compilers
CX      logical	sc_dum2
CX      logical	sc_dum2

      common /sccoma/ larea, prof, extra, ipin, tbxs, tbvxs, tbys, 
     +                ipmap, mx, my, mz, dsize, nextra, lpos,
     +                lx,  ly,  lxold,  lyold,  lxolda,  lyolda, 
     +                lxs, lys, lxsold, lysold,
     +                numid, numbef, numaft, numcol, numcoltot,
     +                magnifk, mapxk, mapyk, ipp

      common /sccomb/ domap,     docont,   bylist,   opscrut,
     +                doslope,   dopref,     doxy,    doform, 
     +                gotlist,  gotprof,  dremove,  dreplace

