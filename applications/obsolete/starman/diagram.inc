CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DIAGRAM.INC   Include file for DIAGRAM
C
C  alan penny                    ral               90-10-05


      integer	maxstars		!Maximum no of stars allowed
      parameter ( maxstars=10000 )
      integer	maxset			!Maximum no of sets of files allowed
      parameter	( maxset=12 )

      integer	kstart			!=1 for two-col, 2 for HR diag
      integer	kfile(4)		!For colour n, file to access. 0 if none
      integer	nset			!Set number in use now
      integer	ntotset			!No of sets read in
      logical	dotnum			!Number stars in plots?
      logical   opened(maxset*4)	!Opened input file?
      logical   loaded(maxset)		!Loaded stars into data storage?
      integer	totstars		!Loaded no of stars in present set

      integer	kcolumn(4,maxset)	!Column data is in
      integer	kncolumn(4,maxset)	!Column no of good measures is in
      real	rawmax(4,maxset)	!Max allowed raw mag in a file in a set
      real	rawmin(4,maxset)	!Min allowed raw mag in a file in a set
      real	xxmin(maxset)		!Min allowed X colour in a set
      real	xxmax(maxset)		!Max allowed X colour in a set
      real	yymin(maxset)		!Min allowed Y mag (or colour) in a set
      real	yymax(maxset)		!Max allowed Y mag (or colour) in a set
      integer	symb(maxset)		!Plot symbol code (PGPLOT code)
      logical	changed(maxset)		!Output values of this set changed since
					!last calculation
      integer	numg(4,maxset)		!Min allowed good measures no (col,set)
      integer	ipf(4,maxset)		!Pointer to input data array (col,set)
      real	zerop(4,maxset)		!Zero point for (col,set)
      real	colcor(4,maxset)	!Col eq value for (col,set)
      integer	setstart(maxset)	!Starting posn for set in storage arrays
      integer	tbxvs(4,maxset)		!No of columns (+5) in file in set
      integer	tbys(maxset)		!No of stars in set
      real      plotrange(4) 		!Plot range allowed

      real	adata(4,maxstars)	!Storage for measures
      real	fnumfg(4,maxstars)	!Storage for no of good measures
      real	xx(maxstars)		!Storage for X plot values
      real	yy(maxstars)		!Storage for Y plot values
      logical	zz(maxstars)		!Storage for (putting a plot point?)

      real	dxlim(2)		!X range to plot out
      real	dylim(2)		!Y range to plot out
      logical	gotlims			!Got display range?
      logical	gotmlims		!Got file mag limits?range?
      logical	doneaxis		!Written axes?
      logical	allnumg			!No of good measures needed applies to all sets?

      character*1  col(4)		!One letter code of colours
      character*70 atitle		!Input title of first file
      character*20 aname(4,maxstars)	!Storage for names

      logical   di_dum1			!Dummy logical for common (*4)
CX      logical   di_dum2			!Dummy logical for common (*4)
CX      logical   di_dum3			!Dummy logical for common (*4)

      common /comma/ kstart,   kfile,      nset,  ntotset,  totstars,
     +               rawmax,  rawmin,      numg,   fnumfg,       ipf, 
     +                zerop,  colcor, plotrange,       xx,        yy, 
     +                dxlim,   dylim,      tbys,    tbxvs,      symb,
     +                xxmin,   xxmax,     yymin,    yymax,   kcolumn, 
     +             kncolumn,   adata,   setstart

      common /commb/  dotnum,     loaded,  gotlims,  doneaxis, 
     +                opened,
     +                    zz,
     +                changed,  gotmlims,  allnumg,  di_dum1

      common /commc/   col, atitle, aname
