CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLIST.INC for TBLIST.F

      integer     nbef(1000)		! Number after decimal point 
      integer     ncol(1000)		! Order of columns to output
      integer     naft(1000)		! Number before decimal point
      integer     numid			! Number of characters in name to output
      integer     dohead		! Output heading: standard format(1);short(2);none(3)

      logical	  dotype		! Output table to console?
      logical	  dofile		! Output table to file?

      logical     tbl_dum1		!Dummy for 4*n logicals
      logical     tbl_dum2		!Dummy for 4*n logicals

      common / tblista /  nbef, ncol, naft, numid, dohead

      common / tblistc / dotype, dofile, tbl_dum1,  tbl_dum2
