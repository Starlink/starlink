CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  TBNMATCH.INC for TBNMATCH.F
C

      integer     knumf			!Max number of input files
      parameter   (knumf=40 )

      integer     kmaxr			!Max number of output rows
      integer     ipnames		!Pointer to names array
      integer     ipnums		!Pointer to numbers array
      integer     ipi(knumf)		!Pointers to input files
      integer     ipo(knumf)		!Pointers to output files
      integer     tbvxi(knumf)		!Numbers of cols (+5) in input files
      integer     tbyi(knumf)		!Numbers of rows in input files
      integer     tbyo			!Numbers of rows in output files
      integer     ncolo			!Column number of flag value
      integer     ktot			!Number of input files

      real        valcol		!Flag value for no data


      common / tbnma /  kmaxr,  ipnames,  ipnums,    ipi,   ipo,
     +                  tbvxi,     tbyi,    tbyo,  ncolo,  ktot, 
     +                 valcol
