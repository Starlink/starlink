CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  IMCUBE.INC


      integer   ipo			!Output image pointer
      integer   nz			!Image Z size
      integer   nt			!Image T size
      integer   ndim			!Image no of dimensions
      integer   nplane			!Code for which plane to cut out
      integer   np(2)			!Range in first ommitted plane to cut from
      integer   npp(2)			!Range in second ommitted plane to cut from
      integer   nxo			!Output Image X size
      integer   nyo			!Output Image Y size

CX      logical imcb_dum1			!Make logicals up to times 4
CX      logical imcb_dum2			!Make logicals up to times 4
CX      logical imcb_dum3			!Make logicals up to times 4



      common / cubea / ipo,    nplane,  nz,    nt,  ndim,  
     +                  nxo,     nyo,   np,   npp


