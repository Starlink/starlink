CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GPOSNS.INC
C


      integer nmaxgp		! Max no of posns that can be got
      parameter ( nmaxgp=4000 )

      integer nmaxgm		! Max no of magnitudes that can be got
      parameter ( nmaxgm=4000 )
      integer nxmag		! No of columns of magnitudes results
      parameter ( nxmag=15 )

      integer kcolgg		! colour code for getting positions cross
      integer kcolgm		! colour code for getting magn positions cross
      integer kcolgp		! colour code for plotting file cross
      real    crsizg		! Size of crosses

      integer numgp		! No of posns got
      integer kxgp(nmaxgp)	! Got positions X coords
      integer kygp(nmaxgp)	! Got positions Y coords
      real    valgp(nmaxgp)	! Got positions image pixel values

      integer numgm		! No of nags got
      real    resmag(nxmag,nmaxgm)	! Magnitude results
      integer nxgb		! X size of magnitude fitting box
      integer nygb		! Y size of magnitude fitting box
      real    rxgf		! Fixed X radius used to fit
      real    rygf		! Fixed Y radius used to fit

      integer ipintab		! Pointer to input posn table
      integer ntbx		! X size of  input posn table (inc 5 for name)
      integer ntby		! Y size of  input posn table 

      logical radgfix		! Fix Gauss radius
      logical gotposnfile	! Opened input position file?
      logical bshow		! Show box round magn posn?

      logical po_dum1		! Dummy to make 4*n logicals
CX      logical po_dum2		! Dummy to make 4*n logicals
CX      logical po_dum3		! Dummy to make 4*n logicals

      common / gposnsa / numgp,     kxgp,   kygp,  valgp,  numgm,
     +                   resmag,    nxgb,   nygb,   rxgf,   rygf, 
     +                   kcolgg,  kcolgp,  crsizg,  ntbx,   ntby,
     +                   ipintab, kcolgm

      common /gposnb / radgfix, gotposnfile, bshow, po_dum1
