CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GAUMAG.INC


      integer nbx	!X size of box round stars
      integer nby	!Y size of box round stars
      real    rxm	!X radius of fit (if fixed) or starting radius if not
      real    rym	!Y radius of fit (if fixed) or starting radius if not

      logical parfix		!Are the radii fixed?

      logical gm_dum1
      logical gm_dum2
      logical gm_dum3

      common / gma / nbx, nby, rxm, rym 

      common / gmb / parfix, gm_dum1, gm_dum2,  gm_dum3
