CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SPRINKLE.INC     Include file for SPRINKLE

      real radc, radt  		      ! cluster king core, tidal radii
      real cxpos, cypos		      ! cluster pixel centre
      real ctdismod, ctred            ! cluster table distance mod, reddening
      real cdismod, cred              ! cluster distance mod, reddening to use
      real cmags, cmage, ccols, ccole ! limits on output cluster HR diag
      real cxs, cxe, cys, cye         ! limits on cluster posns
      real ftdismod, ftred            ! field table distance mod, reddening
      real fdismod, fred              ! field distance mod, reddening to use
      real fmags, fmage, fcols, fcole ! output limits on field  HR diag
      real fxs, fxe, fys, fye         ! ouput limits on field posns
      real fmagstart, fmagstep        ! field table v mag start step
      real fcolstart, fcolstep        ! field table b-v col start step
      real hm1factor, hm2factor       ! height/mag conversion: col 1, col 2
      integer ipo, ipcl, ipfi         ! pointers of output, cluster, field files
      integer tbvxo, tbxo, tbyo       ! Size of output table
      integer tbvxcl, tbycl           ! Size of cluster table
      integer tbvxfi, tbyfi           ! Size of field table
      integer nclust, nfield          ! no of cluster, field stars to do
      integer nranc, nranf            ! random number seeds for cluster, field
      logical cuniform, funiform      ! flag for uniform f,c HR diags
      logical doclust, dofield        ! flag for making cluster, field
      logical dolog                   ! print out stars as calculated
    
      logical s_dum1, s_dum2, s_dum3

      parameter ( tbvxo=12 )
      parameter ( tbxo=7 )

      common / sprinr / radc,    radt,  cxpos,  cypos,    cdismod, 
     +                  cmags,  cmage,  ccols,  ccole,       cred, 
     +                  cxs,      cxe,  cys,      cye,    fdismod, 
     +                  fmags,  fmage,  fcols,  fcole,       fred, 
     +                  fxs,      fxe,  fys,      fye,  hm1factor, 
     +                  hm2factor, ctred, ctdismod, ftred, ftdismod, 
     +                  fmagstart, fmagstep, fcolstart, fcolstep 

      common / sprini /    ipo,   tbyo,  nclust,  nfield,  nranc,   
     +                   nranf,   ipcl,    ipfi,  tbvxcl,  tbycl,  
     +                  tbvxfi,  tbyfi 

      common / sprinb / cuniform, funiform, doclust, dofield,
     +                  dolog,    s_dum1,   s_dum2,  s_dum3  


      integer	ncsect			!Max allowed no of sections
					!in cluster file
      parameter (ncsect=500)
      integer	nfsect			!Max allowed no of sections
					!in field file
      parameter (nfsect=10000)

      integer	nking			!Division of cluster in NKING
					!concentric rings for King distribution
      parameter (nking=2000)
      real	rnking			!Real version of NKING
      parameter (rnking=2000.0)


