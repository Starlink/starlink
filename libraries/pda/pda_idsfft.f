

      SUBROUTINE PDA_IDSFFT(md,ncp,ndp,xd,yd,zd,nxi,nyi,xi,yi,zi,
     :                      iwk,wk,istat,status)

c this subroutine performs smooth surface fitting when the pro-
c jections of the data points in the x-y plane are irregularly
c distributed in the plane.

c the input parameters are
c     md  = mode of computation (must be 1, 2, or 3),
c         = 1 for new ncp and/or new xd-yd,
c         = 2 for old ncp, old xd-yd, new xi-yi,
c         = 3 for old ncp, old xd-yd, old xi-yi,
c     ncp = number of additional data points used for esti-
c           mating partial derivatives at each data point
c           (must be 2 or greater, but smaller than ndp),
c     ndp = number of data points (must be 4 or greater),
c     xd  = array of dimension ndp containing the x
c           coordinates of the data points,
c     yd  = array of dimension ndp containing the y
c           coordinates of the data points,
c     zd  = array of dimension ndp containing the z
c           coordinates of the data points,
c     nxi = number of output grid points in the x coordinate
c           (must be 1 or greater),
c     nyi = number of output grid points in the y coordinate
c           (must be 1 or greater),
c     xi  = array of dimension nxi containing the x
c           coordinates of the output grid points,
c     yi  = array of dimension nyi containing the y
c           coordinates of the output grid points.

c the output parameter is
c     zi  = doubly-dimensioned array of dimension (nxi,nyi),
c           where the interpolated z values at the output
c           grid points are to be stored.
c     istat = error message.
c     status= Starlink error status

c the other parameters are
c     iwk = integer array of dimension
c              max0(31,27+ncp)*ndp+nxi*nyi
c           used internally as a work area,
c     wk  = array of dimension 5*ndp used internally as a
c           work area.

c the very first call to this subroutine and the call with a new
c ncp value, a new ndp value, and/or new contents of the xd and
c yd arrays must be made with md=1.  the call with md=2 must be
c preceded by another call with the same ncp and ndp values and
c with the same contents of the xd and yd arrays.  the call with
c md=3 must be preceded by another call with the same ncp, ndp,
c nxi, and nyi values and with the same contents of the xd, yd,
c xi, and yi arrays.  between the call with md=2 or md=3 and its
c preceding call, the iwk and wk arrays must not be disturbed.
c use of a value between 3 and 5 (inclusive) for ncp is recom-
c mended unless there are evidences that dictate otherwise.

c this subroutine calls the idcldp, idgrid, idpdrv, idptip, and
c idtang subroutines.

c  Starlink Status.
      INTEGER STATUS                  ! Global status

c declaration statements
      dimension xd(100), yd(100), zd(100), xi(101), yi(101), zi(10201),
     :          iwk(13301), wk(500)
      common /idpi  / itpv
      integer istat

*  these variables seem to require caching between calls - TIMJ
      save nl, nt

c   check the inherited error status.
      if (STATUS.NE.0) return

c setting of some input parameters to local variables.
c (for md=1,2,3)
      md0 = md
      ncp0 = ncp
      ndp0 = ndp
      nxi0 = nxi
      nyi0 = nyi

c error check.  (for md=1,2,3)
      if ( md0.ge.1 .and. md0.le.3 ) then
         if ( ncp0.ge.2 .and. ncp0.lt.ndp0 ) then
            if ( ndp0.ge.4 ) then
               if ( nxi0.ge.1 .and. nyi0.ge.1 ) then
                  if ( md0.ge.2 ) then
                     ncppv = iwk(1)
                     ndppv = iwk(2)
                     if ( ncp0.ne.ncppv ) go to 100
                     if ( ndp0.ne.ndppv ) go to 100
                  else
                     iwk(1) = ncp0
                     iwk(2) = ndp0
                  end if
                  if ( md0.ge.3 ) then
                     nxipv = iwk(3)
                     nyipv = iwk(4)
                     if ( nxi0.ne.nxipv ) go to 100
                     if ( nyi0.ne.nyipv ) go to 100
                  else
                     iwk(3) = nxi0
                     iwk(4) = nyi0
                  end if

c allocation of storage areas in the iwk array.  (for md=1,2,3)
                  jwipt = 16
                  jwiwl = 6*ndp0 + 1
                  jwngp0 = jwiwl - 1
                  jwipl = 24*ndp0 + 1
                  jwiwp = 30*ndp0 + 1
                  jwipc = 27*ndp0 + 1
                  jwigp0 = max0(31, 27+ncp0)*ndp0

c triangulates the x-y plane.  (for md=1)
                  if ( md0.le.1 ) then
                     call PDA_IDTANG(ndp0,xd,yd,nt,iwk(jwipt),nl,
     :                               iwk(jwipl),iwk(jwiwl),
     :                               iwk(jwiwp),wk,istat)
                     iwk(5) = nt
                     iwk(6) = nl
                     if ( nt.eq.0 ) then
                        if(istat.ne.0) status=1
                        return
                     end if
                  end if

c determines ncp points closest to each data point.  (for md=1)
                  if ( md0.le.1 ) then
                     call PDA_IDCLDP(ndp0,xd,yd,ncp0,iwk(jwipc),istat)
                     if (iwk(jwipc).eq.0) then
                        if(istat.ne.0) status=1
                        return
                     end if
                  end if

c sorts output grid points in ascending order of the triangle
c number and the border line segment number.  (for md=1,2)
                  if ( md0.ne.3 ) then
                     call PDA_IDGRID(xd,yd,nt,iwk(jwipt),
     :                               nl,iwk(jwipl),nxi0,nyi0,xi,yi,
     :                               iwk(jwngp0+1),iwk(jwigp0+1),
     :                               istat)
                  end if

c estimates partial derivatives at all data points.
c (for md=1,2,3)
                  call PDA_IDPDRV(ndp0,xd,yd,zd,ncp0,iwk(jwipc),
     :                            wk,istat)

c interpolates the zi values.  (for md=1,2,3)
                  itpv = 0
                  jig0mx = 0
                  jig1mn = nxi0*nyi0 + 1
                  nngp = nt + 2*nl
                  do 5 jngp = 1, nngp
                     iti = jngp
                     if ( jngp.gt.nt ) then
                        il1 = (jngp-nt+1)/2
                        il2 = (jngp-nt+2)/2
                        if ( il2.gt.nl ) il2 = 1
                        iti = il1*(nt+nl) + il2
                     end if
                     jwngp = jwngp0 + jngp
                     ngp0 = iwk(jwngp)
                     if ( ngp0.ne.0 ) then
                        jig0mn = jig0mx + 1
                        jig0mx = jig0mx + ngp0
                        do 2 jigp = jig0mn, jig0mx
                           jwigp = jwigp0 + jigp
                           izi = iwk(jwigp)
                           iyi = (izi-1)/nxi0 + 1
                           ixi = izi - nxi0*(iyi-1)
                           call PDA_IDPTIP(xd,yd,zd,nt,iwk(jwipt),nl,
     :                                     iwk(jwipl),wk,iti,xi(ixi),
     :                                     yi(iyi),zi(izi),istat)
 2                      continue
                     end if
                     jwngp = jwngp0 + 2*nngp + 1 - jngp
                     ngp1 = iwk(jwngp)
                     if ( ngp1.ne.0 ) then
                        jig1mx = jig1mn - 1
                        jig1mn = jig1mn - ngp1
                        do 4 jigp = jig1mn, jig1mx
                           jwigp = jwigp0 + jigp
                           izi = iwk(jwigp)
                           iyi = (izi-1)/nxi0 + 1
                           ixi = izi - nxi0*(iyi-1)
                           call PDA_IDPTIP(xd,yd,zd,nt,iwk(jwipt),nl,
     :                                     iwk(jwipl),wk,iti,xi(ixi),
     :                                     yi(iyi),zi(izi),istat)
 4                      continue
                     end if
 5                continue
                  if(istat.ne.0) status=1
                  return
               end if
            end if
         end if
      end if

c error exit

 100  continue
      istat = 4
      status=1

      end
