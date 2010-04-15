      SUBROUTINE PDA_IDBVIP(md,ncp,ndp,xd,yd,zd,nip,xi,yi,zi,iwk,
     :                      wk,istat,status)

c this subroutine performs bivariate interpolation when the pro-
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
c     nip = number of output points at which interpolation
c           is to be performed (must be 1 or greater),
c     xi  = array of dimension nip containing the x
c           coordinates of the output points,
c     yi  = array of dimension nip containing the y
c           coordinates of the output points.

c the output parameter is
c     zi  = array of dimension nip where interpolated z
c           values are to be stored.
c     istat = error message.
c     status= Starlink error status

c the other parameters are
c     iwk = integer array of dimension
c              max0(31,27+ncp)*ndp+nip
c           used internally as a work area,
c     wk  = array of dimension 8*ndp used internally as a
c           work area.

c the very first call to this subroutine and the call with a new
c ncp value, a new ndp value, and/or new contents of the xd and
c yd arrays must be made with md=1.  the call with md=2 must be
c preceded by another call with the same ncp and ndp values and
c with the same contents of the xd and yd arrays.  the call with
c md=3 must be preceded by another call with the same ncp, ndp,
c and nip values and with the same contents of the xd, yd, xi,
c and yi arrays.  between the call with md=2 or md=3 and its
c preceding call, the iwk and wk arrays must not be disturbed.
c use of a value between 3 and 5 (inclusive) for ncp is recom-
c mended unless there are evidences that dictate otherwise.

c this subroutine calls the idcldp, idlctn, idpdrv, idptip, and
c idtang subroutines.

C  Starlink Status.
      INTEGER STATUS                  ! Global status

c declaration statements
      dimension xd(100), yd(100), zd(100), xi(1000), yi(1000),
     :          zi(1000), iwk(4100), wk(800)
      common /idlc  / nit
      common /idpi  / itpv
      integer istat
      save nt, nl

c   Check the inherited global status.
      IF (STATUS.NE.0) RETURN

c setting of some input parameters to local variables.
c (for md=1,2,3)
      md0 = md
      ncp0 = ncp
      ndp0 = ndp
      nip0 = nip

c error check.  (for md=1,2,3)
      if ( md0.ge.1 .and. md0.le.3 ) then
         if ( ncp0.ge.2 .and. ncp0.lt.ndp0 ) then
            if ( ndp0.ge.4 ) then
               if ( nip0.ge.1 ) then
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
                     nippv = iwk(3)
                     if ( nip0.ne.nippv ) go to 100
                  else
                     iwk(3) = nip
                  end if

c allocation of storage areas in the iwk array.  (for md=1,2,3)
                  jwipt = 16
                  jwiwl = 6*ndp0 + 1
                  jwiwk = jwiwl
                  jwipl = 24*ndp0 + 1
                  jwiwp = 30*ndp0 + 1
                  jwipc = 27*ndp0 + 1
                  jwit0 = max0(31, 27+ncp0)*ndp0

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
                     call PDA_IDCLDP(ndp0,xd,yd,ncp0,
     :                               iwk(jwipc),istat)
                     if ( iwk(jwipc).eq.0 ) then
                        if(istat.ne.0) status=1
                        return
                     end if
                  end if

c locates all points at which interpolation is to be performed.
c (for md=1,2)
                  if ( md0.ne.3 ) then
                     nit = 0
                     jwit = jwit0
                     do 2 iip = 1, nip0
                        jwit = jwit + 1
                        call PDA_IDLCTN(ndp0,xd,yd,nt,iwk(jwipt),nl,
     :                                  iwk(jwipl),xi(iip),yi(iip),
     :                                  iwk(jwit),iwk(jwiwk),wk,
     :                                  istat)
 2                   continue
                  end if

c estimates partial derivatives at all data points.
c (for md=1,2,3)
                  call PDA_IDPDRV(ndp0,xd,yd,zd,ncp0,iwk(jwipc),wk,
     :                            istat)

c interpolates the zi values.  (for md=1,2,3)
                  itpv = 0
                  jwit = jwit0
                  do 5 iip = 1, nip0
                     jwit = jwit + 1
                     call PDA_IDPTIP(xd,yd,zd,nt,iwk(jwipt),nl,
     :                               iwk(jwipl),wk,iwk(jwit),
     :                               xi(iip),yi(iip),zi(iip),
     :                               istat)
 5                continue
                  if(istat.ne.0) status=1
                  return

               end if
            end if
         end if
      end if

c error exit

 100  continue
      istat = 1

*   Setup Starlink status.
      STATUS=1

      end
