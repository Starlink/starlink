

      SUBROUTINE PDA_IDCLDP(ndp,xd,yd,ncp,ipc,istat)

c this subroutine selects several data points that are closest
c to each of the data point.

c the input parameters are
c     ndp = number of data points,
c     xd,yd = arrays of dimension ndp containing the x and y
c           coordinates of the data points,
c     ncp = number of data points closest to each data
c           points.

c the output parameter is
c     ipc = integer array of dimension ncp*ndp, where the
c           point numbers of ncp data points closest to
c           each of the ndp data points are to be stored.
*     istat = starlink error message.

c this subroutine arbitrarily sets a restriction that ncp must
c not exceed 25.

c declaration statements
      dimension xd(100), yd(100), ipc(400)
      dimension dsq0(25), ipc0(25)
      data ncpmx/25/
      integer istat

*   check the inherited error status.
      if ( istat.ne.0 ) return

c preliminary processing
      ndp0 = ndp
      ncp0 = ncp
      if ( ndp0.ge.2 ) then
         if ( ncp0.ge.1 .and. ncp0.le.ncpmx .and. ncp0.lt.ndp0 ) then

c calculation
            do 80 ip1 = 1, ndp0

c - selects ncp points.
               x1 = xd(ip1)
               y1 = yd(ip1)
               j1 = 0
               dsqmx = 0.0
               do 10 ip2 = 1, ndp0
                  if ( ip2.ne.ip1 ) then
                     dsqi = PDA_DSQF(x1,y1,xd(ip2),yd(ip2))
                     j1 = j1 + 1
                     dsq0(j1) = dsqi
                     ipc0(j1) = ip2
                     if ( dsqi.gt.dsqmx ) then
                        dsqmx = dsqi
                        jmx = j1
                     end if
                     if ( j1.ge.ncp0 ) go to 20
                  end if
 10            continue
 20            continue
               ip2mn = ip2 + 1
               if ( ip2mn.le.ndp0 ) then
                  do 25 ip2 = ip2mn, ndp0
                     if ( ip2.ne.ip1 ) then
                        dsqi = PDA_DSQF(x1,y1,xd(ip2),yd(ip2))
                        if ( dsqi.lt.dsqmx ) then
                           dsq0(jmx) = dsqi
                           ipc0(jmx) = ip2
                           dsqmx = 0.0
                           do 22 j1 = 1, ncp0
                              if ( dsq0(j1).gt.dsqmx ) then
                                 dsqmx = dsq0(j1)
                                 jmx = j1
                              end if
 22                        continue
                        end if
                     end if
 25               continue
               end if

c - checks if all the ncp+1 points are collinear.
               ip2 = ipc0(1)
               dx12 = xd(ip2) - x1
               dy12 = yd(ip2) - y1
               do 30 j3 = 2, ncp0
                  ip3 = ipc0(j3)
                  dx13 = xd(ip3) - x1
                  dy13 = yd(ip3) - y1
                  if ( (dy13*dx12-dx13*dy12).ne.0.0 ) go to 50
 30            continue

c - searches for the closest noncollinear point.
               nclpt = 0
               do 40 ip3 = 1, ndp0
                  if ( ip3.ne.ip1 ) then
                     do 32 j4 = 1, ncp0
                        if ( ip3.eq.ipc0(j4) ) go to 40
 32                  continue
                     dx13 = xd(ip3) - x1
                     dy13 = yd(ip3) - y1
                     if ( (dy13*dx12-dx13*dy12).ne.0.0 ) then
                        dsqi = PDA_DSQF(x1,y1,xd(ip3),yd(ip3))
                        if ( nclpt.ne.0 ) then
                           if ( dsqi.ge.dsqmn ) go to 40
                        end if
                        nclpt = 1
                        dsqmn = dsqi
                        ip3mn = ip3
                     end if
                  end if
 40            continue
               if ( nclpt.eq.0 ) go to 100
               dsqmx = dsqmn
               ipc0(jmx) = ip3mn

c - replaces the local array for the output array.
 50            continue
               j1 = (ip1-1)*ncp0
               do 60 j2 = 1, ncp0
                  j1 = j1 + 1
                  ipc(j1) = ipc0(j2)
 60            continue
 80         continue
            return
         end if
      end if

c error exit
      istat = 2
      ipc(1) = 0
      return

 100  continue
      istat = 3
      ipc(1) = 0


      end
