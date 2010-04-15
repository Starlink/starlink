

      SUBROUTINE PDA_IDPDRV(ndp,xd,yd,zd,ncp,ipc,pd,istat)

c this subroutine estimates partial derivatives of the first and
c second order at the data points.

c the input parameters are
c     ndp = number of data points,
c     xd,yd,zd = arrays of dimension ndp containing the x,
c           y, and z coordinates of the data points,
c     ncp = number of additional data points used for esti-
c           mating partial derivatives at each data point,
c     ipc = integer array of dimension ncp*ndp containing
c           the point numbers of ncp data points closest to
c           each of the ndp data points.

c the output parameter is
c     pd  = array of dimension 5*ndp, where the estimated
c           zx, zy, zxx, zxy, and zyy values at the data
c           points are to be stored.
*     istat = starlink error message.


c declaration statements
      dimension xd(100), yd(100), zd(100), ipc(400), pd(500)
      real nmx, nmy, nmz, nmxx, nmxy, nmyx, nmyy
      integer istat

*   check the inherited error status.
      if ( istat.ne.0 ) return

c preliminary processing
      ndp0 = ndp
      ncp0 = ncp
      ncpm1 = ncp0 - 1

c estimation of zx and zy
      do 100 ip0 = 1, ndp0
         x0 = xd(ip0)
         y0 = yd(ip0)
         z0 = zd(ip0)
         nmx = 0.0
         nmy = 0.0
         nmz = 0.0
         jipc0 = ncp0*(ip0-1)
         do 50 ic1 = 1, ncpm1
            jipc = jipc0 + ic1
            ipi = ipc(jipc)
            dx1 = xd(ipi) - x0
            dy1 = yd(ipi) - y0
            dz1 = zd(ipi) - z0
            ic2mn = ic1 + 1
            do 20 ic2 = ic2mn, ncp0
               jipc = jipc0 + ic2
               ipi = ipc(jipc)
               dx2 = xd(ipi) - x0
               dy2 = yd(ipi) - y0
               dnmz = dx1*dy2 - dy1*dx2
               if ( dnmz.ne.0.0 ) then
                  dz2 = zd(ipi) - z0
                  dnmx = dy1*dz2 - dz1*dy2
                  dnmy = dz1*dx2 - dx1*dz2
                  if ( dnmz.lt.0.0 ) then
                     dnmx = -dnmx
                     dnmy = -dnmy
                     dnmz = -dnmz
                  end if
                  nmx = nmx + dnmx
                  nmy = nmy + dnmy
                  nmz = nmz + dnmz
               end if
 20         continue
 50      continue
         jpd0 = 5*ip0
         pd(jpd0-4) = -nmx/nmz
         pd(jpd0-3) = -nmy/nmz
 100  continue

c estimation of zxx, zxy, and zyy
      do 200 ip0 = 1, ndp0
         jpd0 = jpd0 + 5
         x0 = xd(ip0)
         jpd0 = 5*ip0
         y0 = yd(ip0)
         zx0 = pd(jpd0-4)
         zy0 = pd(jpd0-3)
         nmxx = 0.0
         nmxy = 0.0
         nmyx = 0.0
         nmyy = 0.0
         nmz = 0.0
         jipc0 = ncp0*(ip0-1)
         do 150 ic1 = 1, ncpm1
            jipc = jipc0 + ic1
            ipi = ipc(jipc)
            dx1 = xd(ipi) - x0
            dy1 = yd(ipi) - y0
            jpd = 5*ipi
            dzx1 = pd(jpd-4) - zx0
            dzy1 = pd(jpd-3) - zy0
            ic2mn = ic1 + 1
            do 120 ic2 = ic2mn, ncp0
               jipc = jipc0 + ic2
               ipi = ipc(jipc)
               dx2 = xd(ipi) - x0
               dy2 = yd(ipi) - y0
               dnmz = dx1*dy2 - dy1*dx2
               if ( dnmz.ne.0.0 ) then
                  jpd = 5*ipi
                  dzx2 = pd(jpd-4) - zx0
                  dzy2 = pd(jpd-3) - zy0
                  dnmxx = dy1*dzx2 - dzx1*dy2
                  dnmxy = dzx1*dx2 - dx1*dzx2
                  dnmyx = dy1*dzy2 - dzy1*dy2
                  dnmyy = dzy1*dx2 - dx1*dzy2
                  if ( dnmz.lt.0.0 ) then
                     dnmxx = -dnmxx
                     dnmxy = -dnmxy
                     dnmyx = -dnmyx
                     dnmyy = -dnmyy
                     dnmz = -dnmz
                  end if
                  nmxx = nmxx + dnmxx
                  nmxy = nmxy + dnmxy
                  nmyx = nmyx + dnmyx
                  nmyy = nmyy + dnmyy
                  nmz = nmz + dnmz
               end if
 120        continue
 150     continue
         pd(jpd0-2) = -nmxx/nmz
         pd(jpd0-1) = -(nmxy+nmyx)/(2.0*nmz)
         pd(jpd0) = -nmyy/nmz
 200  continue
      return
      end
