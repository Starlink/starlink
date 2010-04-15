

      SUBROUTINE PDA_IDTANG(ndp,xd,yd,nt,ipt,nl,ipl,iwl,iwp,wk,
     :                      istat)


c this subroutine performs triangulation.  it divides the x-y
c plane into a number of triangles according to given data
c points in the plane, determines line segments that form the
c border of data area, and determines the triangle numbers
c corresponding to the border line segments.
c at completion, point numbers of the vertices of each triangle
c are listed counter-clockwise.  point numbers of the end points
c of each border line segment are listed counter-clockwise,
c listing order of the line segments being counter-clockwise.

c this subroutine calls the idxchg function.

c the input parameters are
c     ndp = number of data points,
c     xd  = array of dimension ndp containing the
c           x coordinates of the data points,
c     yd  = array of dimension ndp containing the
c           y coordinates of the data points.

c the output parameters are
c     nt  = number of triangles,
c     ipt = integer array of dimension 6*ndp-15, where the
c           point numbers of the vertices of the (it)th
c           triangle are to be stored as the (3*it-2)nd,
c           (3*it-1)st, and (3*it)th elements,
c           it=1,2,...,nt,
c     nl  = number of border line segments,
c     ipl = integer array of dimension 6*ndp, where the
c           point numbers of the end points of the (il)th
c           border line segment and its respective triangle
c           number are to be stored as the (3*il-2)nd,
c           (3*il-1)st, and (3*il)th elements,
c           il=1,2,..., nl.
*     istat = starlink error message.

c the other parameters are
c     iwl = integer array of dimension 18*ndp used
c           internally as a work area,
c     iwp = integer array of dimension ndp used
c           internally as a work area,
c     wk  = array of dimension ndp used internally as a
c           work area.

c declaration statements
      dimension xd(100), yd(100), ipt(585), ipl(600), iwl(1800),
     :          iwp(100), wk(100)
      dimension itf(2)
      data ratio/1.0e-6/, nrep/100/
      integer istat

*   check the inherited error status.
      if ( istat.ne.0 ) return

c preliminary processing
      ndp0 = ndp
      ndpm1 = ndp0 - 1
      if ( ndp0.lt.4 ) then

c error exit
         istat = 5
         nt = 0
         return
      else

c determines the closest pair of data points and their midpoint.
         dsqmn = PDA_DSQF(xd(1),yd(1),xd(2),yd(2))
         ipmn1 = 1
         ipmn2 = 2
         do 50 ip1 = 1, ndpm1
            x1 = xd(ip1)
            y1 = yd(ip1)
            ip1p1 = ip1 + 1
            do 20 ip2 = ip1p1, ndp0
               dsqi = PDA_DSQF(x1,y1,xd(ip2),yd(ip2))
               if ( dsqi.eq.0.0 ) go to 800
               if ( dsqi.lt.dsqmn ) then
                  dsqmn = dsqi
                  ipmn1 = ip1
                  ipmn2 = ip2
               end if
 20         continue
 50      continue
         dsq12 = dsqmn
         xdmp = (xd(ipmn1)+xd(ipmn2))/2.0
         ydmp = (yd(ipmn1)+yd(ipmn2))/2.0

c sorts the other (ndp-2) data points in ascending order of
c distance from the midpoint and stores the sorted data point
c numbers in the iwp array.
         jp1 = 2
         do 100 ip1 = 1, ndp0
            if ( ip1.ne.ipmn1 .and. ip1.ne.ipmn2 ) then
               jp1 = jp1 + 1
               iwp(jp1) = ip1
               wk(jp1) = PDA_DSQF(xdmp,ydmp,xd(ip1),yd(ip1))
            end if
 100     continue
         do 150 jp1 = 3, ndpm1
            dsqmn = wk(jp1)
            jpmn = jp1
            do 120 jp2 = jp1, ndp0
               if ( wk(jp2).lt.dsqmn ) then
                  dsqmn = wk(jp2)
                  jpmn = jp2
               end if
 120        continue
            its = iwp(jp1)
            iwp(jp1) = iwp(jpmn)
            iwp(jpmn) = its
            wk(jpmn) = wk(jp1)
 150     continue

c if necessary, modifies the ordering in such a way that the
c first three data points are not collinear.
         ar = dsq12*ratio
         x1 = xd(ipmn1)
         y1 = yd(ipmn1)
         dx21 = xd(ipmn2) - x1
         dy21 = yd(ipmn2) - y1
         do 200 jp = 3, ndp0
            ip = iwp(jp)
            if ( abs((yd(ip)-y1)*dx21-(xd(ip)-x1)*dy21).gt.ar )
     :           go to 300
 200     continue

         istat = 6
         nt = 0
         go to 99999
      end if
 300  continue
      if ( jp.ne.3 ) then
         jpmx = jp
         jp = jpmx + 1
         do 350 jpc = 4, jpmx
            jp = jp - 1
            iwp(jp) = iwp(jp-1)
 350     continue
         iwp(3) = ip
      end if

c forms the first triangle.  stores point numbers of the ver-
c texes of the triangle in the ipt array, and stores point num-
c bers of the border line segments and the triangle number in
c the ipl array.
      ip1 = ipmn1
      ip2 = ipmn2
      ip3 = iwp(3)
      if (PDA_SIDE2(xd(ip1),yd(ip1),xd(ip2),yd(ip2),xd(ip3),yd(ip3))
     :     .lt.0.0 ) then
         ip1 = ipmn2
         ip2 = ipmn1
      end if
      nt0 = 1
      ntt3 = 3
      ipt(1) = ip1
      ipt(2) = ip2
      ipt(3) = ip3
      nl0 = 3
      nlt3 = 9
      ipl(1) = ip1
      ipl(2) = ip2
      ipl(3) = 1
      ipl(4) = ip2
      ipl(5) = ip3
      ipl(6) = 1
      ipl(7) = ip3
      ipl(8) = ip1
      ipl(9) = 1

c adds the remaining (ndp-3) data points, one by one.
      do 600 jp1 = 4, ndp0
         ip1 = iwp(jp1)
         x1 = xd(ip1)
         y1 = yd(ip1)

c - determines the visible border line segments.
         ip2 = ipl(1)
         jpmn = 1
         dxmn = xd(ip2) - x1
         dymn = yd(ip2) - y1
         dsqmn = dxmn**2 + dymn**2
         armn = dsqmn*ratio
         jpmx = 1
         dxmx = dxmn
         dymx = dymn
         dsqmx = dsqmn
         armx = armn
         do 400 jp2 = 2, nl0
            ip2 = ipl(3*jp2-2)
            dx = xd(ip2) - x1
            dy = yd(ip2) - y1
            ar = dy*dxmn - dx*dymn
            if ( ar.le.armn ) then
               dsqi = dx**2 + dy**2
               if ( ar.lt.(-armn) .or. dsqi.lt.dsqmn ) then
                  jpmn = jp2
                  dxmn = dx
                  dymn = dy
                  dsqmn = dsqi
                  armn = dsqmn*ratio
               end if
            end if
            ar = dy*dxmx - dx*dymx
            if ( ar.ge.(-armx) ) then
               dsqi = dx**2 + dy**2
               if ( ar.gt.armx .or. dsqi.lt.dsqmx ) then
                  jpmx = jp2
                  dxmx = dx
                  dymx = dy
                  dsqmx = dsqi
                  armx = dsqmx*ratio
               end if
            end if
 400     continue
         if ( jpmx.lt.jpmn ) jpmx = jpmx + nl0
         nsh = jpmn - 1
         if ( nsh.gt.0 ) then

c - shifts (rotates) the ipl array to have the invisible border
c - line segments contained in the first part of the ipl array.
            nsht3 = nsh*3
            do 420 jp2t3 = 3, nsht3, 3
               jp3t3 = jp2t3 + nlt3
               ipl(jp3t3-2) = ipl(jp2t3-2)
               ipl(jp3t3-1) = ipl(jp2t3-1)
               ipl(jp3t3) = ipl(jp2t3)
 420        continue
            do 440 jp2t3 = 3, nlt3, 3
               jp3t3 = jp2t3 + nsht3
               ipl(jp2t3-2) = ipl(jp3t3-2)
               ipl(jp2t3-1) = ipl(jp3t3-1)
               ipl(jp2t3) = ipl(jp3t3)
 440        continue
            jpmx = jpmx - nsh
         end if

c - adds triangles to the ipt array, updates border line
c - segments in the ipl array, and sets flags for the border
c - line segments to be reexamined in the iwl array.
         jwl = 0
         do 450 jp2 = jpmx, nl0
            jp2t3 = jp2*3
            ipl1 = ipl(jp2t3-2)
            ipl2 = ipl(jp2t3-1)
            it = ipl(jp2t3)

c - - adds a triangle to the ipt array.
            nt0 = nt0 + 1
            ntt3 = ntt3 + 3
            ipt(ntt3-2) = ipl2
            ipt(ntt3-1) = ipl1
            ipt(ntt3) = ip1

c - - updates border line segments in the ipl array.
            if ( jp2.eq.jpmx ) then
               ipl(jp2t3-1) = ip1
               ipl(jp2t3) = nt0
            end if
            if ( jp2.eq.nl0 ) then
               nln = jpmx + 1
               nlnt3 = nln*3
               ipl(nlnt3-2) = ip1
               ipl(nlnt3-1) = ipl(1)
               ipl(nlnt3) = nt0
            end if

c - - determines the vertex that does not lie on the border
c - - line segments.
            itt3 = it*3
            ipti = ipt(itt3-2)
            if ( ipti.eq.ipl1 .or. ipti.eq.ipl2 ) then
               ipti = ipt(itt3-1)
               if ( ipti.eq.ipl1 .or. ipti.eq.ipl2 ) ipti = ipt(itt3)
            end if

c - - checks if the exchange is necessary.
            if (PDA_IDXCHG(xd,yd,ip1,ipti,ipl1,ipl2,istat)
     :          .ne.0) then

c - - modifies the ipt array when necessary.
               ipt(itt3-2) = ipti
               ipt(itt3-1) = ipl1
               ipt(itt3) = ip1
               ipt(ntt3-1) = ipti
               if ( jp2.eq.jpmx ) ipl(jp2t3) = it
               if ( jp2.eq.nl0 .and. ipl(3).eq.it ) ipl(3) = nt0

c - - sets flags in the iwl array.
               jwl = jwl + 4
               iwl(jwl-3) = ipl1
               iwl(jwl-2) = ipti
               iwl(jwl-1) = ipti
               iwl(jwl) = ipl2
            end if
 450     continue
         nl0 = nln
         nlt3 = nlnt3
         nlf = jwl/2
         if ( nlf.ne.0 ) then

c - improves triangulation.
            ntt3p3 = ntt3 + 3
            do 500 irep = 1, nrep
               do 470 ilf = 1, nlf
                  ilft2 = ilf*2
                  ipl1 = iwl(ilft2-1)
                  ipl2 = iwl(ilft2)

c - - locates in the ipt array two triangles on both sides of
c - - the flagged line segment.
                  ntf = 0
                  do 455 itt3r = 3, ntt3, 3
                     itt3 = ntt3p3 - itt3r
                     ipt1 = ipt(itt3-2)
                     ipt2 = ipt(itt3-1)
                     ipt3 = ipt(itt3)
                     if ( ipl1.eq.ipt1 .or. ipl1.eq.ipt2 .or.
     :                    ipl1.eq.ipt3 ) then
                        if ( ipl2.eq.ipt1 .or. ipl2.eq.ipt2 .or.
     :                       ipl2.eq.ipt3 ) then
                           ntf = ntf + 1
                           itf(ntf) = itt3/3
                           if ( ntf.eq.2 ) go to 460
                        end if
                     end if
 455              continue
                  if ( ntf.lt.2 ) go to 470

c - - determines the vertices of the triangles that do not lie
c - - on the line segment.
 460              continue
                  it1t3 = itf(1)*3
                  ipti1 = ipt(it1t3-2)
                  if ( ipti1.eq.ipl1 .or. ipti1.eq.ipl2 ) then
                     ipti1 = ipt(it1t3-1)
                     if ( ipti1.eq.ipl1 .or. ipti1.eq.ipl2 )
     :                    ipti1 = ipt(it1t3)
                  end if
                  it2t3 = itf(2)*3
                  ipti2 = ipt(it2t3-2)
                  if ( ipti2.eq.ipl1 .or. ipti2.eq.ipl2 ) then
                     ipti2 = ipt(it2t3-1)
                     if ( ipti2.eq.ipl1 .or. ipti2.eq.ipl2 )
     :                    ipti2 = ipt(it2t3)
                  end if

c - - checks if the exchange is necessary.
                  if (PDA_IDXCHG(xd,yd,ipti1,ipti2,ipl1,ipl2,istat)
     :                .ne.0) then

c - - modifies the ipt array when necessary.
                     ipt(it1t3-2) = ipti1
                     ipt(it1t3-1) = ipti2
                     ipt(it1t3) = ipl1
                     ipt(it2t3-2) = ipti2
                     ipt(it2t3-1) = ipti1
                     ipt(it2t3) = ipl2

c - - sets new flags.
                     jwl = jwl + 8
                     iwl(jwl-7) = ipl1
                     iwl(jwl-6) = ipti1
                     iwl(jwl-5) = ipti1
                     iwl(jwl-4) = ipl2
                     iwl(jwl-3) = ipl2
                     iwl(jwl-2) = ipti2
                     iwl(jwl-1) = ipti2
                     iwl(jwl) = ipl1
                     do 462 jlt3 = 3, nlt3, 3
                        iplj1 = ipl(jlt3-2)
                        iplj2 = ipl(jlt3-1)
                        if ( (iplj1.eq.ipl1.and.iplj2.eq.ipti2) .or.
     :                       (iplj2.eq.ipl1.and.iplj1.eq.ipti2) )
     :                       ipl(jlt3) = itf(1)
                        if ( (iplj1.eq.ipl2.and.iplj2.eq.ipti1) .or.
     :                       (iplj2.eq.ipl2.and.iplj1.eq.ipti1) )
     :                       ipl(jlt3) = itf(2)
 462                 continue
                  end if
 470           continue
               nlfc = nlf
               nlf = jwl/2
               if ( nlf.eq.nlfc ) go to 600

c - - resets the iwl array for the next round.
               jwl = 0
               jwl1mn = (nlfc+1)*2
               nlft2 = nlf*2
               do 480 jwl1 = jwl1mn, nlft2, 2
                  jwl = jwl + 2
                  iwl(jwl-1) = iwl(jwl1-1)
                  iwl(jwl) = iwl(jwl1)
 480           continue
               nlf = jwl/2
 500        continue
         end if
 600  continue

c rearranges the ipt array so that the vertices of each triangle
c are listed counter-clockwise.
      do 700 itt3 = 3, ntt3, 3
         ip1 = ipt(itt3-2)
         ip2 = ipt(itt3-1)
         ip3 = ipt(itt3)
         if (PDA_SIDE2(xd(ip1),yd(ip1),xd(ip2),yd(ip2),xd(ip3),yd(ip3))
     :        .lt.0.0 ) then
            ipt(itt3-2) = ip2
            ipt(itt3-1) = ip1
         end if
 700  continue
      nt = nt0
      nl = nl0
      return

 800  continue
      istat = 7
      nt = 0
      return


99999 continue
      end
