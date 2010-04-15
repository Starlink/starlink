

      SUBROUTINE PDA_IDLCTN(ndp,xd,yd,nt,ipt,nl,ipl,xii,yii,iti,
     :                      iwk,wk,istat)

c this subroutine locates a point, i.e., determines to what tri-
c angle a given point (xii,yii) belongs.  when the given point
c does not lie inside the data area, this subroutine determines
c the border line segment when the point lies in an outside
c rectangular area, and two border line segments when the point
c lies in an outside triangular area.

c the input parameters are
c     ndp = number of data points,
c     xd,yd = arrays of dimension ndp containing the x and y
c           coordinates of the data points,
c     nt  = number of triangles,
c     ipt = integer array of dimension 3*nt containing the
c           point numbers of the vertices of the triangles,
c     nl  = number of border line segments,
c     ipl = integer array of dimension 3*nl containing the
c           point numbers of the end points of the border
c           line segments and their respective triangle
c           numbers,
c     xii,yii = x and y coordinates of the point to be
c           located.

c the output parameter is
c     iti = triangle number, when the point is inside the
c           data area, or
c           two border line segment numbers, il1 and il2,
c           coded to il1*(nt+nl)+il2, when the point is
c           outside the data area.
*     istat = starlink error message.

c the other parameters are
c     iwk = integer array of dimension 18*ndp used inter-
c           nally as a work area,
c     wk  = array of dimension 8*ndp used internally as a
c           work area.

c declaration statements
      dimension xd(100), yd(100), ipt(585), ipl(300), iwk(1800), wk(800)
      dimension ntsc(9), idsc(9)
      common /idlc  / nit
      integer istat
      save itipv,ntsc

*   check the inherited error status.
      if ( istat.ne.0 ) return

c preliminary processing
      xs1 = 0.0
      xs2 = 0.0
      ys1 = 0.0
      ys2 = 0.0
      ndp0 = ndp
      nt0 = nt
      nl0 = nl
      ntl = nt0 + nl0
      x0 = xii
      y0 = yii

c processing for a new set of data points
      if ( nit.ne.0 ) then

c checks if in the same triangle as previous.
         it0 = itipv
         if ( it0.gt.nt0 ) then

c checks if on the same border line segment.
            il1 = it0/ntl
            il2 = it0 - il1*ntl
            il1t3 = il1*3
            ip1 = ipl(il1t3-2)
            x1 = xd(ip1)
            y1 = yd(ip1)
            ip2 = ipl(il1t3-1)
            x2 = xd(ip2)
            y2 = yd(ip2)
            if ( il2.ne.il1 ) then

c checks if between the same two border line segments.
               if (PDA_SPDT(x1,y1,x2,y2,x0,y0).le.0.0) then
                  ip3 = ipl(3*il2-1)
                  x3 = xd(ip3)
                  y3 = yd(ip3)
                  if (PDA_SPDT(x3,y3,x2,y2,x0,y0).le.0.0) go to 500
               end if
            else if (PDA_SPDT(x1,y1,x2,y2,x0,y0).ge.0.0) then
               if (PDA_SPDT(x2,y2,x1,y1,x0,y0).ge.0.0) then
                  if (PDA_SIDE(x1,y1,x2,y2,x0,y0).le.0.0) go to 500
               end if
            end if
         else
            it0t3 = it0*3
            ip1 = ipt(it0t3-2)
            x1 = xd(ip1)
            y1 = yd(ip1)
            ip2 = ipt(it0t3-1)
            x2 = xd(ip2)
            y2 = yd(ip2)
            if (PDA_SIDE(x1,y1,x2,y2,x0,y0).ge.0.0) then
               ip3 = ipt(it0t3)
               x3 = xd(ip3)
               y3 = yd(ip3)
               if (PDA_SIDE(x2,y2,x3,y3,x0,y0).ge.0.0) then
                  if (PDA_SIDE(x3,y3,x1,y1,x0,y0).ge.0.0) go to 500
               end if
            end if
         end if
      else
         nit = 1

c - divides the x-y plane into nine rectangular sections.
         xmn = xd(1)
         xmx = xmn
         ymn = yd(1)
         ymx = ymn
         do 50 idp = 2, ndp0
            xi = xd(idp)
            yi = yd(idp)
            xmn = amin1(xi, xmn)
            xmx = amax1(xi, xmx)
            ymn = amin1(yi, ymn)
            ymx = amax1(yi, ymx)
 50      continue
         xs1 = (xmn+xmn+xmx)/3.0
         xs2 = (xmn+xmx+xmx)/3.0
         ys1 = (ymn+ymn+ymx)/3.0
         ys2 = (ymn+ymx+ymx)/3.0

c - determines and stores in the iwk array triangle numbers of
c - the triangles associated with each of the nine sections.
         do 100 isc = 1, 9
            ntsc(isc) = 0
            idsc(isc) = 0
 100     continue
         it0t3 = 0
         jwk = 0
         do 150 it0 = 1, nt0
            it0t3 = it0t3 + 3
            i1 = ipt(it0t3-2)
            i2 = ipt(it0t3-1)
            i3 = ipt(it0t3)
            xmn = amin1(xd(i1), xd(i2), xd(i3))
            xmx = amax1(xd(i1), xd(i2), xd(i3))
            ymn = amin1(yd(i1), yd(i2), yd(i3))
            ymx = amax1(yd(i1), yd(i2), yd(i3))
            if ( ymn.le.ys1 ) then
               if ( xmn.le.xs1 ) idsc(1) = 1
               if ( xmx.ge.xs1 .and. xmn.le.xs2 ) idsc(2) = 1
               if ( xmx.ge.xs2 ) idsc(3) = 1
            end if
            if ( ymx.ge.ys1 .and. ymn.le.ys2 ) then
               if ( xmn.le.xs1 ) idsc(4) = 1
               if ( xmx.ge.xs1 .and. xmn.le.xs2 ) idsc(5) = 1
               if ( xmx.ge.xs2 ) idsc(6) = 1
            end if
            if ( ymx.ge.ys2 ) then
               if ( xmn.le.xs1 ) idsc(7) = 1
               if ( xmx.ge.xs1 .and. xmn.le.xs2 ) idsc(8) = 1
               if ( xmx.ge.xs2 ) idsc(9) = 1
            end if
            do 120 isc = 1, 9
               if ( idsc(isc).ne.0 ) then
                  jiwk = 9*ntsc(isc) + isc
                  iwk(jiwk) = it0
                  ntsc(isc) = ntsc(isc) + 1
                  idsc(isc) = 0
               end if
 120        continue

c - stores in the wk array the minimum and maximum of the x and
c - y coordinate values for each of the triangle.
            jwk = jwk + 4
            wk(jwk-3) = xmn
            wk(jwk-2) = xmx
            wk(jwk-1) = ymn
            wk(jwk) = ymx
 150     continue
      end if

c locates inside the data area.
c - determines the section in which the point in question lies.
      isc = 1
      if ( x0.ge.xs1 ) isc = isc + 1
      if ( x0.ge.xs2 ) isc = isc + 1
      if ( y0.ge.ys1 ) isc = isc + 3
      if ( y0.ge.ys2 ) isc = isc + 3

c - searches through the triangles associated with the section.
      ntsci = ntsc(isc)
      if ( ntsci.gt.0 ) then
         jiwk = -9 + isc
         do 200 itsc = 1, ntsci
            jiwk = jiwk + 9
            it0 = iwk(jiwk)
            jwk = it0*4
            if ( x0.ge.wk(jwk-3) ) then
               if ( x0.le.wk(jwk-2) ) then
                  if ( y0.ge.wk(jwk-1) ) then
                     if ( y0.le.wk(jwk) ) then
                        it0t3 = it0*3
                        ip1 = ipt(it0t3-2)
                        x1 = xd(ip1)
                        y1 = yd(ip1)
                        ip2 = ipt(it0t3-1)
                        x2 = xd(ip2)
                        y2 = yd(ip2)
                        if (PDA_SIDE(x1,y1,x2,y2,x0,y0).ge.0.0) then
                           ip3 = ipt(it0t3)
                           x3 = xd(ip3)
                           y3 = yd(ip3)
                           if (PDA_SIDE(x2,y2,x3,y3,x0,y0).ge.0.0) then
                              if (PDA_SIDE(x3,y3,x1,y1,x0,y0).ge.0.0)
     :                             go to 500
                           end if
                        end if
                     end if
                  end if
               end if
            end if
 200     continue
      end if

c locates outside the data area.
      do 300 il1 = 1, nl0
         il1t3 = il1*3
         ip1 = ipl(il1t3-2)
         x1 = xd(ip1)
         y1 = yd(ip1)
         ip2 = ipl(il1t3-1)
         x2 = xd(ip2)
         y2 = yd(ip2)
         if (PDA_SPDT(x2,y2,x1,y1,x0,y0).ge.0.0) then
            if (PDA_SPDT(x1,y1,x2,y2,x0,y0).lt.0.0) then
               il2 = mod(il1, nl0) + 1
               ip3 = ipl(3*il2-1)
               x3 = xd(ip3)
               y3 = yd(ip3)
               if (PDA_SPDT(x3,y3,x2,y2,x0,y0).le.0.0) go to 400
            else if (PDA_SIDE(x1,y1,x2,y2,x0,y0).le.0.0) then
               il2 = il1
               go to 400
            end if
         end if
 300  continue
      it0 = 1
      go to 500
 400  continue
      it0 = il1*ntl + il2

c normal exit
 500  continue
      iti = it0
      itipv = it0
      return

      end
