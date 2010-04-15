

      SUBROUTINE PDA_IDGRID(xd,yd,nt,ipt,nl,ipl,nxi,nyi,xi,yi,ngp,
     :                      igp,istat)

c this subroutine organizes grid points for surface fitting by
c sorting them in ascending order of triangle numbers and of the
c border line segment number.

c the input parameters are
c     xd,yd = arrays of dimension ndp containing the x and y
c           coordinates of the data points, where ndp is the
c           number of the data points,
c     nt  = number of triangles,
c     ipt = integer array of dimension 3*nt containing the
c           point numbers of the vertices of the triangles,
c     nl  = number of border line segments,
c     ipl = integer array of dimension 3*nl containing the
c           point numbers of the end points of the border
c           line segments and their respective triangle
c           numbers,
c     nxi = number of grid points in the x coordinate,
c     nyi = number of grid points in the y coordinate,
c     xi,yi = arrays of dimension nxi and nyi containing
c           the x and y coordinates of the grid points,
c           respectively.

c the output parameters are
c     ngp = integer array of dimension 2*(nt+2*nl) where the
c           number of grid points that belong to each of the
c           triangles or of the border line segments are to
c           be stored,
c     igp = integer array of dimension nxi*nyi where the
c           grid point numbers are to be stored in ascending
c           order of the triangle number and the border line
c           segment number.
*     istat = starlink error message.


c declaration statements
      dimension xd(100), yd(100), ipt(585), ipl(300), xi(101), yi(101),
     :          ngp(800), igp(10201)
      integer istat

*   check the inherited error status.
      if ( istat.ne.0 ) return

c preliminary processing
      iximn = 0
      nt0 = nt
      nl0 = nl
      nxi0 = nxi
      nyi0 = nyi
      nxinyi = nxi0*nyi0
      ximn = amin1(xi(1), xi(nxi0))
      ximx = amax1(xi(1), xi(nxi0))
      yimn = amin1(yi(1), yi(nyi0))
      yimx = amax1(yi(1), yi(nyi0))

c determines grid points inside the data area.
      jngp0 = 0
      jngp1 = 2*(nt0+2*nl0) + 1
      jigp0 = 0
      jigp1 = nxinyi + 1
      do 300 it0 = 1, nt0
         ngp0 = 0
         ngp1 = 0
         it0t3 = it0*3
         ip1 = ipt(it0t3-2)
         ip2 = ipt(it0t3-1)
         ip3 = ipt(it0t3)
         x1 = xd(ip1)
         y1 = yd(ip1)
         x2 = xd(ip2)
         y2 = yd(ip2)
         x3 = xd(ip3)
         y3 = yd(ip3)
         xmn = amin1(x1, x2, x3)
         xmx = amax1(x1, x2, x3)
         ymn = amin1(y1, y2, y3)
         ymx = amax1(y1, y2, y3)
         insd = 0
         do 50 ixi = 1, nxi0
            if ( xi(ixi).ge.xmn .and. xi(ixi).le.xmx ) then
               if ( insd.ne.1 ) then
                  insd = 1
                  iximn = ixi
               end if
            else if ( insd.ne.0 ) then
               iximx = ixi - 1
               go to 100
            end if
 50      continue
         if ( insd.eq.0 ) go to 200
         iximx = nxi0
 100     continue
         do 150 iyi = 1, nyi0
            yii = yi(iyi)
            if ( yii.ge.ymn .and. yii.le.ymx ) then
               do 110 ixi = iximn, iximx
                  xii = xi(ixi)
                  l = 0
                  if (PDA_SIDE(x1,y1,x2,y2,xii,yii).lt.0) go to 110
                  if (PDA_SIDE(x1,y1,x2,y2,xii,yii).eq.0) l = 1
                  if (PDA_SIDE(x2,y2,x3,y3,xii,yii).lt.0) go to 110
                  if (PDA_SIDE(x2,y2,x3,y3,xii,yii).eq.0) l = 1
                  if (PDA_SIDE(x3,y3,x1,y1,xii,yii).lt.0) go to 110
                  if (PDA_SIDE(x3,y3,x1,y1,xii,yii).eq.0) l = 1
                  izi = nxi0*(iyi-1) + ixi
                  if ( l.eq.1 ) then
                     if ( jigp1.le.nxinyi ) then
                        do 102 jigp1i = jigp1, nxinyi
                           if ( izi.eq.igp(jigp1i) ) go to 110
 102                    continue
                     end if
                     ngp1 = ngp1 + 1
                     jigp1 = jigp1 - 1
                     igp(jigp1) = izi
                  else
                     ngp0 = ngp0 + 1
                     jigp0 = jigp0 + 1
                     igp(jigp0) = izi
                  end if
 110           continue
            end if
 150     continue
 200     continue
         jngp0 = jngp0 + 1
         ngp(jngp0) = ngp0
         jngp1 = jngp1 - 1
         ngp(jngp1) = ngp1
 300  continue

c determines grid points outside the data area.
c - in semi-infinite rectangular area.
      do 800 il0 = 1, nl0
         ngp0 = 0
         ngp1 = 0
         il0t3 = il0*3
         ip1 = ipl(il0t3-2)
         ip2 = ipl(il0t3-1)
         x1 = xd(ip1)
         y1 = yd(ip1)
         x2 = xd(ip2)
         y2 = yd(ip2)
         xmn = ximn
         xmx = ximx
         ymn = yimn
         ymx = yimx
         if ( y2.ge.y1 ) xmn = amin1(x1, x2)
         if ( y2.le.y1 ) xmx = amax1(x1, x2)
         if ( x2.le.x1 ) ymn = amin1(y1, y2)
         if ( x2.ge.x1 ) ymx = amax1(y1, y2)
         insd = 0
         do 350 ixi = 1, nxi0
            if ( xi(ixi).ge.xmn .and. xi(ixi).le.xmx ) then
               if ( insd.ne.1 ) then
                  insd = 1
                  iximn = ixi
               end if
            else if ( insd.ne.0 ) then
               iximx = ixi - 1
               go to 400
            end if
 350     continue
         if ( insd.eq.0 ) go to 500
         iximx = nxi0
 400     continue
         do 450 iyi = 1, nyi0
            yii = yi(iyi)
            if ( yii.ge.ymn .and. yii.le.ymx ) then
               do 410 ixi = iximn, iximx
                  xii = xi(ixi)
                  l = 0
                  if (PDA_SIDE(x1,y1,x2,y2,xii,yii).lt.0) then
                  else if (PDA_SIDE(x1,y1,x2,y2,xii,yii).eq.0) then
                     l = 1
                  else
                     go to 410
                  end if
                  if (PDA_SPDT(x2,y2,x1,y1,xii,yii).lt.0) go to 410
                  if (PDA_SPDT(x2,y2,x1,y1,xii,yii).eq.0) l = 1
                  if (PDA_SPDT(x1,y1,x2,y2,xii,yii).lt.0) go to 410
                  if (PDA_SPDT(x1,y1,x2,y2,xii,yii).eq.0) l = 1
                  izi = nxi0*(iyi-1) + ixi
                  if ( l.eq.1 ) then
                     if ( jigp1.le.nxinyi ) then
                        do 402 jigp1i = jigp1, nxinyi
                           if ( izi.eq.igp(jigp1i) ) go to 410
 402                    continue
                     end if
                     ngp1 = ngp1 + 1
                     jigp1 = jigp1 - 1
                     igp(jigp1) = izi
                  else
                     ngp0 = ngp0 + 1
                     jigp0 = jigp0 + 1
                     igp(jigp0) = izi
                  end if
 410           continue
            end if
 450     continue
 500     continue
         jngp0 = jngp0 + 1
         ngp(jngp0) = ngp0
         jngp1 = jngp1 - 1
         ngp(jngp1) = ngp1

c - in semi-infinite triangular area.
         ngp0 = 0
         ngp1 = 0
         ilp1 = mod(il0, nl0) + 1
         ilp1t3 = ilp1*3
         ip3 = ipl(ilp1t3-1)
         x3 = xd(ip3)
         y3 = yd(ip3)
         xmn = ximn
         xmx = ximx
         ymn = yimn
         ymx = yimx
         if ( y3.ge.y2 .and. y2.ge.y1 ) xmn = x2
         if ( y3.le.y2 .and. y2.le.y1 ) xmx = x2
         if ( x3.le.x2 .and. x2.le.x1 ) ymn = y2
         if ( x3.ge.x2 .and. x2.ge.x1 ) ymx = y2
         insd = 0
         do 550 ixi = 1, nxi0
            if ( xi(ixi).ge.xmn .and. xi(ixi).le.xmx ) then
               if ( insd.ne.1 ) then
                  insd = 1
                  iximn = ixi
               end if
            else if ( insd.ne.0 ) then
               iximx = ixi - 1
               go to 600
            end if
 550     continue
         if ( insd.eq.0 ) go to 700
         iximx = nxi0
 600     continue
         do 650 iyi = 1, nyi0
            yii = yi(iyi)
            if ( yii.ge.ymn .and. yii.le.ymx ) then
               do 610 ixi = iximn, iximx
                  xii = xi(ixi)
                  l = 0
                  if (PDA_SPDT(x1,y1,x2,y2,xii,yii).lt.0) then
                  else if (PDA_SPDT(x1,y1,x2,y2,xii,yii).eq.0) then
                     l = 1
                  else
                     go to 610
                  end if
                  if (PDA_SPDT(x3,y3,x2,y2,xii,yii).lt.0) then
                  else if (PDA_SPDT(x3,y3,x2,y2,xii,yii).eq.0) then
                     l = 1
                  else
                     go to 610
                  end if
                  izi = nxi0*(iyi-1) + ixi
                  if ( l.eq.1 ) then
                     if ( jigp1.le.nxinyi ) then
                        do 602 jigp1i = jigp1, nxinyi
                           if ( izi.eq.igp(jigp1i) ) go to 610
 602                    continue
                     end if
                     ngp1 = ngp1 + 1
                     jigp1 = jigp1 - 1
                     igp(jigp1) = izi
                  else
                     ngp0 = ngp0 + 1
                     jigp0 = jigp0 + 1
                     igp(jigp0) = izi
                  end if
 610           continue
            end if
 650     continue
 700     continue
         jngp0 = jngp0 + 1
         ngp(jngp0) = ngp0
         jngp1 = jngp1 - 1
         ngp(jngp1) = ngp1
 800  continue
      return
      end
