      subroutine itplbv2(z,iu,lx,ly,x,y,n,u,v,w)
*+
* Name:
*    ITPLBV2

* Invocation:
*    CALL ITPLBV2(Z,IU,LX,LY,X,Y,N,U,V,W)
*
* Purpose:
*   Bivariate interpolation

* Description:
*   This subroutine interpolates,from the values of the function
*   given at input grid points in an x-y plane,and for a given set
*   of points in the plane,the values of a single valued bivariate
*   function Z=Z(X,Y).
*   The method is based on piece-wise functions composed of a set
*   of bicubic polynomials in x and y.each polynomial set is applicable to
*   a rectangle of the input poinys in the x-y plane.each polynomial
*   is determined locally.

* Arguments:
*    Z(LX,LY) = REAL ARRAY (Given)
*        Stores function at grid
*    IU = INTEGER (Given)
*        Logical unit number of output channel
*    LX = INTEGER (Given)
*        Number of grid points in x coordinate(greater than2)
*    LY = INTEGER (Given)
*        Number of input points in y coordinate(=2)
*    X(LX) = REAL ARRAY (Given)
*        stores x-coordinates of
*                    input grid points(in asscending order)
*    Y(LY) = REAL ARRAY (Given)
*        Stores y coordinate of input grid points (in
*                    ascending order)
*    N = INTEGER (Given)
*        Number of pionts to be interpolated
*    U(N) = REAL ARRAY (Given)
*        Stores x coordinate of desirid poinrs.
*    V(N) = REAL ARRAY (Given)
*        Y coordinate of desired points.
*    W(N) = REAL ARRAY (Returned)
*        Storing interpolated z values.
*
* Internal variables:
*          ZA = DIVIDED DIFFERENCE OF Z WITH RESPECT TO X
*          ZB = DIVIDED DIFFERENCE OF Z WITH RESPECT TO Y
*          ZAB = SECOND DIVIDED DIFFERENCE WITH RESPECT TO X AND Y
*         ZX = PARTIAL DERIVATIVE OF Z WOTH RESPECT TO X
*          ZY = PARTIAL DERIVATIVE OF Z WITH RESPECT TO Y
*        ZXY = SECOND PARTIAL DEROVATIVE OF Z REPSECT TO X ANDY

* History:
*  Disentangled a bit, 19/6/90 TNW/CAVAD-had previously been "SPAGed"
*  ACD: 28/9/00 Initialise to zero the variables which the compiler
*    previously flagged as being accessed before they were set.  Also
*    commented out apparently unused (but included in a commented-out
*    EQUIVALENCE) variable zxy44.
*    ** Health warning ** I am not convinced that this routine is
*    working correctly.
*
*
*-
      implicit none
      integer lx,ly,n
      real z(lx,ly)
      real x(lx),y(ly),u(n),v(n),w(n)
      real za(5,2),zb(2,5),zab(3,3),zx(4,4),zy(4,4),zxy(4,4)
      real z4a3,z4a4,z4a5
      real z3b4,z3b5,z4b2,z4b5,za2b3,za3b3
      real za4b4,zx33,zx43,zx34,zx44,zy43,zy44,zy34,zxy43
      real zxy34,p10,za4b3,z33
C      real zxy44
*      equivalence (z4a3,za(3,2)),(z4a4,za(4,2)),(z4a5,za(5,2)),
*     :            (z3b4,zb(7)),(z3b5,zb(9)),(z4b2,zb(4)),
*     :            (z4b4,zb(8)),(z4b5,zb(10)),(za2b3,zab(4)),
*     :            (za3b3,zab(5)),(za4b3,zab(3,2)),(za2b4,zab(1,3)),
*     :            (za3b4,zab(2,3)),(za4b4,zab(3,3)),(zx33,zx(2,2)),
*     :            (zx43,zx(3,2)),(zx34,zx(2,3)),(zx44,zx(3,3))
*     :            ,(zy43,zy(3,2)),(zy34,zy(2,3)),
*     :             (zy44,zy(3,3)),(zxy43,zxy(3,2)),
*     :             (zxy34,zxy(2,3)),(zxy44,zxy(3,3)),(p10,zx33)
      integer lx0,lxm1,lxm2,lxp1,ly0,lym1,lym2,lyp1,ix,iy,ixpv,iypv
      real uk,dx,vk,dy,a1,a5,b1,b5,a,q0,a2,b,q1,a4,c,q2,b2,d,q3,b4,e
      real a3sq,x4,x5,y2,y4,b3sq,y5,p02,z23,z24,p03,p12,z32,p13,z34
      real p20,z35,p21,z42,p22,z43,p23,z44,p30,z45,p31,z53,p32,z54,p33
      real w2,wy2,w4,w3,wy3,w1,w5,wx2,wx3,x2
*      equivalence (uk,dx),(vk,dy),(a1,a5,b1,b5,zx(2),a,q0),
*     :             (a2,zx(5),b,q1),(a4,zx(8),c,q2),(b2,zy(2),d,q3),
*     :             (b4,zy(14),e),(x2,zx(3),a3sq),(x4,zx(9)),(x5,zx(12)),
*     :             (y2,zx(14)),(y4,zy(3),b3sq),(y5,zx(15),p02),
*     :             (z23,zy(5),p03),(z24,zy(8),p12),(z32,zy(9),p13),
*     :             (z34,zy(12),p20),(z35,zy(15),p21),(z42,zxy(2),p22),
*     :             (z43,zxy(5),p23),(z44,zxy(3),p30),(z45,zxy(8),p31),
*     :             (z53,zxy(9),p32),(z54,zxy(12),p33),(w2,wy2,w4),
*     :             (w3,wy3,w1,w5),(wx2,zxy(14)),(wx3,zxy(15))

      real x3,a3,y3,b3
      real z4b4,za3b4,za2b4,ws,zx3b3,zx4b3,zy3a3
      real zy4a3
      integer iu0,iu,n0,iix,iiy,k,imn,imx,jx,jy,jxml,jym2,jyml
      integer jx1,jy1,jxm2

C
C  The following variables are initialised to zero because the
C  compiler previously warned that they were accessed before they had
C  been set.  Initialising the variables at the start of the routine
C  mimics what most compilers do.  However, I am not convinced that
C  this behaviour is correct.  The variables which are set here were
C  EQUIVALENCEd in EQUIVALENCE statements which have been commented out
C  at some time in the past.  It is possible that the routine is no
C  longer working (and has not been for some years).
C
C  ACD, 28/9/00.

      P10 = 0.0E0
      ZX34 = 0.0E0
      ZX33 = 0.0E0
      ZX44 = 0.0E0
      ZX43 = 0.0E0
      ZY43 = 0.0E0
      ZY44 = 0.0E0
      ZY34 = 0.0E0
      ZXY43 = 0.0E0
      ZXY34 = 0.0E0

*  preliminary processing
*  setting of some input parameters to local variables

      iu0 = iu
      lx0 = lx
      lxm1 = lx0 - 1
      lxm2 = lxm1 - 1
      lxp1 = lx0 + 1
      ly0 = ly
      lym1 = ly0 - 1
      lym2 = lym1 - 1
      lyp1 = ly0 + 1
      n0 = n

*    check for errors

      if ( lxm2.lt.0 ) then

* error exit

        write (iu0,99001)
99001   format (1x/'  ***  lx = 1 or less./')
      else if ( lym2.lt.0 ) then
        write (iu0,99002)
99002   format (1x/'  ***  ly = 1 or less./')
      else if ( n0.lt.1 ) then
        write (iu0,99003)
99003   format (1x/'  ***  n = 0 or less./')
      else
        do iix = 2,lx0
          if ( x(iix-1).eq.x(iix) ) then
            write (iu0,99004)
99004       format (1x,'  ***  identical x values./')
            write (iu0,99006) ix,x(ix)
            go to 700
          else if ( x(iix-1).gt.x(iix) ) then
            write (iu0,99005)
99005       format (1x/'  ***  x values out of sequence./')
            write (iu0,99006) ix,x(ix)
            go to 700
          end if
99006     format ('   ix =',i6,10x,'x(ix) =',e12.3)
        end do
        do iiy = 2,ly0
          if ( y(iiy-1).eq.y(iiy) ) then
            write (iu0,99007)
99007       format (1x/'  ***  identical y values./')
            write (iu0,99009) iy,y(iy)
            go to 700
          else if ( y(iiy-1).gt.y(iiy) ) then
            write (iu0,99008)
99008       format (1x/'  ***  y values out of sequence./')
            write (iu0,99009) iy,y(iy)
99009       format ('   iy =',i6,10x,'y(iy) =',e12.3)
            go to 700
          end if
        end do

* initial setting of previous values of ix and iy

        ixpv = 0
        iypv = 0

*  main loop

        do k = 1,n0
          uk = u(k)
          vk = v(k)

*  routines to locate the desired point
*  to find out the ix value for which
* (u(k).ge.x(ix-1)).and.(u(k).lt.x(ix))

          if ( lxm2.eq.0 ) then
            ix = 2
          else
            if ( uk.ge.x(lx0) ) then
              ix = lxp1
            else if ( uk.lt.x(1) ) then
              ix = 1
            else
              imn = 2
              imx = lx0
 10           continue
              ix = (imn+imx)/2
              if ( uk.ge.x(ix) ) then
                imn = ix + 1
              else
                imx = ix
              end if
              if ( imx.gt.imn ) go to 10
              ix = imx
            end if
          end if

* to find out the iy value for which
* (v(k).ge.y(iy-1)).and.(v(k).lt.y(iy))

          if ( lym2.eq.0 ) then
            iy = 2
          else if ( vk.ge.y(ly0) ) then
            iy = lyp1
          else if ( vk.lt.y(1) ) then
            iy = 1
          else
            imn = 2
            imx = ly0
 30         continue
            iy = (imn+imx)/2
            if ( vk.ge.y(iy) ) then
              imn = iy + 1
            else
              imx = iy
            end if
            if ( imx.gt.imn ) go to 30
            iy = imx
          end if

* to check if the desired point is in the same tectangle
* as the previous point.if yes skip to the computation
* of the polynomial.

          if (.not.( ix.eq.ixpv .and. iy.eq.iypv )) then
            ixpv = ix
            iypv = iy

* routines to pick up necessary x,y,and z values,to
* compute the za,zb,zab, values and to estimate them
* when neccessary

            jx = ix
            if ( jx.eq.1 ) jx = 2
            if ( jx.eq.lxp1 ) jx = lx0
            jy = iy
            if ( jy.eq.1 ) jy = 2
            if ( jy.eq.lyp1 ) jy = ly0
            jxm2 = jx - 2
            jxml = jx - lx0
            jym2 = jy - 2
            jyml = jy - ly0

* the core area, ie in the rectangle that contains
* the desired point

            x3 = x(jx-1)
            x4 = x(jx)
            a3 = 1.0/(x4-x3)
            y3 = y(jy-1)
            y4 = y(jy)
            b3 = 1.0/(y4-y3)
            z33 = z(jx-1,jy-1)
            z43 = z(jx,jy-1)
            z34 = z(jx-1,jy)
            z44 = z(jx,jy)
            za(3,1) = (z43-z33)*a3
            z4a3 = (z44-z34)*a3
            zb(1,3) = (z34-z33)*b3
            zb(2,3) = (z44-z43)*b3
            za3b3 = (zb(2,3)-zb(1,3))*a3

* in the x direction

            if ( lxm2.eq.0 ) then
              za(2,1) = za(3,1)
              za(2,2) = z4a3
              go to 40
            else if ( jxm2.ne.0 ) then
              x2 = x(jx-2)
              a2 = 1.0/(x3-x2)
              z23 = z(jx-2,jy-1)
              z24 = z(jx-2,jy)
              za(2,1) = (z33-z23)*a2
              za(2,2) = (z34-z24)*a2
              if ( jxml.eq.0 ) go to 40
            end if
            x5 = x(jx+1)
            a4 = 1.0/(x5-x4)
            z53 = z(jx+1,jy-1)
            z54 = z(jx+1,jy)
            za(4,1) = (z53-z43)*a4
            z4a4 = (z54-z44)*a4
            if ( jxm2.eq.0 ) then
              za(2,1) = za(3,1) + za(3,1) - za(4,1)
              za(2,2) = z4a3 + z4a3 - z4a4
            end if
            go to 60
 40         continue
            za(4,1) = za(3,1) + za(3,1) - za(2,1)
            z4a4 = z4a3 + z4a3 - za(2,2)
 60         continue
            za2b3 = (za(2,2)-za(2,1))*b3
            za4b3 = (z4a4-za(4,1))*b3
            if ( jx.gt.3 ) then
              a1 = 1.0/(x2-x(jx-3))
              za(1,1) = (z23-z(jx-3,jy-1))*a1
              za(1,2) = (z24-z(jx-3,jy))*a1
            else
              za(1,1) = za(2,1) + za(2,1) - za(3,1)
              za(1,2) = za(2,2) + za(2,2) - z4a3
            end if
            if ( jx.lt.lxm1 ) then
              a5 = 1.0/(x(jx+2)-x5)
              za(5,1) = (z(jx+2,jy-1)-z53)*a5
              z4a5 = (z(jx+2,jy)-z54)*a5
            else
              za(5,1) = za(4,1) + za(4,1) - za(3,1)
              z4a5 = z4a4 + z4a4 - z4a3
            end if

* in the y direction

            if ( lym2.eq.0 ) then
              zb(1,2) = zb(1,3)
              z4b2 = zb(2,3)
              go to 80
            else if ( jym2.ne.0 ) then
              y2 = y(jy-2)
              b2 = 1.0/(y3-y2)
              z32 = z(jx-1,jy-2)
              z42 = z(jx,jy-2)
              zb(1,2) = (z33-z32)*b2
              z4b2 = (z43-z42)*b2
              if ( jyml.eq.0 ) go to 80
            end if
            y5 = y(jy+1)
            b4 = 1.0/(y5-y4)
            z35 = z(jx-1,jy+1)
            z45 = z(jx,jy+1)
            z3b4 = (z35-z34)*b4
            z4b4 = (z45-z44)*b4
            if ( jym2.eq.0 ) then
              zb(1,2) = zb(1,3) + zb(1,3) - z3b4
              z4b2 = zb(2,3) + zb(2,3) - z4b4
            end if
            go to 100
 80         continue
            z3b4 = zb(1,3) + zb(1,3) - zb(1,2)
            z4b4 = zb(2,3) + zb(2,3) - z4b2
 100        continue
            zab(2,1) = (z4b2-zb(1,2))*a3
            za3b4 = (z4b4-z3b4)*a3
            if ( jy.le.3 ) then
              zb(1,1) = zb(1,2) + zb(1,2) - zb(1,3)
              zb(2,1) = z4b2 + z4b2 - zb(2,3)
            else
              b1 = 1.0/(y2-y(jy-3))
              zb(1,1) = (z32-z(jx-1,jy-3))*b1
              zb(2,1) = (z42-z(jx,jy-3))*b1
            end if
            if ( jy.ge.lym1 ) then
              z3b5 = z3b4 + z3b4 - zb(1,3)
              z4b5 = z4b4 + z4b4 - zb(2,3)
            else
              b5 = 1.0/(y(jy+2)-y5)
              z3b5 = (z(jx-1,jy+2)-z35)*b5
              z4b5 = (z(jx,jy+2)-z45)*b5
            end if

* in the diagonal directions

            if ( lxm2.eq.0 ) then
              zab(1,1) = zab(2,1)
              zab(3,1) = zab(2,1)
              za2b4 = za3b4
              za4b4 = za3b4
              go to 180
            else if ( lym2.eq.0 ) then
              zab(1,1) = za2b3
              za2b4 = za2b3
              zab(3,1) = za4b3
              za4b4 = za4b3
              go to 180
            else
              if ( jxml.eq.0 ) go to 120
              if ( jym2.ne.0 ) then
                zab(3,1) = ((z53-z(jx+1,jy-2))*b2-z4b2)*a4
                if ( jyml.eq.0 ) then
                  za4b4 = za4b3 + za4b3 - zab(3,1)
                  go to 140
                end if
              end if
            end if
            za4b4 = ((z(jx+1,jy+1)-z54)*b4-z4b4)*a4
            if ( jym2.eq.0 ) zab(3,1) = za4b3 + za4b3 - za4b4
 140        continue
            if ( jxm2.eq.0 ) then
              zab(1,1) = zab(2,1) + zab(2,1) - zab(3,1)
              za2b4 = za3b4 + za3b4 - za4b4
              go to 180
            end if
 120        continue
            if ( jym2.ne.0 ) then
              zab(1,1) = (zb(1,2)-(z23-z(jx-2,jy-2))*b2)*a2
              if ( jyml.eq.0 ) then
                za2b4 = za2b3 + za2b3 - zab(1,1)
                go to 160
              end if
            end if
            za2b4 = (z3b4-(z(jx-2,jy+1)-z24)*b4)*a2
            if ( jym2.eq.0 ) zab(1,1) = za2b3 + za2b3 - za2b4
 160        continue
            if ( jxml.eq.0 ) then
              zab(3,1) = zab(2,1) + zab(2,1) - zab(1,1)
              za4b4 = za3b4 + za3b4 - za2b4
            end if

* numerical differentiation.....to determine partial
* derivatives zx,zy,and zxy as weighted means of divided
* differances za,zb,and zab,respectively

 180        continue
            do jy = 2,3
              do jx = 2,3
                w2 = abs(za(jx+2,jy-1)-za(jx+1,jy-1))
                w3 = abs(za(jx,jy-1)-za(jx-1,jy-1))
                ws = w2 + w3
                if ( ws.ne.0.0 ) then
                  wx2 = w2/ws
                  wx3 = w3/ws
                else
                  wx2 = 0.5
                  wx3 = 0.5
                end if
                zx(jx,jy) = wx2*za(jx,jy-1) + wx3*za(jx+1,jy-1)
                w2 = abs(zb(jx-1,jy+2)-zb(jx-1,jy+1))
                w3 = abs(zb(jx-1,jy)-zb(jx-1,jy-1))
                ws = w2 + w3
                if ( ws.eq.0.0 ) then
                  wy2 = 0.5
                  wy3 = 0.5
                else
                  wy2 = w2/ws
                  wy3 = w3/ws
                end if
                zy(jx,jy) = wy2*zb(jx-1,jy) + wy3*zb(jx-1,jy+1)
                zxy(jx,jy) = wy2*(wx2*zab(jx-1,jy-1)+wx3*zab(jx,jy-1))
     :                          + wy3*(wx2*zab(jx-1,jy)+wx3*zab(jx,jy))
              end do
            end do

* when (u(k).lt.x(1)).oru(k).gt.x(lx))

            if ( ix.eq.lxp1 ) then
              w4 = a2*(3.0*a3+a2)
              w5 = 2.0*a3*(a3-a2) + w4
              do jy = 2,3
                zx(4,jy) = (w4*za(4,jy-1)+w5*za(5,jy-1))/(w4+w5)
                zy(4,jy) = zy(3,jy) + zy(3,jy) - zy(2,jy)
                zxy(4,jy) = zxy(3,jy) + zxy(3,jy) - zxy(2,jy)
                do jx = 2,3
                  zx(jx,jy) = zx(jx+1,jy)
                  zy(jx,jy) = zy(jx+1,jy)
                  zxy(jx,jy) = zxy(jx+1,jy)
                end do
              end do
              x3 = x4
              z33 = z43
              do jy = 1,5
                zb(1,jy) = zb(2,jy)
              end do
              a3 = a2
              jx = 3
            else
              if ( ix.ne.1 ) go to 200
              w2 = a4*(3.0*a3+a4)
              w1 = 2.0*a3*(a3-a4) + w2
              do jy = 2,3
                zx(1,jy) = (w1*za(1,jy-1)+w2*za(2,jy-1))/(w1+w2)
                zy(1,jy) = zy(2,jy) + zy(2,jy) - zy(3,jy)
                zxy(1,jy) = zxy(2,jy) + zxy(2,jy) - zxy(3,jy)
                do jx1 = 2,3
                  jx = 5 - jx1
                  zx(jx,jy) = zx(jx-1,jy)
                  zy(jx,jy) = zy(jx-1,jy)
                  zxy(jx,jy) = zxy(jx-1,jy)
                end do
              end do
              x3 = x3 - 1.0/a4
              z33 = z33 - za(2,1)/a4
              do jy = 1,5
                zb(2,jy) = zb(1,jy)
              end do
              do jy = 2,4
                zb(1,jy) = zb(1,jy) - zab(1,jy-1)/a4
              end do
              a3 = a4
              jx = 1
            end if
            za(3,1) = za(jx+1,1)
            do jy = 1,3
              zab(2,jy) = zab(jx,jy)
            end do

* when (v(k).lt.y(1)).or.(v(k).gt.y(ly))

 200        continue
            if ( iy.eq.lyp1 ) then
              w4 = b2*(3.0*b3+b2)
              w5 = 2.0*b3*(b3-b2) + w4
              do jx = 2,3
                if ( jx.ne.3 .or. ix.ne.lxp1 ) then
                  if ( jx.ne.2 .or. ix.ne.1 ) then
                    zy(jx,4) = (w4*zb(jx-1,4)+w5*zb(jx-1,5))/(w4+w5)
                    zx(jx,4) = zx(jx,3) + zx(jx,3) - zx(jx,2)
                    zxy(jx,4) = zxy(jx,3) + zxy(jx,3) - zxy(jx,2)
                  end if
                end if
                do jy = 2,3
                  zy(jx,jy) = zy(jx,jy+1)
                  zx(jx,jy) = zx(jx,jy+1)
                  zxy(jx,jy) = zxy(jx,jy+1)
                end do
              end do
              y3 = y4
              z33 = z33 + zb(1,3)/b3
              za(3,1) = za(3,1) + za3b3/b3
              zb(1,3) = z3b4
              za3b3 = za3b4
              b3 = b2
            else
              if ( iy.ne.1 ) go to 220
              w2 = b4*(3.0*b3+b4)
              w1 = 2.0*b3*(b3-b4) + w2
              do jx = 2,3
                if ( jx.ne.3 .or. ix.ne.lxp1 ) then
                  if ( jx.ne.2 .or. ix.ne.1 ) then
                    zy(jx,1) = (w1*zb(jx-1,1)+w2*zb(jx-1,2))/(w1+w2)
                    zx(jx,1) = zx(jx,2) + zx(jx,2) - zx(jx,3)
                    zxy(jx,1) = zxy(jx,2) + zxy(jx,2) - zxy(jx,3)
                  end if
                end if
                do jy1 = 2,3
                  jy = 5 - jy1
                  zy(jx,jy) = zy(jx,jy-1)
                  zx(jx,jy) = zx(jx,jy-1)
                  zxy(jx,jy) = zxy(jx,jy-1)
                end do
              end do
              y3 = y3 - 1.0/b4
              z33 = z33 - zb(1,2)/b4
              za(3,1) = za(3,1) - zab(2,1)/b4
              zb(1,3) = zb(1,2)
              za3b3 = zab(2,1)
              b3 = b4
            end if
            if ( .not.(ix.ne.1 .and. ix.ne.lxp1) ) then
              jx = ix/lxp1 + 2
              jx1 = 5 - jx
              jy = iy/lyp1 + 2
              jy1 = 5 - jy
              zx(jx,jy) = zx(jx1,jy) + zx(jx,jy1) - zx(jx1,jy1)
              zy(jx,jy) = zy(jx1,jy) + zy(jx,jy1) - zy(jx1,jy1)
              zxy(jx,jy) = zxy(jx1,jy) + zxy(jx,jy1) - zxy(jx1,jy1)

*     determination of coefficients of polynomial

            end if
 220        continue
            zx3b3 = (zx34-zx33)*b3
            zx4b3 = (zx44-zx43)*b3
            zy3a3 = (zy43-zy(2,2))*a3
            zy4a3 = (zy44-zy34)*a3
            a = za3b3 - zx3b3 - zy3a3 + zxy(2,2)
            b = zx4b3 - zx3b3 - zxy43 + zxy(2,2)
            c = zy4a3 - zy3a3 - zxy34 + zxy(2,2)
            d = zxy(3,3) - zxy43 - zxy34 + zxy(2,2)
            e = a + a - b - c
            a3sq = a3*a3
            b3sq = b3*b3
            p02 = (2.0*(zb(1,3)-zy(2,2))+zb(1,3)-zy34)*b3
            p03 = (-2.0*zb(1,3)+zy34+zy(2,2))*b3sq
            p12 = (2.0*(zx3b3-zxy(2,2))+zx3b3-zxy34)*b3
            p13 = (-2.0*zx3b3+zxy34+zxy(2,2))*b3sq
            p20 = (2.0*(za(3,1)-zx33)+za(3,1)-zx43)*a3
            p21 = (2.0*(zy3a3-zxy(2,2))+zy3a3-zxy43)*a3
            p22 = (3.0*(a+e)+d)*a3*b3
            p23 = (-3.0*e-b-d)*a3*b3sq
            p30 = (-2.0*za(3,1)+zx43+zx33)*a3sq
            p31 = (-2.0*zy3a3+zxy43+zxy(2,2))*a3sq
            p32 = (-3.0*e-c-d)*b3*a3sq
            p33 = (d+e+e)*a3sq*b3sq

* computation of polynomial

          end if
          dy = vk - y3
          q0 = z33 + dy*(zy(2,2)+dy*(p02+dy*p03))
          q1 = p10 + dy*(zxy(2,2)+dy*(p12+dy*p13))
          q2 = p20 + dy*(p21+dy*(p22+dy*p23))
          q3 = p30 + dy*(p31+dy*(p32+dy*p33))
          dx = uk - x3
          w(k) = q0 + dx*(q1+dx*(q2+dx*q3))
        end do

* normal exit

        return
      end if
 700  continue
      write (iu0,99010) lx0,ly0,n0
99010 format ('   lx =',i6,10x,'ly =',i6,10x,'n =',
     :        i7/' error detected in routine    itplbv2')
      end
