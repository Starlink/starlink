      subroutine intrpl_s(l,x,y,n,u,v,status)
*+
* Name:
*    INTRPL_S

* Invocation:
*    CALL INTRPL_S(L,X,Y,N,U,V,STATUS)

* Purpose:
*   Cubic spline interpolation using local procedures.
*
* Description:
*   Cubic spline interpolation using local procedures.
*
* Arguments:
*    L = INTEGER (Given)
*        number of input data points
*    X(L) = REAL ARRAY (Given)
*        storing X data points
*    Y(L) = REAL ARRAY (Given)
*        storing Y data points
*    N = INTEGER (Given)
*        number of required points
*    U(N) = REAL ARRAY (Given)
*        stores ordinates of desired
*    STATUS = INTEGER (Given and returned)
*        Error status
*    V(N) = REAL ARRAY (Returned)
*        v on exit stores abscissas

* History:
*    Tidyed up a bit - introduction of IF blocks, TNW 1987
*    Tidied a bit using SPAG, TNW 4/1/89
*    Use of equivalence reduced, TNw 25/1/90
*    single precision version created DJA 11/3/91
*-
      implicit none
      include 'SAE_PAR'
      integer l
      integer n
      real x(l)
      real y(l)
      real u(n)
      integer status
      real v(n)
* ---------------------------------------------------------------
*
* local
*

* output character string for par_wruser

      character chars*64
      real m1,m2,m3,m4,m5,uk,sw,y2
      real y5,t3,a3,a2,a4,t4,y4,x4,y3,x3
      integer imn,imx,j,lm1,lm2,lp1,k,ipv,nstat,i
      equivalence (imn,m1),(imx,m5),(j,sw)

      if(status.ne.SAI__OK) return
*
* preliminary processing
*
      lm1 = l - 1
      lm2 = lm1 - 1
      lp1 = l + 1
      if ( lm2.lt.0 ) then
* error exit
        call par_wruser('L=1 or less.',nstat)
        go to 400
      else if ( n.le.0 ) then
        call par_wruser('N=0 or less',nstat)
        go to 400
      else

* Check X values in ascending order

        do i = 2,l
          if ( x(i-1).eq.x(i) ) then
            go to 100
          else if ( x(i-1).gt.x(i) ) then
            go to 200
          end if
        end do
        ipv = 0
*
* main loop
*
        do k = 1,n
          uk = u(k)
*
* locate desired point
*
          if ( lm2.eq.0 ) then
            i = 2
          else if ( uk.ge.x(l) ) then
            i = lp1
          else if ( uk.lt.x(1) ) then
            i = 1
          else
            imn = 2
            imx = l
 10         continue
            i = (imx+imn)/2
            if ( uk.lt.x(i) ) then
              imx = i
            else
              imn = i + 1
            end if
            if ( imx.gt.imn ) go to 10
            i = imx
          end if
*
* check if i=i(previous)
*
          if ( i.eq.ipv ) go to 60
          ipv = i
*
* find xpy values, estimating if nec.
*
          j = i
          if ( j.eq.1 ) then
            j = 2
          else if ( j.eq.lp1 ) then
            j = l
          end if
          x3 = x(j-1)
          y3 = y(j-1)
          x4 = x(j)
          y4 = y(j)
          a3 = x4 - x3
          m3 = (y4-y3)/a3
          if ( lm2.eq.0 ) then
            m2 = m3
            m4 = m3
            go to 20
          end if
          if ( j.ne.2 ) then
            m1 = x(j-2)
            y2 = y(j-2)
            a2 = x3 - m1
            m2 = (y3-y2)/a2
            if ( j.eq.l ) then
              m4 = m3 + m3 - m2
              go to 20
            end if
          end if
          m5 = x(j+1)
          y5 = y(j+1)
          a4 = m5 - x4
          m4 = (y5-y4)/a4
          if ( j.eq.2 ) m2 = m3 + m3 - m4
 20       continue
          if ( j.gt.3 ) then
            m1 = m1 - x(j-3)
            m1 = (y2-y(j-3))/m1
          else
            m1 = m2 + m2 - m3
          end if
          if ( j.lt.lm1 ) then
            m5 = x(j+2) - m5
            m5 = (y(j+2)-y5)/m5
          else
            m5 = m4 + m4 - m3
          end if
*
* numerical differentiation
*
          if ( i.ne.lp1 ) then
            y2 = abs(m4-m3)
            y5 = abs(m2-m1)
            sw = y2 + y5
            if ( sw.eq.0.0 ) then
              y2 = 0.5
              y5 = 0.5
              sw = 1.0
            end if
            t3 = (y2*m2+y5*m3)/sw
            if ( i.eq.1 ) then
              t4 = t3
              sw = a3 + a4
              t3 = 0.5*(m1+m2-a4*(a3-a4)*(m3-m4)/(sw*sw))
              x3 = x3 - a4
              y3 = y3 - m2*a4
              a3 = a4
              m3 = m2
              go to 40
            end if
          end if
          y5 = abs(m5-m4)
          y2 = abs(m3-m2)
          sw = y5 + y2
          if ( sw.eq.0.0 ) then
            y5 = 0.5
            y2 = 0.5
            sw = 1.0
          end if
          t4 = (y5*m3+y2*m4)/sw
          if ( i.eq.lp1 ) then
            t3 = t4
            sw = a2 + a3
            t4 = 0.5*(m4+m5-a2*(a2-a3)*(m2-m3)/(sw*sw))
            x3 = x4
            y3 = y4
            a3 = a2
            m3 = m4
          end if
*
* determination of coefficients
*
 40       continue
          y2 = (2.0*(m3-t3)+m3-t4)/a3
          y5 = (-m3-m3+t3+t4)/(a3*a3)
*
* computation of the polynomial
*
 60       continue
          uk = uk - x3
          v(k) = y3 + uk*(t3+uk*(y2+uk*y5))
        end do
        return
      end if
 100  continue
      call par_wruser('Identical X values.',nstat)
      go to 300
 200  continue
      call par_wruser('X values out of sequence',nstat)
 300  continue
      write (chars,99001) i,x(i)
99001 format ('   i =',i7,10x,'x(i) =',e12.3)
      call par_wruser(chars,nstat)
 400  continue
      write (chars,99002) l,n
99002 format ('   l =',i7,10x,'n =',i7)
      call par_wruser(chars,nstat)
      status = 2
      end
