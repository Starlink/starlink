      subroutine aic_d(gstore,ngauss,data,dens,m,w,aic,funct)
*+
* Name:
*    AIC_D

* Invocation:
*    CALL AIC_D(GSTORE,NGAUSS,DATA,DENS,M,W,AIC,FUNCT)
* Description:
*  To evaluate Akaike's information criterion. Simplified version
*  of GET_AIC, using double precision arrays etc.
*
* Purpose:
*  To evaluate Akaike's information criterion. Simplified version
*  of GET_AIC, using double precision arrays etc.
*
* Arguments:
*      GSTORE(4,NGAUSS,1) = REAL ARRAY (Given)
*        Fit parameters (scaled)
*                              Order for first dimension is:-
*                                base,sigma,height,centre
*      NGAUSS = INTEGER (Given)
*        Number of components
*      DATA(M) = DOUBLE PRECISION ARRAY (Given)
*        X array data
*      DENS(M) = DOUBLE PRECISION ARRAY (Given)
*        Intensity data
*      M = INTEGER (Given)
*        Number of pixels in range
*      W(M) = DOUBLE PRECISION ARRAY (Given)
*        weights
*      FUNCT = REAL (Given)
*        Function to evaluate Gaussian or whatever
*      AIC = REAL (Returned)
*        Akaike's information criterion
*    Subroutines/functions referenced:
*      None

* History:
*   T.N.Wilkins, Cambridge,  9-MAY-1989
*       "           "        17-AUG-1992 Altered to take funct as argument
*-
      implicit none
      integer ngauss
      real gstore(4,ngauss,1),funct
      integer m,nparms
      double precision data(m)
      double precision dens(m)
      double precision w(m)
      real aic
      integer i,j,k
      real diff,tot,xx,ss,xc(4)

*

      ss = 0.0

* Evaluate weighted sum of squares

      xc(1) = 0.0
      do j=1,m
        xx = real(data(j))
        tot = gstore(1,1,1)
        do i=1,ngauss
          do k = 2, 4
            xc(k) = gstore(k,i,1)
          enddo
          tot = tot + funct(xx,xc)
        end do
        diff = tot - real(dens(j))
        ss = ss + real(w(j)) * diff * diff
      end do

* Evaluate AIC

      nparms = ngauss*3+1
      aic = real(m) * log(ss) + real(2 * nparms)
      end
