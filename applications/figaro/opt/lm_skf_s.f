      subroutine lm_skf_s(iflag,m,n,xc,rc,fjac,ldfjac,dens,data,weight)
*+
* Name:
*    LM_SKF_S

* Invocation:
*    CALL LM_SKF_S(IFLAG,M,N,XC,RC,FJAC,LDFJAC,DENS,DATA,WEIGHT)
*
* Description:
*    To calculate residuals or Jacobian for a SKEW fit model.
*
* Purpose:
*    To calculate residuals or Jacobian for a SKEW fit model.
*
* Arguments:
*      IFLAG = INTEGER (Given)
*        Flag to determine action of routine, 1 => fvec,
*                        2 => fjac
*      M = INTEGER (Given)
*        Number of data points
*      N = INTEGER (Given)
*        Number of variables
*      XC(N) = DOUBLE PRECISION ARRAY (Given)
*        Function parameters
*      LDFJAC = INTEGER (Given)
*        Dimension of fjac
*      DENS(M) = DOUBLE PRECISION ARRAY (Given)
*        Y data
*      DATA(M) = DOUBLE PRECISION ARRAY (Given)
*        X data
*      WEIGHT(M) = DOUBLE PRECISION ARRAY (Given)
*        Weights on data
*      RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        Residuals
*      FJAC(LDFJAC,N) = DOUBLE PRECISION ARRAY (Returned)
*        Jacobian
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge,  14-OCT-1991 based on lm_mgf_s and sk_fun
* History:
*-
      implicit none
      integer m
      integer n
      double precision xc(n)
      double precision rc(m)
      integer ldfjac,iflag
      double precision fjac(ldfjac,n)
      double precision data(m),dens(m),weight(m)

*

      integer i

* local
      double precision fexp,zscore,bra,skeww
      double precision A
      double precision d
      double precision f
      double precision c
      double precision e
*

* 2.0d0*dlog(2.0d0)

      parameter (A = 1.386294361d0)
*
*     calculates residual between model and data for skewed gaussian

* local

      double precision A0
      double precision rmeanr
      integer BASE, WIDTH, HEIGHT, CENTRE, SKEW
      parameter (BASE = 1, WIDTH = 2, HEIGHT = 3, CENTRE = 4,
     :     SKEW = 5)

* dlog(2.0d0)

      parameter ( A0 = 0.6931471806d0 )
*
* compute residuals
*

      if(iflag.eq.1) then
        do  i   =   1,m
          zscore = ( data(i)- xc(CENTRE) )/ xc(WIDTH)
          rmeanr = 2.0d0 * xc(SKEW) * zscore
          skeww  = rmeanr
*
*   Test to see if RMEANR<=-1
*
          if (rmeanr.gt.-1.0d0) then
            bra  = (log((1.0d0+rmeanr)))/(xc(SKEW))
            fexp = exp(-A0*(bra*bra))
          else
            bra  = 0.0d0
            fexp = 0.0d0
          end if
          rc(i)= ( xc(HEIGHT) * fexp - dens(i) + xc(BASE) ) * weight(i)
        enddo

      else

        do  i   =   1,m
          zscore = ( data(i)- xc(CENTRE) )/ xc(WIDTH)
          rmeanr = 2.0d0 * xc(SKEW) * zscore
          skeww  = rmeanr
*
*   Test to see if RMEANR<=-1
*
          if (rmeanr.gt.-1.0d0) then
            bra  = (log((1.0d0+rmeanr)))/(xc(SKEW))
            fexp = exp(-A0*(bra*bra))
          else
            bra  = 0.0d0
            fexp = 0.0d0
          end if
*   base

          fjac(i,1) = weight(i)

*   height

          fjac(i,3) = fexp * weight(i)
          d        = 1.0d0+skeww
*
*  ** Carry out test to see if d=0
*
          if  (d.gt.0.0d0) then
            f        = A*xc(HEIGHT)*fexp*bra * weight(i)
            c        = 2.0d0*f/(d*xc(WIDTH))
            e        = bra-2.0d0*zscore/d

*     mean

            fjac(i,4) = c

*     fwhm

            fjac(i,2) = zscore*c

*     skew

            fjac(i,5) = f*e/xc(SKEW)
          else
            fjac(i,2) = 0.0d0
            fjac(i,3) = 0.0d0
            fjac(i,4) = 0.0d0
            fjac(i,5) = 0.0d0
          end if
        end do
      endif
      end
