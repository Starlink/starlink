      subroutine powerlaw_fit(m,n,xc,rc,ajc,ajtjc,gc
     :     ,max_parms,normalization_wave,data,dens,weight)
*+
* Name:
*    POWERLAW_FIT

* Invocation:
*    CALL POWERLAW_FIT(M,N,XC,RC,AJC,AJTJC,GC
*          ,MAX_PARMS,NORMALIZATION_WAVE,DATA,DENS,WEIGHT)

*
* Purpose:
*       Calculate residual and derivatives between model and data

* Description:
*   Calculate derivatives for a POWER LAW component in the
*   continuum.

* Arguments:
*       DATA(M) = DOUBLE PRECISION ARRAY (Given)
*        channel numbers etc. scaled to range 0-1
*       DENS(M) = DOUBLE PRECISION ARRAY (Given)
*        counts scaled to range 0-1
*       XC(1) = DOUBLE PRECISION ARRAY (Given)
*        Spectral Index
*       XC(2) = DOUBLE PRECISION ARRAY (Given)
*        Amplitude
*       XC(3) = DOUBLE PRECISION ARRAY (Given)
*        Centre
*       RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        residuals
*       ajtjc
*       ajc          : derivatives
*
*
*-
      implicit none

* import

      integer n
      integer m
      double precision xc(n)
      double precision data(m),dens(m),weight(m)

* export

* residuals

      double precision rc(m)

* jacobian matrix at the point xc

      double precision ajc(m,n)
      integer max_parms
      double precision ajtjc(max_parms,max_parms)
      double precision gc(max_parms)
      double precision normalization_wave
* ------------------------------------------------------------
* local

      double precision sum,fexp,zscore
      integer i
      integer j
      integer INDEX
      integer AMPLITUDE
      integer CENTRE
      parameter (CENTRE = 3)
      parameter (AMPLITUDE = 2)
      parameter (INDEX = 1)
      integer k
*
* compute derivatives
*
* Define the normaliation_wavlength to be at XMAX
      normalization_wave = data(m)

      do i = 1, m
        zscore = ( data(i) - xc(CENTRE) ) / normalization_wave
        fexp   = zscore ** ( -xc(INDEX) )
        rc(i)  = ( xc(AMPLITUDE) * fexp - dens(i) ) * weight(i)
        ajc(i,AMPLITUDE) = fexp * weight(i)
        ajc(i,CENTRE) = xc(AMPLITUDE) * fexp * normalization_wave
     :       * xc(INDEX)* weight(i)/ zscore
        ajc(i,INDEX) =  -xc(AMPLITUDE) * fexp * weight(i) * log(zscore)
      end do
*
* GC    : the j(transpose)*r which is the multiple
*         of the gradient and the sum of squares
* AJTJC : the hesian matrix nb since symmetric do l .le. i
*
      do  i  =  1,n
        sum=0.0
        do  k  =  1,m
          sum   =   sum+ajc(k,i)*rc(k)
        end do
        gc(i)  =   sum
        do  j  =  1,i
          sum  =    0.0
          do  k   =  1,m
            sum   =    sum+ajc(k,i)*ajc(k,j)
          end do
          ajtjc(j,i) =   sum
        end do
      end do
      end
