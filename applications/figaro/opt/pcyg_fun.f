      subroutine pcyg_fun(m,n,xc,rc,ajc,ajtjc,max_parms,gc,data,
     :                  dens,weight)
*+
* Name:
*    PCYG_FUN

* Invocation:
*    CALL PCYG_FUN(M,N,XC,RC,AJC,AJTJC,MAX_PARMS,GC,DATA,
*                       DENS,WEIGHT)

* Description:
* Purpose:
* Calculate derivatives for a P Cygni Profile fit comprising
* a emission line and and an absorption line gaussian.
*
* Arguments:-
*    M = INTEGER (Given)
*        Number of points in data
*    XC(7) = DOUBLE PRECISION ARRAY (Given)
*        Fit parameters
*    N = INTEGER (Given)
*
*    DATA(M) = DOUBLE PRECISION ARRAY (Given)
*        X data
*    DENS(M) = DOUBLE PRECISION ARRAY (Given)
*        Y data
*    WEIGHT(M) = DOUBLE PRECISION ARRAY (Given)
*        weights
*    MAX_PARMS = INTEGER (Given)
*        Maximum number of parameters
*    RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        Residuals on fit
*    AJC(M,N) = DOUBLE PRECISION ARRAY (Returned)
*        Derivatives
*    AJTJC(MAX_PARMS,MAX_PARMS) (d) Hessian
*    GC(MAX_PARMS) = DOUBLE PRECISION ARRAY (Returned)
*        Multiple of gradient and sum of squares

* History:
*  TNW 24/1/89, LJC removed
*  TNW 18/3/91, LSQ_FS2 and RESID_FS2 combined
*  TNW Cambridge, 20-JUN-1991 Bug fix re weights
*  DJA Manchester 1-July-1991 Created from FS2_FUN
*-
      implicit none
      integer max_parms
      double precision ajtjc(max_parms,max_parms),gc(max_parms)
      integer m
      double precision weight(m)
      double precision data(m)
      double precision dens(m)
      integer n
      double precision xc(n),rc(m),ajc(m,n)
      double precision h1
      double precision h2
      double precision v1
      double precision v2
      double precision invw1
      double precision invw2
      double precision inv2sigsq1
      double precision inv2sigsq2
      double precision invsigsq1
      double precision invsigsq2
      double precision expval1
      double precision expval2
      double precision x
      double precision b
      Integer BASE
      integer HEIGHT_1
      integer HEIGHT_2
      integer WIDTH_1
      integer WIDTH_2
      integer CENTRE_1
      integer CENTRE_2
      Parameter ( BASE = 1)
      Parameter ( WIDTH_1 = 2)
      Parameter (HEIGHT_1 = 3)
      Parameter (CENTRE_1 = 4)
      Parameter (WIDTH_2 = 5)
      Parameter (HEIGHT_2 = 6)
      Parameter (CENTRE_2 = 7)

      integer i,j,k
      double precision sum
      double precision score1,score2,zs1,zs2
      b     = xc(BASE)
      h1    = abs( xc(HEIGHT_1))
      v1    = xc(CENTRE_1)
      invw1 = 1.0d0/xc( WIDTH_1)
      h2    = abs( xc(HEIGHT_2) )
      v2    = xc(CENTRE_2)
      invw2 = 1.0d0/xc(WIDTH_2)
      invsigsq1    = invw1 * invw1
      invsigsq2    = invw2 * invw2
      inv2sigsq1 = 0.5 * invsigsq1
      inv2sigsq2 = 0.5 * invsigsq2

      do i = 1, m
        x        = data(i)
        zs1      = x-v1
        zs2      = x-v2
        score1   = zs1*zs1
        score2   = zs2*zs2
        expval1  = exp( -score1 * inv2sigsq1 )
        expval2  = exp( -score2 * inv2sigsq2 )
*   diff wrt h1 - emission line

        ajc(i,HEIGHT_1) = expval1 * weight(i)

*   diff wrt h2 - absorption line

        ajc(i,HEIGHT_2) = - expval2 * weight(i)

*   diff wrt v1 - emission line

        ajc(i,CENTRE_1) = ajc(i,HEIGHT_1) * h1 * zs1 * invsigsq1

*   diff wrt v2- absorption line

        ajc(i,CENTRE_2) = - ajc(i,HEIGHT_2) * h2 * zs2 * invsigsq2

*   diff wrt b

        ajc(i,BASE) = weight(i)

*   diff wrt s1

        ajc(i,WIDTH_1) = ajc(i,CENTRE_1) * zs1 * invw1

*   diff wrt s2

        ajc(i,WIDTH_2) = ajc(i,CENTRE_2) * zs2 * invw2

* Residuals

        rc(i) = (h1 * expval1 - h2 * expval2 + b - dens(i) )
     :                                           * weight(i)
      enddo
*
* GC    : the j(transpose)*r which is the multiple
*         of the gradient and the sum of squares
* AJTJC : the hesian matrix nb since symmetric do l .le. i
*
      do  i  =  1,n
        sum=0.0d0
        do k = 1,m
          sum = sum+ajc(k,i)*rc(k)
        end do
        gc(i) = sum
        do j = 1,i
          sum = 0.0d0
          do k = 1,m
            sum = sum+ajc(k,i)*ajc(k,j)
          end do
          ajtjc(j,i) = sum
        end do
      end do
      end
