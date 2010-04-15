      subroutine FNHN_fun(m,n,xc,rc,ajc,ajtjc,max_parms,gc,data,dens,
     :                  weight)
*+
* Name:
*    FNHN_FUN

* Invocation:
*    CALL FNHN_FUN(M,N,XC,RC,AJC,AJTJC,MAX_PARMS,GC,DATA,DENS,
*                       WEIGHT)

* Description:
* Perform a 3 component fit to a combination of H alpha and
* the [NII] 6548/6584 lines. The seperation of the two [NII] lines
* with respect to H alpha is FIXED and the widths are assumed to be equal.
* This routine differs from FS2_FUN in the fact that the WIDTHS
* of the two lines are forced to be identical. Thus it is more
* restricitve than the former function.
* It is EXPLICITLY assumed that the DATA() array passed to this
* routine has been SCALED so that it corresponds to REDSHIFT.
* any other chioce of scalign will produce erroneous results.
*
* Arguments:-
*    M = INTEGER (Given)
*        Number of points in data
*    XC(5) = INTEGER ARRAY (Given)
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
*
*   LJC removed, TNW 24/1/89
*  TNW 18/3/91, LSQ_FW2 and RESID_FW2 combined
*  TNW Cambridge, 20-JUN-1991 Bug fix re weights
*  DJA MAN 28-Jun-1991 Created from Fs2_fun
*-
      implicit none
      integer max_parms
      double precision ajtjc(max_parms,max_parms),gc(max_parms)
      integer m
      double precision data(m)
      double precision dens(m)
      double precision weight(m)
      integer n
      double precision xc(n),rc(m),ajc(m,n)
      double precision separation_1
      double precision separation_2
      double precision h1
      double precision h2
      double precision h3
      double precision v3
      double precision v1
      double precision v2
      double precision w1
      double precision w2
      double precision w3
      double precision dv1, dv2, dv3
      double precision sigsq1
      double precision sigsq2
      double precision sigsq3
      double precision twosigsq1
      double precision twosigsq2
      double precision twosigsq3
      double precision sum
      double precision x
      double precision zs1,zs2,score1,score2,expval1,expval2,b
      double precision zs3,score3,expval3
      integer i,j,k

* Symbolic constants giving the seperation of the Special line
* groups in Redshift. The Seperations are defined relative to Halpha
* for NII-H alpha - NII. H_NII_Z1 is for [NII]6548 and _Z2 for
* [NII]6584.

      Double Precision H_NII_Z1
      Double Precision H_NII_Z2
      Parameter (H_NII_Z1 = -0.0022425)
      Parameter (H_NII_Z2 = 0.0031668)

* Symbolic constants giving the seperation of the Special line
* groups in Angstroms. The Seperations are defined relative to the
*  Halpha for NII-H alpha - NII.

      Double Precision H_NII_SEP1
      Double Precision H_NII_SEP2
      Parameter (H_NII_SEP1 = -14.717)
      Parameter (H_NII_SEP2 = 20.783)

* Symbolic constants defining the order of the model paramters
* in the array XC.

      Integer HEIGHT_1
      integer HEIGHT_2
      integer HEIGHT_3
      integer WIDTH_1
      integer WIDTH_2
      integer REDSHIFT
      integer BASE
      parameter( HEIGHT_1 = 3 )
      parameter (HEIGHT_2 = 5)
      parameter (HEIGHT_3 = 6)
      parameter (WIDTH_2 = 7)
      parameter (WIDTH_1 = 2)
      parameter (REDSHIFT = 4)
      parameter (BASE = 1)

* unraveal XC into more meaningful parameters for clarity
* NB ABS() added to HEIGHTS to try and force EMISSION line
* solutions.
      b  = xc( BASE )
      separation_1 = 1 + H_NII_Z1
      separation_2 = 1 + H_NII_Z2
      h1 = abs( xc( HEIGHT_1) )
      v1 = xc( REDSHIFT )
      w1 = xc( WIDTH_1 )
      h2 = abs( xc( HEIGHT_2) )
      w2 = xc( WIDTH_2 )
      h3 = abs( xc( HEIGHT_3 ) )
      w3 = xc( WIDTH_2 )
*    assign v2,v3 allowing for the stretching of H_NII_Z1 / H_NII_Z2
*    the rest seperation of the components as we move i redshift.
*     written this way
*    primarily for transparency in derviative calculation

      v2 = v1 * separation_1 + H_NII_Z1
      v3 = v1 * separation_2 + H_NII_Z2

*     widths of [NII] lines are taken as equal
*         really should allow for their Z difference as well.
*     might be better to allow them to be free because of
*     contamination of 6548 by sky lines.?

      w3 = w2

      sigsq1    = w1 * w1
      sigsq2    = w2 * w2
      sigsq3    = w3 * w3

      twosigsq1 = 2 * sigsq1
      twosigsq2 = 2 * sigsq2
      twosigsq3 = 2 * sigsq3

*    loop over the data points
*    determiing the residuals and the derivatives.

      do i = 1, m
        x      = data(i)
        zs1      = x-v1
        zs2      = x-v2
        zs3      = x-v3

        score1   = zs1*zs1
        score2   = zs2*zs2
        score3   = zs3*zs3

        expval1  = exp( -score1 / twosigsq1 )
        expval2  = exp( -score2 / twosigsq2 )
        expval3  = exp( -score3 / twosigsq3 )

*   diff wrt h1

        ajc(i,HEIGHT_1) = expval1 * weight(i)


*   diff wrt h2

        ajc(i,HEIGHT_2) = expval2 * weight(i)

*   diff wrt h3

        ajc(i,HEIGHT_3) = expval3 * weight(i)

*   diff wrt v1

        dv1 = ajc(i, HEIGHT_1) * h1 * zs1 / sigsq1

*   diff wrt v2

        dv2 = ajc(i, HEIGHT_2) * h2 * zs2  / sigsq2

*   diff wrt v3

        dv3 = ajc(i, HEIGHT_3) * h3 * zs3  / sigsq3

* combine the 3 componetns and allow for the fact that
* the [NII] lines are refered to the rest wavelength of H alpha.
* by multiplying by SEPERATION

        ajc(i, REDSHIFT) = dv1 + dv2 * separation_1
     :                         + dv3 * separation_2

*   diff wrt b

        ajc(i, BASE) = weight(i)

*   diff wrt to sigma

        ajc(i, WIDTH_2) = dv3 * zs3 / w3 + dv2 * zs2 / w2
        ajc(i, WIDTH_1) = dv1 * zs1 / w1

* residual

        rc(i) = (h1 * expval1 + h2 * expval2 + h3 * expval3
     :                 + (b - dens(i)) * weight(i))
      enddo
*
* GC    : the j(transpose)*r which is the multiple
*         of the gradient and the sum of squares
* AJTJC : the hesian matrix nb since symmetric do l .le. i
*
      do i = 1,n
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
