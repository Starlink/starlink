      subroutine FOII_fun(m,n,xc,rc,ajc,ajtjc,
     :                  max_parms,gc,data,dens,weight)
*+
* Name:
*    FOII_FUN

* Invocation:
*    CALL FOII_FUN(M,N,XC,RC,AJC,AJTJC,
*                       MAX_PARMS,GC,DATA,DENS,WEIGHT)

* Description:
* Perform a 2 component fit to a combination of the [OII] 3729/3726
* lines. The seperation of the two lines is FIXED and the widths
* are assumed to be equal.
* This routine differs from FS2_FUN in the fact that the WIDTHS
* of the two lines are forced to be identical. Thus it is more
* restricitve than the former function.
* It is EXPLICITLY assumed that the DATA() array passed to this
* routine has been SCALED so that it corresponds to REDSHIFT.
* any other chioce of scalign will produce erroneous results.
* Arguments:-
*    M = INTEGER (Given)
*        Number of points in data
*    XC(5) = DOUBLE PRECISION ARRAY (Given)
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
      double precision separation

      double precision h1
      double precision h2
      double precision v1
      double precision v2
      double precision w1
      double precision w2
      double precision dv1, dv2
      double precision sigsq1
      double precision sigsq2
      double precision twosigsq1
      double precision twosigsq2
      double precision sum
      double precision x
      double precision zs1,zs2,score1,score2,expval1,expval2,b
      integer i,j,k

* Symbolic constants giving the seperation of the Special line
* groups in Redshift. The Seperations are defined relative to the
* lower redshift line.

      Double Precision OII_Z
      Parameter (OII_Z = 7.38025D-4)

* Symbolic constants giving the seperation of the Special line
* groups in Angstroms. The Seperations are defined relative to the
* lower redshift line.

      Double Precision OII_SEP
      Parameter (OII_SEP = 2.75)

* Symbolic constants defining the order of the model paramters
* in the array XC.

      Integer HEIGHT_1
      integer HEIGHT_2
      integer WIDTH
      integer REDSHIFT
      integer BASE
      parameter( HEIGHT_1 = 3 )
      parameter (HEIGHT_2 = 5)
      parameter (WIDTH = 2)
      parameter (REDSHIFT = 4)
      parameter (BASE = 1)

* unraveal XC into more meaningful parameters for clarity
      b  = xc(BASE)
      separation = 1 + OII_Z
      h1 = abs( xc(HEIGHT_1) )
      v1 = xc(REDSHIFT)
      w1 = xc(WIDTH)
      h2 = abs( xc(HEIGHT_2) )

*    assign v2 allowing for the stretching of SII_Z the rest seperation
*    of the components as we move i redshift. written this way
*    primarily for transparency in derviative calculation

      v2 = v1 * separation + OII_Z

*     widths of lines are taken as equal

      w2 = w1

      sigsq1    = w1 * w1
      sigsq2    = w2 * w2
      twosigsq1 = 2 * sigsq1
      twosigsq2 = 2 * sigsq2

*    loop over the data points
*    determiing the residuals and the derivatives.

      do i = 1, m
        x      = data(i)
        zs1      = x-v1
        zs2      = x-v2
        score1   = zs1*zs1
        score2   = zs2*zs2
        expval1  = exp( -score1 / twosigsq1 )
        expval2  = exp( -score2 / twosigsq2 )

*   diff wrt h1

        ajc(i,HEIGHT_1) = expval1 * weight(i)

*   diff wrt h2

        ajc(i,HEIGHT_2) = expval2 * weight(i)

*   diff wrt v1

        dv1 = ajc(i, HEIGHT_1) * h1 * zs1 / sigsq1

*   diff wrt v2

        dv2 = ajc(i, HEIGHT_2) * h2 * zs2  / sigsq2

* combine the two componetns and allow for the fact that
* the 2nd line is refered to the rest wavelength of the first.
* by multiplying by 1 + SII_Z

        ajc(i, REDSHIFT) = dv1 + dv2 * separation

*   diff wrt b

        ajc(i, BASE) = weight(i)

*   diff wrt to sigma

        ajc(i, WIDTH) = dv1 * zs1 / w1 + dv2 * zs2 / w2

* residual

        rc(i) = (h1 * expval1 + h2 * expval2 + (b - dens(i))*weight(i))
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
