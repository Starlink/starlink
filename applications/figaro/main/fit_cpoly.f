      integer function fit_cpoly(weight,npts,x,y,coeffs,rssq,max_fit,
     :                      max_kplus1,work,a)
*+
* Name:
*    FIT_CPOLY

* Invocation:
*   (INTEGER) = FIT_CPOLY(WEIGHT,NPTS,X,Y,COEFFS,RSSQ,MAX_FIT,
*                           MAX_KPLUS1,WORK,A)

* Purpose:
*     Performs a least-squares fit to data using
*     Chebyshev polynomials.

* Description:
*     The fit can be of any specified order  up to (MAX_KPLUS1-1).
*     For more information on weighting and other considerations, see
*     the PDA manual.
*     The status of the FIT is returned via the Routine Name. All
*     None Zero values are FAIL conditions
*     Value             CONDITION
*       0                 Ok
*       >0                error

* Arguments
*   WEIGHT(NPTS) = DOUBLE PRECISION ARRAY (Given)
*      Weight Given to each point in the fit.  Points given the weight
*      DELETE = 1E-6 are effectively excluded from the fit.
*   NPTS = INTEGER (Given)
*      Number of Points for fit, including those for which WEIGHT[I] =
*      DELETE
*   X(NPTS) = DOUBLE PRECISION ARRAY (Given)
*      X axis data for fit
*   Y(NPTS) = DOUBLE PRECISION ARRAY (Given)
*      Y axis data for fit
*   MAX_KPLUS1 = INTEGER (Returned)
*      The maximum cheby order + 1 that can be fitted. This is currently
*      set =20
*   RSSQ(MAX_KPLUS1) = DOUBLE PRECISION ARRAY (Returned)
*      Residual Sum of Squares as a function of Order up to MAX_FIT
*   MAX_FIT = INTEGER (Returned)
*      The maximum coeefs that could be used.  This will normally be
*      MAX_KPLUS1. However a test is made to ensure that the number of
*      orders to be fitted does not exceed the number of points for
*      which WEIGHT[I] = DELETE
*   COEFFS(MAX_KPLUS1,MAX_KPLUS1) = DOUBLE PRECISION ARRAY (Returned)
*      The Cheby Poly coeficents for all orders up to MAX_FIT
*   WORK(NPTS*3+2*MAX_KPLUS1) (Workspace)

* Subroutines/functions called:
*      PDA_DPLINT              : Least squares Chebyshev polynomial fit
*      ERR_REP          : Output error message if routine fails

* History:
*     Altered to use fig_nagerr, TNW 7/10/88
*     Altered to use getwork, TNW 28/11/88
*     Altered to get wk2 by getwork, and to use dyn_increment, TNW
*     15/12/88
*     Altered 24/2/89 arrays Now passed from above
*     test to see how many points are left which have not
*     been assigned the weight DELETE /DJA?
*     Workspace passed from above, TNW/CAVAD 19/9/90
*     Replaced NAG e02adf with PDA_DPOLFT  JWP March 97
*     Changed matrix A to fit with new PDA version AJH August 97
*------------------------------------------------------------------
      implicit none
*
* import
*
      integer max_kplus1
      integer npts
      double precision x(npts)
      double precision y(npts)
      double precision weight(npts)

*
* export
*
      integer max_fit
      double precision rssq(max_kplus1)
      double precision coeffs(max_kplus1,max_kplus1)

* workspace
      double precision work(*)

*
* local
* added for pda support

      integer maxnpts
      integer locmxkp1
      parameter (maxnpts = 2048)
      parameter (locmxkp1 = 10)
      integer ndeg
      integer ierr
      double precision eps
      double precision a(3*maxnpts + 3*(locmxkp1))
      double precision weight2(maxnpts)
      double precision r(locmxkp1)

* do loop

      integer i

* number of points not deleted

      integer i_count
      double precision DELETE
      parameter (DELETE = 1.0d-6)

* error flag

      integer ifail
      integer status
* --------------------------------------------------------------
* Square weights

      DO 1 I = 1, npts
         weight2(I) = weight(I) * weight(I)
 1    CONTINUE



*
*  perform chebyshev fit to data
*
      ifail = 0
*
* Number of coefficients must be less than number of data points
* which have not been deleted

      i_count = 0
      do i = 1,npts
        if(weight(i).gt.DELETE) then
          i_count = i_count + 1
        end if
      end do
      if(max_fit.gt.i_count) then
        call par_wruser('Number of coefficients exceeds data points',
     :        status)
        call par_wruser('Reseting order to maximum allowed',status)
      end if
      max_fit =  min(max_kplus1,i_count)
*
* call nag fitting routine
*
*     call e02adf(npts,max_fit,max_kplus1,x,y,weight,work,
*     :            work(3*npts+1),coeffs,rssq,ifail)

* Perform fits for all degrees from 0 to max_fit
      eps=0D0

      CALL PDA_DPOLFT(npts, x, y, weight2, max_fit-1, ndeg, eps, r, ierr
     :        ,a,ifail)

      IF (IFAIL .NE. 0) THEN
      CALL PAR_WRUSER('Error in call to PDA_DPOLFT in fit_cpoly.f'
     : ,STATUS)
      ENDIF


      do 2 i = 1,max_fit-1
         rssq(i) = 0D0
 2    continue
      rssq(max_fit) = eps


* return Status if FIT via routine NAme

      fit_cpoly = ifail
      end




