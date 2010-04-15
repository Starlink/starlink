      subroutine bmguess(guess,work,minht,sigred,deccntr,funct)
*+
* Name:
*    BMGUESS

* Invocation:
*    CALL BMGUESS(GUESS,WORK,MINHT,SIGRED,DECCNTR,FUNCT)

* Purpose:
*  Guess a multiple Gaussian/Lorentzian to fit to data

* Description:
*  To guess the parameters for a multiple gaussian fit in batch mode, no
*  previous fit is assumed. This routine will keep on guessing further
*  gaussians until one fails to pass on the values of width or the
*  minimum height from tolerances, or until deccntr(FIT_NCMP) gaussians
*  are guessed. After finding these, this routine allows the best
*  guesses, in terms of the value of Akaike's Information Criterion, to
*  be selected (this selects the number of components). It is advisable
*  to use a test such as this for determining the number of components,
*  and this can be done here or after the optimisation.

* Arguments:
*    MINHT = REAL (Given)
*        Minimum height
*    SIGRED = REAL (Given)
*        Amount by which sigma can exceed max
*                            value and is reduced to value rather
*                            than component rejected.
*    FUNCT = REAL (Given)
*        Function to evaluate Gaussian or whatever
*    GUESS(4,*) = REAL ARRAY (Returned)
*        Fit parameters (scaled)
*                            Order for first dimension is:-
*                             base,sigma,height,centre
*    WORK(M*3) = REAL ARRAY (Workspace)
* Global variables:
*    MINSIG = REAL (Given)
*        Minimum sigma
*    MAXSIG = REAL (Given)
*        Maximum sigma
*    MPTS = INTEGER (Given)
*        Number of points in range
*    DATSC = DOUBLE PRECISION (Given)
*        DATA scaling factor
*    DENSC = DOUBLE PRECISION (Given)
*        DENS scaling factor
*    DATAPTR = INTEGER (Given)
*        Pointer to scaled version of ADATA
*    DENSPTR = INTEGER (Given)
*        Pointer to scaled version of ADENS
*    WEIGHTPTR = INTEGER (Given)
*        Pointer to weights
*
* Author:
*   T.N.Wilkins Manchester September 1987
*
* History:
*   TNW 26/10/88 PTR1 added
*   TNW 23/11/88 Changed to no longer use common block
*   TNW Cambridge, 9/5/89 SIGRED added, and relevant part of routine
*   made to work (bug fix)
*   TNW 25/4/90, Use WORK rather than DENS for finding peaks. Also minor
*                change to smoothing.
*   TNW 22/4/91, Minimum width set (sigma) to 1 for gettting range for
*                centroid calculations.
*   TNW 17/9/91 Changes to allow use for absorption lines, etc.
*    "  20/9/91 Made single precision almost completely
*    " 2/12/91 All workspace passed as WORK array (PTR1 removed)
*    " 9,12/6/92 Minor improvements
*    " 31/7/92 Now handles all lines as emission, converting at start
*              and end. This avoids large numbers of if statements.
*    " 17/8/92 Accept funct as argument, so can deal with different
*              models.
*    " 21/8/92 Use opt_cmn/dynamic_mem
*  TNW: 28th June 1993, reflect changes in opt_cmn
*  TNW: 29th June 1993, N removed
*  TNW: 27th January 1994, a little more checking on setting smaxsig
*-
      implicit none
      include 'opt_cmn'
      include 'status_inc'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'fit_coding_inc'
      include 'PRM_PAR'
      integer MAX_PARS
      real sigred
      parameter (MAX_PARS=4)
      real guess(MAX_PARS,*)
      real minht
      real work(mpts*3)
      real sguess(MAX_PARS)
      real dd,zdens,base,sum,x1,xn,x0,sigma
      real sminsig,smaxsig,sminht,maxwk,invm,tmaxsig
      real EFOLD
      parameter (EFOLD = 2.35482)
      logical go
      character*48 chars
      real funct
      external funct
      integer i,left,right,idiff,icentre,pstat,maic,len1
      integer ngauss,cnv_fmtcnv,status,nbad,itmp
      real aic,bstaic,test,factor
      data smaxsig,sminht/0.2,0.1/

* Store maximum allowed number of components

      max_cmp = deccntr(FIT_NCMP)
      go = max_cmp.gt.1

* Get guesses for first gaussian

      call opt_guess_one(sguess,.true.,work,%VAL(CNF_PVAL(dataptr)),
     :                   %VAL(CNF_PVAL(densptr)),deccntr(FIT_MODEL))

      icentre = nint(sguess(4)*real(mpts))

      invm = 1.0/real(mpts)
      sminsig = 2.0*invm

      if(minsig.gt.0.0) sminsig = minsig/real(datsc)

* Make sure maximum sigma is at least minimum (or leave as default)

      tmaxsig = maxsig/real(datsc)
      if(tmaxsig.gt.sminsig) smaxsig = tmaxsig
      if(minht.gt.0.0) sminht = minht/real(densc)

* Put data into work array

      status = cnv_fmtcnv('double','float',%VAL(CNF_PVAL(densptr)),
     :                    work(mpts+1),mpts,nbad)

* Smooth data

      call robust_smooth(work(mpts+1),mpts,work,work(2*mpts+1),1,
     :                   status)

* Limit peak, first find 60% height levels

      base = sguess(1)

* Convert absorption to equivalent emission!

      if(deccntr(FIT_ABS).eq.1) then
        factor = -1.0
        do i = 1, mpts
          work(i) = 1.0 - work(i)
        enddo
        sguess(1) = 1.0 - sguess(1)
        sguess(3) = -sguess(3)
      else
        factor = 1.0
      endif
      left = 1
      right = mpts
      icentre = min(right,max(left,icentre))
      test = sguess(1)+sguess(3)*0.6
      do i = 1, icentre
        if(work(i).lt.test) left = i
      end do
      do i = mpts, icentre, -1
        if(work(i).lt.test) right = i
      end do

* Delimit area of spectrum to find centriod and sigma for

      idiff = min((icentre - left), (right - icentre))
      idiff = max(idiff,1)
      itmp = nint(real(idiff)*1.7)
      left = max(1,(icentre - itmp))
      right = min(mpts,(icentre + itmp))
      xn = 0.0
      x1 = 0.0
      sum = 0.0

* Get mean and sigma for range we're using

      do i = left, right
        zdens =  max((work(i) - sguess(1)),0.0)
        dd = real(i)*invm*zdens
        xn = dd + xn
        sum = sum + dd * real(i)*invm
        x1 = x1 + zdens
      end do

*  Check we've got a reasonable amount of data, if not then we'll return
*  one component only, and set x1 to a useable value.

      if(x1.lt.sminht*0.1) then
         x1 = sminht*0.1
         go = .false.
         call opt_wruser('Not sure there''s any data here to fit!',pstat
     :        )
      endif
      x0 = xn/x1

* Sigma width must not be larger than half full width at 60% peak

      sigma = sqrt( max( 0.0,((sum/x1)-x0*x0) ) )
      sigma = max( sminsig,min( abs(sigma),(real(idiff)*invm) ) )

* Sigma must not be over maximum value!

      sigma = min(sigma,smaxsig)

      sguess(2) = sigma
      sguess(4) = x0

* Put parameters in order required for multiple gaussian fits

* base

      guess(1,1) = base
      guess(2,1) = sguess(2)
      guess(3,1) = factor*sguess(3)
      guess(4,1) = sguess(4)
      ngauss = 1

*  Only partially subtract base, but we might as well make sure the data
*  has zero for its minimum value. If base is negative then completely
*  remove it. For absorption lines we take the base as half way between
*  the maximum value (which for scaled data is 1) and the value we
*  obtained. The reason for only partially subtracting the base is that
*  it is common for the base value we've obtained to be biased by lines,
*  which would lead to under-estimating the line strengths, or even
*  missing them altogether.

      if(go.and.(sguess(1).gt.0.0)) then
        sguess(1) = sguess(1) * 0.5
      end if

* Look for gaussians, in order of decreasing height, until one is found
* which fails tols

      do while(go)

*   Remove this peak from work and find next peak

        icentre = nint(sguess(4)*real(mpts))
        idiff = nint(sguess(2)*real(mpts))

*   First of all get rid of any residuals from last peak altogether, by
*   setting work to zero for +/- sigma, then remove rest using guessed
*   parameters.

        do i = max(1,(icentre-idiff)),min(mpts,(icentre+idiff))
          work(i) = 0.0
        end do
        maxwk = VAL__MINR
        do i = 1, mpts

*     Subtract previous component, but don't allow "over-subtraction"
*     - i.e. limit to range >0

          work(i) = max(0.0,(work(i) -  funct((real(i)*invm),sguess)))

*     Look for next centre

          if(work(i).gt.maxwk) then
            icentre = i
            maxwk = work(i)
          end if
        end do

*   Look for edges of new line

        left = 1
        test = maxwk*0.6
        do i = 1, icentre
          if(work(i).lt.test) left = i
        end do
        right = mpts
        do i = mpts, icentre, -1
          if(work(i).lt.test) right = i
        end do

        idiff = min((icentre - left), (right - icentre))
        itmp = nint(real(idiff)*1.7)
        left = max(1,(icentre - itmp))
        right = min(mpts,(icentre + itmp))
        xn = 0.0
        x1 = 0.0

*   Centriod, and find sigma

        do i = left, right
          zdens =  max(work(i),0.0)
          dd = real(i)*invm*zdens
          xn = dd + xn
          sum = sum + dd * real(i)*invm
          x1 = x1 + zdens
        end do

*   Take this component as failed by default, and set GO to true if
*   we decide to look for more components

        go = .false.

*   Avoid dividing by zero

        if(x1.gt.0.0) then
          x0 = xn/x1
          sigma = sqrt(max(0.0,((sum/x1)-x0*x0)))
          sigma = min(abs(sigma),(real(max(2,idiff))*invm))

*     If sigma just too large, may still be ok (checked after fitting
*     anyway)

          if((sigma.gt.smaxsig).and.(sigma.lt.(smaxsig*sigred))) then
            sigma = smaxsig
          end if

*     Put parameters in array as required for multiple gaussian fits,
*     if height and width ok. If not, then output reason for failure
*     to detect next line (may be useful to user trying to decide best
*     values for tols).

          if(maxwk.lt.sminht) then
            write(chars
     :,'(''Height of next component too low ('',e12.5,'')'')')
     :        maxwk*real(densc)
            call opt_wruser(chars,pstat)
          else if(sigma.lt.sminsig) then
            write(chars
     :,'(''Width of next component too low ('',e12.5,'')'')')
     :       sigma*EFOLD*real(datsc)
            call opt_wruser(chars,pstat)
          else if(sigma.gt.smaxsig) then
            write(chars
     :,'(''Width of next component too high ('',e12.5,'')'')')
     :       sigma*EFOLD*real(datsc)
            call opt_wruser(chars,pstat)
          else
            sguess(2) = sigma
            sguess(3) = maxwk
            sguess(4) = x0
            sguess(1) = 0.0
            ngauss = ngauss + 1
            guess(2,ngauss) = sguess(2)
            guess(3,ngauss) = factor*sguess(3)
            guess(4,ngauss) = sguess(4)

*       Check if maximum number of gaussians reached

            go = ngauss.lt.max_cmp
          end if
        end if

      end do

* If we are to check AICs now, then do so.

      if(deccntr(FIT_STST).eq.PREAIC) then
        bstaic = VAL__MAXR
        do i = 1, ngauss
          call aic_d(guess,i,%VAL(CNF_PVAL(dataptr)),
     :               %VAL(CNF_PVAL(densptr)),mpts,
     :               %VAL(CNF_PVAL(weightptr)),aic,funct)
          len1 = 0
          call chr_puti(i,chars,len1)
          call chr_putc(' components, AIC = ',chars,len1)
          call chr_putr(aic,chars,len1)
          call par_wruser(chars(:len1),pstat)
          if(aic.lt.bstaic) then
            maic = i
            bstaic = aic
          end if
        end do

        ngauss = maic
      end if

* Write number of components found into coding array

      deccntr(FIT_NCMP) = ngauss

      end
