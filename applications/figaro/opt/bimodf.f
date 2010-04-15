      real function bimodf(ifirst,n,ldist,xbar,xbarl,xbarr,v1,v2,imin
     :     ,sdens,prompt,factor,gmode)
*+
* Name:
*    BIMODF

* Invocation:
*   (REAL) = BIMODF(IFIRST,N,LDIST,XBAR,XBARL,XBARR,V1,V2,IMIN
*        ,SDENS,PROMPT,FACTOR,GMODE)

* Purpose:
*   Test bimodality of distribution, and return optimimum dividing point

* Description:
*      Returns an f-ratio which is low if the array "lsdist" is a
*  unimodal distribution, high if it is bimodal.
*
*   Ronald P. Larkin, The Rockefeller University, New York NY 10021
*
*      Patterned loosely after J.A. Hartigan, "Distribution problems in
*  clustering", in "Classification and Clustering", Ed. by J. Van Ryzin,
*  Academic press, N.Y. 1977 pp45-71
*
* Arguments:
*     IFIRST = INTEGER (Given)
*        The first valid element of ldist
*     N = INTEGER (Given)
*         "  last   "      "     "    "
*     SDENS(*) = REAL ARRAY (Given)
*
*     PROMPT = LOGICAL (Given)
*        If to prompt for value of "FRAC"
*     FACTOR = CHARACTER*(*) (Given)
*
*     GMODE = REAL (Given)
*        Mode, if less than 0 on entry then to evaluate,
*                   otherwise done here (if >0 then assumed already
*                   subtracted from data).
*     XBAR = REAL (Returned)
*        mean of the distribution considered as unimodal
*     XBARR,XBARL = REAL (Returned)
*        bin locations of the two means (not modes). Can be
*                  used to compute grouped means.
*     V1 = REAL (Returned)
*        the variance of the distribution considered as unimodal
*     V2 = REAL (Returned)
*        the mane variance of the two halves of the distribution
*                   considered as bimodal.
*     IMIN = INTEGER (Returned)
*        the bin number of the last bin in the left mode part (is
*                   negative and signals the type of error if an error
*                   occured). Errors:
*                                   -1 : Too few points
*                                   -2 : Error calculating variance
*                                         -too little data
*                                   -3 : Error calculating variance
*                                         -too low value data
*                                   -4 : Beginning of data not found
*                                   -5 : End of data not found
*                                   -6 : No data found (or only 1 point)
*                                   -7 : Problem getting variances of
*                                        sub-distributions
*                   Note that for -4,-5,-6 data is only considered if
*                   above threshold.
*     LDIST(3*(N-IFIRST+1)) = REAL ARRAY (Workspace)
*        a histogram, in the form of a 1-dimensional real array
*                   (all elements must be positive or zero). This is for
*                   elements 1-m, rest is used otherwise
*=======================================================================
*     FRAC      the minmum count level acceptable as a peak. The default
*               is 3 times the standard deviation on the background.
*     The program computed the lowest mane variance of the right and
*   left halves of the distribution for all values of IMIN which give
*   enough entries in the bins to the right and left to satisfy the FRAC
*   criterion. It returns zeroes if data are not usable; in this case,
*   IMIN is negative and signals the type of error.

* History:
*   Some tidying up, T.N.Wilkins Manchester 6/7/88, 4/11/88
*   Alteration so as to work for MODE<0, TNW 2/12/88
*   Workspace all passed from above, TNW Cambridge,19/9/90
*   GMODE argument added, with changes corresponding to it TNW 19-20/3/91
*-
      implicit none
      include 'PRM_PAR'
      logical anyok
      integer ok
      real sdens(*)
      real ldist(*)
      real frac
      real xr,xl
      real bimodf_xmean
      integer ip1,i,j,ii
      real v1,v2
      real vr,vl
      integer ibeg,iend
      real factor
      real xbarl,xbarr,xbar
      integer imin
      integer ifirst,n
      real mode
      real gmode
      real value
      integer m
      logical par_qnum,qstat,gen_similar
      real vbar
      real datmax
      logical prompt

* Number of bins

      m = n-ifirst+1

      mode = gmode

      datmax = 0.0
      if(mode.lt.0.0) then

* Calculate mode

         call get_median(sdens(ifirst),ldist(m+1),m,mode)

* subtract base, but don't allow to drop below 0

         do i = 1,m
            ldist(i) = max((sdens(i+ifirst-1)-mode),0.0)
            datmax = max(datmax,ldist(i))
         end do
      else

*   Just check data is >=0, and get maximum

         do i = 1,m
            ldist(i) = max(sdens(i+ifirst-1),0.0)
            datmax = max(datmax,ldist(i))
         end do
      end if

* Calculate frac

      if(mode.gt.0.0) then
         frac = 3*sqrt(mode)*factor
      else
         frac = factor*sqrt(datmax)
      end if

      if(prompt) then
         qstat = par_qnum('Enter value of frac',0.0,VAL__MAXR,frac,.true
     :        .,' ',value)
         frac = value
      end if

      if(n-ifirst.lt.5) then
         imin = -2
         go to 500
      end if

*   Find the variance of the distribution

      xbar = bimodf_xmean(1,m,ldist,v1,imin)
      if(imin.lt.0) then
         go to 500
      end if

*   Find the beginning and end values of i

*   Beginning

      ibeg = 0
      do ii = 1,m
         if(ldist(ii).ge.frac) then

*     End: Continue until we find a value above frac

            iend = m

*     order reversed 30/11/88

            do j = m,ii,-1
               if(ldist(j).ge.frac) then
                  go to 13
               end if
               iend = iend-1
            end do
            imin = -5
            go to 500
         end if
         ibeg = ibeg+1
      end do
      imin = -4
      go to 500

 13   continue
      if(ibeg.ge.iend) then
         imin = -6
         go to 500
      end if

* Find the value of i giving the minimum mean variance

      anyok = .false.
      v2 = VAL__MAXR
      do i = ibeg,iend

* Left side of i

         xl = bimodf_xmean(1,i,ldist,vl,ok)
         if(ok.lt.0) go to 11
         ip1 = i+1

*   Right side of i

         xr = bimodf_xmean(ip1,m,ldist,vr,ok)
         if(ok.lt.0) go to 11
         vbar = (vr+vl)*0.5
         if(vbar.gt.v2) go to 11

*   Record this one as minimum average variance

         anyok = .true.
         v2 = vbar
         xbarl = xl
         xbarr = xr
         imin = i
 11      continue
      end do

      if(.not.anyok) then
         imin = -7
         go to 500
      else if(gen_similar(v2,0.0)) then

*  Bimodf must be inexpressibly large

         bimodf = VAL__MAXR
      else
         bimodf = v1/v2
      end if
      return

* Some kind of error: n too small, ifirst too large, frac too large or
* too small

 500  bimodf = 0.0
      xbarr = 0.0
      xbarl = 0.0
      end
