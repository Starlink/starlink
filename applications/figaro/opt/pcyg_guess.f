      subroutine pcyg_guess(dens,m,guess,work)
*+
* Name:
*    PCYG_GUESS

* Invocation:
*    CALL PCYG_GUESS(DENS,M,GUESS,WORK)

* Purpose:
*   To make first guesses for a P Cygni profile.

* Description:
*   To make first guesses for a P Cygni profile. This uses a peak-hunting
*   algorithm. First of all the highest point in the data is found, then
*   the lowest. This doesn't in fact worry about whether the absorption
*   feature is red or blue shifted wrt the emission feature.
*
* Arguments:
*     DENS(M) = REAL ARRAY (Given)
*        Y (intensity) data array
*     M = INTEGER (Given)
*        Dimension of DATA/DENS
*     GUESS(4,2) = DOUBLE PRECISION ARRAY (Returned)
*        Guesses to parameters
*     WORK(M*3) = REAL ARRAY (Workspace)
* Authors:
*   T.N.Wilkins, IoA Cambridge, 18/9/91 based on GUESS_2
*-
      implicit none
      integer m
      double precision dens(m)
      real work(m*4)
      real guess(4,2)
*
      real irm
      real minval,maxval,eht,aht,ecen,acen,ewid,awid,c1
      integer minpos,maxpos,incr,i,cross,status,cnv_fmtcnv,nbad,dpos

* Inverse of REAL(M)

      irm = 1.0/real(m)

* Copy to work array. We first of all put in in work(2*m+1...3*m)
* This means that when we've smoothed it we can have it in work(1...m)
* I'm not too sure whether we'd be better with the base evaluation after
* the smoothing.

      dpos = 1 + 2*m
      status = cnv_fmtcnv('double','float',dens,work(dpos),m,nbad)

* Base

      call get_median(work(dpos),work,m,c1)

* Smooth data

      call robust_smooth(work(dpos),m,work,work(m+1),1,status)

      guess(1,1) = c1
      guess(1,2) = c1

      minval = work(1)
      maxval = work(1)
      minpos = 1
      maxpos = 1
      do i = 2, m
        if(work(i).gt.maxval) then
          maxval = work(i)
          maxpos = i
        else if(work(i).lt.minval) then
          minval = work(i)
          minpos = i
        end if
      end do

* Trap if there seems to have been a problem with c1 (we'll assume for
* the moment that a suitable profile is present!).

      if(((c1-minval).lt.0.05).or.((maxval-c1).lt.0.05)) then
        c1 = 0.5
      end if

      eht = maxval - c1
      ecen = real(maxpos)*irm

      aht = minval - c1
      acen = real(minpos)*irm

      incr = (maxpos - minpos)/abs(maxpos - minpos)

      do i = minpos, maxpos, incr
        if(work(i).lt.c1) cross = i
      end do

      awid = real(abs(minpos-cross))*irm*0.3

      ewid = real(abs(maxpos-cross))*irm*0.3

* Put guesses into guess array

      if(minpos.lt.maxpos) then
        guess(2,1) = awid
        guess(3,1) = aht
        guess(4,1) = acen
        guess(2,2) = ewid
        guess(3,2) = eht
        guess(4,2) = ecen
      else
        guess(2,1) = ewid
        guess(3,1) = eht
        guess(4,1) = ecen
        guess(2,2) = awid
        guess(3,2) = aht
        guess(4,2) = acen
      end if

      end
