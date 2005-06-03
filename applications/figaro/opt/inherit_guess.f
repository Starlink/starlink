      subroutine inherit_guess(direction,guess,line,xsect,nwindow,iy,
     :                  deccntr,fstat)
*+
* Name:
*    INHERIT_GUESS

* Invocation:
*    CALL INHERIT_GUESS(DIRECTION,GUESS,LINE,XSECT,NWINDOW,IY,
*                       DECCNTR,FSTAT)

*
* Description:
*    To obtain previous results from the results array, at the current
*    or previous block, and make these the current guesses.
*
* Purpose:
*    To obtain previous results from the results array, at the current
*    or previous block, and make these the current guesses.
*
* Arguments:
*     DIRECTION = INTEGER (Given)
*        Where to get results from
*     LINE = INTEGER (Given)
*        Current line
*     XSECT = INTEGER (Given)
*        Current cross-section
*     NWINDOW = INTEGER (Given)
*        Number of cross-sections in window
*     DECCNTR(*) = INTEGER ARRAY (Given and returned)
*        Fit coding
*     FSTAT = INTEGER (Given and returned)
*        If previous results found
*     GUESS(*) = REAL ARRAY (Returned)
*        Guess store
* Global variables:
*     D_RPTR = INTEGER (Given)
*        Pointer to results block (include file arc_dims)
*     STAPTR = INTEGER (Given)
*        Pointer to fit status block (include file arc_dims)
*     NZP = INTEGER (Given)
*        1st dimension of results block (include file arc_dims)
*     NYP = INTEGER (Given)
*        2nd dimension of results block (include file arc_dims)
*     NXP = INTEGER (Given)
*        3rd dimension of results block (include file arc_dims)
*     GPSCAL = LOGICAL (Given)
*        If to scale results to current data (include file arc_dims)
*
*   MGAUSS argument added 19/1/89 T.N.Wilkins Manchester
*   Subtract DENSZERO from base, also use gen_clipf.
*                                         TNW 16/4/91 Cambridge
*   Set GET_PREV false for NCOMP=0, TNW 22/4/91
*   Renamed INHERIT_GUESS, and altered for new version of FIT_LINE,
*     TNW, 9/91
*-
      implicit none
      include 'arc_dims'
      include 'status_inc'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'fit_coding_inc'
      include 'opt_cmn'
      integer iy
      integer line,xsect

* number of xsects in block

      integer nwindow

* Export

      integer fstat
      real guess(*)

* Local

      integer odeccntr(MAX_DECODE_CONTROL)
      integer pstat
      character*40 chars
      real max_height
      integer oldans
      integer nlow,nhigh
      real odensc
      real scalefac
      logical scalefound
      integer direction
      real factors(5)
      integer icount,ocount,ref
      include 'DYNAMIC_MEMORY'
      data factors/1.0,0.9,0.7,0.5,0.3/

* Look in block given by direction if possible, otherwise in current
* block.

      oldans = xsect + nwindow*direction
      oldans = min(spdim1,max(1,oldans))

* Decode status word and read in parameters as they are (i.e. not
* bothering initially whether the fit is the same as the current one)

      call getres(%VAL(CNF_PVAL(d_rptr)),line,oldans,iy,guess,odeccntr,
     :            odensc,%VAL(CNF_PVAL(staptr)),fstat)
      fstat = -fstat

* I we are to take the fit model etc., completely from the old fit, then
* copy the required information over, but being careful to keep the
* bits which have changed. More bits may need adding with time!

      if(deccntr(FIT_MODEL).eq.0) then
        deccntr(FIT_ABS) = odeccntr(FIT_ABS)
        deccntr(FIT_MODEL) = odeccntr(FIT_MODEL)
        deccntr(FIT_TYPE) = odeccntr(FIT_TYPE)
        deccntr(FIT_WEIGH) = odeccntr(FIT_WEIGH)
        deccntr(FIT_OPT) = odeccntr(FIT_OPT)
        deccntr(FIT_CONSTR) = odeccntr(FIT_CONSTR)
        deccntr(BACK_REMOV) = odeccntr(BACK_REMOV)
        deccntr(BACK_ORDER) = odeccntr(BACK_ORDER)
        deccntr(BACK_MODEL) = odeccntr(BACK_MODEL)
      end if

      if( (deccntr(FIT_MAN).eq.MAN_NOALTER).and.
     :    (deccntr(FIT_MODEL).ne.odeccntr(FIT_MODEL)) ) then
        call opt_wruser('Previous fit not compatable',pstat)
        fstat = -1
      else
        write(chars,'(''Number of gaussians in previous fit = ''
     :       ,i1)') odeccntr(FIT_NCMP)
        call par_wruser(chars,pstat)
        if(odeccntr(FIT_NCMP).eq.0) fstat = -1
      end if

      if(fstat.ne.0) return

* Set number of components, ignoring any over the allowed maximum

      deccntr(FIT_NCMP) = min(deccntr(FIT_NCMP),odeccntr(FIT_NCMP))

* Scale height by old scale factor if possible

      scalefound = gpscal.and.(odensc.gt.1e-4)

      if(scalefound) then
        scalefac=odensc
      else
        scalefac=real(densc)
      end if

      max_height=-2000.0

* Scale results from old fit, and move to correct positions for
* components > 1


      ocount = deccntr(FIT_NCMP) * 4 -1

      do ref = deccntr(FIT_NCMP)*4, 4, -4
        ocount = ocount - 1
        guess(ref) = (guess(ocount)-real(datazero))/real(datsc) !centre
        ocount = ocount - 1
        icount = ref - 1
        guess(icount) = guess(ocount) / scalefac            !height
        max_height = max(max_height,abs(guess(icount)))
        ocount = ocount - 1
        guess(ref - 2) = guess(ocount) / real(datsc)         !width
        guess(ref - 3) = (guess(1) - real(denszero))/ scalefac !base
      end do

      if(gpscal.and.((.not.scalefound).or.(max_height.gt.1.0))) then

*  Scale heights to fit current data

        if(.not.scalefound) then
          max_height=max_height*factors(deccntr(FIT_NCMP))
        end if
        if(max_height.gt.1.0e-3) then
          ref = deccntr(FIT_NCMP) * 4 - 1
          do ocount = 3, ref
            guess(ocount) = guess(ocount)/max_height
          end do
        end if
      end if
      fstat = 0

* Ensure answers are sensible - may not be if fit had a NAG error.
* We limit the values to the range -1 to 1

      call gen_clipf(guess,4*deccntr(FIT_NCMP),-1.0,1.0,nlow,nhigh,
     :                  guess)

      end
