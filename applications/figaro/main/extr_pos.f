      subroutine extr_pos(results,resvar,fitsta,x,y,npts,ylim,
     :     arc,w,xarr,nchans,err)
*+
* Name:
*    EXTR_POS

* Invocation:
*    CALL EXTR_POS(RESULTS,RESVAR,FITSTA,X,Y,NPTS,YLIM,
*          ARC,W,XARR,NCHANS,ERR)

* Purpose:
*   Extract positions from results arrays
* Description:
*      To copy the line positions from the results structure and the
*    mean x-section values into the arrays X and Y respectively.
*    NPTS is also set for each line. This assumes that the data was
*    fitted with model "BG".
*
* Subroutines/functions called:
*     SET_WEIGHTS  : Set values of weights array
*     ZERO_DBLE    : Zero a double precision array
*     ZERO_INT     : Zero an integer array
*
*     PAR_QUEST    : Get yes/no response from user
*     PAR_WRUSER   : Write string to user
*
* Arguments:
*    RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results array
*    RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results array variance
*    FITSTA(NCNTRL,NYP,NXP) = INTEGER ARRAY (Given)
*        Results array variance
*    XARR(NCHANS) = REAL ARRAY (Given)
*        Channels (or xsects for COMB)
*    NCHANS = INTEGER (Given)
*        Number of channels (or xsects for COMB)
*    X(NXP,LINE_COUNT) = DOUBLE PRECISION ARRAY (Returned)
*        X positions
*    Y(NXP,LINE_COUNT) = DOUBLE PRECISION ARRAY (Returned)
*        Y positions
*    NPTS(LINE_COUNT) = INTEGER ARRAY (Returned)
*        Number of points found for each line
*    YLIM(2,LINE_COUNT) = DOUBLE PRECISION ARRAY (Returned)
*        Y limits (for Chebyshev fitting)
*    ARC(NSLCT,LINE_COUNT) = INTEGER*2 ARRAY (Returned)
*
*    W(NXP,LINE_COUNT) = DOUBLE PRECISION ARRAY (Returned)
*        Weights
*    ERR(NXP) = REAL ARRAY (Workspace)
*
*  Global variables:
*    MXPARS = INTEGER (Given)
*
*    NYP = INTEGER (Given)
*
*    NXP = INTEGER (Given)
*
*    LINE_COUNT = INTEGER (Given)
*        Number of lines
*    NSLCT = INTEGER (Given)
*
*
* History:
*  T.N.Wilkins, Cambridge, 26/3/91 Mostly re-written
*      "            "      1-8/7/91 Changes for new results structure
*-
      implicit none
      include 'arc_dims'
      real results(mxpars,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
      integer fitsta(ncntrl,nyp,nxp)
      integer nchans
      real xarr(nchans)
      integer npts(line_count)
      integer*2 arc(nslct,line_count)
      double precision x(nxp,line_count),y(nxp,line_count)
      double precision w(nxp,line_count)
      double precision ylim(2,line_count)
      real err(nxp)
* ----------------------------------------------------------------------
*
* Local

      real block_cen
      integer cstat,line,xsect
      logical keep_nag_err,par_quest
      real xint
      integer status,get_parnum,posbc,poscen
      character*22 chars
      include 'status_inc'

*  Zero out arrays

      call zero_dble(x,nxp*line_count)
      call zero_dble(y,nxp*line_count)
      call zero_int(npts,line_count)

* Initialise

      keep_nag_err = par_quest(
     :      'keep fits with nag errors for poly fitting?',.false.)

* Get locations of elements of results that we are interested in

      posbc = get_parnum('Space1_pos')
      poscen = get_parnum('Centre_1')

*  Loop over lines

      do line = 1, line_count
        do xsect = 1, nxp
          call decode_status(ncntrl,fitsta(1,line,xsect),deccntr)

*     Is there a valid fit status?

          cstat = deccntr(FIT_STAT)

*        Was fit succesful (which may include NAG errors)?

          if (((cstat.eq.1).or.((cstat.eq.2).and.(keep_nag_err))).and.
     :            (deccntr(FIT_NCMP).eq.1)) then
            block_cen = results(posbc,line,xsect)

*           Are we at the start of a new fit?

            if(xsect.eq.nint(block_cen)) then
              xint = results(poscen,line,xsect)

*             Is the centre of the fit in range (defined by axis array)?

              if((xint.ge.xarr(1)).and.(xint.le.xarr(nchans))) then

*              All okay! We'll use the fit

                npts(line) = npts(line) + 1
                y(npts(line),line) = dble(block_cen)
                x(npts(line),line) = dble(xint)
                err(npts(line)) = sqrt(resvar(poscen,line,xsect))
              end if
            end if
          end if
        end do

* Work out limits for Chebyshev fits, and sort out weighting

        ylim(1,line) = y(1,line)
        ylim(2,line) = y(npts(line),line)
        if(npts(line).gt.0) then
          call weight_fit(err,npts(line),w(1,line),.true.)
          if(arc(1,line) .eq. ARC_NO_FITS) then
             arc(1,line) = ARC_ORIG
          endif
        else
          write(chars,'(''No points for line'',i4)')line
          call par_wruser(chars,status)
          arc(1,line) = ARC_NO_FITS
        end if
      end do
      end
