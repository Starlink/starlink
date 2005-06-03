      subroutine plot_all_fits(results,nagerr,vcorr,vtype,velplt,
     :     fitsta)
*+
* Name:
*    PLOT_ALL_FITS

* Invocation:
*    CALL PLOT_ALL_FITS(RESULTS,NAGERR,VCORR,VTYPE,VELPLT,
*          FITSTA)

*
* Purpose:
*    To extract and plot results from the data cube.
*
* Description:
*    To extract and plot results from the data cube.
*
* Arguments-
*   RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results cube
*   NAGERR = LOGICAL (Given)
*        If to make plots of fits with NAG errors
*   VCORR = REAL (Given)
*        Velocity corection (to given reference frame)
*   VTYPE = INTEGER (Given)
*        Code to give reference frame
*   VELPLT = LOGICAL (Given)
*        If to use velocity scale for X axis
*   FITSTA(NCNTRL,NYP,SPDIM1,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status
* Global variables:
*      IDSPTR, IDSEND = INTEGER (Given)
*        "Pointers" to line names
*      D_WPTR = INTEGER (Given)
*        Pointer to wavelengths of lines
*      LINE_COUNT = INTEGER (Given)
*        Number of lines
*      D_VSPTR = INTEGER (Given)
*        Pointer to extracted spectrum
*      D_TLPTR = INTEGER (Given)
*        Pointer to left tram lines
*      D_TRPTR) = INTEGER ARRAY (Given)
*        Pointer to right tram lines
*      D_XPTR = INTEGER (Given)
*        Pointer to X array data
*      XUNITS = CHARACTER*(*) (Given)
*        X units for plot
*      TITLE = CHARACTER*(*) (Given)
*        Title for plot
*      LEGEND(2) = CHARACTER*(*) ARRAY (Given)
*        Legends for plot
*      SPDIM1 = INTEGER (Given)
*        Number of cross-sections in data
*      WAVDIM = INTEGER (Given)
*        Number of channels in data
*      D_SPTR = INTEGER (Given)
*        Pointer to main file data array
*
* History:
*   Changed to use GET_PREVFIT, TNW 24/11/88
*   Changed to use GET_PARNUM, TNW/CAVAD 19/7/89
*   Less prompting in this routine TNW 3/1/91
*   Simpler check for new fits TNW 21-MAR-1991
*   New results structures, TNW 1-8/7/91
*   TNW/Cambridge 11-JUN-1992 LINE_NAME removed from call
*   TNW/Durham, 17-MAY-1993 Don't look for Space?_Pos if spdim? = 1
*-
      implicit none
      include 'SAE_PAR'
      integer status
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      real results(mxpars,nyp,spdim1,spdim2)
      integer fitsta(ncntrl,nyp,spdim1,spdim1)
      logical nagerr
      real vcorr
      integer vtype
      logical velplt
* ----------------------------------------------------------------------
      integer line
      integer ix,iy
      integer cstat
      integer midx,midy
      integer chr_len
      integer get_parnum,pos1,pos2
      real value
      logical ok
      include 'status_inc'
      character dynamic_chars
      include 'DYNAMIC_MEMORY'
      equivalence (dynamic_chars,dynamic_mem)


      status = SAI__OK

      if(spdim1.gt.1) then
        pos1 = get_parnum('Space1_pos')
      else
        pos1 = 1
      endif
      if(spdim1.gt.2) then
        pos2 = get_parnum('Space2_pos')
      else
        pos2 = 1
      endif

      do line = 1, line_count
        do iy = 1, spdim2
          do ix = 1, spdim1

* Check if fit different from previous x-sect

            ok = .true.
            value = results(pos1,line,ix,iy)
            if(value.gt.0.0) then
              midx = nint(value)
              ok = midx.eq.ix
            end if
            if(ok.and.(spdim2.gt.1)) then
              value = results(pos2,line,ix,iy)
              if(value.gt.0.0) then
                midy = nint(value)
                ok = midy.eq.iy
              end if
            end if

            if(ok) then

* Get and decode fit_status

              call decode_status(ncntrl,fitsta(1,line,ix,iy),deccntr)
              cstat = deccntr(FIT_STAT)
              if((cstat.eq.1).or.((cstat.eq.2).and.nagerr)) then

*    Set up labels

                title='File '//datafile(:chr_len(datafile))

                call plot_line(line,.false.,vcorr,
     :               %VAL( CNF_PVAL(d_wptr) ),
*     :               ix,iy,velplt,vtype,dynamic_chars(idsptr:idsend),
     :               ix,iy,velplt,vtype,idstring,
     :               results,%VAL(d_vptr),status)

*         fit a success or nag error


              end if
*     new fit

            end if

*   over position

          end do
        end do

* over lines

      end do
      end
