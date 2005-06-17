      subroutine table(results,nagerr,hex,status)
*+
* Name:
*    TABLE

* Invocation:
*    CALL TABLE(RESULTS,NAGERR,HEX,STATUS)
*
* Purpose:
*   Produce table of results

* Description:
*    To find and extract results from the data cube. The results are
*   output to a file.
*
*   (Nwindow is the number of x-sections the data was averaged over,
*   i.e. the data was averaged over first_xsect to
*   first_xsect-1 + nwindow).

* Arguments:
*    RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results "cube"
*    NAGERR = LOGICAL (Given)
*        If to show fits with NAG errors
*    HEX = LOGICAL (Given)
*        If hex data
*    STATUS = INTEGER (Given and returned)
*        Error status
*
* Global variables:
*    IDSPTR,IDSEND = INTEGER (Given)
*        "Pointers" to names of lines (in arc_dims)
*    D_WPTR = INTEGER (Given)
*        Pointer to wavelengths of lines (in arc_dims)

* Authors:
*  T.N.Wilkins Manchester until 1/89, Cambridge until 9/92

* History:
*  Use of DSA_GET/FREE_LU TNW 4/11/88
*  TNW: 29-JUN-1989 Made to use CHR routines.
*  TNW:T.N.Wilkins Cambridge, 17-JUL-1989 Made to use get_parnum
*  TNW: 21-MAR-1991 Simpler check for new fits
*  TNW: 15-APR-1991 Changed to use ARCDIMS
*  TNW: 11-JUN-1992 LINE_NAME removed from call
*- ---------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      real results(mxpars,nyp,spdim1,spdim2)
      logical nagerr,hex
      integer status
      include 'status_inc'
      integer line
      integer ix,iy
      integer lu
      integer midx,midy
      integer ppos1,ppos2
      logical par_quest
      integer chr_len
      logical indic_fails,ok
      integer get_parnum,len1
      real value
      character*41 date,chars*80

* Check if fits with NAG errors are to have their fit parameters output
* (any fit attempted will have its fit_status decoded and the relevant
* information output, even if the fit failed).

      indic_fails = par_quest('Indicate presence of failed fits',
     :     .false.)

* by the way, date here is ignored

      call chr_fill(' ',chars)
      len1 = 0
      call chr_appnd(datafile,chars,len1)
      call chr_putc('.res',chars,len1)
      call dsa_open_text_file(chars,' ','new',.true.,lu,date,
     :        status)
      if(status.ne.SAI__OK) return

      write(lu,10) datafile(:chr_len(datafile))
 10   format(2x,'LINE FITS CONTAINED IN FILE ',2a)
      write(lu,11)
 11   format(2x,'Fits made by package TWODSPEC')
      call get_date(date)
      write(lu,12) date(:chr_len(date))
 12   format(2x,'(print out made on ',a,')')

      if(spdim1.gt.1) ppos1 = get_parnum('Space1_pos')
      if(spdim2.gt.1) ppos2 = get_parnum('Space2_pos')

      do line = 1, line_count
        do iy = 1,spdim2
          do ix = 1,spdim1

* Check if fit different from previous x-sect

            ok = .true.
            if(spdim1.gt.1) then
              value = results(ppos1,line,ix,iy)
              ok = value.ne.VAL__BADR
              if(ok) then
                midx = nint(value)
                ok = ix.eq.midx
                if(ok.and.(spdim2.gt.1)) then
                  value = results(ppos2,line,ix,iy)
                  ok = value.ne.VAL__BADR
                  if(ok) then
                    midy = nint(value)
                    ok = iy.eq.midy
                  end if
                end if
              end if
            end if

            if(ok) then

*         Output fit results to file

              call output_fit(%VAL(CNF_PVAL(staptr)),results,
     :                        %VAL(CNF_PVAL(d_vptr)),ix,iy,line,deccntr,
     :                        lu,nagerr,indic_fails,idstring,
*     :             indic_fails,dynamic_chars(idsptr:idsend),
     :                        %VAL(CNF_PVAL(d_wptr)),hex)

*     new fit

            end if

*   over position

          end do
        end do

* over lines

      end do
      call dsa_free_lu(lu,status)
      end
