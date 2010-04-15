      subroutine windplot(line,xstart,xend,ystart,yend,line_name,
     :     nwindx,nwindy,iblock,jblock,wait,samblk,istartx,iendx,
     :     istarty,iendy,ptitle,status)
*+
* Name:
*    WINDPLOT

* Invocation:
*    CALL WINDPLOT(LINE,XSTART,XEND,YSTART,YEND,LINE_NAME,
*          NWINDX,NWINDY,IBLOCK,JBLOCK,WAIT,SAMBLK,ISTARTX,IENDX,
*          ISTARTY,IENDY,PTITLE,STATUS)

* Purpose:
*    To plot the current window of the data

* Description:
*     The data for the current location is extracted and plotted. If a
*     fit has already been made at that location (for that blocking),
*     this may be plotted.

* Arguments:
*      D_XPTR = INTEGER (Given)
*        Pointer to X array data (inc common)
*      D_VSPTR = INTEGER (Given)
*        Pointer to intensity data (in common)
*      LINE = INTEGER (Given)
*        current line
*      XSTART = INTEGER (Given)
*        Starting cross-section limit
*      XEND = INTEGER (Given)
*        End cross-section limit
*      YSTART = INTEGER (Given)
*        Starting Y limit
*      YEND = INTEGER (Given)
*        End Y limit
*      LINE_NAME(NYP) = CHARACTER*10 ARRAY (Given)
*        Line names
*      NWINDX = INTEGER (Given)
*        Width of current window (cross-sections)
*      NWINDY = INTEGER (Given)
*        Width of current window (Y)
*      IBLOCK = INTEGER (Given)
*        Current block number (X)
*      IBLOCK = INTEGER (Given)
*        Current block number (Y)
*      WAIT = LOGICAL (Given)
*        If to wait 3 seconds before plotting
*      PTITLE = CHARACTER*(*) (Given)
*        Plot title
*      PLTOLD = LOGICAL (Given and returned)
*        If to plot old fits (in common ARCDIMS include file)
*      SAMBLK = LOGICAL (Given and returned)
*
*      STATUS = INTEGER (Given and returned)
*        Error status
*      ISTARTX = INTEGER (Returned)
*        Start cross-section displayed
*      IENDX = INTEGER (Returned)
*        End cross-section displayed
*      ISTARTY = INTEGER (Returned)
*        Start position displayed in Y
*      IENDY = INTEGER (Returned)
*        End position displayed in Y
* Common referenced:
*     arc_dims
* Subroutines/functions referenced:

* Author:
*   TNW: T.N.Wilkins, Cambridge

* History:
*   TNW: 12-OCT-1989, original version
*   TNW: 14-AUG-1990 Changes to plot labels
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      include 'arc_dims'
      integer line
      integer xstart
      integer xend
      character*10 line_name(nyp)
      character*(*) ptitle
      integer nwindx
      integer iblock
      logical wait
      logical samblk
      integer istartx,iendx,len2
      integer istarty,iendy,ystart,yend,nwindy,jblock
      logical plot_old_fit,qstat
*

      if(status.ne.SAI__OK) return
      istartx = xstart + nwindx*(iblock-1)
      iendx   = istartx + nwindx - 1

* test to see if reached end

      iendx = min(iendx,xend)

      istarty = ystart + nwindy*(jblock-1)
      iendy   = istarty + nwindy - 1

* test to see if reached end

      iendy = min(iendy,yend)
*
* concatonate to get TITLE and SUBTITLE
*
      title = ptitle
      len2 = 0
      call chr_fill(' ',legend(1))
      if(spdim1.gt.1) then
         call chr_putc('Position X = ',legend(1),len2)
         call chr_puti(istartx,legend(1),len2)
         if(istartx.ne.iendx) then
            call chr_putc('-',legend(1),len2)
            call chr_puti(iendx,legend(1),len2)
         end if
         if(spdim2.gt.1) then
            call chr_putc(', Y = ',legend(1),len2)
            call chr_puti(istarty,legend(1),len2)
            if(istarty.ne.iendy) then
               call chr_putc('-',legend(1),len2)
               call chr_puti(iendy,legend(1),len2)
            end if
         end if
      end if
      len2 = 0
      call chr_fill(' ',legend(2))
      call chr_appnd(line_name(line),legend(2),len2)
      call chr_putc(' (Line No = ',legend(2),len2)
      call chr_puti(line,legend(2),len2)
      call chr_putc(')',legend(2),len2)
*
* get integral over the window
*
      call extr3(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,spdim2,istartx,
     :           iendx,istarty,iendy,%VAL(CNF_PVAL(d_vsptr)))

* display the window

      if(wait) call sla_wait(3.0)
      call disp_window(%VAL(CNF_PVAL(d_tlptr)),%VAL(CNF_PVAL(d_trptr)),
     :                 line,%VAL(CNF_PVAL(d_xptr)),
     :                 %VAL(CNF_PVAL(d_vsptr)),wavdim)
      if(pltold) then
         qstat = plot_old_fit(%VAL(CNF_PVAL(d_rptr)),
     :                        %VAL(CNF_PVAL(d_vptr)),line,istartx,iendx,
     :                        istarty,iendy,samblk,.false.,
     :                        %VAL(CNF_PVAL(staptr)))
      end if
      end
