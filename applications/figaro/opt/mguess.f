      subroutine mguess(inst,guess,sdata,sdens,xlim,ncomp,ngauss,
     :     ifbpen,diags,resid,fit_values,fit_total,plot_flag,
     :     xunits,title,legend,vbase,scaled_pars,funct,iopt,status)
*+
* Name:
*    MGUESS

* Invocation:
*    CALL MGUESS(INST,GUESS,SDATA,SDENS,XLIM,NCOMP,NGAUSS,
*          IFBPEN,DIAGS,RESID,FIT_VALUES,FIT_TOTAL,PLOT_FLAG,
*          XUNITS,TITLE,LEGEND,VBASE,SCALED_PARS,FUNCT,IOPT,STATUS)

* Purpose:
*   Allow user to set/alter guesses for manual fitting

* Description:
*   Routine for getting the user to input guesses for the NAG fitting
*   routine to use for fitting multiple gaussians.
*   Prompt the user to change the position, height and width of the
*   gaussian to achieve a visually better fit
*   The terminal is used to alter the parameters of the fitting
*   Gaussians and the following symbols are allowed:
*   Height: increase - U  and decrease - D  ( up and down )
*   Position:  increase - R  and  decrease - L  ( right and left )
*   Width:  increase - W and decrease - N  ( wide and narrow )
*   There is also the option of setting the parameters using a cursor.
*   The altered gaussian is then replotted, unless overwriteing is not
*   possible, in which case it is replotted only as required by the user
* .
*
* Arguments:
*    INST = REAL (Given)
*      Instrumental resolution
*    SDATA(M) = REAL ARRAY (Given)
*      X array data (for region being dealt with)
*    SDENS(M) = REAL ARRAY (Given)
*      Extracted spectrum (for part required)
*    XLIM(2) = REAL ARRAY (Given)
*      Limits of line (trams)
*    IFBPEN = LOGICAL (Given)
*      If a blank pen is defined
*    DIAGS(2) = INTEGER ARRAY (Given)
*      DIAGRAM zones for plots
*    TIMES = INTEGER (Given)
*      Number of guesses (attempts!)
*    PLOT_FLAG = INTEGER (Given)
*      Flag to indicate if re-plotting e.t.c. required
*                    - if =2 then redraw whole plot
*                    - if =1 then add extra gaussian
*                    - if =0 then leave as it is
*    NGAUSS = INTEGER (Given)
*      Number of gaussians
*    XUNITS = CHARACTER*(*) (Given)
*      X units for display
*    TITLE = CHARACTER*(*) (Given)
*      Not used-required for calls to multi_resid
*    LEGEND(2) = CHARACTER*60 ARRAY (Given)
*      Not used-required for calls to multi_resid
*    VBASE(M) = REAL ARRAY (Given)
*      Variable component of base
*    MAX_CMP = INTEGER (Given)
*      Maximum number of components
*    FUNCT = REAL (Given)
*      Function to evaluate model
*    GUESS(MAX_PARS,MAX_CMP,TIMES) = REAL ARRAY (Given and returned)
*      Store for guesses
*    NCOMP = INTEGER (Given and returned)
*      Number of component being worked on
*    RESID(M) = REAL ARRAY (Given and returned)
*      Residuals
*    FIT_VALUES(M,NGAUSS) = REAL ARRAY (Given and returned)
*      Values of fits for individual gaussians
*    FIT_TOTAL(M) = REAL ARRAY (Given and returned)
*      Total value of fit (all gaussians + base)
*    IOPT = INTEGER (Returned)
*      Option for menu above. 16 - add, 17 - list, 18 - fit
*    STATUS = INTEGER (Given and returned)
*      Non-zero if an error occured
*    SCALED_PARS(*) = REAL ARRAY (Workspace)
*      Scaled parameters
* Global variables:
*    DENSC = DOUBLE PRECISION (Given)
*      Density scale factor (include file opt_cmn)
*    DATSC = DOUBLE PRECISION (Given)
*      X array scale factor (include file opt_cmn)
*    DATAZERO = DOUBLE PRECISION (Given)
*      Zero point for scaling X array (include file opt_cmn)
*    DENSZERO = DOUBLE PRECISION (Given)
*      Zero point for scaling density array (include file opt_cmn)
*    MPTS = INTEGER (Given)
*      Number of points in range (include file opt_cmn)

* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92 then Durham

* History:
*   TNW: Present version
*   Name of dia_seld changed to gr_seld, 8/7/88
*   ERROR Argument added TNW 16/12/88
*   MGAUSS argument added, TNW 18/1/89
*   SCALED_PARS dimensioned in terms of MAX_PARMS TNW 19/1/89
*   & made argument 24/1/89, together with removal of opt include file
*   TNW: 3/90 PGPLOT version
*   TNW: 16/4/91 DENSZERO added to argument list
*   TNW: 28/8/91 Array IFCHNG added, cursor mode extended
*   TNW: 3/12/91 Diagram "zones" passed as 1 argument.
*   TNW: 21/8/92 Changed so functions gaussian/lorentz passed around
*        as arguments (external), opt_cmn now used here.
*   TNW: 8th Sept 1992 max_cmp passed in common
*   TNW: 23/2/94 Added pgbbuf/pgebuf
*   TNW: 8,9/3/94 More can be handled by KEY_MENU
*-
      implicit none
      include 'opt_cmn'
      include 'SAE_PAR'
      integer diags(3)
      integer ncomp,ngauss
      integer plot_flag
      real funct
      external funct
      real sdata(mpts),sdens(mpts)
      real xlim(2)
      real vbase(mpts)
      integer status
      real resid(mpts),fit_values(mpts,ngauss),fit_total(mpts)
      integer MAX_PARS
      parameter (MAX_PARS=4)
      real guess(MAX_PARS,max_cmp,times)
      real scaled_pars(*)
      logical loop
      character*(*) xunits,title
      character*60 legend(2)
      real L_DEF,W_DEF,H_DEF
      logical valid
      integer pstat
      integer iopt
      real factor
      parameter (L_DEF = 2000.)
      parameter (H_DEF = 50.)
      parameter (W_DEF = 3.)
      real hstep
      real lstep
      real wstep
      real sigma
      integer i
      real lamy,inst
      real sgpars(4)
      logical ifbpen,curupd,anytrue
      logical replot
      integer NICON
      parameter (NICON = 10)
      real yrange(3),value,value2,icons(4,NICON)
      save yrange
      integer new_line
      character*29 chars
      logical ifchng(0:9)
      real EFOLD
      parameter (EFOLD=2.35482004)
      logical resid_flag
      integer OPT_UP, OPT_DOWN, OPT_RIGHT, OPT_LEFT, OPT_WIDER,
     :     OPT_NARROW, OPT_REPLOT, OPT_NEWCMP, OPT_CURSOR,
     :     OPT_BASEUP, OPT_BASEDO, OPT_ADD, OPT_CHANGE, OPT_DELETE
      parameter (OPT_UP = 1, OPT_DOWN = 2, OPT_RIGHT = 3, OPT_LEFT = 4,
     :     OPT_WIDER = 5, OPT_NARROW = 6, OPT_REPLOT = 7,
     :     OPT_NEWCMP = 9,OPT_CURSOR = 10, OPT_BASEUP = 11,
     :     OPT_BASEDO = 12, OPT_ADD = 16, OPT_CHANGE = 8,
     :     OPT_DELETE = 21)

* set up the default step sizes for changing the
* first guesses

      hstep =  1.0/H_DEF
      lamy = xlim(2)-xlim(1)
      lstep = lamy/L_DEF
      wstep = inst/W_DEF

* loop over the options

      loop = .true.
      resid_flag=.false.
      if(plot_flag.eq.2) then

*  Plot data and guesses to fit

         call pgbbuf
         call pgpage

*  Draw icons

         call gr_seld(diags(3),status)
         call pgwindow(0.0,1.0,0.0,real(NICON))
         do i = 1, NICON
            icons(1,i) = 0.1
            icons(2,i) = 0.9
            icons(3,i) = real(NICON-i) + 0.1
            icons(4,i) = icons(3,i) + 0.8
            call profwarr(0.1,icons(3,i),0.8,i)
         enddo

         call gr_seld(diags(1),status)
         call disp_window2(xlim(1),xlim(2),sdata,sdens,mpts,xunits,
     :        .false.)
         call rescale_store(scaled_pars,guess,ngauss,times,4)
         call comp_plot(scaled_pars,ngauss,mpts,fit_values,sdata,yrange
     :        ,funct)
         call pgebuf
         resid_flag=.true.

* update existing plot, allowing for added gaussian.

      else if(plot_flag.eq.1) then
         call pgbbuf
         call gr_spen(0)
         call pgline(mpts,sdata,fit_total)
         call gr_seld(diags(2),status)
         call pgbin(mpts,sdata,resid,.true.)
         call gr_seld(diags(1),status)
         call gr_spen(3)
         sgpars(2)=guess(2,ncomp,times)*real(datsc)
         sgpars(3)=guess(3,ncomp,times)*real(densc)
         sgpars(4)=guess(4,ncomp,times)*real(datsc)+real(datazero)
         if(sgpars(3).gt.0.0) then
            sgpars(1) = yrange(1)
         else
            sgpars(1) = yrange(2)
         end if
         call plot_gaussian(sgpars,4,mpts,fit_values,sdata,ngauss,funct)
         scaled_pars(1)=guess(1,ncomp,times)*real(densc) + real(denszero
     :        )
         call pgebuf
      end if
      if(plot_flag.ge.1) then
         call pgbbuf
         call multi_plot(scaled_pars,ngauss,mpts,fit_values,fit_total,
     :        sdata,4,vbase,yrange)

*  Plot residuals

         call gr_seld(diags(2),status)
         call multi_resid(sdata,sdens,mpts,resid,fit_total,resid_flag,
     :        .true.,title,legend)
         call pgebuf
      end if
      curupd = .false.

      do while(loop)
 1       continue

* Select main zone

         call gr_seld(diags(1),status)
         replot=.false.
         if(.not.curupd) then
            call key_menu(iopt,factor,icons,diags,status)
         end if

*   Nothing changed yet!

         do i = 0, ngauss
            ifchng(i) = .false.
         end do
         if(status.ne.SAI__OK) then

            loop = .false.


*   increase height

         else if(iopt.eq.OPT_UP) then
            guess(3,ncomp,times) = guess(3,ncomp,times) +
     :           (factor*hstep)
            ifchng(ncomp) = .true.


*   decrease height

         else if(iopt.eq.OPT_DOWN) then

            guess(3,ncomp,times) =
     :           guess(3,ncomp,times) - (factor*hstep)
            ifchng(ncomp) = .true.


*   increase centre

         else if(iopt.eq.OPT_RIGHT) then

            guess(4,ncomp,times) =
     :           guess(4,ncomp,times)  + (factor*lstep)
            ifchng(ncomp) = .true.


*   decrease centre

         else if(iopt.eq.OPT_LEFT) then

            guess(4,ncomp,times) =
     :           guess(4,ncomp,times) - (factor*lstep)
            ifchng(ncomp) = .true.


*   increase width

         else if(iopt.eq.OPT_WIDER) then

            guess(2,ncomp,times) =
     :           guess(2,ncomp,times) + (factor*wstep)

            ifchng(ncomp) = .true.

*   decrease width

         else if(iopt.eq.OPT_NARROW) then

            sigma=guess(2,ncomp,times)-(factor*wstep)
            ifchng(ncomp) = .true.

*     Width cannot be less then 2*resolution

            if ((sigma*EFOLD).le.inst) then
               call par_wruser('Too narrow, set to minimum',pstat)
               sigma=inst/EFOLD
            endif
            guess(2,ncomp,times)=sigma


*   Refresh plot

         else if(iopt.eq.OPT_REPLOT) then

            replot=.true.

*   Select nearest component to cursor

         else if((iopt.eq.OPT_CHANGE).or.(iopt.eq.OPT_DELETE)) then

            ncomp = 1
            value = abs(guess(4,1,times)*real(datsc)+real(datazero)
     :           -factor)
            do i = 2, ngauss
               value2 = abs(guess(4,i,times)*real(datsc)+real(datazero)
     :              -factor)
               if(value2.lt.value) then
                  value = value2
                  ncomp = i
               endif
            enddo
            if(iopt.eq.OPT_DELETE) loop = .false.

         else if(iopt.ge.OPT_ADD) then

*   Other options dealt with by routine above this one

            loop = .false.

*   change component being worked on

         else if(iopt.eq.OPT_NEWCMP) then

            new_line=nint(factor)
            if(valid(1,ngauss,new_line,'Component')) then
               ncomp=new_line
               write(chars,'(''Component changed to'',i2)') ncomp
            end if
            call par_wruser(chars,pstat)
            go to 1


*   Set values using cursor

         else if(iopt.eq.OPT_CURSOR) then

            call cursor_set(guess,ncomp,ngauss,vbase,sdata,ifchng,curupd
     :           )

*   base up

         else if(iopt.eq.OPT_BASEUP) then

            do i=1,ngauss
               guess(1,i,times) = guess(1,i,times) + (factor*hstep)
            end do
            ifchng(0) = .true.


*   base down

         else if(iopt.eq.OPT_BASEDO) then

            do i=1,ngauss
               guess(1,i,times) = guess(1,i,times) - (factor*hstep)
            end do
            ifchng(0) = .true.

         endif
         scaled_pars(1)=guess(1,ncomp,times)*real(densc) + real(denszero
     :        )

*  Now plot the sum of gaussians and the straight line fit to the
*  background
*  Overwrite if possible

         if(loop) then
            if(ifbpen.and.(.not.replot)) then

*   If nothing has changed then we don't re-plot! With the
*   cursor option nothing may have changed.

               if(anytrue(ifchng,ngauss+1)) then

                  call pgbbuf

*   Overwrite old gaussian and sum of gaussians. If we've used the
* cursor
*   option there may be more than one component to update

                  call gr_spen(0)
                  do i = 1, ngauss
                     if(ifchng(i)) then
                        call pgline(mpts,sdata,fit_values(1,i))
                     end if
                  end do
                  call pgline(mpts,sdata,fit_total)
                  call gr_seld(diags(2),status)
                  call pgbin(mpts,sdata,resid,.true.)
                  call gr_seld(diags(1),status)
                  call gr_spen(3)
                  scaled_pars(1)=guess(1,1,times)*real(densc)
     :                 + real(denszero)
                  do i = 1, ngauss
                     if(ifchng(i)) then
                        sgpars(2)=guess(2,i,times)*real(datsc)
                        sgpars(3)=guess(3,i,times)*real(densc)
                        sgpars(4)=guess(4,i,times)*real(datsc) +
     :                       real(datazero)
                        if(sgpars(2).gt.0.0) then
                           sgpars(1) = yrange(1)
                        else
                           sgpars(1) = yrange(2)
                        end if

                        call plot_gaussian(sgpars,4,mpts,fit_values
     :                       ,sdata,i,funct)
                     end if
                  end do
                  call multi_plot(scaled_pars,ngauss,mpts,fit_values,
     :                 fit_total,sdata,4,vbase,yrange)
                  call gr_seld(diags(2),status)
                  call multi_resid(sdata,sdens,mpts,resid,fit_total,
     :                 .false.,.true.,title,legend)
                  call pgebuf
               end if
            else if(replot) then

*  Replot data

               call pgbbuf
               call pgpage

*  Draw icons

               call gr_seld(diags(3),status)
               call pgwindow(0.0,1.0,0.0,real(NICON))
               do i = 1, NICON
                  call profwarr(0.1,icons(3,i),0.8,i)
               enddo

               call gr_seld(diags(1),status)
               call disp_window2(xlim(1),xlim(2),sdata,sdens,mpts,
     :              xunits,.false.)

*  Plot gaussians

               call rescale_store(scaled_pars,guess,ngauss,times,4)
               call comp_plot(scaled_pars,ngauss,mpts,fit_values,sdata,
     :              yrange,funct)

*   Plot sum of gaussians, note back may not be 4, but effect ok

               call multi_plot(scaled_pars,ngauss,mpts,fit_values,
     :              fit_total,sdata,4,vbase,yrange)

*   Plot residuals

               call gr_seld(diags(2),status)
               call multi_resid(sdata,sdens,mpts,resid,fit_total,.true.,
     :              .true.,title,legend)
               call pgebuf
            end if
         end if
      end do

      end
