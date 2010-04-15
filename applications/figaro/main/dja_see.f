      subroutine dja_see(iblock,xstart,xend,nfit,line,line_name,instant
     :     ,nnew,nold,nfailed,maskedout,deccntr,status)
*+
* Name:
*    DJA_SEE

* Invocation:
*    CALL DJA_SEE(IBLOCK,XSTART,XEND,NFIT,LINE,LINE_NAME,INSTANT
*                 ,NNEW,NOLD,NFAILED,MASKEDOUT,DECCNTR,STATUS)

* Purpose:
*   Look at sub-sections of current window

* Description:
*   Run through the elements of a block individually allowing them
*   to be inspected,averaged in subgroups and enabling then to
*   be fitted .
*
* Arguments:
*    IBLOCK = INTEGER (Given)
*        current block number(window number)
*    XSTART = REAL (Given)
*        current start of WINDOW IN x-sections
*    XEND = REAL (Given)
*        current end   "   "     "     "
*    NFIT = INTEGER (Given)
*        number of current fits requested to date
*    LINE = INTEGER (Given)
*        current line number
*    LINE_NAME(LINE) = CHARACTER*10 ARRAY (Given)
*        line names
*    INSTANT = INTEGER (Given)
*        fits to be done now
*    NNEW = INTEGER (Given)
*
*    NOLD = INTEGER (Given)
*
*    NFAILED = INTEGER (Given)
*
*    MASKEDOUT = INTEGER (Given)
*
*    DECCNTR(*) = INTEGER ARRAY (Given)
*        Fit model
*    STATUS = INTEGER (Given)
*
* Global variables:
*    D_TLPTR = INTEGER (Given)
*        Pointer to channels bound of current line (include file
* arc_dims)
*    D_TRPTR = INTEGER (Given)
*        Pointer to channels bound of current line (include file
* arc_dims)
*    D_CPTR = INTEGER (Given)
*        Pointer to control array (include file arc_dims)
*
* Authors:
*   DJA: D.J.Axon Manchester
*   TNW: T.N.Wilkins Manchester until 1/89, then Cambridge until 9/92

* History:
*   DJA: Original version
*   TNW: 1/11/89 Call QMENU directly
*   TNW: Changes for new results structure 1-8-JUL-1991
*   TNW: DECCNTR added to argument list, now use set_fit_menu, control
*        removed from argument list.
*   TNW: 11/2/94 Use qmenu to get limits for group, also bug fix on next
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      include 'arc_dims'
      integer line
      character*10 line_name(line)
      real xstart
      real xend
      integer nfit
      integer iblock
      logical instant
      integer nnew,nold,nfailed
      integer maskedout

* local

* do loop

      integer i
      integer len2
      logical qstat

* do loop control

      logical look

* current xsect start

      integer ixstart

* current xsect end

      integer ixend

* value recieved from par_qnum

      real value

* "     "       "     "

      integer see_nwindow

* menu choice

      integer key
      include 'status_inc'

* "       "    fit status

      integer fit_status(MAX_CONTROL)
      logical redraw
      integer pstat

* external refernces


* returns a real number

      logical par_qnum

      integer dumi
      real values(2)
      character dumc
      integer NDICT
      parameter (NDICT = 5)
      character*47 dict_see(NDICT)
      integer OPT_FIT, OPT_NEXT, OPT_LAST, OPT_GROUP
      parameter (OPT_FIT = 1, OPT_NEXT = 2, OPT_LAST = 3,
     :     OPT_GROUP = 4)

      data dict_see/
     :     'FIT         : Set fit for this X-section',
     :     'NEXT        : Display next X-section',
     :     'LAST        : Display previous X-section',
     :     'GROUP %Fstart %Fend : Add a range of X-sections',
     :     'EXIT        : Return to Window Menu'/
* ---------------------------------------------------------------------
      look=.true.

* set up to SEE in individual crossections

      qstat=par_qnum('Enter start Xsection Number',xstart,xend,xstart
     :     ,.true.,' ',value)
      ixstart = nint(value)
      ixend = ixstart

      redraw=.true.
      do while(look)

* test to see if end of current WINDOW has been reached

         if (redraw) then
            if(ixend.gt.nint(xend)) then
               ixend = nint(xend)
            end if

*     concatonate to get TITLE and SUBTITLE

            call chr_fill(' ',title)
            len2 = 0
            call chr_putc('SEE : Window number = ',title,len2)
            call chr_puti(iblock,title,len2)
            len2 = 0
            call chr_fill(' ',legend(1))
            call chr_putc('Xsect',legend(1),len2)
            call encode_range(' ','s',ixstart,ixend,legend(1),len2)
            legend(2) = line_name(line)

            call par_wruser(title,pstat)
            call par_wruser(legend(1),pstat)
*
* get integral over the window
*
            call fig_xtract(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,
     :                      ixstart,ixend,%VAL(CNF_PVAL(d_vsptr)))

* display the window

            call disp_window(%VAL(CNF_PVAL(d_tlptr)),
     :                       %VAL(CNF_PVAL(d_trptr)),line,
     :                       %VAL(CNF_PVAL(d_xptr)),
     :                       %VAL(CNF_PVAL(d_vsptr)),wavdim)

*   redraw

         end if
         redraw=.true.

* offer the options

         call qmenu('See Menu',dict_see,NDICT,OPT_FIT,values,dumc,key,
     :        dumi,status)

* exit

         if((key.eq.NDICT) .or. (status.ne.SAI__OK)) then
            look=.false.

*  fit

         else if(key.eq.OPT_FIT) then

* get the model type

            call set_fit_menu(redraw,instant,deccntr,wavdim,gpscal
     :           ,prvfit,usepeak,bimtst,tyaic,curmcmp,prvpos,mgauss
     :           ,line_count,errpre,inherit,status)
            if(redraw) then
               call disp_window(%VAL(CNF_PVAL(d_tlptr)),
     :                          %VAL(CNF_PVAL(d_trptr)),line,
     :                          %VAL(CNF_PVAL(d_xptr)),
     :                          %VAL(CNF_PVAL(d_vsptr)),wavdim)
            end if

* encode into control

            call encode_contrl(deccntr,ncntrl,fit_status)
            do i = ixstart,ixend
               call set_control(%VAL(CNF_PVAL(d_cptr)),line,i,1,
     :                          fit_status)
            end do
            nfit = nfit + 1
            if(instant) then

* proceede with model fit

               see_nwindow=ixend-ixstart+1
               call one_line(see_nwindow,%VAL(CNF_PVAL(d_tlptr)),
     :                       %VAL(CNF_PVAL(d_trptr)),ixstart,ixend,nfit,
     :                       line_name,%VAL(CNF_PVAL(d_cptr)),line,nnew,
     :                       nold,nfailed,maskedout,.false.,.true.,
     :                       redraw,1,1,status)
            else
               redraw=.false.
            end if

* group

         else if(key.eq.OPT_GROUP) then

            if((values(1).lt.xstart).or.(values(1).gt.xend)) then
               call par_wruser('Start outside range',pstat)
               redraw=.false.
            else if((values(2).lt.values(1)).or.(values(2).gt.xend))
     :              then
               call par_wruser('End outside range or less than start'
     :              ,pstat)
               redraw=.false.
            else
               ixstart = nint(values(1))
               ixend = nint(values(2))
            end if

* next

         else
            if(key.eq.OPT_NEXT) then
               ixstart = ixstart + 1
               if(ixstart .gt. nint(xend) ) then
                  ixstart = nint(xstart)
                  call par_wruser('SEE> Scrolled to Start of WINDOW'
     :                 ,pstat)
               end if

*  last

            else if(key.eq.OPT_LAST) then
               ixstart = ixstart - 1
               if(ixstart .lt. nint(xstart)) then
                  ixstart = nint(xend)
                  call par_wruser('SEE> Scrolled to end of WINDOW',pstat
     :                 )
               end if
            end if
            ixend   = ixstart
         end if
      end do
      end
