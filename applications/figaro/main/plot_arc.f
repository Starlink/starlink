      subroutine plot_arc(sdens,arc,wavelength,results,status)
*+
* Name:
*    PLOT_ARC

* Invocation:
*    CALL PLOT_ARC(SDENS,ARC,WAVELENGTH,RESULTS,STATUS)

* Purpose:
*   Plot arc spectrum with lines identified

* Description:
*   To produce a plot of the arc with the wavelengths of the lines used
*  in the fit marked beside them.
*
* Arguments:
*    SDENS(WAVDIM) = REAL ARRAY (Workspace)
*        Y data
*    ARC(NSLCT,LINE_COUNT) = INTEGER*2 ARRAY (Given)
*        Line useage array
*    WAVELENGTH(LINE_COUNT) = REAL ARRAY (Given)
*        Wavelengths of lines
*    RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results array
*    STATUS = INTEGER (Given and returned)
*        Global status
* Global variables:
*    SPDIM1 = INTEGER (Given)
*        Number of cross-sections in data (include file arc_dims)
*    D_SPTR = INTEGER (Given)
*        Pointer to intensity array (include file arc_dims)
*    D_XPTR = INTEGER (Given)
*        Pointer to X data (include file arc_dims)
*    XLABEL = CHARACTER*(*) (Given)
*        X label (include file arc_dims)
*    MXPARS = INTEGER (Given)
*        Dimension of results (include file arc_dims)
*    NYP = INTEGER (Given)
*            "     "     " (include file arc_dims)
*    NXP = INTEGER (Given)
*            "     "     " (include file arc_dims)
*    WAVDIM = INTEGER (Given)
*        Number of channels in data (include file arc_dims)
*    LINE_COUNT = INTEGER (Given)
*        Number of lines identified (include file arc_dims)
*
* Author:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92 then
*           Durham
*   ACD: A C Davenhall Edinburgh
*
* History:
*   TNW: 3-SEP-1991 Use common as much as possible
*   TNW: 2-AUG-1993 Use rx2chn so works with x axis not 1,2,3,...
*   TNW: 14-OCT-1993 New version with totally cursor control etc.
*   TNW: 19-APR-1994 Dimensions of ARC corrected
*   ACD: 19-DEC-2000 Added the missing final argument STATUS.
*-
      implicit none
      include 'SAE_PAR'
      include 'arc_dims'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      real results(mxpars,nyp,nxp)
      integer*2 arc(nslct,line_count)
      real sdens(wavdim)
      real wavelength(line_count)

* Local

      integer midpos
      integer chr_len,len1
      character*60 label
      real y1,x1
      real chan
      integer ichan
      integer itemp,llen,oldlen
      integer i,get_parnum,ixstart,ixend,rx2chn,pgcurse,nplot,j
      character*15 chars
      character key,chr_upper,lkey
      logical iloop,loop,edit

      midpos=max((spdim1/2),1)
      ixstart=max((midpos-5),1)
      ixend=min((midpos+5),spdim1)
      call fig_xtract(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,ixstart,
     :                ixend,sdens)

* Initialise title to name of data file

      label = datafile(:60)
      call gr_soft(status)
      loop = status.eq.SAI__OK
      nplot = 1
      do while(loop)
         do j = 1, nplot
            if(j.eq.2) call gr_soft(status)
            call plot_spect(wavdim,%VAL(CNF_PVAL(d_xptr)),sdens,
     :           label(:chr_len(label)),xlabel(:chr_len(xlabel)),' ')

* Put wavelengths by lines

            do i=1,line_count
               if(arc(1,i).eq.0) then
                  chan = results(get_parnum('Centre_1'),i,midpos)
                  x1 = chan
                  ichan = rx2chn(%VAL(CNF_PVAL(d_xptr)),wavdim,chan)

* Check that value of sdens is not higher to one side of fitted peak

                  itemp = ichan
                  if(sdens(ichan-1).gt.sdens(ichan)) itemp = ichan-1
                  if(sdens(ichan+1).gt.sdens(itemp)) itemp = ichan+1
                  y1 = sdens(itemp)*1.01
                  len1 = 0
                  call chr_putr(wavelength(i),chars,len1)
                  call pgptext(x1,y1,90.0,0.0,chars(:len1))
               end if
            end do
         end do
         call pgmtxt('t',1.0,1.0,1.0,'Hit ? for help')
         iloop = .true.
         nplot = 1
         do while(iloop)
            status = pgcurse(x1,y1,key) - 1
            key = chr_upper(key)
            if(key.eq.'H') then

*   Hardcopy

               call gr_hard(status)
               iloop = .false.
               nplot = 2
            else if(key.eq.'?') then

*   Help

               call pgenv(0.0,1.0,0.0,1.0,0,-1)
               call pgtext(0.1,0.9,'H - Make hardcopy of plot')
               call pgtext(0.1,0.8,'E - Edit title ("QQ" to end)')
               call pgtext(0.1,0.7,'Q - Quit')
               call pgtext(0.1,0.1,'Hit any key to return to plot')
               status = pgcurse(x1,y1,key) - 1
               iloop = .false.
            else if(key.eq.'Q') then

*  Quit

               loop = .false.
               iloop = .false.
            else if(key.eq.'E') then

*  Edit title

               edit = .true.
               llen = chr_len(label)
               lkey = ' '
               do while(edit)
                  status = pgcurse(x1,y1,key) - 1
                  call pgsci(0)
                  call pgmtxt('t',2.0,0.5,0.5,label(:llen))
                  if((key.eq.'Q').and.(lkey.eq.'Q')) then
                     label(llen:llen) = ' '
                     edit = .false.
                     llen = oldlen
                  else if(ichar(key).eq.127) then
                     label(llen:llen) = ' '
                     llen = llen - 1
                  else if(llen.lt.len(label)) then
                     oldlen = llen
                     llen = llen + 1
                     label(llen:llen) = key
                  else
                     call par_wruser('String too long',status)
                     oldlen = llen
                  end if
                  call pgsci(1)
                  call pgmtxt('t',2.0,0.5,0.5,label(:llen))
                  lkey = key
               end do
            else
               call par_wruser('Invalid key hit',status)
            end if
         end do
      end do
      end
