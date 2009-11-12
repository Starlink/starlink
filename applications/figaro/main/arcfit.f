      subroutine arcfit(order,results,resvar,fitsta,wavein,arc,carray,
     :     chisqmax,nfits,wavelength,val,val2,ss,a,fit_coeffs,rc,w,
     :     channel,sigy,unused_w,unused_c,polydata,accept,success,
     :     status,maxnpts)
*+
* Name:
*    ARCFIT

* Invocation:
*    CALL ARCFIT(ORDER,RESULTS,RESVAR,FITSTA,WAVEIN,ARC,CARRAY,
*           CHISQMAX,NFITS,WAVELENGTH,VAL,VAL2,SS,A,FIT_COEFFS,RC,W,
*           CHANNEL,SIGY,UNUSED_W,UNUSED_C,POLYDATA,ACCEPT,SUCCESS,
*           STATUS,MAXNPTS)

* Purpose:
*    Evaluation dispersion relation.

* Description:
*      Work out the wavelength calibration to a spectrum by performing a
*      weighted least squares polynomial fit to the arc line data held
*      in RESULTS.
*
* Arguments:
*     ORDER = INTEGER (Given)
*        Order for polynomials
*     RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results block
*     RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results block variance
*     FITSTA(NCNTRL,NYP,NXP) = INTEGER ARRAY (Given)
*        Fit status
*     POLYDATA = LOGICAL (Given and returned)
*        If polynomial data created this run of program, or if used
*     WAVEIN = REAL (Given and returned)
*        Wavelengths of lines
*     ARC(NSLCT,LINE_COUNT) = INTEGER ARRAY (Given and returned)
*        Arc usage array-see below
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*     CARRAY(11,SPDIM1) = DOUBLE PRECISION ARRAY (Returned)
*        Coefficients of fit (power series polynomial)
*     CHISQMAX = REAL (Returned)
*        Maximum value of chi-squared
*     NFITS = INTEGER (Returned)
*        Number of polynomial fits
*     MAXNPTS = INTEGER (Given)
*        Max number of data points
*     ACCEPT = LOGICAL (Returned)
*        If ACCEPT chosen (rather than QUIT)
*     WAVELENGTH(LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*     VAL(LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*     VAL2(LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*     SS(MAX_KPLUS1) = DOUBLE PRECISION ARRAY (Workspace)
*        Residual sum of squares
*     A(MAX_KPLUS1,MAX_KPLUS1) = DOUBLE PRECISION ARRAY (Workspace)
*        Stores cheby coeffs
*     FIT_COEFFS(MAX_KPLUS1) = DOUBLE PRECISION ARRAY (Workspace)
*     RC(LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*     W(LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*     CHANNEL(LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*     UNUSED_W(LINE_COUNT) = REAL ARRAY (Workspace)
*     UNUSED_C(LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*     SIGY(LINE_COUNT) = REAL ARRAY (Workspace)
*     SUCCESS = LOGICAL (Workspace)
*
* Global variables:
*     LINE_COUNT = INTEGER (Given)
*        Number of lines (include file arc_dims)
*     WAVDIM = INTEGER (Given)
*        Number of channels in data (include file arc_dims)
*     SPDIM1 = INTEGER (Given)
*        Number of cross-sections in data (include file arc_dims)
*     MXPARS,NYP,NXP = INTEGER (Given)
*        Dimensions of results block (include file arc_dims)
*     IDSPTR, IDSEND = INTEGER (Given)
*        Start and end of names of lines (include file arc_dims)
*
* Note on the array ARC:
*    0   - ok, both for continuity corrected and original data
*    1   - ok for original data only
*    4   - no fits
*    10  - deleted, if undeleted would be ok for both continuity
*          corrected and original data
*    11  - deleted, if undeleted would be ok for original data only
*
* Subroutine/functions called
*   ARCPLOT       : Plot results of fitting, line list, etc. and offer
*                   user options for this part of program
*   COPD2D        : Copy of data from one double precision array to
*                   another
*   DJA_ARCURVE   : Produce diagnostic plots
*   E_NPOLY       : Evaluate polynomial
*   WEIGHT_FIT    : Set weights for polynomial fitting from errors
*   SORT3D        : Sort wavelengths etc.
*   ZERO_DBLE     : Zero double precision array
*
*   DSA_FREE_WORKSPACE : Free virtual memory
*   DSA_GET_WORK_ARRAY : Get virtual memory
*   GEN_CHB2NO    : Convert Chebyshev polynomials to "normal"
*   GEN_REVR8     : Reverse double precision array
*   GEN_SUBAD     : Subtract 2 double precision arrays
*   PAR_WRUSER    : Write string to user
*   PAR_QNUM      : Get number from user
*   PAR_QUEST     : Get yes/no response from user

* Authors:
*   TNW: T.N.Wilkins, Manchester until 1/89, Cambridge until 9/92, then
*        Durham
*   DJA: D.J.Axon, Manchester
*   AJH: Anthony Holloway, Manchester
*   ACD: A C Davenhall, Starlink, Edinburgh

* History:
*  Various changes, including bug fix TNW 27/7/88
*  LINNAM as workspace from "above" 11/10/88 TNW
*  TNW 28/11/88 TNW Changed to use getwork and bytesdef include file
*  TNW 5/12/88 Chnaged to use more GEN_ routines and ZERO_DBLE
*  TNW 6/12/88 Changed to use COPD2D
*  DJA 27/2/89 Proper Test for Norder exceeding Kp1 added
*  TNW 30/11/89 Continuity correction results moved 11 to 12 and 12 to
*  13. This is to prevent overwriting the base error.
*  TNW 3/4/90 Various bug fixes (now will work if line 1 duff, also
*      copes with magic values).
*  TNW 8/2/91 Change to workspace, other minor changes
*  TNW 11/2/91 SUCCESS passed from above
*  TNW 26/3/91 Line name referenced by "pointers"-not argument of ARCFIT
*  TNW 17/6/92 unused_w made single precision.
*  TNW 9,10/7/92 Bug fixes
*  TNW 9/7/93 Bug fix-now use gen_elemf rather than assume start is 1
*      and end is wavdim
*  TNW 28/7/93 Change to call of sort3d
*  TNW 8/10/93 Use arcplot
*  TNW: 28/1/94 Moved 1st call to ARCPLOT out of loops
*  AJH: 1/9/97 Re-write call to fit_cpoly to expect PDA_DPOLFT return
*      of A3 matrix of co-effs. Also change of gen_cheb2no to 
*      PDA_DPCOEF to obtain Taylor series co-effs.
*  ACD: 28/9/00 Remove character strings continued across continuation
*      lines and remove local unused variables.
*  ACD: 15/12/00 Added status argument in called to PAR_WRUSER.
*-
* --------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'PRM_PAR'
      integer status
      include 'arc_dims'

*      Import

      real wavein(nyp)
      real results(mxpars,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
      integer fitsta(ncntrl,nyp,nxp)

*      Export

      integer*2 arc(nslct,line_count)
      integer order,nfits
      double precision carray(11,spdim1)
      real chisqmax
      double precision maxendw,minstartw
      logical accept

*      Local

* Integer

      include 'status_inc'
      integer get_parnum,ppos
      logical polytab
      integer fit_cpoly
      integer nag_error
      integer xsect,kp1,i,width,j,line,fline
      real rwidth
      integer lincnt
      integer unused_cnt
      integer itest
      integer ifail

* max order of polynomial

      integer MAX_KPLUS1
      integer ied
      integer ignore

* parameter

      parameter (MAX_KPLUS1 = 10)

* PDA
      integer maxnpts
      integer maxnpts2
      parameter (maxnpts2 = 2048)
*     fix incoming format
*      double precision athree(3*maxnpts + 3*max_kplus1,line_count)
      double precision a3new(3*maxnpts2 + 3*max_kplus1)

* logical

      logical loop
      logical weight
      logical par_qnum,qstat,par_quest
      logical usenagerr
      logical polydata
      logical success(line_count)

* Real

      real value
      real adisp,chisq
      real gen_elemf

* double precision

      double precision wavelength(line_count)
      double precision val(line_count)
      double precision val2(line_count)
      double precision ss(MAX_KPLUS1)
      double precision a(MAX_KPLUS1,MAX_KPLUS1)
*PDA      double precision a(3*line_count+3*MAX_KPLUS1)
      double precision fit_coeffs(MAX_KPLUS1)
      double precision xc(11),rc(line_count)
      double precision w(line_count)
      double precision channel(line_count)
      real sigy(line_count)
      real unused_w(line_count)
*     real rrc
      double precision unused_c(line_count)
      logical production
      integer slot,slot2,slot3,slot4,slot5,slot6
      integer pstat
      integer cend,cstart,ccopy
      integer len1
      integer ptr1,ptr2,ptr3,ptr4,ptr5,ptr6
*
* character
*
      character*79 chars
      data polytab,usenagerr,weight/.true.,.false.,.true./
*
      production = .false.
      nfits = 0
      maxendw = 0.0d0
      chisqmax = 0.0
      minstartw = VAL__MAXD
*
* fitting/editing loop
*
      xsect = 1
*
* We need to set initial fit parameters. Set nag_error flag to -1 to
* force arcplot to display a table (since we don't have any fits yet)
*
      nag_error = -1
      call arcplot(%VAL(CNF_PVAL(d_xptr)),%VAL(CNF_PVAL(d_vsptr)),xc,
     :             results,wavein,arc,resvar,xsect,ied,nag_error,weight,
     :             order,idstring,polydata,polytab,usenagerr,idstring,
     :             polydata,polytab,usenagerr,status)

      do while(xsect.le.spdim1)

         loop = .true.

         do while(loop)

            if(status.ne.SAI__OK) return
*
* copy over the data allowing for wavelength=-1 lines
*
            rwidth = VAL__BADR
            fline = 0
            ppos = get_parnum('Space1_pos')
            do while((rwidth.eq.VAL__BADR).and.(fline.lt.line_count))
               fline = fline + 1
               rwidth = resvar(ppos,fline,xsect)
            end do
            if(rwidth.gt.0.0) width = nint(sqrt(rwidth))
            if((width.gt.1).and.(.not.polydata)) then
               xsect = nint(results(ppos,fline,xsect))
            else
               width = 1
            end if
            unused_cnt = 0
            lincnt = 0
            do line = 1,line_count
               call decode_status(ncntrl,fitsta(1,line,xsect),deccntr)
               itest = deccntr(FIT_STAT)

* Get data from file, continuity corrected if asked for.

               if (polydata) then
                  ppos = get_parnum('Contincent')
                  if (arc(1,line).eq.ARC_OK) then
                     lincnt = lincnt + 1
                     channel(lincnt) = results(ppos,line,xsect)
                     wavelength(lincnt) = wavein(line)
                     if(weight) then

*         (if not weighting fits then this is ignored)

                        sigy(lincnt) = resvar(ppos,line,xsect)
                        if(sigy(lincnt).gt.0.0) then
                           sigy(lincnt) = sqrt(sigy(lincnt))
                        end if
                     end if
                  else
                     unused_cnt = unused_cnt + 1
                     unused_c(unused_cnt) = results(ppos,line,xsect)
                     unused_w(unused_cnt) = wavein(line)
                  end if

*      Otherwise get original fits, checking fits ok

               else if (((itest.le.2).and.(wavein(line).gt.0.0)).and.
     :                 ((itest.eq.1).or.(usenagerr))) then
                  ppos = get_parnum('Centre_1')
                  if (arc(1,line).le.ARC_ORIG) then
                     lincnt = lincnt + 1
                     channel(lincnt) = results(ppos,line,xsect)
                     wavelength(lincnt) = wavein(line)
                     sigy(lincnt) = sqrt(resvar(ppos,line,xsect))
                  else
                     unused_cnt = unused_cnt + 1
                     unused_c(unused_cnt) = results(ppos,line,xsect)
                     unused_w(unused_cnt) = wavein(line)
                  end if
                  success(line) = .true.
               else
                  success(line) = .false.
               end if
            end do
*
* check that there are enough lines
*
            if(lincnt.lt.(order+1)) then
               call par_wruser(
     :          'Insufficient arc lines for fit - "ARCFIT" abandoned'
     :              ,pstat)
               if (production) nfits = nfits + 1

*  Set error flag for arcplot

               nag_error = -1

*     enough lines for fit

            else
               call weight_fit(sigy,lincnt,w,weight)

*     sort into ascending order, needed because lines might not be in
*     order if additional lines were added at any time.

               call dsa_get_work_array(lincnt*3+2*MAX_KPLUS1,'double',
     :                                 ptr1,slot,status)
               if(status.ne.SAI__OK) then
                  return
               end if
               call sort3d(wavelength,channel,w,lincnt,status)

* Zero out 2d coefficients array

               call zero_dble(a,MAX_KPLUS1*MAX_KPLUS1)
               call zero_dble(a3new,3*maxnpts2 + 3*max_kplus1)
 
               nag_error = fit_cpoly(w,lincnt,channel,wavelength,a,ss,
     :                     kp1,MAX_KPLUS1,%VAL(CNF_PVAL(ptr1)),a3new)
               call dsa_free_workspace(slot,status)

* test to see if the Order requested by the user is less or equal
* to the maximum order which the number of data points allowed
* if it was then choose that order. If it wasnt then choose
* the maximum order that we did allow. Because of precautions
* taken above this condition SHOULD NEVER OCCUR.

               if(order+1.le.kp1) then
                  kp1 = order+1
               else
                  call par_wruser(
     :                 'Not enough data points for requested Order',
     :                 pstat)
                  len1 = 0
                  call chr_putc('Maximum order allowed is ',chars,len1)
                  call chr_puti(kp1-1,chars,len1)
                  call par_wruser(chars(:len1),pstat)
               end if

* If a NAG error has occured then some of the lines
* will not have been in ascending order.

               if(nag_error.ne.0) then
                  call par_wruser('Channels of lines:-',pstat)
                  do i = 1,lincnt
                     write(chars,'(1x,e12.5)')channel(i)
                     call par_wruser(chars,pstat)
                  end do
                  call par_wruser('(these should be in ascending Order)'
     :                 ,pstat)
                  if (production) nfits = nfits+1
               else

* Copy chebyshev polynomial coefficients into 1d array
* For nag only
*                  do i = 1,kp1
*                     fit_coeffs(i) = a(kp1,i)
*                  end do
                  call zero_dble(xc,11)

* Convert to "normal" polynomial coefficients (can cope with order up
* to 10).
* For nag
*                  call gen_chb2no(order,channel(1),channel(lincnt)
*     :                 ,fit_coeffs,xc)
*                  call gen_revr8(xc,kp1,1,.true.,xc)

* For PDA
* CHECK OUTPUT FROM HERE

               IFAIL = 0
               CALL PDA_DPCOEF( ORDER, 0D0, XC, a3new, IFAIL )
               IF ( IFAIL .NE. 0) THEN
               CALL PAR_WRUSER('Error during conversion of fit '/
     :           /'coeffs to Taylor series co-effs',pstat)
               ENDIF



*
*   evaluate mean dispersion, etc.
*
                  val(1) = gen_elemf(%VAL(CNF_PVAL(d_xptr)),1)
                  val(3) = gen_elemf(%VAL(CNF_PVAL(d_xptr)),wavdim)
                  val(2) = (val(3)+val(1))/2.0d0

                  call e_npoly(val,val2,xc,kp1,3)

                  minstartw = min(minstartw,val2(1))
                  maxendw   = max(maxendw,val2(3))

* Type out results if required

                  if(polytab) then
                     call par_wruser(' ',pstat)
                     len1 = 0
                     call chr_fill(' ',chars)
                     call chr_putc(' ARCFIT :   Order = ',chars,len1)
                     call chr_puti(order,chars,len1)
                     call chr_putc(',  cross-section',chars,len1)
                     call encode_range(' ','s',xsect,xsect+width-1,chars
     :                    ,len1)
                     call par_wruser(chars(:len1),pstat)
                     call par_wruser(' ',pstat)
                     call par_wruser('Coefficients of fit are -',pstat)
                     call par_wruser(' ',pstat)

*   first coefficients

                     write(chars,'(3(1p,e13.5))')(xc(i),i = 1,3)
                     call par_wruser(chars(:42),pstat)
                     write(chars,'(3(1p,e13.5))')(xc(i),i = 4,6)
                     call par_wruser(chars(:42),pstat)
                     call par_wruser(' ',pstat)
                     adisp = real(val2(3)-val2(1))/real(wavdim)
*
*   and also give start, end and central wavelengths
*
                     len1 = 0
                     call chr_fill(' ',chars)
                     call chr_putc('Start wavelength = ',chars,len1)
                     call chr_putr(real(val2(1)),chars,len1)
                     call chr_putc(',',chars,len1)
                     len1 = 33
                     call chr_putc('End wavelength = ',chars,len1)
                     call chr_putr(real(val2(3)),chars,len1)
                     call par_wruser(chars(:len1),pstat)
                     len1 = 0
                     call chr_fill(' ',chars)
                     call chr_putc('Central wavelength = ',chars,len1)
                     call chr_putr(real(val2(2)),chars,len1)
                     minstartw = min(minstartw,val2(1))
                     maxendw = max(maxendw,val2(3))
                     call chr_putc(',',chars,len1)
                     len1 = 33
                     call chr_putc('Mean dispersion (per channel) = '
     :                    ,chars,len1)
                     call chr_putr(adisp,chars,len1)
                     call par_wruser(chars(:len1),pstat)
*
*   produce title for table
*
                     call par_wruser(' ',pstat)
                     call par_wruser(
     :     '          Line    Wavelength    Calculated   Discrepancy',
     :                    pstat)
                     call par_wruser(
     :                    '                                Wavelength'
     :                    ,pstat)
                     call par_wruser(' ',pstat)
*
*   P R I N T   T A B L E
*
                     call e_npoly(channel,val,xc,kp1,lincnt)
                     chisq = 0.0
                     do  line = 1,lincnt
                        rc(line) = wavelength(line)-val(line)
                        write(chars,'(4f14.2)',iostat=ignore)
     :                       channel(line),wavelength(line),val(line)
     :                       ,rc(line)
                        chisq = chisq + real(rc(line)*rc(line))
                        call par_wruser(chars(:56),pstat)
                     end do
*
                     if(unused_cnt.gt.0) then
                        call e_npoly(unused_c,val,xc,kp1,unused_cnt)
                        call par_wruser('Lines not used in this fit : -'
     :                       ,pstat)
                        do  i = 1,unused_cnt
*                           rrc = unused_w(i) - real(val(i))
                           write(chars,'(4f14.2)',iostat=ignore)
     :                          unused_c(i),unused_w(i),val(i)
                           call par_wruser(chars(:56),pstat)
                        end do
                     end if
*
*   and print out chi-squared
*
                     if (lincnt.ne.kp1) chisq = chisq/(lincnt-kp1)
                     chisqmax = max(chisqmax,chisq)
                     call par_wruser(' ',pstat)
                     len1 = 0
                     call chr_putc('Chi-squared (Ang**2) = ',chars,len1)
                     call chr_putr(chisq,chars,len1)
                     call par_wruser(chars(:len1),pstat)
                  end if
               end if
            end if
*
* exit control
*
            if(.not.production) then
               call arcplot(%VAL(CNF_PVAL(d_xptr)),
     :                      %VAL(CNF_PVAL(d_vsptr)),xc,results,wavein,
     :                      arc,resvar,xsect,ied,nag_error,weight,order,
     :                      idstring,polydata,polytab,usenagerr,status)
*
* accept
*
               if(ied.eq.1) then
                  loop = .false.
                  if(polytab) polytab = par_quest('Produce tables?',
     :                 .false.)
                  accept = .true.
*
* edit
*
               else if(ied.eq.4) then
                  call e_npoly(channel,val,xc,kp1,lincnt)
                  call gen_subad(lincnt,wavelength,val,rc)
*
* diagnostics
*

*       Get vm
*    PTR1 LINCNT    (r)
*    PTR2 LINCNT    (r)
*    PTR3 LINCNT    (r)
*    PTR4 LINCNT    (r)
*    PTR5 LINCNT    (r)
*    PTR6 LINCNT    (r)

                  call dsa_get_work_array(lincnt,'float',ptr1,slot,
     :                                    status)
                  call dsa_get_work_array(lincnt,'float',ptr2,slot2,
     :                                    status)
                  call dsa_get_work_array(lincnt,'float',ptr3,slot3,
     :                                    status)
                  call dsa_get_work_array(lincnt,'float',ptr4,slot4,
     :                                    status)
                  call dsa_get_work_array(lincnt,'float',ptr5,slot5,
     :                                    status)
                  call dsa_get_work_array(lincnt,'float',ptr6,slot6,
     :                                    status)
                  if(status.ne.SAI__OK) then
                     return
                  end if
                  call dja_arcurve(results,resvar,lincnt,xc,
     :                             wavelength,kp1,xsect,rc,
     :                             %VAL(CNF_PVAL(ptr1)),
     :                             %VAL(CNF_PVAL(ptr2)),
     :                             %VAL(CNF_PVAL(ptr3)),
     :                             %VAL(CNF_PVAL(ptr4)),
     :                             %VAL(CNF_PVAL(ptr5)),
     :                             %VAL(CNF_PVAL(ptr6)))

                  call dsa_free_workspace(slot6,status)
                  call dsa_free_workspace(slot5,status)
                  call dsa_free_workspace(slot4,status)
                  call dsa_free_workspace(slot3,status)
                  call dsa_free_workspace(slot2,status)
                  call dsa_free_workspace(slot,status)

               else if(ied.eq.2) then
                  accept = .false.
                  return

*       ied

               end if

*     .not. production

            end if
            if (production) loop = .false.

*   loop

         end do

*  Put results into array for iroutp and set new value of xsect.

         if (polydata) then
            call copd2d(11,xc,carray(1,xsect))
            xsect = xsect + 1
         else
            ppos = get_parnum('Space1_pos')
            value = resvar(ppos,fline,xsect)
            value = sqrt(max(VAL__SMLR,value))*2.0
            do j = xsect,min(spdim1,(xsect+max(0,nint(value)-1)))
               call copd2d(11,xc,carray(1,j))
            end do
            xsect = xsect+max(1,nint(value))
         end if
         if (.not.production) then
            xsect = 1
            chisqmax = 0.0
         end if
         production = .true.


* xsect.le.spdim1

      end do

      if(spdim1.gt.1) then
         len1 = 0
         call chr_putc('Minimum start wavelength = ',chars,len1)
         call chr_putr(real(minstartw),chars,len1)
         call chr_putc(', maximum end wavelength = ',chars,len1)
         call chr_putr(real(maxendw),chars,len1)
         call par_wruser(chars(:len1),pstat)

* If required copy coefficients from one cross-sections to another.

         do while(par_quest(
     :        'Copy any coefficients from one line to another?',.false.)
     :        )
            qstat = par_qnum('Copy from crossection number',1.0,
     :           real(spdim1),0.0,.false.,' ',value)
            ccopy = nint(value)
            qstat = par_qnum('Copy to start crossection number',1.0,
     :           real(spdim1),0.0,.false.,' ',value)
            cstart = nint(value)
            qstat = par_qnum('Copy to end crossection number',
     :           real(cstart),real(spdim1),0.0,.false.,' ',value)
            cend = nint(value)
            do j = cstart,cend
               call copd2d(11,carray(1,ccopy),carray(1,j))
            end do
            write(chars,
     : '(''Values of crossection'',i4,'' copied to'',i4,'' to '',i4)')
     :        ccopy,cstart,cend
            call par_wruser(chars,pstat)
         end do
      end if
      end
