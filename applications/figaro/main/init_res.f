      subroutine init_res(ifarc,status)
*+
* Name:
*    INIT_RES

* Invocation:
*    CALL INIT_RES(IFARC,STATUS)

* Purpose:
*   Initialize all the required values of the .RES structure for
*   a NEW analysis

* Description:
*   Initialize all the required values of the .RES structure for
*   a NEW analysis

* Arguments:
*    IFARC = LOGICAL (Given)
*        If arc or similar (i.e. not object data)
*    STATUS = INTEGER (Given and returned)
*        Error status

* History:
*   Optimised a bit, T.N.Wilkins 6/7/88
*   Use of accres, TNW 18/10/88
*   Initialisation of .template added, TNW 27/11/89, altered TNW 15/2/90
*   Changes for new results structure, TNW 1-10/7/91
*   Remove local unused variables: ACD, 28/9/00
* --------------------------------------------------------------------

* must declare everything

      implicit none
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
*-
      logical ifarc
      integer status
      include 'status_inc'
      integer*2 ival
      integer fit_status(MAX_CONTROL)
      integer*2 MISSING
      parameter (MISSING = 1)
      integer ntemp,ntemp2
C      integer ptr,slot


* Set results cube and variance to bad values

      call gen_cfill(1,mxpars*spdim1*spdim2*nyp,val__badr,
     :               %VAL(CNF_PVAL(d_rptr)))

C     print *,'gen_cfill1'

*      call gen_cfill(1,mxpars*spdim1*spdim2*nyp,val__badr,
*     :               %VAL(CNF_PVAL(d_rptr)))



      call gen_cfill(1,mxpars*spdim1*spdim2*nyp,val__badr,
     :               %VAL(CNF_PVAL(d_vptr)))

C     print *,'gen_cfill1'

*      call gen_cfill(1,mxpars*spdim1*spdim2*nyp,val__badr,
*     :               %VAL(CNF_PVAL(d_vptr)))

* zero out wavelength and trams arrays

      call zero_real(%VAL(CNF_PVAL(d_tlptr)),nyp)
C     print *,'zero1'
      call zero_real(%VAL(CNF_PVAL(d_trptr)),nyp)
C     print *,'zero2'
      call zero_real(%VAL(CNF_PVAL(d_wptr)),nyp)
C     print *,'zero3'
* no singles to doubles and no old or new fits either

      noldsum    = 0
      nnewsum    = 0
      sgnowdgsum = 0

* encode default model in fit_status word

      call encode_contrl(default_model,ncntrl,fit_status)

* store the requested operation in CONTROL

      call set_cont(%VAL(CNF_PVAL(d_cptr)),ncntrl,nxp*nyp*spdim2,
     :              fit_status)

* set the initial values of mask and fit_status for each point

      ival = 1

* mask in all points

      call set_short(%VAL(CNF_PVAL(d_mptr)),nxp*nyp*spdim2,ival)
C     print *,'setshort'

* Set fit status array to zeros

      call zero_int(%VAL(CNF_PVAL(staptr)),ncntrl*nxp*nyp*spdim2)
c     print *,'zeroint'
* Set arc usage array to zero

      call zero_short(%VAL(CNF_PVAL(d_aptr)),nyp*nslct)
C     print *,'zeroshort'
*set the iteration number: by definition one so far

      iteration = 1
      call accres(' ','more.twodspec.iteration','ws',1,iteration,' ',
     :            status)
C     print *,'write iter'

* Fill .PARAMS array

*      call fill_params(dynamic_chars(parptr:parend),(spdim2.gt.1),
*     :                  mxpars,(ifarc.and.(spdim1.gt.1)),mgauss)

      call fill_params(parval,(spdim2.gt.1),
     :                  mxpars,(ifarc.and.(spdim1.gt.1)),mgauss)

C     print *,'fillparams'
* Fill .template.axis[1].data_array

      ntemp2 = 1
      call accres(' ','more.twodspec.template.axis[1].data_array','si'
     :     ,ntemp2,ntemp,' ',status)

C     print *,'axis1'
      call accres(' ','more.twodspec.template.axis[1].data_array','wf',
     :            ntemp,%VAL(CNF_PVAL(d_xptr)),' ',status)
C     print *,'axis2'
* Initialise tolerances

      call init_tols(tolerance,MAXTOL,ifarc,%VAL(CNF_PVAL(d_xptr)),
     :               ntemp)

C     print *,'init_tols'
      gestol(1) = tolerance(5)
      gestol(2) = tolerance(6)
      gestol(3) = tolerance(9)

      call accres(' ','more.twodspec.tols','wf',MAXTOL,tolerance,' ',
     :            status)
C     print *,'writ tols'

* Fill IDS array with "?"

*      call chr_fill('?',dynamic_chars(idsptr:idsend))
      call chr_fill('?',idstring)

* If we have 3-d data then we will also have a total intensity array to
* fill

      if(spdim2.gt.1) then
        call set_intensity(%VAL(CNF_PVAL(d_sptr)),
     :                     %VAL(CNF_PVAL(totptr)),wavdim,spdim1,spdim2)

      end if

* Fill existant groups element
* Not currently implemented TNW 1991

*      call getwork(nyp,'short',ptr,slot,status)
*      call zero_short(%VAL(CNF_PVAL(ptr),nyp)

*      call accres(' ','more.twodspec.groups.all.data_array','ws',nyp,
*     :            %VAL(CNF_PVAL(ptr),' ',status)

*      call set_short(%VAL(CNF_PVAL(ptr),nyp,MISSING)

*      call accres(' ','more.twodspec.groups.sky.data_array','ws',nyp,
*     :            %VAL(CNF_PVAL(ptr),' ',status)
*      call dsa_free_workspace(slot,status)
      end

