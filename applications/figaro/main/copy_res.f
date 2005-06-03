      subroutine copy_res(onpars,oldres,oldcon,oldpar,newres,newvar,
     :            control,fitsta,oarc,arc,resnam,ifarc)
*+
* Name:
*    COPY_RES

* Invocation:
*    CALL COPY_RES(ONPARS,OLDRES,OLDCON,OLDPAR,NEWRES,NEWVAR,
*                 CONTROL,FITSTA,OARC,ARC,RESNAM,IFARC)

* Purpose:
*   Salvage old-style results structure

* Description:
*    To copy the results from old to new format. To simplify the
*    coding we copy everything into the variance array as errors (i.e.
*    sigma), and then square the valid errors in position. A simple
*    plane to plane copy could fail with very old format input files,
*    since parameters were not always separated (for example Skew and
*    Centre_2 could occupy the same plane). In addition some items
*    such as Chebyorder have been removed.
*
* Arguments:
*      ONPARS = INTEGER (Given)
*        Old 1st dims of results
*      OLDRES(ONPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Old results structure
*      OLDCON(1,NYP,NXP,SPDIM2) = INTEGER ARRAY (Given)
*        Old control array
*      OLDPAR = CHARACTER*(*) (Given)
*        Old parameter names array
*      OARC(NYP) = INTEGER*2 ARRAY (Given)
*        Old line selection array
*      RESNAM = CHARACTER*3 (Given)
*        Name of old results structure ("res" or "fib")
*      IFARC = LOGICAL (Given)
*        If called from ARC2D
*      NEWRES(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Returned)
*        New results
*      NEWVAR(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Returned)
*        Variance on new results
*      CONTROL(3,NXP,SPDIM2) = INTEGER ARRAY (Returned)
*        New control array
*      FITSTA(3,NYP,NXP,SPDIM2) = INTEGER ARRAY (Returned)
*        Fit status array
*      ARC(NSLCT,NYP) = INTEGER*2 ARRAY (Returned)
*        Line selection array
* Global variables:
*      NXP = INTEGER (Given)
*        Old 3rd dims of results (include file arc_dims)
*      NYP = INTEGER (Given)
*        Number of lines room allowed for (include file arc_dims)
*      SPDIM2 = INTEGER (Given)
*        2nd spatial dimension (include file arc_dims)
*      MXPARS = INTEGER (Given)
*        Number of parameters (include file arc_dims)
*      LINE_COUNT = INTEGER (Returned)
*        Number of lines (include file arc_dims)
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge, 26-JUN-1991
*    A.C. Davenhall, Edinburgh, Starlink

* History:
*    T.N.Wilkins, Cambridge, 9-JUL-1991 Altered so called from map_res
*         "           "      29-OCT-1991 More use of parameters, handle
*                            case of structure from ARC2D correctly.
*    A.C. Davenhall, 06-SEP-00: Regularised the use of pointers d_tlptr
*         and d_trptr to the Starlink style.

*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      integer onpars
      real oldres(onpars,nyp,nxp,spdim2)
      integer oldcon(1,nyp,nxp,spdim2)
      integer control(3,nyp,nxp,spdim2)
      integer*2 arc(nslct,nyp),oarc(nyp)
      integer status,len1
      character*(*) oldpar,resnam
      real newres(mxpars,nyp,nxp,spdim2)
      real newvar(mxpars,nyp,nxp,spdim2)
      integer fitsta(3,nyp,nxp,spdim2)
      integer param,line,ix,iy,get_parnum
      integer tdptr,tadptr,ndim,dims(4),pstat
      integer ppos(4),oget_parnum,nppos(2),cmp,parpos,oparpos
      real value,hnwind,start
      include 'status_inc'
      include 'fit_coding_inc'
      include 'PRM_PAR'
      logical ifarc
      character dynamic_chars
      include 'DYNAMIC_MEMORY'
      equivalence (dynamic_mem,dynamic_chars)
      character*14 string
      character*7 newnam(3)
      character*9 oldnam(3)
      character*13 olderr(3)
      data newnam/'Centre_','Width_','Height_'/
      data oldnam/'Cent(re_)','Wid(th_)','Hei(ght_)'/
      data olderr/'E(_)Cen(tre_)','E_Width_','E_Height_'/

*   Initialise results structure

      status = SAI__OK

*  check that the old parameter names array is actually valid!

      call chkparr(onpars,spdim2,oldpar)

* Find out if we are dealing with an arc or not, and then initialise new
* results structure.

      call init_res(ifarc,status)

      nppos(1) = get_parnum('Space1_pos')
      if(spdim2.eq.1) then
        ppos(1) = oget_parnum('1st_Xsect',oldpar)
        ppos(2) = oget_parnum('Nxsects',oldpar)
      else
        nppos(2) = get_parnum('Space2_pos')
        ppos(1) = oget_parnum('X_Position',oldpar)
        ppos(2) = oget_parnum('BlockXspat',oldpar)
        ppos(3) = oget_parnum('Y_Position',oldpar)
        ppos(4) = oget_parnum('BlockYspat',oldpar)
      end if

* A number of arrays can just be copied over straight, so we'll do those
* first


* Point to new structure

      call accres('data','results','fi',0,0,' ',status)

* Map new template structure

      call map_str('more.twodspec.template.data_array','FU',tdptr,1,
     :     ndim,dims,status)

      call map_str('more.twodspec.template.axis[1].data_array','FU',
     :     tadptr,1,ndim,dims,status)

*  Point to old structure

      call accres('data',resnam,'fi',0,0,' ',status)

* Read rest_wave array

      call accres(' ','rest_wave','RF',nyp,%VAL( CNF_PVAL(d_wptr) ),
     :            ' ', status)

* Read ids array

*      call accres(' ','ids','RC',10*nyp,0,dynamic_chars(idsptr:idsend)
      call accres(' ','ids','RC',10*nyp,0,idstring,status)

* Read traml array

      call accres(' ','traml','RF',nyp,%VAL( CNF_PVAL(d_tlptr) ),' ',
     :            status)

* Read tramr array

      call accres(' ','tramr','RF',nyp,%VAL( CNF_PVAL(d_trptr) ),' ',
     :            status)

* Read mask array

      call accres(' ','mask','RS',nyp*nxp*spdim2,
     :            %VAL( CNF_PVAL(d_mptr) ),' ',status)

* Read template structure

      call accres(' ','template.data_array','RF',wavdim,
     :     dynamic_mem(tdptr),' ',status)
      call accres(' ','template.axis[1].data_array','RF',wavdim,
     :     dynamic_mem(tadptr),' ',status)

* Read tols array

      call accres(' ','tols','RF',maxtol,tolerance,' ',status)

* Read iteration

      call accres(' ','iteration','RS',1,iteration,' ',status)
      if(status.ne.SAI__OK) then
        status = 0
        call accres(' ','itt','RS',1,iteration,' ',status)
      end if

*  Set new results structure to current

      call accres('data','results','fi',0,0,' ',status)

*  Unmap template arrays

      call accres(' ','more.twodspec.template.data_array','UM',wavdim,
     :     dynamic_mem(tdptr),' ',status)
      call accres(' ','more.twodspec.template.axis[1].data_array','UM'
     :     ,wavdim,dynamic_mem(tadptr),' ',status)

* Start by getting number of lines

      call get_lincnt(%VAL( CNF_PVAL(d_tlptr) ),line_count,nyp)

* Now copy over results and control arrays

      do iy = 1, spdim2
        do ix = 1, nxp
          do line = 1, line_count

*      Copy over control array

            call decode_status(1,oldcon(1,line,ix,iy),deccntr)
            call encode_contrl(deccntr,ncntrl,control(1,line,ix,iy))

*      First get fit status and decode it

            value = oldres(oget_parnum('Status',oldpar),line,ix,iy)
            if(value.ne.VAL__BADR) then
              call decode_status(1,nint(value),deccntr)
              if(deccntr(FIT_STAT).gt.0) then

*        Chebyshev polynomial order

                if(deccntr(BACK_MODEL).eq.3) then
                  deccntr(BACK_ORDER) = nint(oldres(
     :                  oget_parnum('Chebyorder',oldpar),line,ix,iy))
                end if
                call encode_contrl(deccntr,ncntrl,
     :               fitsta(1,line,ix,iy))

*        Copy over blocking information

                hnwind = oldres(ppos(2),line,ix,iy) * 0.5
                start = oldres(ppos(1),line,ix,iy) - 0.5
                newres(nppos(1),line,ix,iy) = start + hnwind
                newvar(nppos(1),line,ix,iy) = hnwind
                if(spdim2.gt.1) then
                  hnwind = oldres(ppos(4),line,ix,iy) * 0.5
                  start = oldres(ppos(3),line,ix,iy) - 0.5
                  newres(nppos(2),line,ix,iy) = start + hnwind
                  newvar(nppos(2),line,ix,iy) = hnwind
                end if

*         Now the fit parameters

*         Base

                parpos = get_parnum('Base')
                newres(parpos,line,ix,iy) =
     :              oldres(oget_parnum('Base',oldpar),line,ix,iy)
                newvar(parpos,line,ix,iy) =
     :              oldres(oget_parnum('E_Base',oldpar),line,ix,iy)
                do cmp = 1, deccntr(FIT_NCMP)

*           Parameters

*           Centre, width and height

                  do param = 1, 3
                    len1 = 0
                    call chr_appnd(newnam(param),string,len1)
                    call chr_puti(cmp,string,len1)
                    parpos = get_parnum(string(:len1))

                    len1 = 0
                    call chr_appnd(oldnam(param),string,len1)
                    call chr_puti(cmp,string,len1)
                    oparpos = oget_parnum(string(:len1),oldpar)
                    newres(parpos,line,ix,iy) =
     :                         oldres(oparpos,line,ix,iy)

                    len1 = 0
                    call chr_appnd(olderr(param),string,len1)
                    call chr_puti(cmp,string,len1)
                    oparpos = oget_parnum(string(:len1),oldpar)
                    newvar(parpos,line,ix,iy) =
     :                         oldres(oparpos,line,ix,iy)

                  end do
                end do

*         If skew or Cauchy fits, then copy those over

                if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then

                  parpos = get_parnum('Skew_1')
                  newres(parpos,line,ix,iy) =
     :             oldres(oget_parnum('Skew',oldpar),line,ix,iy)
                  newvar(parpos,line,ix,iy) =
     :                oldres(oget_parnum('E(_)Skew',oldpar),line,ix,iy)

                else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then

                  parpos = get_parnum('Cauchy_1')
                  newres(parpos,line,ix,iy) =
     :                oldres(oget_parnum('Cauch(y)',oldpar),line,ix,iy)
                  newvar(parpos,line,ix,iy) =
     :            oldres(oget_parnum('E(_)Cau(chy)',oldpar),line,ix,iy)

                end if

*         DENSC

                oparpos = oget_parnum('Dens_scale',oldpar)
                if(oparpos.ne.0) then
                  newres(get_parnum('Dens_scale'),line,ix,iy) =
     :                 oldres(oparpos,line,ix,iy)
                endif

*         AIC

                oparpos = oget_parnum('Akaike IC',oldpar)
                if(oparpos.ne.0)
     :             newres(get_parnum('Akaike IC'),line,ix,iy) =
     :              oldres(oparpos,line,ix,iy)

*         Square errors

                do param = 1, mxpars
                  if(newvar(param,line,ix,iy).ne.VAL__BADR) then
                    newvar(param,line,ix,iy) = newvar(param,line,ix,iy)
     :                                       * newvar(param,line,ix,iy)
                  end if
                end do
              end if
            end if
          end do
        end do
      end do

* Copy over line selection array

      do line = 1, line_count
        arc(1,line) = oarc(line)
      end do

      call par_wruser(
     :   'Copy complete-use DELOBJ & TRIMFILE to remove old structure',
     :     pstat)
      end
