      subroutine map_res(ifcomb,ifarc,nocube,status)
*+
* Name:
*    MAP_RES

* Invocation:
*    CALL MAP_RES(IFCOMB,IFARC,NOCUBE,STATUS)

* Purpose:
*   Map results structure arrays (and read in some info)

* Description:
*   If we get to here then the .RES structure exists, but just
*   in case test to see if each componnent is there and if so map it
*   its contents and return all the required pointers via common
*   if not then the REFINE file is in error
*
* Arguments
*    IFCOMB = LOGICAL (Given)
*        If comb structure
*    IFARC = LOGICAL (Given)
*        If called from ARC2D
*    NOCUBE = LOGICAL (Given and returned)
*        If cube found
*    STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*
* History:
*  Altered to set descriptor, TNW 23/8/88
*  Altered to use MAPCHR, TNW 11/10/88, also use of dta_tyvar
*  eliminated.
*  Altered to use ACCRES TNW 18/10/88
*  Spurious call to fig_dtaerr removed, TNW 27/1/89
*  STATUS added to argument list TNW/CAVAD 25/9/89
*  PAR_COM, separate common block, TNW 18/12/89
*  Merged back, TNW 11/1/90
*  Made useable in FIBDISP etc. TNW 6/2/90
*  Now checks parameter names array, TNW 14/5/90
*  Altered for new results structure (doesn't have to do much checking),
*     TNW 1/7/91
*  AJH 1/99 Changed map_data mode from r,w to READ, WRITE
*  AJH 5/99 CHanged map_str mode from FU to WF (same bug as before)
*  ACD: 28/9/00 Remove local unused variables.
      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
*- ----------------------------------------------------------------
      logical nocube,ifcomb,ifarc
C      logical defined
C      integer chr_len
      integer status
      integer pstat
      integer ndim
      integer dims(4)
      integer iopt
C      integer dtalen
      integer cptr,rptr,aptr,onpars
* pptr,pptrend
      character*1000 oldparstr
C      character*(DAT__SZLOC) nloc
      character*3 resnam
      character*18 dict(2)
      integer dumi
      real dumr
      character dumc
C      character*70 dtanam
      data dict/'NEW : New mode','CLONE : Clone mode'/

      if(status.ne.SAI__OK) return

C     print4000, status
C4000 format(1x, 'Entered subroutine MAP_RES, status: ', i20)

*
*     Get dimensions of clone data and check if the Crossection
*     dimensions are the same as those of IMAGE
*
      resnam = '   '
      call accres('data','results','fi',0,0,' ',status)
      if(status.ne.SAI__OK) then
*        call check_res(ifcomb,resnam,cptr,aptr,rptr,pptr,pptrend,onpars
C        print *,'dataarray notok'
        call check_res(ifcomb,resnam,cptr,aptr,rptr,oldparstr,onpars
     :       ,status)
      end if
      nocube = status.ne.SAI__OK
      if(nocube) then
        call par_wruser('There is no results block to REFINE',pstat)
        if(refine.and.(.not.batch)) then
          status=0

          call qmenu('Wrong Mode Menu',dict,2,1,dumr,dumc,iopt,dumi,
     :         status)

          clone = iopt.eq.2
          refine=.false.
        end if
        return
      end if

      ndim = 4


      call map_str('data_array','fu',d_rptr,4,ndim,dims,status)
C     print1000, ndim, dims(1), dims(2), dims(3), dims(4), status
C1000 format(3X, 'After map_str for data_array:-' /
C    :  3x, 'ndim, dims: ', i6, i6, i6, i6, i6 /
C    :  3x, 'status: ', i20 )

* assign the dimensions

      mxpars = dims(1)
      nyp = dims(2)
      if(ndim.gt.2) then
        nxp = dims(3)
      else
        nxp = 1
      end if
      if(ndim.gt.3) then
        spdim2 = dims(4)
      else
        spdim2 = 1
      end if

C     print1001, mxpars, nxp, nyp, spdim2, d_rptr
C1001 format(3x, 'mxpars, nxp, nyp, spdim2, d_rptr: ',
C    :  i4, i4, i4, i4, i20)
C     print1002
C1002 format(3x, 'Dump of data array:-')
C     call arrdump (mxpars, nyp, nxp, 1, %val(CNF_PVAL(d_rptr)))


* Work out maximum number of fit parameters and components

      mparms = mxpars - 5
      if(spdim2.gt.1) mparms = mparms - 2
      mgauss = max(1,mparms/3)
      curmcmp = mgauss

      call map_str('variance','fu',d_vptr,4,ndim,dims,status)
C     print1003, d_vptr, status
C1003 format(3x, 'after mapping variance, d_vptr, status:', i15, i20)


* Map the MASK array

      call map_str('more.twodspec.itmask','su',d_mptr,4,ndim,dims,
     :     status)
C     print *,'map_str3'

* Map the CONTROL array

      call map_str('more.twodspec.control','iu',d_cptr,4,ndim,dims,
     :     status)
      ncntrl = dims(1)
C     print *,'map_str4'

* Map the rest wavelengths array

      call map_str('more.rest_wave','fu',d_wptr,4,ndim,dims,status)
C     print *,'map_str5'

* Map parameter names, IDS, arc and trams arrays


* ORIG

*      call map_str('more.params','cu',parptr,4,ndim,dims,status)

* ALT

*       call dsa_specific_structure('data',
*     :               'more.params','R',dtanam,status)
*        dtalen = chr_len(dtanam)

*            call dta_loc(dtanam(:dtalen)//'.more.params'
*     :                 ,nloc,status)

* Check to see if the item is defined

*             call dat_state(nloc,defined,status)
*             if (defined) then
*             call dat_get0c(nloc, parval, status)
*             else
*	     print *,'Not defined'
*             endif

* 16/6 changed work ' ' to 0

      call accres(' ','more.params','rc',mxpars*10,0,
     : parval(:mxpars*10),status)
C     print *,'params'



*IDS

*      call map_str('more.ids','cu',idsptr,4,ndim,dims,status)
*      call map_str('more.ids','rc',idstring,4,ndim,dims,status)
*      idsend = idsptr + nyp*10 - 1

*16/6 changed work ' ' to 0

       call accres(' ','more.ids','rc',10*nyp,0,idstring(:10*nyp),
     :            status)

C     print *,'ids'

      call map_str('more.tramr','fu',d_trptr,4,ndim,dims,status)

C     print *,'map_str'

      call map_str('more.traml','fu',d_tlptr,4,ndim,dims,status)
C     print *,'map_str'

      call map_str('more.twodspec.fit_status','iu',staptr,4,ndim,dims
     :     ,status)
C     print *,'map_str'

* Map the ARC usage/select array

      call map_str('more.twodspec.select','su',d_aptr,4,ndim,dims,
     :     status)
      nslct = dims(1)
C     print *,'map_str'

*      parend = parptr + mxpars*10 - 1

* If 3-d data array map total intensity array

      if(spdim2.gt.1) then
        call map_str('more.twodspec.total_intensity','fu',totptr,4,
     :       ndim,dims,status)
C     print *,'map_str'
      end if

*     Copy over contents of old results and control arrays

      if(resnam.ne.'   ') then

*  Map data, this may be done twice (but that shouldn't matter). copy_res
*  calls init_res which needs the wavelength axis (or 2nd axis for comb),
*  and may need the main data array (3-d data only).

        call map_data(ifcomb,'READ',status)
        if(status.ne.SAI__OK) return
        call copy_res(onpars,%VAL(CNF_PVAL(rptr)),%VAL(CNF_PVAL(cptr)),
*     :       dynamic_chars(pptr:pptrend),%VAL(CNF_PVAL(d_rptr)),
     :                oldparstr,%VAL(CNF_PVAL(d_rptr)),
     :                %VAL(CNF_PVAL(d_vptr)),%VAL( CNF_PVAL(d_cptr)),
     :                %VAL(CNF_PVAL(staptr)),%VAL(CNF_PVAL(aptr)),
     :                %VAL(CNF_PVAL(d_aptr)),resnam,ifarc)

*  Unmap old structures

        call accres('data',resnam,'fi',0,0,' ',status)
        call accres(' ','data','um',0,0,' ',status)
        call accres(' ','control','um',0,0,' ',status)
        call accres(' ','arc','um',0,0,' ',status)
        call accres(' ','params','um',0,0,' ',status)
      else

* Read in iteration and tolerances

        call accres(' ','more.twodspec.iteration','rs',1,iteration,' ',
     :       status)
C     print *,'iteration'

        call accres(' ','more.twodspec.tols','RF',MAXTOL,tolerance,' '
     :       ,status)
C     print *,'tols'

        call accres(' ','more.twodspec.guess_tols','RF',NGTOL,gestol
     :       ,' ',status)
C     print *,'guesstols',gestol

      end if
C     print4001, ifcomb, ifarc, nocube, status
C4001 format(1x, 'Leaving MAP_RES, ifcomb, ifarc, nocube, status: ',
C    :  l4, l4, l4, i20)
      end
