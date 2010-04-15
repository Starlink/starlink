      subroutine check_res(ifcomb,resnam,cptr,aptr,rptr,oldparstr,
     :            onpars,status)
*+
* Name:
*    CHECK_RES

* Invocation:
*    CALL CHECK_RES(IFCOMB,RESNAM,CPTR,APTR,RPTR,PPTR,PPTREND,
*                 ONPARS,STATUS)

* Purpose:
*   Check if old format results structure present

* Description:
*    To see if an old format results structure is present. This assumes
*    that a new format is not present.
*    If an old format is present, then the current format is created and
*    the results copied over.
*
* Arguments:
*      IFCOMB = LOGICAL (Given)
*        If called from COMB
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*      RESNAM = CHARACTER*3 (Returned)
*        'res' or 'fib'-name of old structure
*      CPTR = INTEGER (Returned)
*        Pointer to old control array
*      RPTR = INTEGER (Returned)
*        Pointer to old results array
*      APTR = INTEGER (Returned)
*        Pointer to old arc array
*      PPTR,PPTREND = INTEGER (Returned)
*        Pointers to start and end of old parameter
*                         names array
*      ONPARS = INTEGER (Returned)
*        1st dimension of old results array
*    Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge, 26-JUN-1991 - 8-JUL-1991
*    ACD: A C Davenhall, Starlink, Edinburgh
* History:
*    T.N.Wilkins, Cambridge, 9-JUL-1991 Altered so called from map_res
*    ACD: 28/9/00 Remove local unused variable.
*-
      implicit none
      integer status,onpars,pstat
      include 'SAE_PAR'
      include 'arc_dims'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'
      integer rptr,cptr,pptr,pptrend,ndim,dims(4),aptr
C      integer dtalen, dta_loc, chr_len
C      character*(DAT__SZLOC) nloc
C      character*70 dtanam
      character*4 ftype
      character*3 resnam
      character*(*) oldparstr
      logical ifcomb
C      logical defined

* Look for the old structures

      status = SAI__OK
      call accres('data','res','fi',0,0,' ',status)
      if(status.eq.SAI__OK) then
        resnam = 'res'
      else
        status = SAI__OK
        call accres('data','fib','fi',0,0,' ',status)
        if(status.eq.SAI__OK) then
          resnam = 'fib'
        else
          return
        end if
      end if

*  Tell the user what we're going to do

      call par_wruser(
     :'Old format results structure found, please wait while I copy the'
     :,pstat)
      call par_wruser('results over',pstat)

*  First we need size of old structure

      ndim = 4
      call accres(' ','data','si',ndim,dims,' ',status)
      onpars = dims(1)
      mxpars = (dims(1)/2)
      nyp = dims(2)
      nxp = dims(3)
      spdim2 = dims(4)
      if(ifcomb) then
        ftype = 'COMB'
      else
        ftype = 'RECT'
      end if
      call crres(ftype,status)

*  Set pointer to old structure

      call accres('data',resnam,'fi',0,0,' ',status)

*  Map data, params and control arrays from old structure

*      call map_str('params','cu',pptr,4,ndim,dims,status)
* REPLACE WITH get0c



*      call dsa_specific_structure(' ','params','R',dtanam,status)

*        dtalen = chr_len(dtanam)

*            call dta_loc(dtanam(:dtalen)//'.'//'params'
*     :                 ,nloc,status)

* Check to see if the item is defined

*             call dat_state(nloc,defined,status)
*             if (defined) then
*             call dat_get0c(nloc, parval, status)

*             else

*             endif

*              parval=' '



      call map_str('data','fr',rptr,4,ndim,dims,status)
      call map_str('control','ir',cptr,4,ndim,dims,status)
      call map_str('arc','sr',aptr,4,ndim,dims,status)

      pptrend = pptr + 10*onpars - 1

* Set pointer to new structure

      call accres('data','results','fi',0,0,' ',status)
      end





