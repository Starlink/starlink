      subroutine clone_mode(ifcomb,clopen,nocube,ifarc,status)
*+
* Name:
*    CLONE_MODE

* Invocation:
*    CALL CLONE_MODE(IFCOMB,CLOPEN,NOCUBE,IFARC,STATUS)

* Purpose:
*   Clone line ids

* Description:
*   Grab the res struc to be cloned, if copy then copy the whole
*   structure over, otherwise just the line ids, trams and wavelengths.
*   For the case of copy = .false. a .RES structure must be created and
*   initialised
*
* Arguments:
*      COPY = LOGICAL (Given)
*        If to use COPY mode in LONGSLIT (in common)
*      IFCOMB = LOGICAL (Given)
*        If the file is a comb
*      IFARC = LOGICAL (Given)
*        If the file is an arc
*      CLOPEN = LOGICAL (Returned)
*        If a clone file is opened ok
*      NOCUBE = LOGICAL (Returned)
*        If no cube created/found
*      LINE_COUNT = INTEGER (Returned)
*        Number of lines (in common)
*    Subroutines/functions referenced:
*      CLONE_IT            : Open clone file
*      CLONE_MATCH         : Match clone to present file
*      DSA_GET_WORK_ARRAY  : Get virtual memory
*      GET_LINE_COUNT      : Get the number of lines identified
*      MAP_DATA            : Map the data arrays
*      MAP_RES             : Map the arrays in the results structure
*      NEW_ARC             : Create a results structure
*
*      DSA_FREE_WORKSPACE  : Free virtual memory
*      PAR_WRUSER          : Write a string to the user

* Author:
*   T.N.Wilkins, Manchester (TNW)
*   A.J. Holloway, Jodrell Bank (AJH)
*   A.C. Davenhall, Edinburgh, Starlink (ACD)

* History:
*   T.N.Wilkins Manchester 9-10/88
*   TNW 10/10/88 IFCOMB added to call to CLONE_IT
*   TNW 28/11/88 Change to handling of vm
*   TNW/CAVAD 2/2/90 ARC array now copied.
*       "     2/1/91 Dimension of work arrays determined above, so 50 is
*   not hard-wired in here.
*   AJH 1/99 Changed mode access to map_data from r,w to READ,WRITE
*   ACD 6/9/00 Regularised the use of pointer d_tlptr to the Starlink style.
*-
      implicit none
      integer status
      include 'arc_dims'
      logical ifcomb,clopen,nocube,ifarc
      integer ptr1,ptr2,ptr3,ptr4,ptr5,ptr6,ptr7,end6
      integer slot,slot2,slot3,slot4,slot5,slot6,slot7
      integer nbytes,ndims,dims(1)
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      if(status.ne.SAI__OK) return

      if(copy) then
        call clone_it(clopen,ifcomb,status)
        call map_data(ifcomb,'READ',status)
        call map_res(ifcomb,.false.,nocube,status)
      else
        call new_arc(ifcomb,status)
        if(refine) then
          clone=.false.
          return
        end if
        call map_data(ifcomb,'READ',status)
        call map_res(ifcomb,.false.,nocube,status)
        if(nocube.or.status.ne.SAI__OK) return
        call init_res(ifarc,status)
        call clone_it(clopen,ifcomb,status)

        if (clopen) then

*     Locate CLONE results structure and find the number of line spaces
*     in it.

          call accres('clone','results','fi',1,1,' ',status)
          ndims = 1
          call accres(' ','more.traml','si',ndims,dims,' ',status)

*  Get virtual memory:
*    PTR1   dims(1)  (d)
*    PTR2   dims(1)  (d)
*    PTR3   dims(1)  (d)
*    PTR4   dims(1)  (d)
*    PTR5   dims(1)  (r)
*    PTR6 dims(1)*10 (c)
*    PTR7   dims(1)  (s)

          call dsa_get_work_array(dims(1),'double',ptr1,slot,status)
          call dsa_get_work_array(dims(1),'double',ptr2,slot2,status)
          call dsa_get_work_array(dims(1),'double',ptr3,slot3,status)
          call dsa_get_work_array(dims(1),'double',ptr4,slot4,status)
          call dsa_get_work_array(dims(1),'float',ptr5,slot5,status)
          call dsa_get_work_array(dims(1)*10-1,'char',ptr6,slot6,status)
          call dsa_get_work_array(dims(1),'short',ptr7,slot7,status)
          if(status.ne.SAI__OK) return

          call clone_match(idstring,
     :      %VAL(CNF_PVAL(d_tlptr)),%VAL(CNF_PVAL(d_trptr)),
     :      %VAL(CNF_PVAL(d_wptr)),%VAL(CNF_PVAL(ptr1)),
     :      %VAL(CNF_PVAL(ptr2)),%VAL(CNF_PVAL(ptr3)),
     :      %VAL(CNF_PVAL(ptr4)),%VAL(CNF_PVAL(ptr5)),
     :      %VAL(CNF_PVAL(ptr6)),ifcomb,%VAL(CNF_PVAL(d_aptr)),
     :      %VAL(CNF_PVAL(ptr7)),dims(1),status,%VAL(dims(1)*10 - 1))

          call dsa_free_workspace(slot7,status)
          call dsa_free_workspace(slot6,status)
          call dsa_free_workspace(slot5,status)
          call dsa_free_workspace(slot4,status)
          call dsa_free_workspace(slot3,status)
          call dsa_free_workspace(slot2,status)
          call dsa_free_workspace(slot,status)

        end if
      end if
      if(status.eq.SAI__OK) then
        call get_lincnt(%VAL( CNF_PVAL(d_tlptr) ),line_count,nyp)
      end if
      end
