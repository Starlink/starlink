      subroutine shdiagnosis(ifsoft,ifcomb,status)
*+
* Name:
*    SHDIAGNOSIS

* Invocation:
*    CALL SHDIAGNOSIS(IFSOFT,IFCOMB,STATUS)

* Purpose:
*   Plot diagnostics

* Description:
*    To plot graphs to give diagnostics of the variation of line
*  centre  v. X-sect e.t.c. This will only perform the plots using
*  DIAGNOSIS_PLOT for the first component (that is linewidth v. centre
*  and error on centre v. height).
*
* Arguments:-
*   IFSOFT = LOGICAL (Given)
*        If plots in softcopy
*   IFCOMB = LOGICAL (Given)
*        .true. if called from COMB
*   STATUS = INTEGER (Given)
*        Error status, 0=ok
*
* Global variables:
*   D_RPTR    (NZP,NYP,NXP) = REAL ARRAY (Given)
*        Dynamic_mem(d_rptr) "cube"
*   LINE_COUNT = INTEGER (Given)
*        Number of lines
*   D_WPTR    (LINE_COUNT) = REAL ARRAY (Given)
*        Wavelengths of lines
*   IDSPTR:IDSEND (LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Names of lines
*   DATAFILE = CHARACTER*(*) (Given)
*        Name of image file
*
* Author:
*   T.N.Wilkins Manchester
*
* History:
*   TNW 29/11/88 Changed to use getwork
*   TNW/CAVAD 20/7/89 PARAMS passed to PLOTVEL
*   TNW/CAVAD 18/9/89 PARAMS removed!
*   TNW/CAVAD 1/11/89 QMENU used
*   TNW/CAVAD 27/4/90 arcdims common used
*       "     3/1/91 No longer need to allow for use by LONGSLIT
*       "     1-8/7/91 Changes for new results structure
*-
      implicit none
      integer status
      include 'arc_dims'
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      logical ifsoft
      logical ifcomb

* Local

      integer ptr1,ptr2,ptr3,ptr4,slot,slot2,slot3,slot4
      logical par_quest
      logical loop
      integer nels
      integer iopt,get_parnum
      integer NDICT,CENTVCHN,WIDVCHN,ECENVHEI
      parameter (NDICT = 4,
     :           CENTVCHN = 1,
     :           WIDVCHN  = 2,
     :           ECENVHEI = 3)
      character*35 dict(NDICT)
      integer dumi
      real dumr
      character dumc
      data dict/
     :     'C_V_X  : Line centre v. channel',
     :     'W_V_C  : Linewidth v. centre',
     :     'EC_V_H : Error on centre v. height?',
     :     'EXIT   : Exit'/
*

      loop = .true.
      do while(loop)
        if(ifsoft) call gr_soft(status)

*   Get option required

        if(ifcomb) then
          dict(1)(24:30) = 'channel'

*         may be in monolith, so not re-initialised

        else
          dict(1)(24:30) = 'x-sect '
        end if

        call qmenu('Diagnosis Plots',dict,NDICT,0,dumr,dumc,iopt,dumi,
     :       status)
*
*   Plot line position v. channel no.
*
        if(iopt.eq.CENTVCHN) then
          call dsa_get_work_array(nxp*mgauss,'float',ptr1,slot,status)
          call dsa_get_work_array(nxp*mgauss,'float',ptr2,slot2,status)
          call dsa_get_work_array(nxp*mgauss,'float',ptr3,slot3,status)
          call dsa_get_work_array(nxp*mgauss,'float',ptr4,slot4,status)
          if(status.ne.SAI__OK) return

          call plotvel(%VAL(CNF_PVAL(d_rptr)),%VAL(CNF_PVAL(d_vptr)),
*     :             %VAL(CNF_PVAL(staptr),dynamic_chars(idsptr:idsend)
     :                %VAL(CNF_PVAL(staptr)),idstring,
     :                %VAL(CNF_PVAL(d_wptr)),%VAL(CNF_PVAL(ptr1)),
     :                %VAL(CNF_PVAL(ptr2)),%VAL(CNF_PVAL(ptr3)),
     :                %VAL(CNF_PVAL(ptr4)),.false.,0,0.0,ifsoft,
     :                par_quest('Show fits with NAG errors?',.false.),
     :                ifcomb,0)

          call dsa_free_workspace(slot4,status)
          call dsa_free_workspace(slot3,status)
          call dsa_free_workspace(slot2,status)
          call dsa_free_workspace(slot,status)

* Plot linewidth v. centre

        else if(iopt.eq.WIDVCHN) then
          call dsa_get_work_array(nyp,'float',ptr1,slot,status)
          call dsa_get_work_array(nyp,'float',ptr2,slot2,status)
          call dsa_get_work_array(nyp,'float',ptr3,slot3,status)
          call dsa_get_work_array(nyp,'float',ptr4,slot4,status)
          if(status.ne.SAI__OK) return

          call diagnosis_plt(%VAL(CNF_PVAL(d_rptr)),
     :                       %VAL(CNF_PVAL(d_vptr)),
     :                       %VAL(CNF_PVAL(staptr)),
     :                       get_parnum('Width_1'),
     :                       get_parnum('Centre_1'),
     :                       'Linewidth v. centre','Centre','Width',
     :                       .true.,%VAL(CNF_PVAL(ptr1)),
     :                       %VAL(CNF_PVAL(ptr2)),
     :                       %VAL(CNF_PVAL(ptr3)),
     :                       %VAL(CNF_PVAL(ptr4)),ifsoft)

          call dsa_free_workspace(slot4,status)
          call dsa_free_workspace(slot3,status)
          call dsa_free_workspace(slot2,status)
          call dsa_free_workspace(slot,status)

*  Plot error on centre v. height

        else if(iopt.eq.ECENVHEI) then
          call dsa_get_work_array(nyp,'float',ptr1,slot,status)
          call dsa_get_work_array(nyp,'float',ptr2,slot2,status)
          call dsa_get_work_array(nyp,'float',ptr3,slot3,status)
          call dsa_get_work_array(nyp,'float',ptr4,slot4,status)
          if(status.ne.SAI__OK) return

          call diagnosis_plt(%VAL(CNF_PVAL(d_rptr)),
     :                       %VAL(CNF_PVAL(d_vptr)),
     :                       %VAL(CNF_PVAL(staptr)),
     :                       (-get_parnum('Centre_1')),
     :                       get_parnum('Height_1'),
     :                       'Error on centre v. height',
     :                       'Height','Error on centre',
     :                       .false.,%VAL(CNF_PVAL(ptr1)),
     :                       %VAL(CNF_PVAL(ptr2)),%VAL(CNF_PVAL(ptr3)),
     :                       %VAL(CNF_PVAL(ptr4)),ifsoft)

          call dsa_free_workspace(slot4,status)
          call dsa_free_workspace(slot3,status)
          call dsa_free_workspace(slot2,status)
          call dsa_free_workspace(slot,status)

*   exit

        else
          loop = .false.
        end if
      end do
      end
