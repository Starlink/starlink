      subroutine contin_corr(aa,ylim,used,polydata,status,
     :                        a3all,maxnpts)
*+
* Name:
*    CONTIN_CORR

* Invocation:
*    CALL CONTIN_CORR(AA,YLIM,USED,POLYDATA,STATUS,A3ALL,MAXNPTS)

* Purpose:
*   Continuity correct arc lines

* Description:
*     To fit Chebyshev polynomials to the positions of the arc lines as
*   located by ARC2D, so as to enable continuity correction to be made.
*   The values of the polynomial may then be used rather than the
*   original data.

* Global variables:
*     LINE_COUNT = INTEGER (Given)
*        Number of lines (include file arc_dims)
*     NXP,NYP,MXPARS = INTEGER (Given)
*        Dimensions of results "cube" (include file arc_dims)
*     WAVDIM, SPDIM1 = INTEGER (Given)
*        Dimensions of data (include file arc_dims)
*     RPTR = INTEGER (Given)
*        Pointer to results "cube" (the results cube is written to,
*        although RPTR is unchanged) (include file arc_dims)
*
* Arguments:
*    POLYDATA = LOGICAL (Given)
*    STATUS = INTEGER (Given and returned)
*    AA(MAX_KPLUS1,LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*    YLIM(2,LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*    USED(LINE_COUNT) = LOGICAL ARRAY (Workspace)
*    ATHREE(3*MAXNPTS+3*MAX_KPLUS1) = DOUBLE PRECISION ARRAY   

* Authors:
*  TNW: T.N.Wilkins. Manchester until 1/89, Cambridge until 9/92
*  AJH: A.J.Holloway. Manchester 10/97

* History:
*   TNW: Original version
*   TNW: 15/2/89 Change to workspace - less confusing
*   TNW: 3/1/91 Use menu to decide whether to accept fits
*   TNW: 26/3/91 Workspace changes
*   TNW: 8/6/92     "     made slightly clearer
*   TNW: 9/7/93 workspace for fill_map (set_poly_results) reduced
*   AJH: 5/10/97 included PDA variables for passing to epoly calls

      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      include 'arc_dims'
      logical polydata
*-
      integer cpxptr,w2ptr,cpyptr
      integer kp1,slot,slot2,slot3,slot4,slot5,slot6,slot7,slot8,slot9
      integer MAX_KPLUS1
      parameter (MAX_KPLUS1=10)
      integer maxnpts
      integer maxnpts2
      parameter (maxnpts2=2048)
      integer npts
      double precision aa(MAX_KPLUS1,line_count)
      double precision athree(3*maxnpts2+3*max_kplus1)
      double precision a3all(3*maxnpts2+3*max_kplus1,line_count)
      integer ptr1,ptr2,ptr3,ptr4,ptr5,ptr6
      integer key
      double precision ylim(2,line_count)
      logical ok,go
      logical used(line_count)
      character*22 dict(3)
      integer dumi
      real dumr
      character dumc
      data dict/
     :     'ACCEPT : Accept fits',
     :     'RETRY  : Try again',
     :     'QUIT   : Give up'/

* The following commentary seems to be partially at odds with the code, 
* as found.  Obtained workspace using multiple DSA_GET_WORK_ARRAY calls 
* checking required types and sizes against called routines.  Added PTR6
* rather than different PTR1s. ---MJC

* Get workspace:                    EXTRA..  CONTROL.. SET_POL..
*   W2PTR   SPDIM1*LINE_COUNT (d)   P         P          P
*   CPXPTR  SPDIM1*LINE_COUNT (d)   P         P          W (PTR5)
*   PTR1    SPDIM1            (d)   W         W
*   CPYPTR  SPDIM1*LINE_COUNT (d)   P         P
*   PTR2    400               (d)             W
*   PTR3    SPDIM1*3 + 40     (d)             W
*   PTR4    LINE_COUNT        (d)                        W
*   PTR6    SPDIM1*4          (d)                        W
*   NPTS    LINE_COUNT        (i)   P         P          P
*
* This is made to over-lie as much as possible.
*
*   PTR5 = CPXPTR and the workspace it accesses is the same size
* possibly
* P=passed, W=workspace

* Maximum number of elements needed for PTR1-4+CPYPTR:
*     (spdim1*4+line_count) is for SET_POL..
*     (spdim1*(line_count+4)+440) is for CONTROL..

* Now add element for CPXPTR, multiple by VAL__NBD and add bytes for 
*NPTS

      call dsa_get_work_array(spdim1*line_count,'double',w2ptr,slot,
     :                        status)
      call dsa_get_work_array(spdim1*line_count,'double',cpxptr,slot2,
     :                        status)
      call dsa_get_work_array(spdim1*line_count,'double',cpyptr,slot3,
     :                        status)
      call dsa_get_work_array(spdim1,'double',ptr1,slot4,status)
      call dsa_get_work_array(400,'double',ptr2,slot5,status)
      call dsa_get_work_array(3*spdim1+2*MAX_KPLUS1,'double',ptr3,slot6,
     :                        status)
      call dsa_get_work_array(line_count,'double',ptr4,slot7,status)
      call dsa_get_work_array(line_count,'int',npts,slot8,status)
      call dsa_get_work_array(spdim1*4,'double',ptr6,slot9,status)
      if(status.ne.SAI__OK) then
        go to 550
      end if
      ptr5 = cpxptr

* Extract positions from results block into a form that control_cpoly
* can handle.

      call extr_pos(%VAL(CNF_PVAL(d_rptr)),%VAL(CNF_PVAL(d_vptr)),
     :              %VAL(CNF_PVAL(staptr)),%VAL(CNF_PVAL(cpxptr)),
     :              %VAL(CNF_PVAL(cpyptr)),%VAL(CNF_PVAL(npts)),ylim,
     :              %VAL(CNF_PVAL(d_aptr)),%VAL(CNF_PVAL(w2ptr)),
     :              %VAL(CNF_PVAL(d_xptr)),wavdim,%VAL(CNF_PVAL(ptr1)))

* Fit polynomials in X-Sect direction.

      go=.true.
      do while(go)
        call control_cpoly(%VAL(CNF_PVAL(cpyptr)),
     :                     %VAL(CNF_PVAL(cpxptr)),%VAL(CNF_PVAL(npts)),
     :                     spdim1,line_count,aa,MAX_KPLUS1,kp1,
     :                     'X-Sects','Channels',%VAL(CNF_PVAL(w2ptr)),
     :                     %VAL(CNF_PVAL(ptr1)),used,batch,
     :                     %VAL(CNF_PVAL(ptr2)),%VAL(CNF_PVAL(ptr3)),
     :                     athree,a3all)

*     Do we accept the fits?

        call qmenu('Continuity Fits',dict,3,1,dumr,dumc,key,dumi
     :       ,status)
        ok = key.eq.1
        go = key.eq.2
      end do

* Free workspace

      if(.not.ok) goto 550

* Put positions interpolated etc. from fits into results block.

      call set_poly_res(%VAL(CNF_PVAL(d_rptr)),%VAL(CNF_PVAL(d_vptr)),
     :                  aa,MAX_KPLUS1,ylim,kp1,%VAL(CNF_PVAL(w2ptr)),
     :                  %VAL(CNF_PVAL(npts)),%VAL(CNF_PVAL(ptr5)),
     :                  used,%VAL(CNF_PVAL(d_aptr)),
     :                  %VAL(CNF_PVAL(ptr6)),polydata,
     :                  %VAL(CNF_PVAL(ptr4)),athree,maxnpts2,a3all)

* Release workspace

      call dsa_free_workspace(slot9,status)
      call dsa_free_workspace(slot8,status)
      call dsa_free_workspace(slot7,status)
      call dsa_free_workspace(slot6,status)
      call dsa_free_workspace(slot5,status)
      call dsa_free_workspace(slot4,status)
      call dsa_free_workspace(slot3,status)
      call dsa_free_workspace(slot2,status)
      call dsa_free_workspace(slot,status)
 550  continue
      end
