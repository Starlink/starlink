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
*   The values of the polynomial may then be used rather than the original
*   data.

* Global variables:
*     LINE_COUNT = INTEGER (Given)
*        Number of lines (include file arc_dims)
*     NXP,NYP,MXPARS = INTEGER (Given)
*        Dimensions of results "cube" (include file arc_dims)
*     WAVDIM, SPDIM1 = INTEGER (Given)
*        Dimensions of data (include file arc_dims)
*     RPTR = INTEGER (Given)
*        Pointer to results "cube" (the results cube is written to, although
*        RPTR is unchanged) (include file arc_dims)
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
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      include 'arc_dims'
      logical polydata
*-
      integer cpxptr,w2ptr,cpyptr
      integer kp1,slot
      integer MAX_KPLUS1
      parameter (MAX_KPLUS1=10)
      integer maxnpts
      integer maxnpts2
      parameter (maxnpts2=2048)
      integer npts
      double precision aa(MAX_KPLUS1,line_count)
      double precision athree(3*maxnpts2+3*max_kplus1)
      double precision a3all(3*maxnpts2+3*max_kplus1,line_count)
      integer ptr1,ptr2,ptr3,ptr4,ptr5
      integer nels,nels1cp,key
      double precision ylim(2,line_count)
      logical ok,go
      logical used(line_count)
      character*22 dict(3)
      integer dumi
      real dumr
      character dumc
      include 'PRM_PAR'
      include 'DYNAMIC_MEMORY'
      data dict/
     :     'ACCEPT : Accept fits',
     :     'RETRY  : Try again',
     :     'QUIT   : Give up'/

* Get workspace:                    EXTRA..  CONTROL.. SET_POL..
*   W2PTR   SPDIM1*LINE_COUNT (d)   P         P          P
*   CPXPTR  SPDIM1*LINE_COUNT (d)   P         P          W (PTR5)
*   PTR1    SPDIM1            (d)   W         W
*   PTR1    SPDIM1*4          (d)                        W
*   CPYPTR  SPDIM1*LINE_COUNT (d)   P         P
*   PTR2    400                (d)             W
*   PTR3    SPDIM1*3 + 40     (d)             W
*   PTR4    LINE_COUNT         (d)                        W
*   NPTS    LINE_COUNT         (i)   P         P          P
*
*  This is made to over-lie as much as possible.
*
*   PTR5 = CPXPTR and the workspace it accesses is the same size
* possibly
* P=passed, W=workspace

* Maximum number of elements needed for PTR1-4+CPYPTR:
*     (spdim1*4+line_count) is for SET_POL..
*     (spdim1*(line_count+4)+440) is for CONTROL..

      nels1cp = max((spdim1*4+line_count),
     :     (spdim1*(line_count+4)+440))

* Now add element for CPXPTR, multiple by VAL__NBD and add bytes for NPTS

      nels =    (
     :          spdim1*line_count*2            ! W2PTR+CPXPTR/PTR5
     :          + nels1cp                       ! As above
     :          )*VAL__NBD
     :          + line_count*VAL__NBI           ! NPTS
      call getvm(nels,w2ptr,slot,status)
      if(status.ne.SAI__OK) then
        go to 550
      end if
      cpxptr  = w2ptr + spdim1 * line_count * VAL__NBD
      ptr5 = cpxptr
      ptr1 = cpxptr + spdim1 * line_count * VAL__NBD
      cpyptr = ptr1 + spdim1 * VAL__NBD
      ptr2 = cpyptr + spdim1 * line_count * VAL__NBD
      ptr3 = ptr2 + 400 * VAL__NBD
      ptr4 = ptr1 + 4 * spdim1 * VAL__NBD
      npts = ptr1 + nels1cp * VAL__NBD

*  Extract positions from results block into a form that control_cpoly can
* handle.

      call extr_pos(%VAL( CNF_PVAL(d_rptr) ),%VAL( CNF_PVAL(d_vptr) ),
     :      %VAL( CNF_PVAL(staptr) ),dynamic_mem(cpxptr),
     :      dynamic_mem(cpyptr),dynamic_mem(npts),ylim,
     :      %VAL( CNF_PVAL(d_aptr) ),dynamic_mem(w2ptr),
     :      dynamic_mem(d_xptr),wavdim,dynamic_mem(ptr1))

* Fit polynomials in X-Sect direction.

      go=.true.
      do while(go)
        call control_cpoly(dynamic_mem(cpyptr),dynamic_mem(cpxptr),
     :      dynamic_mem(npts),spdim1,line_count,aa,MAX_KPLUS1,kp1,
     :      'X-Sects','Channels',dynamic_mem(w2ptr),dynamic_mem(ptr1),
     :      used,batch,dynamic_mem(ptr2),dynamic_mem(ptr3),
     :      athree,a3all)

*     Do we accept the fits?

        call qmenu('Continuity Fits',dict,3,1,dumr,dumc,key,dumi
     :       ,status)
        ok = key.eq.1
        go = key.eq.2
      end do

* Free workspace

      if(.not.ok) goto 550

* Put positions interpolated etc. from fits into results block.

      call set_poly_res(%VAL( CNF_PVAL(d_rptr) ),
     :         %VAL( CNF_PVAL(d_vptr) ),aa,MAX_KPLUS1,ylim,kp1,
     :         dynamic_mem(w2ptr),dynamic_mem(npts),dynamic_mem(ptr5),
     :         used,%VAL( CNF_PVAL(d_aptr) ),dynamic_mem(ptr1),polydata,
     :         dynamic_mem(ptr4),athree,maxnpts2,a3all)

* Release workspace

      call dsa_free_workspace(slot,status)
 550  continue
      end
