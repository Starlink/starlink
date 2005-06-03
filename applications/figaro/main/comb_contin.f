      subroutine comb_contin(aa,ylim,used,ifcomb,status)
*+
* Name:
*    COMB_CONTIN

* Invocation:
*    CALL COMB_CONTIN(AA,YLIM,USED,IFCOMB,STATUS)

* Purpose:
*   To fit Chebyshev polynomials to the locations of the teeth located
*   by comb and to output the coefficients.

* Description:
*   To fit Chebyshev polynomials to the locations of the teeth located
*   by comb and to output the coefficients.
*
* Arguments:
*    IFCOMB = LOGICAL (Given)
*        If main routine is COMB
*    STATUS = INTEGER (Given and returned)
*        Error status
*    AA(MAX_KPLUS1,LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*        Polynomial coefficients
*    YLIM(2,LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*        Y limits for polynomial
*    USED(LINE_COUNT) = LOGICAL ARRAY (Workspace)
*        If line used

* Author:
*     TNW: T.N.Wilkins. Manchester until 1/89, Cambridge until 9/92, then
*     Durham.
*     ACD: A C Davenhall. Edinburgh.

* History:
*   TNW: Original version
*   TNW: 11/2/91 Workspace changes
*   TNW: 26/3/91      "       "
*   TNW: 27/3/91 Minor changes
*   TNW: 28/3/91 Altered to work for ARCSDI to.
*   TNW: 12/6/92 Allow for unix dsa_open_text_file
*   TNW: 11/6/93 Make variable NAME long enough
*   ACD: 15/12/00 Make definition of MAX_KPLUS1=10 consistent with
*          value elsewhere, add maxnpts2, athree and a3all.
*
      implicit none
      include 'SAE_PAR'

* Input

      include 'arc_dims'
      include 'CNF_PAR'          ! For CNF_PVAL function
*-
      integer status
      logical ifcomb

* Passed for virtual memory only

      double precision ylim(2,line_count)
      integer MAX_KPLUS1
      parameter (MAX_KPLUS1=10)
      integer maxnpts2
      parameter (maxnpts2=2048)
      double precision aa(MAX_KPLUS1,line_count)
      double precision athree(3*maxnpts2+3*max_kplus1)
      double precision a3all(3*maxnpts2+3*max_kplus1,line_count)
      integer npts
      integer nused
      integer cpxptr,w2ptr,cpyptr,ptr1,ptr3,nels
      integer slot,ptr2
      integer kp1
      character*70 chars
      logical used(line_count)
      logical go
      integer i,lu,key,line
      character name*10,label1*8,label2*8
      integer notherax,ax1pos,ax2pos
      integer dumi
      real dumr
      character dumc
      character*20 dict(3)
      include 'PRM_PAR'
      include 'DYNAMIC_MEMORY'
      data dict/
     :     'ACCEPT : Accept fits',
     :     'RETRY  : Try again',
     :     'QUIT   : Give up'/

* Get workspace                  EXTRA... CONTROL.. PLOT_SD..
*  PTR1    NXP (d)                  W         W
*  PTR3    NXP*3+40       (d)                 W
*  CPXPTR  NXP*LINE_COUNT (d)       P         P         P
*  CPYPTR  NXP*LINE_COUNT (d)       P         P         P
*  W2PTR   NXP*LINE_COUNT (d)       P         P
*  PTR2    400 (d)                            W
*  NPTS    LINE_COUNT     (i)       P         P         P
*

      nels = (nxp*4+nxp*line_count*4+440)*VAL__NBD
     :            + line_count*VAL__NBI
      call getvm(nels,cpxptr,slot,status)
      if(status.ne.SAI__OK) then
        go to 550
      end if
      cpyptr = nxp*VAL__NBD*line_count + cpxptr
      w2ptr = cpyptr + nxp*VAL__NBD*line_count
      ptr1 = w2ptr + nxp*VAL__NBD*line_count
      ptr2 = ptr1 + nxp*VAL__NBD
      ptr3 = ptr2 + 400*VAL__NBD
      npts = ptr3 + (nxp*3+40)*VAL__NBD

      if(ifcomb) then
        name = 'comb.gmc'
        notherax = spdim1
        ax1pos = cpxptr
        ax2pos = cpyptr
        label1 = 'Channels'
        label2 = 'X-Sects'
      else
        name = 'arcsdi.gmc'
        notherax = wavdim
        ax1pos = cpyptr
        ax2pos = cpxptr
        label1 = 'X-Sects'
        label2 = 'Channels'
      end if

*  Extract positions from results block into a form that control_cpoly can
* handle. N.B. PTR1-3 are for workspace only within the routines called
* here, the values of the arrays on entry are not used anywhere.

      call extr_pos(%VAL( CNF_PVAL(d_rptr) ),%VAL( CNF_PVAL(d_vptr) ),
     :      %VAL( CNF_PVAL(staptr) ),dynamic_mem(cpxptr),
     :      dynamic_mem(cpyptr), dynamic_mem(npts),ylim,
     :      %VAL( CNF_PVAL(d_aptr) ),dynamic_mem(w2ptr),
     :      dynamic_mem(d_xptr),notherax,dynamic_mem(ptr1))
      if(batch) then
        call gr_hard(status)
      else
        call gr_soft(status)
      end if
      call plot_sdist(wavdim,spdim1,dynamic_mem(npts),nxp,
     :   dynamic_mem(ax2pos),dynamic_mem(ax1pos),line_count)

* Fit polynomials in X-Sect direction.

      go = .true.
      do while(go)
        call control_cpoly(dynamic_mem(cpyptr), dynamic_mem(cpxptr),
     :    dynamic_mem(npts), nxp, line_count, aa, MAX_KPLUS1, kp1,
     :    label1, label2, dynamic_mem(w2ptr), dynamic_mem(ptr1), 
     :    used, batch, dynamic_mem(ptr2), dynamic_mem(ptr3),
     :    athree, a3all)      

*     Do we accept the fits?

        call qmenu('Continuity Fits',dict,3,1,dumr,dumc,key,dumi
     :       ,status)

        go = key.eq.2
      end do
      if(key.eq.3) goto 550

* Copy coefficients so that there are no "blank" coefficients

      nused = 0
      do line = 1,line_count
        if(used(line)) then
          nused = nused + 1
          do i = 1,kp1
            aa(i,nused) = aa(i,line)
          end do
        end if
      end do

* Free workspace

      call dsa_free_workspace(slot,status)
*
*  Save the coefficients and the starting locations of the tooth
*
      call dsa_open_text_file(name,' ','new',.true.,lu,chars,
     :        status)
      write(lu,1) nused,kp1
    1 format(2(1x,i4))
      do line = 1,nused
        write (lu,2) (ylim(i,line),i=1,2)
      end do
      do line = 1,nused
        write(lu,2) (aa(i,line),i=1,kp1)
      end do
    2 format(10(1pe13.4))
      write(lu,'(''From file '',a)') datafile
      call dsa_free_lu(lu,status)
 550  continue
      end
