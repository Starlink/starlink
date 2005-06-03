      subroutine cheby_base(max_kp1,coeffs,kp1,ifseek,work,status)
*+
* Name:
*    CHEBY_BASE

* Invocation:
*    CALL CHEBY_BASE(MAX_KP1,COEFFS,KP1,IFSEEK,WORK,STATUS)

* Purpose:
*  To fit a Chebyshev polynomial to a base.
*
* Description:
*  To fit a Chebyshev polynomial to a base.
*
* Arguments:
*      MAX_KP1 = INTEGER (Given)
*        Maximum order for fit + 1
*      IFSEEK = LOGICAL (Given)
*        If to use SEEK mode (to decide on order)
*                      If KP1.lt.2 then SEEK used anyway
*      KP1 = INTEGER (Given and returned)
*        Order+1
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*      COEFFS(MAX_KP1) = DOUBLE PRECISION ARRAY (Returned)
*        Fit coefficients
*      WORK(WAVDIM*6+MAX_KP1*MAX_KP1+MAX_KP1*3) = DOUBLE PRECISION ARRAY (Workspace)
*    Subroutines/functions referenced:

* History:
*   T.N.Wilkins, Cambridge, 14-AUG-1990
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer max_kp1
      double precision coeffs(max_kp1)
      integer kp1
      logical ifseek
*

      double precision work(*)
      integer status
      include 'arc_dims'
      logical seek,plot
      integer ptr0,ptr1,ptr2,weightptr,cnv_fmtcnv,nbad,len1,pstat,ptr3
      character*30 chars
      include 'DYNAMIC_MEMORY'

      if(status.ne.SAI__OK) return

* Workspace passed from above, we divide up the array since it's all
* double precision and it makes it slightly less non-standard this way!
*    PTR0  WAVDIM (d)
*    PTR1  MAX_KP1*MAX_KP1 (d)
*    PTR2  WAVDIM (d)
*    WEIGHTPTR  WAVDIM (d)
*    PTR3  WAVDIM*3+MAX_KP1*3 (d)

      ptr0 = 1
      ptr1 = 1 + wavdim
      ptr2 = ptr1 + max_kp1*max_kp1
      weightptr = ptr2 + wavdim
      ptr3 = weightptr + wavdim

* Transfer base to double precision array for fitting

      status = cnv_fmtcnv('float','double',dynamic_mem(d_vsptr),
     :       work(ptr0),wavdim,nbad)
      if(status.ne.SAI__OK) goto 500
      status = cnv_fmtcnv('float','double',dynamic_mem(d_xptr),
     :       work(ptr2),wavdim,nbad)
      if(status.ne.SAI__OK) goto 500

* Set weights inside tram lines to 1.0e-6

      call tram_weights(line_count,%VAL( CNF_PVAL(d_tlptr) ),
     :          %VAL( CNF_PVAL(d_trptr) ),dynamic_mem(d_xptr),wavdim,
     :          work(weightptr))

* Perform fitting and get user to decide on order

      seek = ifseek.or.(kp1.lt.2)
      plot = seek
      if(.not.(batch.and.seek)) then
        call contrl_cpoly2(work(ptr2),work(ptr0),wavdim,
     :            work(weightptr),coeffs,max_kp1,kp1,xunits,' ',2,
     :            work(ptr1),plot,seek,work(ptr3))
      end if

      return

 500  continue
      len1 = 0
      call chr_putc('Conversion error, ',chars,len1)
      call chr_puti(status,chars,len1)
      call par_wruser(chars(:len1),pstat)
      end
