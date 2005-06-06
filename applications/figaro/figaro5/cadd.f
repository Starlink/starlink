      subroutine cadd( STATUS )
*+
* Name:
*    CADD

* Invocation:
*    CALL CADD( STATUS )
*
* Purpose:
*   To add a continuum to 2-dimensional data.

* Description:
*  A polynomial previously fitted to the continuum is evaluated and
*  this is added.

* Parameters:
*    IMAGE = FILE (Read)
*        Name of image for input
*    OUTPUT = FILE (Write)
*        OUTput Name of output file
*        OUTPUT is the name of the resulting spectrum. If OUTPUT is the
*        same as INPUT the data in the input file will be modified in
*        situ.Otherwise a new file will be created.

* Authors:
*   T.N.Wilkins Macnhester
*   A.J.Holloway "
* History
*
* Changed dsa_map.. from 'u' to 'UPDATE'
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      integer max_ord,jptr,iptr,i
      integer nl,ni
      integer mord
      integer kp1l
      integer ptr1,ptr2,slot,slot1,slot2
      integer nlr,nir
      integer coef_siz,dims_csub(2)
      parameter (max_ord=20)
      integer dims(2),ndim,nelm
*  ---------------------------------------------------------------------
      mord=max_ord
      status = SAI__OK
      call dsa_open(status)
*
*   Get name of input file
*
      call dsa_input('image','image',status)
*
*     Get dimensions of input data
*
      call dsa_data_size('image',2,ndim,dims,nelm,status)
      nl=dims(1)
      if(ndim.eq.2) then
        ni=dims(2)
      else
        ni=1
      end if
*
*  Get name of output file
*
      call dsa_output('output','output','image',0,0,status)
*
*  Set up coefficient structure
*
      call accres('output','continuum','fi',0,0,' ',status)
      call accres(' ','ni','ri',1,nir,' ',status)
      call accres(' ','nl','ri',1,nlr,' ',status)
      if(status.ne.SAI__OK) goto 500
      if (nl.ne.nlr) then
        call par_wruser('nl different from coeffs',status)
      end if
      if (ni.ne.nir) then
        call par_wruser('ni different from coeffs',status)
      end if
      dims_csub(1)=max_ord
      dims_csub(2)=ni
      coef_siz=ni*max_ord
      ndim = 2
      call accres(' ','coeff','si',ndim,dims_csub,' ',status)
      coef_siz=dims_csub(1)*dims_csub(2)
      call accres(' ','coeff','du',coef_siz,jptr,' ',status)
*
*  Map the data
*
      call dsa_map_data('output','UPDATE','float',iptr,slot,status)
*
*   Fit vignetting
*
      kp1l=max_ord
      call dsa_get_work_array(nl,'double',ptr1,slot1,status)
      call dsa_get_work_array(nl,'double',ptr2,slot2,status)
      if(status.ne.SAI__OK) goto 500
      do i=1,ni
        call correct2(%VAL(CNF_PVAL(iptr)),%VAL(CNF_PVAL(jptr)),nl,ni,
     :                mord,kp1l,i,%VAL(CNF_PVAL(ptr1)),
     :                %VAL(CNF_PVAL(ptr2)),.true.)
      end do

  500 continue
      call dsa_close(status)
      end
