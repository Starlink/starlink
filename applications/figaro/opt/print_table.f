      subroutine print_table(table,idef,max_cmp,dg_parm,model)
*+
* Name:
*    PRINT_TABLE

* Invocation:
*    CALL PRINT_TABLE(TABLE,IDEF,MAX_CMP,DG_PARM,MODEL)


* Purpose:
*  Print out the stored guesses or answers for a multiple gaussian fit
*
* Description:
*  Print out the stored guesses or answers for a multiple gaussian fit
*
* Arguments:
*    TABLE(MAX_PARS,MAX_CMP,MAX_TIMES) = REAL ARRAY (Given)
*        Guess store
*    IDEF = INTEGER (Given)
*        Number of slots used
*    MAX_CMP = INTEGER (Given)
*        Maximum number of components
*                                         allowed for
*    MODEL = INTEGER (Given)
*        Model (e.g. Gaussian)
*   Work space:
*    DG_PARM(*) = REAL ARRAY (Given)
*        scaled parameters
*-
* import
      implicit none
      integer idef
      integer MAX_PARS
      parameter (MAX_PARS=4)
      integer max_cmp,model
      real table(MAX_PARS,max_cmp,*)

* fit parameters -work array

      real dg_parm(*)
      integer k1
      integer k2
      integer k3
      integer n_gauss
      integer i,j
      integer status
      character*72 chars
      double precision EFOLD,mult
      parameter (EFOLD=2.35482004d0)
      include 'fit_coding_inc'
* --------------------------------------------------------------

      call par_wruser(
     : 'Store   Component   Centre    fwhm       Height    Base',status)
* loop over the stores
      if(model.eq.GAUSSIAN_MODEL) then
        mult = EFOLD
      else if(model.eq.LORENTZ_MODEL) then
        mult = 2.0d0
      else
        mult = 1.0d0
      endif
      do j = 1,idef

* special case of first gaussian + base

        n_gauss=0
        do i=1,max_cmp
          if(table(4,i,j).gt.1.0e-6) n_gauss=i
        end do
        i=1
        if(n_gauss.gt.0) then

          call rescale_store(dg_parm,table,n_gauss,j,4)
          call par_wruser(' ',status)
          write(chars,'(i1,i3,4x,1pg15.7,3(2x,1pg11.4 ))')
     +    j,i,dg_parm(4),dg_parm(2)*mult,dg_parm(3),dg_parm(1)
          call par_wruser(chars,status)

* do the remaining gaussians

          do i=2,n_gauss
            k1=(i-1)*3+2
            k2=k1+1
            k3=k1+2
            write(chars,'(i1,i3,4x,1pg15.7,2(2x,1pg11.4 ))')
     :         j,i,dg_parm(k3),dg_parm(k1)*mult,dg_parm(k2)
            call par_wruser(chars,status)
          end do
        end if
      end do
      end
