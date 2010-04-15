       subroutine fix_pars(guess_store,times,bound,n_gauss,max_cmp,
     :     max_times,status)
*+
* Name:
*    FIX_PARS

* Invocation:
*    CALL FIX_PARS(GUESS_STORE,TIMES,BOUND,N_GAUSS,
*       MAX_CMP,MAX_TIMES,STATUS)

* Purpose:
*   Fix parameters

* Description:
*   In order to FIX the position of any gaussian parameter it
*   is neccesary only to set the lower bounds and the upper bounds
*   equal
*
* Arguments:
*    GUESS_STORE(MAX_PARS,MAX_CMP,MAX_TIMES = REAL (Given)
*        the gaussian guesses
*    TIMES = INTEGER (Given)
*        current iteration number
*    N_GAUSS = INTEGER (Given)
*        current component number
*    MAX_CMP = INTEGER (Given)
*
*    MAX_TIMES = INTEGER (Given)
*
*    STATUS = INTEGER (Given and returned)
*
*    BOUND(MAX_PARS,MAX_CMP,MAX_TIMES,2) = REAL ARRAY (Returned)
*        the bounds store
* History:
*  TNW 12/1/89 max_cmp made argument.
*  TNW 24/1/89 max_times made argument.
*  TNW 10/9/92 Minor changes
*
      implicit none
      integer MAX_PARS
      parameter (MAX_PARS = 4)
*-
      integer times
      integer n_gauss
      integer max_times
      integer max_cmp
      real guess_store(MAX_PARS,max_cmp,max_times)
      integer status
      real bound(MAX_PARS,max_cmp,max_times,2)

* local
      integer i_gauss
      integer iopt
      integer idef

* do loop control

      logical loop
      integer i,j,k
* ---------------------------------------------------------------
      loop = .true.
      idef = 5
      do while(loop)
        call comp_menu(iopt,idef,i_gauss,n_gauss,status)

*   fix the base

        if (iopt.eq.1) then
          do j = 1,2
            do k = 1, max_cmp
              bound(1,k,times,j) = guess_store(1,k,times)
            end do
          end do
          idef = 2

*   fix a single parameter

        else if( (iopt.gt.1).and.(iopt.lt.5) ) then
          do j = 1,2
            bound(iopt,i_gauss,times,j)=guess_store(iopt,i_gauss,times)
          end do

* set the defualt option

          if(iopt.eq.4) then
            idef = 1
          else
            idef = iopt + 1
          end if


*   fix all the parameters

        else if(iopt.eq.5) then
          do j = 1,2
            do i = 2,4
              bound(i,i_gauss,times,j) = guess_store(i,i_gauss,times)
            end do
          end do
          idef = 5

*   exit

        else
          loop = .false.
        end if
      end do
      end
