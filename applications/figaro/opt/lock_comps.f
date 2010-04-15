      subroutine lock_comps(guess_store,times,n_gauss,bounds,max_cmp
     :   ,max_times,rank,link,sort_centres,sorted)
*+
* Name:
*    LOCK_COMPS

* Invocation:
*    CALL LOCK_COMPS(GUESS_STORE,TIMES,N_GAUSS,BOUNDS,MAX_CMP
*        ,MAX_TIMES,RANK,LINK,SORT_CENTRES,SORTED)

* Purpose:
*   Lock order of components

* Description:
*   Fix the order of the centres of the gaussian components
*   by defining bounds on which are the centres
*   of the previous parameters
*   this requires that the guess_store array be sorted
*   into ascending order of centres.
*
* Arguments:
*    GUESS_STORE(MAX_PARS,MAX_CMP,MAX_TIMES = REAL (Given)
*
*    TIMES = INTEGER (Given)
*
*    N_GAUSS = INTEGER (Given)
*
*    BOUNDS(MAX_PARS,MAX_CMP,MAX_TIMES,2) = REAL ARRAY (Given)
*
*    MAX_CMP = INTEGER (Given)
*
*    MAX_TIMES = INTEGER (Given)
*
*    RANK(MAX_CMP) = INTEGER ARRAY (Given)
*
*    LINK(MAX_CMP) = INTEGER ARRAY Workspace (Given)
*
*    SORT_CENTRES(MAX_CMP) = DOUBLE PRECISION ARRAY (Given)
*
*    SORTED(MAX_CMP) = DOUBLE PRECISION ARRAY (Given)
*
* History:
*  Altered to use fig_nagerr, TNW 7/10/88
*  Change to m01daf, 1/12/88
*  Change to avoid use of common include file, bug fix, TNW 24/1/89
*  Removed NAG and replaced with PDA - JWP Feb 1997
*
      implicit none
      integer MAX_PARS
      parameter (MAX_PARS=4)
*-
      integer times
      integer max_cmp
      integer max_times
      integer n_gauss
      real bounds(MAX_PARS,max_cmp,max_times,2)
      real guess_store(MAX_PARS,max_cmp,max_times)

      double precision sort_centres(max_cmp+2)
      integer rank(max_cmp+2), link(max_cmp+2)
      integer ifail
      integer i
      double precision sorted(max_cmp)

      include 'SAE_PAR'

      do i = 1,n_gauss
        sort_centres(i)=guess_store(4,i,times)
      end do
      ifail=1

* Get rank to ascending order

      CALL PDA_SAARD(sort_centres,max_cmp+2,n_gauss,1,rank,link,ifail)
      if(ifail.ne.0) then
        call ERR_REP(' ','PDA_SAARD error in lock_comps', SAI__ERROR)
        return
      end if
      CALL PDA_IPERM( n_gauss, rank )
      if(ifail.ne.0) then
        call ERR_REP(' ','PDA_IPERM error in lock_comps', SAI__ERROR)
        return
      end if

* Sort into ascending order (done this way to get required rank)

      do i=1,n_gauss
        sorted(rank(i)) = sort_centres(i)
      end do

* loop over the locked components
* making special provisions for the first and last
* components

      do i = 1,n_gauss
        if(rank(i).ne.1) then
          bounds(4,i,times,2) = real(sorted(rank(i)-1))
        else
          bounds(4,i,times,2) = 1.0e-6
        end if
        if(rank(i).lt.n_gauss) then
          bounds(4,i,times,1) = real(sorted(rank(i)+1))
        else
          bounds(4,i,times,1) = 1.0
        end if
      end do
      end









