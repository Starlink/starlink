      subroutine rms(answers,average,err,n,m)
*+
* Name:
*    RMS

* Invocation:
*    CALL RMS(ANSWERS,AVERAGE,ERR,N,M)

* Description:
*  Form the average values of heckman OR whittle quartiles and determine
*  the standard deviation of each one given n monte simulations.
*
* Purpose:
*  Form the average values of heckman OR whittle quartiles and determine
*  the standard deviation of each one given n monte simulations.
*
* Arguments:
*    N = INTEGER (Given)
*        number of quartiles
*    M = INTEGER (Given)
*        number of realizations
*    ANSWERS(N,M) = REAL ARRAY (Given)
*        quartile locations
*    AVERAGE(N) = REAL ARRAY (Returned)
*        average location of quartile
*    ERR(N) = REAL ARRAY (Returned)
*        standard deviation of qaurtile
*
*-
      implicit none
      integer m
      integer n
      real answers(n,m)
      real average(n)
      real err(n)
* local
      integer j
      integer k
      real mean
      real sd
      real diff
* ---------------------------------------------------------------
      do j = 1,n
        mean=0.0
        do k = 1,m
          mean = mean + answers(j,k)
        end do
        average(j) = mean/real(m)
        sd = 0.0
        do k = 1,m
          diff = answers(j,k)-average(j)
          sd   = sd +diff*diff
        end do
        err(j) = sqrt( sd/ real(m-1) )
      end do
      end
