      subroutine error_lmder(m,n,x,fvec,fjac,ldfjac,iflag)
*+
* Name:
*    ERROR_LMDER

* Invocation:
*    CALL ERROR_LMDER(M,N,X,FVEC,FJAC,LDFJAC,IFLAG)

* Purpose:
*  Dummy routine to return error flag for lmder, to abort fitting.

* Description:
*  Dummy routine to return error flag for lmder, to abort fitting.
*-
      implicit none
      integer m,n,ldfjac,iflag
      double precision x(n),fvec(m),fjac(ldfjac,n)
      iflag = -1
      end
