      subroutine set_short(array,n,value)
*+
* Name:
*    SET_SHORT

* Invocation:
*    CALL SET_SHORT(ARRAY,N,VALUE)

* Purpose:
*    To set all the values of an integer*2 array to a given value.

* Description:
*    To set all the values of an integer*2 array to a given value.

* Arguments:
*      N = INTEGER (Given)
*        Dimension of array
*      VALUE = INTEGER (Given)
*        Value to set array to
*      ARRAY(N) = INTEGER ARRAY (Returned)
*        Array to set to value

* History:
*    T.N.Wilkins Manchester 6/7/88
*-
      implicit none
      integer n,i
      integer*2 value,array(n)

      do i = 1, n
        array(i) = value
      end do
      end
