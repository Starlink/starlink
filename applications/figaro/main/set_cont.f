      subroutine set_cont(array,ncntrl,n,value)
*+
* Name:
*    SET_CONT

* Invocation:
*    CALL SET_CONT(ARRAY,NCNTRL,N,VALUE)

* Purpose:
*   Set control array

* Description:
*    To set all the values of the control array to a given model.
*    This sets ARRAY(I,*) to VALUE(I) for I = 1, NCNTRL.
*
* Arguments:
*      NCNTRL = INTEGER (Given)
*        Number of control elements per fit
*      N = INTEGER (Given)
*        Number of fit positions
*      VALUE(NCNTRL) = INTEGER ARRAY (Given)
*        Value(s) to set array to
*      ARRAY(NCNTRL,N) = INTEGER ARRAY (Returned)
*        Array to set to value(s)
*
*    T.N.Wilkins Cambridge 11/4/91
*-
      implicit none
      integer n,i,ncntrl,j,value(ncntrl),array(ncntrl,n)

      do j = 1, n
        do i = 1, ncntrl
          array(i,j) = value(i)
        end do
      end do
      end
