      subroutine setvals(data,ndim,dims)
*+
* Name:
*    SETVALS

* Invocation:
*    CALL SETVALS(DATA,NDIM,DIMS)

* Purpose:
*  Set a region of an array to a value.
*
* Description:
*  Set a region of an array to a value.
*
* Arguments:
*      NDIM = INTEGER (Given)
*        Number of dimensions of data
*      DIMS(NDIM) = INTEGER ARRAY (Given)
*        Dimensions
*      DATA(*) = REAL ARRAY (Returned)
*        Data array to set
*    Subroutines/functions referenced:

* Author:
*   T.N.Wilkins, Cambridge, 22-OCT-1990
*-
      implicit none
      include 'PRM_PAR'
      real data(*)
      integer ndim
      integer dims(ndim)
      integer i,MAXDIM
      real value
      parameter (MAXDIM=7)
      integer s(MAXDIM),e(MAXDIM),pos(MAXDIM),j,mult,diff(MAXDIM)
      logical more,qstat,par_qnum

*

      do i = 1, ndim
        qstat = par_qnum('Array start',1.0,real(dims(i)),1.0,.true.,' ',
     :      value)
        s(i) = nint(value)
        qstat = par_qnum('Array end',1.0,real(dims(i)),real(dims(i)),
     :      .true.,' ',value)
        e(i) = nint(value)
      end do
      qstat = par_qnum('Value',VAL__MINR,VAL__MAXR,0.0,.true.,' ',value)

      i = 1
      mult = 1

* Check this*************************

      do j = 1, ndim
        pos(j) = s(j)
        i = i + (pos(j)-1)*mult

* DIFF is to take the value from the pixel after E to the next S.

        diff(j) = mult*(dims(j) + s(j) - e(j) - 1)
        mult = mult*dims(j)
      end do

      more = .true.
      do while(more)
        data(i) = value
        pos(1) = pos(1) + 1
        i = i + 1
        do j = 1, (ndim-1)
          if(pos(j).gt.e(j)) then
            pos(j) = s(j)
            pos(j+1) = pos(j+1) + 1
            i = i + diff(j)
          end if
        end do
        more = pos(ndim).le.e(ndim)
      end do

      end
