      subroutine extr3(data,d1,d2,d3,s2,e2,s3,e3,output)
*+
* Name:
*    EXTR3

* Invocation:
*    CALL EXTR3(DATA,D1,D2,D3,S2,E2,S3,E3,OUTPUT)

* Purpose:
*  Take cut in 3-d data

* Description:
*  The data in the range S2-E2, S3-E3 is summed and written to OUTPUT,
*  retaining its position in the 1st dimension.
*
* Arguments:
*      DATA(D1,D2,D3) = REAL ARRAY (Given)
*        Input data
*      D1 = INTEGER (Given)
*        1st dimension of DATA
*      D2 = INTEGER (Given)
*        2nd dimension of DATA
*      D3 = INTEGER (Given)
*        3rd dimension of DATA
*      S2 = INTEGER (Given)
*        Start to extract in direction 2
*      E2 = INTEGER (Given)
*        End to extract in direction 2
*      S3 = INTEGER (Given)
*        Start to extract in direction 3
*      E3 = INTEGER (Given)
*        End to extract in direction 3
*      OUTPUT(D1) = REAL ARRAY (Returned)
*        Extracted data
* Subroutines/functions referenced:

* History:
*   T.N.Wilkins, Cambridge,  2-JAN-1990
*-
      implicit none
      integer d1
      integer d2
      integer d3
      real data(d1,d2,d3)
      integer s2
      integer e2
      integer s3
      integer e3
      real output(d1)
      real value
      include 'PRM_PAR'
*
      integer i,j,k

*  Zero output array

      call zero_real(output,d1)

*  Add data together

      do k = s3, e3
        do j = s2, e2
          do i = 1, d1
            value = data(i,j,k)
            if(value.ne.VAL__BADR) then
              output(i) = output(i) + value
            end if
          end do
        end do
      end do
      end
