      subroutine getX_template(data,ni,nl,intensityl)
*+
* Name:
*    GETX_TEMPLATE

* Invocation:
*    CALL GETX_TEMPLATE(DATA,NI,NL,INTENSITYL)

* Purpose:
*   This extracts the data from a slice through the middle, in the X
*   direction into a Real Array.

* Arguments:
*   This extracts the data from a slice through the middle, in the X
*   direction into a  Real*4 Array. The user
*   selected limits ASTART and AEND for the axis define the limits
*   of the array DATA which will be used for the collapse in this direction.
*   Each template covers the complete axis range of the other axis.
*   All operation are performed in PIXEL SPACE rather than
*   with the actual values contained in the AXIS1.DATA and AXIS2.DATA
*   arrays.

* Arguments:
*   NI = INTEGER (Given)
*        Input image dimensions
*   NL = INTEGER (Given)
*        Input image dimensions
*   INTENSITYL[NL] = REAL (Given)
*        Output R*4 spectrum formed
*   DATA[NL,NI] = INTEGER (Returned)
*        Input image
*                       between ASTART and AEND for axis 2 and covering
*                       the whole of NL
*- ---------------------------------------------------------------------
*
      implicit none
      integer ni,nl
      real data(nl,ni)
      real intensityl(nl)
*
      integer astart,aend
      real dummy1,dummy2
      integer status,i,j

      status = 0

* get the X template range

      call dsa_axis_range('image',2,' ',.false.,dummy1,dummy2,astart,
     :   aend,status)

* form the template

      do i = 1, nl
        intensityl(i) = data(i,astart)
      end do
      do j = astart+1, aend
        do i = 1, nl
          intensityl(i) = data(i,j) + intensityl(i)
        end do
      end do

      end
