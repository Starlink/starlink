      subroutine getY_template(data,ni,nl,intensityi)
*+
* Name:
*    GETY_TEMPLATE

* Invocation:
*    CALL GETY_TEMPLATE(DATA,NI,NL,INTENSITYI)

* Description:
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
*   DATA[NL,NI] = INTEGER (Returned)
*        Input image
*   INTENSITYI[NI] = REAL (Returned)
*        Output R*4 spectrum formed
*                       between ASTART and AEND for axis 1 and covering
*                       the whole of NI
*- ---------------------------------------------------------------------
*
      implicit none
      integer ni,nl
      real data(nl,ni)
      real intensityi(ni)
*
      integer astart,aend
      real dummy1,dummy2
      integer status,i,j

      status = 0

* Extract data in the Y direction

      call dsa_axis_range('image',1,' ',.false.,dummy1,dummy2,astart,
     :   aend,status)

      do j = 1, ni
        intensityi(j) = data(astart,j)
        do i = astart+1, aend
          intensityi(j) = data(i,j) + intensityi(j)
        end do
      end do
      end
