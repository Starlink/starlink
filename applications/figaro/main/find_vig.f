      subroutine find_vig(data,ni,nl,intensityi,intensityl)
*+
* Name:
*    FIND_VIG

* Invocation:
*    CALL FIND_VIG(DATA,NI,NL,INTENSITYI,INTENSITYL)

* Purpose:
*   To find the vignetting in x-section and channel directions.

* Description:
*   To find the vignetting in x-section and channel directions.
*   This extracts the data from a slice through the middle, in both
*   directions into Double Precision Arrays. The user
*   selected limits ASTART and AEND for each axis separately define
*   the arrays of the array along the orthogonal axis which will
*   be used for the collapse in each direction. Each template
*   covers the complete axis range of the other axis.
*   All operation are performed in PIXEL SPACE rather than
*   with the actual values contained in the AXIS1.DATA and AXIS2.DATA
*   arrays.
* Arguments:
*  Parameters - (">" input, "<" Output, "!" Modified, "W" workspace)
*   NI = INTEGER (Given)
*        Input image dimensions
*   NL             (i):Input image dimensions
*   DATA[NL,NI] = INTEGER (Given)
*        Input image
*   INTENSITYL[NL] = DOUBLE PRECISION (Returned)
*        Output Double precision spectrum formed
*                       between ASTART and AEND for axis 2 and covering
*                       the whole of NL
*   INTENSITYI[NI] = DOUBLE PRECISION (Returned)
*        Output Double precision spectrum formed
*                       between ASTART and AEND for axis 1 and covering
*                       the whole of NI
*- ---------------------------------------------------------------------
*
      implicit none
      include 'SAE_PAR'
      integer ni,nl
      real data(nl,ni)
      double precision intensityi(ni),intensityl(nl)
*
      integer astart,aend
      real dummy1,dummy2
      integer status,i,j

      status = SAI__OK

* Extract data in both directions

      call dsa_axis_range('image',1,' ',.false.,dummy1,dummy2,astart,
     :   aend,status)

      do j = 1, ni
        intensityi(j) = dble(data(astart,j))
        do i = astart+1, aend
          intensityi(j) = dble(data(i,j)) + intensityi(j)
        end do
      end do

      call dsa_axis_range('image',2,' ',.false.,dummy1,dummy2,astart,
     :   aend,status)

      do i = 1, nl
        intensityl(i) = dble(data(i,astart))
      end do
      do j = astart+1, aend
        do i = 1, nl
          intensityl(i) = dble(data(i,j)) + intensityl(i)
        end do
      end do
      end
