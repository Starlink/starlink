      subroutine fill_dat(sdata,m,hsdata)
*+
* Name:
*    FILL_DAT

* Invocation:
*    CALL FILL_DAT(SDATA,M,HSDATA)

* Description:
*   This gives a higher resolution version of the X array for plotting.
*   Re-written on original code's being 'lost'
*
* Purpose:
*   This gives a higher resolution version of the X array for plotting.
*   Re-written on original code's being 'lost'
*
* Arguments:
*    SDATA(M) = REAL ARRAY (Given)
*        X data
*    M = INTEGER (Given)
*        Dimension of SDATA
*    HSDATA(M*5) = REAL ARRAY (Returned)
*        Higher resolution version of SDATA
*
*-
      implicit none
      integer m
      real sdata(m),hsdata(m*5)
      integer i
      real dispersion

      dispersion = (sdata(m)-sdata(1))/real((m-1)*5)
      do i = 1,m*5
        hsdata(i) = sdata(1)+(i-1)*dispersion
      end do
      end
