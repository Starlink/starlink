      subroutine hpfil(n,in,out,work,nwidth)
*+
* Name:
*   HPFIL

* Purpose:
*  High pass filter and set negative numbers to 0

* Description:
*  A low-pass filtered version of the data is produced, and subtracted
*  from the data.

* Arguments:
*  N = INTEGER (Given)
*    Number of data points
*  IN = REAL ARRAY (Given)
*    Data to be filtered
*  OUT = REAL ARRAY (Returned)
*    Filtered data
*  WORK  = REAL ARRAY (workspace)

* Authors:
*  TNW: T.N.Wilkins, Durham

* History:
*  9/8/93 Original version
*-
      implicit none
      integer n,is,ie,nw,i,nwidth,nw2
      real in(n),out(n),work(*)
      nw2 = 2*nwidth
      do i = 1, n
         is = max(1,i-nw2)
         ie = min(n,i+nw2)
         nw = ie - is + 1
         call copr2r(nw,in(is),work)
         call cla_med(nw,work,out(i))
      enddo
      do i = 1, n
         out(i) = max(0.0,(in(i) - out(i)))
      enddo
      end

