      subroutine submode(bin,npixel,mode,median,sigma2,mean)
*+
* Name:
*    SUBMODE

* Invocation:
*    CALL SUBMODE(BIN,NPIXEL,MODE,MEDIAN,SIGMA2,MEAN)

* Purpose:
*   Calculate mode, median ,mean, and standard deviation of array
*
* Description:
*   Calculate mode, median, mean, and standard deviation of 1d array
*   WHOSE ELEMENTS ARE IN ASCENDING ORDER
*
* Arguments:
*    BIN(NPIXEL) = REAL ARRAY (Given)
*       Data array to get mode etc. of
*    NPIXEL = INTEGER (Given)
*       Dimension of BIN
*    MODE = REAL (Returned)
*       Mode
*    MEDIAN = REAL (Returned)
*       Median
*    SIGMA2 = REAL (Returned)
*       Standard deviation * 2
*    MEAN = REAL (Returned)
*       Mean
*-
      implicit none
      integer npixel
      real bin(npixel)
      real mode,median,mean
      integer length,iodd,m,i
      real sigma2
      real false,cx

      length = npixel/2
      iodd = npixel-length*2
      m = length+1
      median = bin(m)
      if(iodd.ne.1) then
        median = (median+bin(length))*0.5
      end if
      false = bin(1)
*
* do mean calculation
*
      mean = 0.0
      sigma2 = 0.0
      do i = 1,npixel
        cx = bin(i)-false
        mean = mean+cx
        sigma2 = sigma2+cx*cx
      end do
      sigma2 = sigma2/npixel
      mean = mean/npixel
      sigma2 = sigma2-mean*mean
      sigma2 = sqrt(sigma2)*2.0
      mean = mean+false
      mode = mean-3.0*(mean-median)
      end
