      subroutine tram_weights(line_count,left,right,sdata,wavdim,
     :     weights)
*+
* Name:
*    TRAM_WEIGHTS

* Invocation:
*    CALL TRAM_WEIGHTS(LINE_COUNT,LEFT,RIGHT,SDATA,WAVDIM,WEIGHTS)

* Purpose:
*  Set weights to exclude lines

* Description:
*  To set the weights to 1, except for within line boundaries, where
*  they are to be set to 1.0d-6.
*
* Arguments:
*      LINE_COUNT = INTEGER (Given)
*        Number of lines identified
*      LEFT(LINE_COUNT) = REAL ARRAY (Given)
*        Left trams
*      RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*        right trams
*      SDATA(WAVDIM) = REAL ARRAY (Given)
*        Wavelengths array (first axis of data)
*      WAVDIM = INTEGER (Given)
*        Number of channels in data
*      WEIGHTS(WAVDIM) = DOUBLE PRECISION ARRAY (Returned)
*        Weights array
* Subroutines/functions referenced:

* History:
*   T.N.Wilkins, Cambridge, 14-AUG-1990
*-
      implicit none
      integer line_count
      real left(line_count)
      real right(line_count)
      integer wavdim
      real sdata(wavdim)
      double precision weights(wavdim)

*

      integer i,line,istart,iend,rx2chn

* To start with set weights to 1.0. This really should use any errors
* array

      do i = 1, wavdim
        weights(i) = 1.0d0
      end do
      do line = 1, line_count

*  Get range of lines, in channel numbers

        istart = rx2chn(sdata,wavdim,left(line))
        iend = rx2chn(sdata,wavdim,right(line))
        istart = max(istart,1)
        istart = min(istart,wavdim)
        iend = max(iend,1)
        iend = min(iend,wavdim)
        do i = istart, iend
          weights(i) = 1.0d-6
        end do
      end do
      end
