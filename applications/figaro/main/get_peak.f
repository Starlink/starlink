      subroutine get_peak(line,sdata,sdens,wavdim,left,right)
*+
* Name:
*    GET_PEAK

* Invocation:
*    CALL GET_PEAK(LINE,SDATA,SDENS,WAVDIM,LEFT,RIGHT)

* Purpose:
*  Find peak of line

* Description:
*  To get the peak position of the line between the boundaries left and
* right. This is returned in double precision for use in Nag fitting.
*
* Functions called:
*     RX2CHN = INTEGER (Given)
*        Convert real array value (for linear array) to array element
*     GEN_PMAX = INTEGER (Given)
*        Locate maximum value in array
*
* Arguments:
*    SDATA(WAVDIM) = REAL ARRAY (Given)
*        X data
*    SDENS(WAVDIM) = REAL ARRAY (Given)
*        Y data
*    WAVDIM = INTEGER (Given)
*        Dimension of sdata/sdens
*    LEFT = REAL (Given)
*        Left tram (X units)
*    RIGHT = REAL (Given)
*        Right "    "   "
*    LINE = DOUBLE PRECISION (Returned)
*        Peak of line

* History:
*    T.N.Wilkins. Manchester February 1987
*    Altered TNW 9/11/88 to use GEN_PMAX, CHN2RX removed
*-
      implicit none
      double precision line
      integer wavdim
      real sdata(wavdim),sdens(wavdim),left,right
* ----------------------------------------------------------------------
      integer ist,npts,gen_pmax
      integer rx2chn
      integer maxpt
      ist = rx2chn(sdata,wavdim,left)
      npts = rx2chn(sdata,wavdim,right) - ist + 1
      maxpt = gen_pmax(sdens(ist),npts) + ist - 1
      line = dble(sdata(maxpt))
      end
