      subroutine spline_base(wavdim,sdata,sdens,line_count,left
     :  ,right,m,adata,dbase,status,xin,yin,x)
*+
* Name:
*    SPLINE_BASE

* Invocation:
*    CALL SPLINE_BASE(WAVDIM,SDATA,SDENS,LINE_COUNT,LEFT
*       ,RIGHT,M,ADATA,DBASE,STATUS,XIN,YIN,X)

* Purpose:
*  Obtain base using spline interpolation.

* Description:
*  To obtain the base within the boundaries of the current line using
*  cubic spline interpolation.
*
* Arguments:
*      WAVDIM = INTEGER (Given)
*        Number of channels in data
*      SDATA(WAVDIM) = REAL ARRAY (Given)
*        X array data
*      SDENS(WAVDIM) = REAL ARRAY (Given)
*        Intensity data
*      LINE_COUNT = INTEGER (Given)
*        Number of lines identified
*      LEFT(LINE_COUNT) = REAL ARRAY (Given)
*        Left boundaries of lines
*      RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*        Right boundaries of lines
*      M = INTEGER (Given)
*        Number of channels in current line
*      ADATA(M) = DOUBLE PRECISION ARRAY (Given)
*        X array within line boundaries only
*      X(WAVDIM) = REAL ARRAY (Given)
*        As SDATA, but taken directly from D_XPTR (this
*                         is in case velocity scale for X axis in use)
*      STATUS = INTEGER (Given and returned)
*        Error status (0=ok)
*      DBASE(M) = DOUBLE PRECISION ARRAY (Returned)
*        Base
*      YIN(WAVDIM) = DOUBLE PRECISION ARRAY (Workspace)
*        SDENS with lines removed
*      XIN(WAVDIM) = DOUBLE PRECISION ARRAY (Workspace)
*        SDATA with lines removed
*    Subroutines/functions referenced:
*      INTRPL,DX2CHN,CNV_FMTCNV
* Author:
*   T.N.Wilkins, Cambridge, 20-AUG-1990
*-
      implicit none
      integer wavdim
      real sdata(wavdim)
      real x(wavdim)
      real sdens(wavdim)
      integer line_count
      real left(line_count)
      real right(line_count)
      integer m
      double precision adata(m)
      double precision dbase(m)
      integer status
      double precision xin(wavdim)
      double precision yin(wavdim)

*

      include 'PRM_PAR'
      integer nin,rx2chn,cnv_fmtcnv,nbad,nless,line,i,istart,iend,curpos

* First of all, copy the arrays SDATA and SDENS to work arrays (double
* precision). Do we need the whole lot?

      status = cnv_fmtcnv('float','double',sdata,xin,wavdim,nbad)
      status = cnv_fmtcnv('float','double',sdens,yin,wavdim,nbad)

* Remove from the work arrays all entries within the boundaries of the
* lines, shifting the array entries as required. The method is first to
* flag all values as bad, if we are to ignore them, and then remove
* them. This removes the need for an extra work array, but does slow
* things down a bit.

      do line = 1, line_count
        istart = rx2chn(x,wavdim,left(line))
        iend = rx2chn(x,wavdim,right(line))
        istart = max(1,min(wavdim,istart))
        iend = max(1,min(wavdim,iend))
        do i = istart, iend
          yin(i) = val__badd
        end do
      end do

      curpos = 1
      nin = wavdim
      do while(curpos.le.nin)
        iend = curpos

*    Compare iend with wavdim as extra precaution against access
*    violation

        do while((yin(iend).eq.val__badd).and.(iend.le.nin).and.
     :                  (iend.lt.wavdim))
          iend = iend + 1
        end do
        nless = iend - curpos
        if(nless.gt.1) then
          nin = nin - nless
          do i = curpos, nin
            xin(i) = xin(i+nless)
            yin(i) = yin(i+nless)
          end do
        end if
        curpos = curpos + 1
      end do

* Use INTRPL to interpolate the Y values in this range

      call intrpl(nin,xin,yin,m,adata,dbase,status)
      end
