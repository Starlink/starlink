      subroutine spline_trams(wavdim,x,y,line_count,left
     :  ,right,yout,status,xin,yin)
*+
* Name:
*    SPLINE_TRAMS

* Invocation:
*    CALL SPLINE_TRAMS(WAVDIM,X,Y,LINE_COUNT,LEFT
*       ,RIGHT,YOUT,STATUS,XIN,YIN)

* Purpose:
*  Replace data with spline interpolation

* Description:
*  Replace data within the boundaries defined by LEFT and RIGHT using
*  cubic spline interpolation.
*
* Arguments:
*      WAVDIM = INTEGER (Given)
*        Number of channels in data
*      x(WAVDIM) = REAL ARRAY (Given)
*        X array data
*      y(WAVDIM) = REAL ARRAY (Given)
*        Intensity data
*      LINE_COUNT = INTEGER (Given)
*        Number of lines identified
*      LEFT(LINE_COUNT) = REAL ARRAY (Given)
*        Left boundaries of lines
*      RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*        Right boundaries of lines
*
*      STATUS = INTEGER (Given and returned)
*        Error status (0=ok)
*      YOUT(WAVDIM) = REAL ARRAY (Returned)
*        replacment data output by intrpl
*      YIN(WAVDIM) = REAL ARRAY (Workspace)
*        y with lines removed
*      XIN(WAVDIM) = REAL ARRAY (Workspace)
*        x with lines removed
*    Subroutines/functions referenced:
*      INTRPL_S
*   Based on modified version of SPLINE_BASE
*   Note use of INTRPL_S the single precision version of INTRPL
*   D.J.Axon, Manchester, 11-FEB-1991
*-
      implicit none
      integer wavdim
      integer line_count
      real x(wavdim)
      real y(wavdim)
      real left(line_count)
      real right(line_count)
      real xin(wavdim)
      real yin(wavdim)
      real yout(wavdim)
      integer status

* local
      integer nin,rx2chn,line,istart,iend,curpos
      logical EXCLUDED

* start at first point and assigning number of points not excluded
* by trams to 0.
      curpos = 1
      nin    = 0
* loop over the input number of channels testing each point as we go
* Remove from the work arrays all entries within the boundaries of the
* lines, shifting the array entries as required.

      do while(curpos.le.wavdim)
        EXCLUDED = .FALSE.

* for each point in Y loop over the TRAMS checking if the point is EXCLUDED
* since it is inside a TRAM.The loop continues unitl all TRAMS have been
* tested or the point is EXCLUDED. By doing it this way we do not need to
* worry if the TRAMS have been sorted.

        line = 1
        do while((.NOT.EXCLUDED).and.(line.le.line_count))
          istart = rx2chn(x,wavdim,left(line))
          iend   = rx2chn(x,wavdim,right(line))
          istart = max(1,min(wavdim,istart))
          iend   = max(1,min(wavdim,iend))

* if the point is within the current TRAM then set EXCLUDED = .TRUE.
* and terminate the loop

          if(curpos.ge.istart.and.curpos.le.iend) then
            EXCLUDED = .TRUE.
          else
            line = line + 1
          end if

        end do

* Test to see if the current point is excluded. If it is not then it is
* not between any of the trams so copy it over to Xin and Yin

        if(.NOT.EXCLUDED) then
          nin = nin + 1
          xin(nin) = x(curpos)
          yin(nin) = y(curpos)
        end if

        curpos = curpos + 1

      end do

* Use INTRPL_S to interpolate the Y values in this range
* because of the way INTRPL_S works we simply get it to return
* all WAVDIM points. Those point which already exist in YIN
* are returned unchanged.

      call intrpl_s(nin,xin,yin,wavdim,x,yout,status)
      end
