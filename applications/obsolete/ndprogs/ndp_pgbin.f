      subroutine ndp_pgbin(xvals,yvals,work1,work2,nelm,badpix,center)
C+
C
C   -----------------
C   N D P _ P G B I N
C   -----------------
C
C   Description
C   -----------
C   Calls PGPLOT routine PGBIN to plot a histogram style graph of a 1-D
C   array. If the bad data flag is set, the line string is broken at magic
C   value pixels.
C
C
C   Parameters
C   ----------
C   XVALS  (> real array). X values of array elements.
C   YVALS  (> real array). Y values of array elements.
C   WORK1  (> real array). Work array.
C   WORK2  (> real array). Work array.
C   NELM   (> integer). Number of array elements to be plotted.
C   BADPIX (> logical). Bad data flag.
C   CENTER (> logical). Instruction to plot X values at bin centres.
C          Otherwise, values are plotted at the left edges of bins.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Starlink PGPLOT:
C     PGBIN
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'MAGIC_VALUES'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   25-NOV-1992   - Unix version (GOLDJIL)
C
C+-----------------------------------------------------------------------------
c
      implicit none
c
      integer   nelm
      logical   badpix,center
      real      xvals(nelm),yvals(nelm),work1(nelm),work2(nelm)
c
      integer   i,j
c
      include 'MAGIC_VALUES'
c
      if(.not.badpix)then
        call pgbin(nelm,xvals,yvals,center)
      else
        j=0
        do i=1,nelm
          if(yvals(i).gt.magic_float)then
            j=j+1
            work1(j)=xvals(i)
            work2(j)=yvals(i)
          else if(j.gt.0)then
            call pgbin(j,work1,work2,center)
            j=0
          end if
        end do
        if(j.gt.0)then
          call pgbin(j,work1,work2,center)
        end if
      end if
c
      end
