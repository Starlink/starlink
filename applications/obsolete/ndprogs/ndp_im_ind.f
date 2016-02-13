      subroutine ndp_image_index(nelm,low,high,input,badpix,output,
     : status)
C+------------------------------------------------------------------------------
C
C   -----------------------------
C   N D P _ I M A G E _ I N D E X
C   -----------------------------
C
C   Description
C   -----------
C   Translate a real array into an array of integer colour indices.
C
C
C   Parameters
C   ----------
C   NELM     (> integer) Number of elements in array
C   LOW      (> real). Lowest data value to be displayed.
C   HIGH     (> real). Highest data value to be displayed.
C   INPUT    (> real array) Input array
C   BADPIX   (> logical) Flag for magic values.
C   OUTPUT   (< integer array) Output array
C   STATUS   (! integer) Status variable
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library NDP:
C     NDP_DEVICE_INDEX
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   21-JAN-1992   - Original program
C
C
C+-----------------------------------------------------------------------------
      implicit none
C
C     Parameters
C
      integer nelm,status
      integer output(nelm)
      real low,high,input(nelm)
      logical badpix
C
C     Local variables
C
      integer ci_start,ci_end,ncol,lev,i
      real rat,magic_float,dat
C
C     Inherited status
C
      if (status .ne. 0) go to 500
C
C     Magic value
C
      call dsa_get_flag_value('FLOAT',magic_float,status)
C
C     Number of colours
C
      call ndp_device_index(ci_start,ci_end,status)
      if (status .ne. 0) go to 500
      ncol = ci_end - ci_start + 1
C
C     A useful constant
C
      rat = float(ncol)/(high - low)
C
C     Rescale now
C
      if (.not. badpix) then
         do i = 1,nelm
            lev = nint((input(i) - low)*rat) + ci_start
            lev = max(ci_start,lev)
            lev = min(ci_end,lev)
            output(i) = lev
         end do
      else
         do i = 1,nelm
            dat = input(i)
            if (dat .eq. magic_float) then
               lev = ci_start
            else
               lev = nint((dat - low)*rat) + ci_start
               lev = max(ci_start,lev)
               lev = min(ci_end,lev)
            end if
            output(i) = lev
         end do
      end if
C
C     Exit
C
  500 continue
      end
