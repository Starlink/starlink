      subroutine ndp_device_index(ci_start,ci_end,status)
C+------------------------------------------------------------------------------
C
C   -------------------------------
C   N D P _ D E V I C E _ I N D E X
C   -------------------------------
C
C   Description
C   -----------
C   Gets range of colour indices allowed for a PGPLOT device.
C
C
C   Parameters
C   ----------
C   CI_START (< integer) First colour index
C   CI_END   (< integer) Last colour index
C   STATUS   (! integer) Status variable
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_WRUSER
C
C   PGPLOT:
C     PGQCOL
C     PGQINF
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   IMPLICIT NONE / Names > 6 characters
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
C   03-FEB-1992   - Now checks that a device is open first (JRL)
C
C
C+-----------------------------------------------------------------------------
      implicit none
C
C     Parameters
C
      integer ci_start,ci_end,status
C
C     Local variables
C
      integer mincol,maxcol,ncol,length
      character pgstate*80
C
C     Inherited status
C
      if (status .ne. 0) go to 500
C
C     Check that a device is open
C
      call pgqinf('state',pgstate,length)
      if (pgstate .ne. 'OPEN') then
         call dsa_wruser(' \n')
         call dsa_wruser('No PGPLOT device open -- programming error\n')
         call dsa_wruser(' \n')
         status = 1
         go to 500
      end if
C
C     Number of colours
C
      call pgqcol(mincol,maxcol)
C
C     If you can write in the background colour then subtract an extra
C     colour index.
C
      if (mincol .eq. 0) then
         ncol = maxcol - mincol - 1
      else
         ncol = maxcol - mincol
      end if
      if (ncol .lt. 2) then
         call dsa_wruser('Fewer than two colour levels are available\n')
         status = 1
         go to 500
      end if
C
C     Now set up the colour index range
C
      ci_start = 4
      ci_end = maxcol
C
C     Exit
C
  500 continue
      end
