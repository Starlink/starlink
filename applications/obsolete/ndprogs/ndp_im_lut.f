      subroutine ndp_image_lut(table,status)
C+-----------------------------------------------------------------------------
C
C   -------------------------
C   N D P _ I M A G E _ L U T
C   -------------------------
C
C   Description
C   -----------
C   Fills in colour lookup table.
C
C
C   Parameters
C   ----------
C   TABLE    (> Character) Name of LUT.
C   STATUS   (! Integer) Status variable
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_FREE_LU
C     DSA_GET_LU
C     DSA_WRUSER
C
C   Library FIG:
C     FIG_OPFILE
C
C   Library GEN:
C     GEN_FORTERR
C
C   Library ICH:
C     ICH_FOLD
C     ICH_LEN
C
C   Library NDP:
C     NDP_DEVICE_INDEX
C
C   PGPLOT:
C     PGQCR
C     PGQINF
C     PGSCR
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Author/s
C   --------
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   21-JAN-1992   - Original program loosly based on routines written by
C                   Nick Fuller and Guy Rixon.
C   04-APR-1992   - Modified to get round a feature of X-windows that
C                   results in the same background and foreground colour.
C                   (GOLDJIL)
C   06-MAY-1992   - Now uses PGQCR to base colours around foreground & back-
C                   ground. (GOLDJIL)
C   13-NOV-1992   - Unix version (GOLDJIL)
C   06-OCT-1994   - Removed unused variables. (GJP)
C
C+-----------------------------------------------------------------------------
      implicit none
C
C     Parameters
C
      character table*(*)
      integer status
C
C     Functions
C
      integer ich_fold
      integer ich_len
C
C     Local variables
C
      integer dumint,ci_start,ci_end,unit,fstat,i,j,index,ncol
      integer*2 col(3,256)
      real colours(3),colour
      real c0(3),c1(3)
      character message*80,table_up*80
C
      integer RED,GREEN,BLUE
      parameter (RED=1,GREEN=2,BLUE=3)
C
C     Inherited status
C
      if (status .ne. 0) go to 500
C
C     Make the LUT filename upper case.
C
      table_up = table
      dumint = ich_fold(table_up)
C
C     If no LUT name, then get out of here
C
      if (table_up .eq. ' ') go to 500
C
C     Get the number of colour levels.
C
      call ndp_device_index(ci_start,ci_end,status)
      ncol = ci_end - ci_start + 1
      if (status .ne. 0) go to 500
C
C     If grey scale was requested, create grey levels...
C
      if ((table_up .eq. 'GREY') .or. (table_up .eq. 'GRAY')) then
         do i = 1,256
            do j = 1,3
               col(j,i) = i
            end do
         end do
C
C        ...otherwise get logical unit and open colour table file.
C
      else
         call dsa_get_lu(unit,status)
         if (status .ne. 0) go to 500
c        call fig_opfil2 (table_up,'LUT',unit,'unformatted',fstat)
         call fig_opfile (table_up,'LUT',unit,fstat)
         if (fstat .ne. 0) then
            call gen_forterr (fstat,.false.,message)
            call dsa_wruser('Unable to open colour table file '//
     :      table_up(:ich_len(table_up))//'.')
            call dsa_wruser(message(:ich_len(message))//'\\n')
            status = 1
            go to 500
         end if
C
C      Read colour table file
C
         do j=1,3
            read(unit,*,iostat=fstat) (col(j,i),i=1,256)
            if (fstat .ne. 0) then
               call gen_forterr(fstat,.false.,message)
               call dsa_wruser('Error reading from colour table file '//
     :         table_up(:ich_len(table_up))//'.')
               call dsa_wruser(message(:ich_len(message))//'\\n')
               status = 1
	       write(6,*) j,i
               go to 500
            end if
         end do
         call dsa_free_lu(unit,status)
      end if
C
C      Set colour indices based on foreground/background colours
C
      call pgqcr(0,c0(RED),c0(GREEN),c0(BLUE))
      call pgqcr(1,c1(RED),c1(GREEN),c1(BLUE))
      do i = ci_start,ci_end
         index = int(255.0*real(i - ci_start)/real(ncol)) + 1
         do j = 1,3
            colour = c0(j)+(c1(j)-c0(j))*real(col(j,index))/256.0
            colours(j) = min(1.0,colour)
         end do
         call pgscr(i,colours(1),colours(2),colours(3))
      end do
C
C     Exit
C
  500 continue
      end
