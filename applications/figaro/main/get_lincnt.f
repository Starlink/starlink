      subroutine get_lincnt(tram,line_count,nyp)
*+
* Name:
*    GET_LINCNT

* Invocation:
*    CALL GET_LINCNT(TRAM,LINE_COUNT,NYP)

* Purpose:
*   Get the value of line count by checking for non-zero values of
* the array tram.

* Description:
*   Get the value of line count by checking for non-zero values of
* the array tram.
*
* Arguments:
*    TRAM(NYP) = REAL ARRAY (Given)
*        One of the tram arrays
*    NYP = INTEGER (Given)
*        Dimension of tram array
*    LINE_COUNT = INTEGER (Returned)
*        Number of lines
*-
      implicit none
      include 'PRM_PAR'
      integer nyp
      real tram(nyp)
      integer line_count
      character*22 chars
      integer pstat
* ------------------------------------------------------------------
      integer i

      line_count = 0
      do i = 1 ,nyp
        if(tram(i).gt.VAL__SMLR) line_count=i
      end do

      write(chars,'(''Number of lines is'',i3)')line_count
      call par_wruser(chars,pstat)
      end
