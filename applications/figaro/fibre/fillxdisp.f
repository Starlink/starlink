      subroutine fillxdisp(xdisp,ny,lunit)
*+
* Name:
*    FILLXDISP

* Invocation:
*    CALL FILLXDISP(XDISP,NY,LUNIT)
* Purpose:
*  To fill the XDISP array.

* Description:
*  The values are read from the file on unit lunit.
*
* Arguments:
*     NY = INTEGER (Given)
*        Dimensions of XDISP array
*     LUNIT = INTEGER (Given)
*        Unit number of file
*     XDISP(NY) = REAL ARRAY (Returned)
*        X displacement array
*
*   Subroutine referenced:
*    PAR_WRUSER  : Write string to user

* Author:
*  T.N.Wilkins Manchester 25/5/88
*-
      implicit none
      include 'SAE_PAR'
      integer ny,lunit,i,status
      real xdisp(ny)
      character*50 chars
      logical go
      do i = 1,ny
        go = .true.
        do while(go)
          read(lunit,'(a)',iostat=status)chars
          go = .false.
          if(status.eq.SAI__OK) then
            if(chars(1:1).eq.'!') then
              call par_wruser(chars(2:50),status)
              go = .true.
            else
              read(chars,'(f7.2)',iostat=status)xdisp(i)
            end if
          end if
        end do
        if(status.ne.SAI__OK) then
          call par_wruser('Error reading XDISP array from file',status)
          return
        end if
      end do
      end
