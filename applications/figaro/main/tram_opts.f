      subroutine tram_opts(nopts,auto)
*+
* Name:
*    TRAM_OPTS

* Invocation:
*    CALL TRAM_OPTS(NOPTS)
*
* Purpose:
*     Display cursor options.

* Description:
*     The cursor options are listed on the graphics screen.

* Arguments:
*     NOPTS = INTEGER (Given)
*        Number of options - see PICK_REGIONS2
*     AUTO = LOGICAL (Given)
*        If automatic line location allowed
* History:
*  TNW: 6/8/93 Whole option from segments now just another segment.
*  TNW: 10/8/93 Simplified a lot (only expect NOPTS = 4 or 8).
*  TNW: 6/8/93 PGPLOT version
*-
      implicit none
      integer nopts,status,pgcurse,i
      logical auto
      real y,x
      character cur
      character*43 help(11)
      data help/
     :     'Locate lines by marking either side of line',
     :     '? : Display this help',
     :     'E : End selection',
     :     'I : Ignore the line (2nd tram only)',
     :     'D : Delete this line',
     :     'Y : set the Y scale of the display',
     :     'Z : Zoom in',
     :     'F : Scroll to next segment',
     :     'B : Scroll to previous segment',
     :     'W : Display whole spectrum',
     :     'O : Zoom out'/
*
* Output the instructions.

      call pgenv(0.0,1.0,0.0,1.0,0,-1)
      do i = 1, nopts + 1
         y = 1.0 - real(i)*0.07
         call pgtext(0.1,y,help(i))
      enddo
      if(auto) then
         y = y - 0.07
         call pgtext(0.1,y,'A : Auto line search (will redo last one)')
      endif
      y = y - 0.07
      call pgtext(0.1,y,'Any other key to include the line')
      call pgtext(0.1,0.03,'Hit any key now to return to spectrum')
      status = pgcurse(x,y,cur) - 1
      end
