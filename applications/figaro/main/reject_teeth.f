      subroutine reject_teeth(window_pos,nwindow,max_window,traml,
     :   tramr)
*+
* Name:
*    REJECT_TEETH

* Invocation:
*    CALL REJECT_TEETH(WINDOW_POS,NWINDOW,MAX_WINDOW,TRAML,
*        TRAMR)

* Purpose:
*  To reject "teeth" found automatically.

* Description:
*  To reject "teeth" found automatically. This is done by marking to
*  either side of them with a cursor.
*
* Arguments:
*    MAX_WINDOW = INTEGER (Given)
*        Maximum allowed number of windows
*    WINDOW_POS(MAX_WINDOW) = REAL ARRAY (Given and returned)
*        Window positions
*    NWINDOW = INTEGER (Given and returned)
*        Number of windows
*    TRAML(MAX_WINDOW) = REAL ARRAY (Given and returned)
*        Left trams
*    TRAMR(MAX_WINDOW) = REAL ARRAY (Given and returned)
*        Right trams
* History:
*    WINDOW_POS now real, TNW 16/6/92
*
*- --------------------------------------------------------------------
      implicit none
      integer max_window
      real window_pos(max_window)
      real traml(max_window)
      real tramr(max_window)
      integer nwindow
      real tram1
      real tram2
      integer j,wind
      integer status
      integer next
      logical found
      logical par_quest

      do while(par_quest('Do you want to delete any teeth?',.false.))
        call par_wruser(
     :   'Put tramlines around centre of tooth to be rejected',status)
*
* Select tooth to reject.
*
        call pick_regions(tram1,tram2)
        wind=1
        found= .false.
        do while(.not.found)
          if ((window_pos(wind).le.tram2).and.(window_pos
     :       (wind).ge.tram1)) then
            do j=wind,nwindow-1
              found=.true.
              next=j+1
              window_pos(j)=window_pos(next)
              traml(j)=traml(next)
              tramr(j)=tramr(next)
            end do
            traml(nwindow) = 0.0
            tramr(nwindow) = 0.0
            nwindow=nwindow-1
            if(wind.gt.nwindow) found=.true.
          end if
          wind=wind+1
        end do
      end do
      end
