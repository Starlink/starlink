      subroutine pick_regions(tram1,tram2)
*+
* Name:
*    PICK_REGIONS

* Invocation:
*    CALL PICK_REGIONS(TRAM1,TRAM2)

* Purpose:
*   Pick_regions allows two tramline positions to be defined.

* Description:
*   Pick_regions allows two tramline positions to be defined.

* Arguments:
*  TRAM1 = REAL (Returned)
*     Position of Left HAnd edge of Region
*  TRAM2 = REAL (Returned)
*     Position of Right HAnd edge of Region
*
* Variables :
*
*  STRING      : General output character string
*   X           : X value returned by cursor
*   Y           : not used
*
*
* Subroutines referenced:
*      GR_VLINE     : Draw vertical polyline
*      PAR_WRUSER   : Write character string to user
*      GR_CURIN     : Inquire if cursor available, and get range of plot
*                      =etc.
*
*-
*  --------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
*
* logical
*
      logical ignore
      logical gen_similar
*
* real*4
*
      real dummy
      real tram1,tram2
      integer status,pgcurse
      character*1 key
      status = SAI__OK
*
* Inquire if cursor available etc.
*
      call gr_curin(2,status)
      if(status.ne.SAI__OK) return
*
*      1 S T  T R A M  L I N E
*      -----------------------
*
      call sync
      ignore = pgcurse(tram1,dummy,key) .eq. 1
*
*      2 N D    T R A M    L I N E
*      ---------------------------
*
      do while(ignore)
        call sync
        status = pgcurse(tram2,dummy,key) - 1
*
* Check for legality.
*
        ignore = gen_similar(tram2,tram1)
        if (ignore) then
          call par_wruser ('*Line Ignored : Tram lines Equal!',status)
        else
*
* Order so that CHAN2 > CHAN1.
*
          if (tram1 .gt. tram2) then
            dummy = tram1
            tram1 = tram2
            tram2 = dummy
          end if
          call gr_vline(tram1)
          call gr_vline(tram2)
        end if

* ignore

      end do
      end
