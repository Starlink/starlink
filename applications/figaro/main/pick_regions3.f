      subroutine pick_regions3(tram1,tram2,count,max_work)
*+
* Name:
*    PICK_REGIONS3

* Invocation:
*    CALL PICK_REGIONS3(TRAM1,TRAM2,COUNT,MAX_WORK)

* Purpose:
*    Define trams

* Description:
*    Pick_regions allows two tramline positions to be defined using
*    cursors. The left and right bounds of the selected regions are
*    returned in TRAM1 and TRAM2 and a maximum number of MAX_WORK trams
*    may be handled by this routine

* Arguments:
*   MAX_WORK = INTEGER (Given)
*       MAximum number of trams that can be handled
*   COUNT  = INTEGER (Returned)
*      Actual numnber of trams derfined
*   TRAM1(MAX_WORK) = REAL ARRAY (Returned)
*      Left hand bound for each tram
*   TRAM2(MAX_WORK) = REAL ARRAY (Returned)
*      Right hand bound for each tram

* Variables :
*
*   XCHAN1(2)   : Lowest channel indicated by cursor
*   XCHAN2(2)   : Highest channel indicated by cursor
*   KEY         : Key depressed when returning cursor position
*   STRING      : General output character string
*   X           : X value returned by cursor
*    Y           : not used
*
* Subroutines referenced:
*      GR_VLINE     : Draw vertical polyline
*      PAR_WRUSER   : Write character string to user
* History:
*     Bug fix, in which X2 not set, TNW, 8-APR-1991
*
*- --------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'

* size of Tram1 and Tram2

      integer max_work

* output list of tramlines

      real tram1(max_work)

* output list of tramlines

      real tram2(max_work)
      integer count
*
* logical
*
      logical gen_similar
      logical ignore,end_sel
*
* real*4
*
      real x1,x2,dummy

      integer status,pgcurse
      character key, chr_upper
*
* Inquire if cursor available
*
      status = SAI__OK
      call gr_curin(2,status)
*
*           1 S T  T R A M  L I N E
*           -----------------------
*
      end_sel = status.ne.SAI__OK
      call par_wruser('I : to ignore, E : to end',status)

* keep selecting points till the user stops or the works sapce
* provided is full. SInce this rotuine is re-enterable with COUNT
* already incremented from previous passes this must be
* checked.

      do while(.not.end_sel)
        if(count.eq.max_work) then
          end_sel = .true.
          call par_wruser(
     :      'Cursor Work Space is Full cannot add more points',status)
        else
          ignore =.true.
          do while(ignore)
            call sync
            status = pgcurse(x1,dummy,key) - 1
            key = chr_upper(key)
            ignore = key.eq.'I'
            if (ignore) then
              call par_wruser('Line ignored',status)
            end if
          end do
        end if
        if (key.eq.'E') end_sel=.true.
*
*           2 N D    T R A M    L I N E
*           ---------------------------
*
        ignore =.true.
        do while((ignore).and.(.not.end_sel))
          call sync
          status = pgcurse(x2,dummy,key) - 1
          key = chr_upper(key)
*
* Check for legality.
*
          ignore = gen_similar(x2,x1)
          if (ignore) then
            call par_wruser ('Line Ignored : Tram lines Equal!'
     :         ,status)
          end if
          if (key.eq.'I') then
            ignore=.true.
            call par_wruser('Line ignored',status)
          else
            end_sel = key.eq.'E'
          end if
          if(.not.ignore) then
*
* Order so that CHAN2 > CHAN1.
*
            if (x2 .lt. x1) then
              dummy = x1
              x1 = x2
              x2 = dummy
            end if
            call gr_vline(x1)
            call gr_vline(x2)
            count = count + 1
            tram1(count) = x1
            tram2(count) = x2

          end if

*   ignore

        end do
      end do
      end
