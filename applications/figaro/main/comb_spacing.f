      subroutine comb_spacing(ni,xplot,yplot,level,tram1,centre,tram2
     :     ,line_count,mwindow)
*+
* Name:
*    COMB_SPACING

* Invocation:
*    CALL COMB_SPACING(NI,XPLOT,YPLOT,LEVEL,TRAM1,CENTRE,TRAM2
*          ,LINE_COUNT,MWINDOW)

* Purpose:
*   Locate comb "teeth" in cut through data

* Description:
*   Determine the seperation between the teeth of a dirac comb, i.e
*   a multi-slit continuum spectrum , used to map the s-distortion
*   of a spectrum.
*
* Arguments:
*    NI = INTEGER (Given)
*        total width of spectrum
*    XPLOT(NI) = REAL ARRAY (Given)
*        crossections data
*    YPLOT(NI) = REAL ARRAY (Given)
*        counts/flux data
*    LEVEL = REAL (Given)
*        threshold level for tooth boundary
*    MWINDOW = INTEGER (Given)
*        Maximum number of windows
*    TRAM1(MAX_WINDOW) = REAL ARRAY (Returned)
*        left edge of  windows
*    TRAM2(MAX_WINDOW) = REAL ARRAY (Returned)
*        right edge of windows
*    LINE_COUNT = INTEGER (Returned)
*        actual number of windows
*    CENTRE(MAX_WINDOW) = REAL ARRAY (Workspace)
*        centre of windows
*
* Subroutines/functions referenced:
*     GET_CENTRE   : Find centriod of data
*     PLOT_TEETH   : Plot "teeth" onto plot of cut through data
*     REJECT_TEETH : Allow user to reject "teeth" using cursor
*     GR_CLEAR     : Advance graphisc frame
*     SETUP_ARC2   : Select "teeth" manually
*
*     PAR_QUEST    : Obtain yes/no response from user
*     PAR_WRUSER   : Write character string to user
*
*     SGS_SPEN     : Select graphics pen
* History:
*  TNW 1/12/88 Change of NAG routines
*  TNW 5/9/90 Various sorting out-work and work1 made real rather than
*             double precision.
*  TNW 12-17/6/92 Can now handle xplot array not 1,2,3,4,... Other tidying
*             as well, including removal of Nag sorts.
*  TNW 5/11/92 MWINDOW made argument
*

* must declare everything

      implicit none

* Max number of allowed windows

      integer mwindow
*-
      integer ni
      real level
      integer line_count
      real xplot(ni)
      real yplot(ni)
      real tram1(mwindow)
      real tram2(mwindow)
      real centre(mwindow)
*
* local
*
      double precision centriod
      double precision sigma

* fraction of current height-define tooth edge

      real test

* work variable

      real update
      integer itmp

* do loop

      integer j

* current height of tooth

      real cur_max

* the guess position for max of data

      integer init_pos

* start crossection (should be xplot(1))

      real xstart

* end crossection number

      real xend

* Integer versions of trams.

      integer itram1, itram2

* Save itram2 for first tooth found, for use when direction of search
* changed

      integer itram2sv

* if looking for "teeth"

      logical seek

* if current (left/right) tram found

      logical found

      character*40 chars
      real c_test
      integer status
      logical par_quest
      real disp
* --------------------------------------------------------------------
      xstart    = xplot(1)
      xend      = xplot(ni)
      init_pos = 1
      cur_max  = 0.0
      disp = (xend - xstart)/real(ni - 1)

* no windows found to start with

      line_count  = 0

* find a starting position as maximum value in the array

      do j =1, ni
        if(yplot(j).gt.cur_max) then
          cur_max  = yplot(j)
          init_pos = j
        end if
      end do

* find the location of points on either side of init_pos which
* are .lt. CUR_MAX/LEVEL

      test = cur_max*level
      j    = init_pos

* up the spectrum

      found = .false.
      itram2 = init_pos
      do while ((.not.found) .and. (itram2.lt.ni))
        itram2 = itram2 + 1
        found  = yplot(itram2).lt.test
      end do
      tram2(1) = xplot(itram2)
      itram2sv = itram2
      if(.not.found) then
        call par_wruser('Tooth off end of Data',status)
        tram2(1) =  xend
      else
* down
        found = .false.
        itram1     = init_pos
        do while ((.not.found) .and. (itram1.gt.1))
          itram1 = itram1 - 1
          found  = yplot(itram1).lt.test
        end do
        if(.not.found) then
          call par_wruser('Tooth off start of Data',status)
          tram1(1) = xstart
        else
          line_count = 1
          tram1(1) = xplot(itram1)

* get an improved estimate of centre of first tooth by centrioding

          call get_centre(xplot,yplot,itram1,itram2,centriod,sigma)

* test to see if a centriod has actually been found
* if it has update the postion of the first tooth

          if( (centriod.ge.tram1(1)) .and. (centriod.le.tram2(1)) ) then
            centre(1) = real(centriod)
          else
            centre(1) = xstart
          end if
*
*   look for more teeth -first down the spectrum
*
          seek = .true.
          c_test=test
          do while (seek)

*    search for next point above the threshold which will
*    therfore mark the RIGHT HAND edge of the next tooth
*    Use tram from last tooth found.

            found  = .false.
            itram2 = max(itram1-1,1)
            do while ((.not.found) .and. (itram2.gt.1))
              itram2 = itram2 - 1
              found  = yplot(itram2).gt.c_test
            end do
            if(.not.found) then
              call par_wruser('Tooth off start of Data',status)
              seek = .false.
            else

*      update the number of windows and store the RIGHT hand tram

              line_count = line_count+ 1
              tram2(line_count) = xplot(itram2)

*      search for other end of window

              found  = .false.
              itram1 = max(itram2-1,1)
              do while ((.not.found) .and. (itram1.gt.1))
                itram1 = itram1 - 1
                found  = yplot(itram1).lt.c_test
              end do

*      check to see if a LEFT boundary has been found

              if(.not.found) then
                call par_wruser('Tooth off start of Data',status)
                line_count = line_count - 1
                seek    = .false.
              else
                tram1(line_count) = xplot(itram1)

*        and find the centriod value in that range up the spectrum

                call get_centre(xplot,yplot,itram1,itram2,centriod,
     :               sigma)

*        test to see if tooth is blank

                if( (centriod.ge.tram1(line_count)).and.
     :               (centriod.le.tram2(line_count)) ) then
                  centre(line_count) = real(centriod)

*           update the new tram limits and check if they are still on
*           the spectrum

                  tram1(line_count) =
     :                 min(xend,max(tram1(line_count),xstart))

                  tram2(line_count) =
     :                 min(xend,max(tram2(line_count),xstart))

*            test if at start of spectrum

                  if(itram1.eq.1) then
                    seek  = .false.
                    call par_wruser('start of spectrum reached',
     :                   status)

                  else
                    cur_max = yplot(itram1)
                    do j = itram1+1,itram2
                      cur_max = max(cur_max,yplot(j))
                    end do
                    c_test =  cur_max*level
                  end if
                else
                  write(chars,
     :                 '(''window'',i5,'' rejected -as blank'')')
     :                 line_count
                  call par_wruser(chars,status)
                  line_count = line_count - 1
                end if
              end if
            end if

*    check that max windows not reached

            if(line_count .eq. mwindow) then
              call par_wruser('Max number of windows reached',status)
              seek = .false.
            end if
          end do

* sort into ascending order

          do j = 1,line_count/2

            itmp = line_count-j+1

            update = centre(itmp)
            centre(itmp) = centre(j)
            centre(j) = update

            update = tram1(itmp)
            tram1(itmp) = tram1(j)
            tram1(j) = update

            update = tram2(itmp)
            tram2(itmp) = tram2(j)
            tram2(j) = update

          end do

* ------------------------------------------------------------------
* Search up the spectrum.
* -----------------------------------------------------------------

          c_test=test
          seek = line_count .lt. mwindow
          itram2 = itram2sv
          do while (seek)

* search for next point above the threshold which will
* therfore mark the LEFT HAND edge of the next tooth

            found  = .false.
            itram1 = min(itram2+1,ni)
            do while ((.not.found) .and. (itram1.lt.ni))
              itram1 = itram1 + 1
              found  = yplot(itram1).gt.c_test
            end do

            if(.not.found) then
              call par_wruser('Tooth off end of Data',status)
              seek = .false.
            else

*     update the number of windows and store the RIGHT hand tram

              line_count = line_count+ 1
              tram1(line_count) = xplot(itram1)

*     search for other end of window

              itram2 = min(itram1 + 1,ni)
              found  = .false.
              do while ((.not.found) .and. (itram2.lt.ni))
                itram2 = itram2 + 1
                found  = yplot(itram2).lt.c_test
              end do

*      check to see if a RIGHT boundary has been found

              if(.not.found) then
                call par_wruser('Tooth off end of Data',status)
                line_count = line_count - 1
                seek = .false.
              else
                tram2(line_count) = xplot(itram2)

*       and find the centriod value in that range up the spectrum

                call get_centre(xplot,yplot,itram1,itram2,centriod,
     :               sigma)

*      test to see if tooth is blank

                if( (centriod.ge.tram1(line_count) ).and.
     :               (centriod.le.tram2(line_count) )) then
                  centre(line_count) = real(centriod)

* update the new tram limits and check if they are still on the
* the spectrum

                  tram1(line_count) = max(min(tram1(line_count),xend),
     :                 xstart)
                  tram2(line_count) = min(max(tram1(line_count),xstart)
     :                 ,xend)
                  if(tram2(line_count).ge.xend) then
                    seek  = .false.
                    call par_wruser('End of spectrum reached',status)
                  else
                    cur_max = yplot(itram1)
                    do j = itram1+1,itram2
                      cur_max = max(cur_max,yplot(j))
                    end do
                    c_test =  cur_max*level
                  end if
                else
                  write(chars,'(a,i5,a)')
     :                 'window',line_count,' rejected -as blank'
                  call par_wruser(chars,status)
                  line_count = line_count - 1
                end if
              end if
            end if
            if(line_count .eq. mwindow) then
              call par_wruser('Max number of windows reached',status)
              seek = .false.
            end if

          end do

*   .not.found

        end if

* .not. found

      end if

* Setting itmp to 0 means that we enter the following loop unless line_count
* is zero.

      itmp = 0
      do while(line_count.ne.itmp)

* Save current value of line_count, we'll re-plot if the user changes this.

        itmp = line_count
        if(line_count.gt.1) then
          do j = 1, line_count - 1
            if(tram2(j).lt.tram1(j+1)) then
              update     = (tram2(j)+tram1(j+1))*0.5
              tram2(j)   = update
              tram1(j+1) = update
            end if
          end do

* Sort out end trams

          update   = abs(centre(1)-tram2(1))
          tram1(1) = max(xstart,(centre(1)-update))
          update   = abs(centre(line_count)-tram1(line_count))
          tram2(line_count) = centre(line_count)+update
          tram2(line_count)= min(xend,(centre(line_count)
     :         +update))
        else
          update = min((tram1(1)-xstart)*0.1,5.0*disp)
          tram1(1) = max(xstart,tram1(1)-update)
          update   = min((xend-tram2(1))*0.1,5.0*disp)
          tram2(1) = tram2(1)+update
          tram2(1) = min(xend,(tram2(1)+update))
        end if

* Plot out teeth on the cut.

        call plot_teeth(line_count,centre,tram1,tram2)

* Reject teeth manually if required.

        call reject_teeth(centre,line_count,mwindow,tram1,tram2)

* Re-plot cut if any teeth deleted, so that we can see the current
* situation

        if(line_count.ne.itmp) then
          call gr_spen(1)
          call plot_spect(ni,xplot,yplot,' ','X-sects',' ')
        endif

      enddo

      if(line_count.eq.0) then

        if(par_quest('No teeth found-add them manually?',.true.)) then
          call setup_arc2(tram1,tram2,.false.,status)
        end if

      end if

      call gr_spen(1)
      end
