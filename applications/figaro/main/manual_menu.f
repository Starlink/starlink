      subroutine manual_menu(key,line_name,line_count,spdim2
     :     ,values,status)
*+
* Name:
*    MANUAL_MENU

* Invocation:
*    CALL MANUAL_MENU(KEY,LINE_NAME,LINE_COUNT,SPDIM2,VALUES,STATUS)
*
* Purpose:
*  Decide on the menu option in manual style analysis

* Description:
*  QMENU is called for the menu selection, and then MENU if required for
*  the line selection (if the line is selected by name).

* Arguments:
*     LINE_COUNT = INTEGER (Given)
*        Number of lines
*     LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Names of lines
*     SPDIM2 = INTEGER (Given)
*        2nd spatial dimension
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=okay
*     KEY = INTEGER (Returned)
*        Menu choice
*     VALUES(4) =  REAL (Returned)
*        Menu values

* Authors:
*  TNW: T.N.Wilkins Cambridge

* History:
*  TNW: 18-APR-1991 based on MANUAL_OPTS, now incorporating WINDOW_MENU
*      as well.
*  TNW: 14-MAR-1994, return values from WIDTH option
*-
      implicit none
      include 'SAE_PAR'
      character*20 string
      integer line_count,menu
      character*10 line_name(line_count)
      integer key,key1,key2,spdim2
      logical loop
      integer nvals,pstat,OPT_FIT, OPT_CHANGE
      parameter (OPT_FIT = 6, OPT_CHANGE = 10)
      integer status,MDICT,ndict,i
      parameter (MDICT = 17)
      real values(4)
      character*53 dict(MDICT)
      data dict/
     :     '%T[name] : Line name to go to that line',
     :     'LAST    : Move back to previous Line',
     :     'NEXT    : Move on to next line in list',
     :     'TRIM    : LIMIT the range of X-sects',
     :     'CHECK   : Check previous fits (20 to a screen)',
     :     'FIT     : Fit this window as it stands',
     :     'ADVANCE : Go to next SPATIAL CUT',
     :     'BACK    : Go to previous window',
     :     'SEE     : Look at individual elements of window',
     :     'WIDTH   : Change window width',
     :     'SCAN    : Scan through windows',
     :     'OLD     : Start/stop plotting old fits',
     :     'DELETE  : Delete fits in this range',
     :     'HARD    : Produce hardcopy of data with fit',
     :     'EXIT    : Return to main menu',
     :     'UP      : Go up (Y spatial)',
     :     'DOWN    : Go down (Y spatial)'/
* ---------------------------------------------------------------

* answer

      loop = .true.

      if(spdim2.eq.1) then
         dict(OPT_CHANGE) =
     :        'WIDTH %FWidth %fPosition : Change window width'
         ndict = MDICT - 2
      else
         dict(OPT_CHANGE) =
     :        'WIDTH %FXwidth %FYwidth %fX %fY : Change window width'
         ndict = MDICT
      endif
      do while(loop)

* Basic menu

         call qmenu('Manual Mode',dict,ndict,OPT_FIT,values,string,key1,
     :        nvals,status)

* Flag any unused values as invalid

         do i = nvals+1, 4
            values(i) = -1
         enddo

         if(status.eq.SAI__OK) then
            if(key1.eq.1) then

* Check for line name

               key2 = menu(line_name,line_count,string,0)
               if(key2.eq.-1) then
                  call par_wruser('Ambiguous reply-try again',pstat)
               else if(key2.eq.0) then
                  call par_wruser('Line not found-try again',pstat)
               else
                  loop = .false.
               endif
            else
               loop = .false.
            end if
            if(key1.ne.1) then
               key = key1-1
            else
               key = -key2
            end if
         else
            loop = .false.
         end if
      end do
      end
