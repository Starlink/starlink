      subroutine key_menu(iopt,factor,icons,diags,status)
*+
* Name:
*    KEY_MENU

* Invocation:
*    CALL KEY_MENU(IOPT,FACTOR,ICONS,DIAGS,STATUS)

* Purpose:
*   To return the option for altering the guesses for the parameters
*   for a multiple component fit.

* Description:
*   To return the option for altering the guesses for the parameters
*   for a multiple component fit.

* Arguments:
*    IOPT = INTEGER (Returned)
*       Option selected
*    FACTOR = REAL (Returned)
*       Number associated with option
*    ICONS(4,8) = REAL ARRAY (Given)
*       Coordinates of icons
*    STATUS = INTEGER (Given and returned)
*       Global status

* Authors:
*   TNW: T.N.Wilkins, Durham

* History:
*   TNW: 23/2/94, Use cursor rather than qmenu.
*   TNW: 8,9/3/94 More can be handled by KEY_MENU
*-
      implicit none
      include 'SAE_PAR'
      integer iopt,diags(3),NICON
      parameter (NICON = 10)
      real factor,x,y,curfac,icons(4,NICON),x1,y1
      save curfac
      integer status,i,pgcurse,NDICT,dia,id,refs(NICON),refs2(NICON)
      integer end1
      parameter (NDICT = 22)
      integer OPT_1, OPT_9, OPT_HELP, OPT_PLUS, OPT_MINUS, OPT_CHANGE
      parameter (OPT_1 = 9, OPT_9 = 17, OPT_HELP = 21, OPT_PLUS = 22,
     :     OPT_MINUS = 23, OPT_CHANGE = 8)
      character key,chr_upper
      character*29 keys
      character*39 dict_key(NDICT)
      data dict_key/
     :     'U   : Up','D   : Down','R   : Move centre right',
     :     'L   : Move centre left','W   : Wider',
     :     'N   : Narrower','P   : Plot-to replot',
     :     '1-9 : To change to line number N',
     :     'V   : To set values to cursor position',
     :     'I   : Base up','K   : Base down',
     :     '?   : Display help',
     :     '+   : Increase "speed" of changes',
     :     '-   : Decrease "speed" of changes',
     :     'A   : Introduce new Gaussian',
     :     'T   : List parameters of all Gaussians',
     :     'F   : Optimize the fit',
*     :     'M   : Alter fitting routine',
     :     '                           ',
     :     'E   : Abandon fitting of this profile',
     :     'X   : Delete current component',
     :     'C   : Change component to nearest',
     :     '   Hit any key to return to plots'/
*      data keys/'UDRLWNPC123456789VIK?+-ATFMEX'/
      data keys/'UDRLWNPC123456789VIK?+-ATF"EX'/
      data curfac/1.0/
      data refs/1,2,4,3,6,5,11,12,18,21/
      data refs2/1,2,4,3,6,5,10,11,17,12/

      if(status.ne.SAI__OK) return
 1    continue
      factor = curfac
      status = pgcurse(x,y,key) - 1
      if(status.ne.SAI__OK) return
      id = 0
      x1 = x
      y1 = y
      call gr_conv(diags(1),diags(3),x1,y1)
      call test_areas(NICON,4,icons,x1,y1,id)
      if(id.ne.0) then
         iopt = refs(id)
         if(iopt.ne.OPT_HELP) return
      else
         key = chr_upper(key)
         iopt = index(keys,key)
      endif
      if(iopt.eq.OPT_HELP) then

*  Help, display help, and set option to replot

         call pgenv(0.0,1.0,0.0,1.0,0,-1)
         call gr_open(dia,status)
         end1 = (NDICT+1)/2
         do i = 1, end1
            call pgtext(0.1,real(end1+1-i)/real(end1+1),dict_key(i))
         enddo
         do i = end1 + 1, NDICT
            call pgtext(0.6,real(end1+1-i+end1)/real(end1+1),dict_key(i)
     :           )
         enddo
         do i = 1, NICON
            if(refs2(i).gt.end1) then
               x = 0.51
               y = real(end1+1-refs2(i)+end1)/real(end1+1) - 0.02
            else
               x = 0.01
               y = real(end1+1-refs2(i))/real(end1+1) - 0.02
            endif
            call profwarr(x,y,0.05,i)
         enddo
         status = pgcurse(x,y,key) - 1
         iopt = 7
         call gr_seld(1,status)
         call gr_annul(dia,status)
      else if(iopt.eq.OPT_PLUS) then
         curfac = curfac * 1.4
         goto 1
      else if(iopt.eq.OPT_MINUS) then
         curfac = curfac * 0.7
         goto 1
      else if(iopt.eq.OPT_CHANGE) then
         factor = x
      else if(iopt.gt.OPT_9) then
         iopt = iopt - 8
         factor = x
      else if(iopt.ge.OPT_1) then
         factor = real(iopt - 8)
         iopt = 9
      else if(iopt.eq.0) then
         call par_wruser('Invalid key',status)
         goto 1
      endif
      end




