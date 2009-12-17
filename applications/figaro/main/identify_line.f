      subroutine identify_line(work_name,work_table,line_name,
     :        wavelength,ids,sdata,left,right,status)
*+
* Name:
*    IDENTIFY_LINE

* Invocation:
*    CALL IDENTIFY_LINE(WORK_NAME,WORK_TABLE,LINE_NAME,
*             WAVELENGTH,IDS,SDATA,LEFT,RIGHT,STATUS)
* Purpose:
*   Assign wavelengths and names to lines

* Description:
*   Identify a line from the current working table or subset
*   thereof,and enter the wavelength and name of the line
*   in WAVELENGTH and LINE_NAME.
*     The user has the option to display subsets of the current table
*   and choose by number in this, just to give an approximate
*   wavelength (the more precise value and the name will be taken from
*   the tables), or to type in the wavelength and name in full (without
*   reference to the tables). The identification is confirmed before
*   being accepted by this routine. If enough lines have been identified
*   to allow in/exterpolation to give a first guess to the wavelength
*   this is done, both using cubic splines and by a Chebyshev polynomial
*   fit.

* Arguments:
*     LINE_COUNT = INTEGER (Given)
*        number of lines to be identified
*     IDS = INTEGER (Given)
*        number of known lines
*     WORK_NAME(*) = CHARACTER*10 ARRAY (Given)
*        name of known lines
*     WORK_TABLE(*) = REAL ARRAY (Given)
*        wavelength of known lines
*     LEFT(LINE_COUNT) = REAL ARRAY (Given)
*        Left trams
*     RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*        Right trams
*     SDATA(WAVDIM) = REAL ARRAY (Given)
*        X data array
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*     LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Returned)
*        name asigned to line
*     WAVELENGTH(LINE_COUNT) = REAL ARRAY (Returned)
*        wavelength of line

* Variables:
*     J       :counter of lines identified
*  LOCATION   :counter of position in work arrays
*  NUMB       :stores current locations on screen
*  VALUE      :screen line number entered as ID

* History:
*  23 to 25-Oct-1989 TNW/CAVAD Major changes, allow identification in
*  any order, also combine previously separate options. virtually a
*  rewrite.
*  17-7-90 TNW, Alterations so ids=0 is ok
*  8-FEB-1991 TNW, Changes to workspace handling
*  12-FEB-1991 TNW, Work arrays reduced + minor changes
*  14-MAR-1991 TNW, Bug fix re work arrays
*  TNW 10/6/92 changes to display_line
*  TNW 6/4/93 fix bug found when compiled on DECstation
*  TNW 28/7/93 Allow for larger tables (up to 99999!)
*  AJH 15/10/97 Removed call to qwave.
*  ACD: 28/9/00 Remove local unused variables.
* ----------------------------------------------------------------
*-
      implicit none

      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      integer oldloc,location,istop
      integer ival
      integer it,ip1
      integer ids
      integer status
      include 'arc_dims'
      real left(line_count),right(line_count)
      integer jlast,j,i,k
      real sdata(wavdim)
      real arfind2
*
      logical find,ok,distab,confirm
      logical keep,more
      logical par_quest

      integer ptr1,dwaves,ptr3,slot,slot2,slot3,slot4,pstat
      integer NLSET,oldj,len1
      parameter (NLSET = 40)
      integer numb(NLSET)
*
* character
*
      character*10 line_name(line_count)
      character*10 work_name(*)
      character*72 chars
*
*

      real work_table(*)
      real value,halfwid
      real  wavelength(line_count)
C      real get_wave
      double precision dline
      integer cent
      real qwave
      integer nval,key,ncent,ndef
      logical if_qwave
      integer OPT_WAVE, OPT_WIDTH, OPT_NEXT, OPT_DISP, OPT_QUIT, OPT_ID
     :     ,OPT_FORW, OPT_BACK, OPT_NUM
      parameter (OPT_WAVE = 0, OPT_WIDTH = 1, OPT_NEXT = 2,
     :     OPT_DISP = 3, OPT_ID = 4, OPT_FORW = 7, OPT_QUIT = 5,
     :     OPT_BACK = 8, OPT_NUM = 9)
      integer NDICT,tndict
      parameter (NDICT = 10)
      character*57 dict(NDICT)
      data dict/
     :     '%F[number]       : Line wavelength',
     :     'WIDTH %f[number] : Redraw with different width',
     :     'NEXT             : Go to next line',
     :     'DISPLAY          : Start/stop displaying line tables',
     :     'ID %F[wavelength] %T[name] : Give line ID/wavelength',
     :     'QUIT             : Exit leaving lines unidentified',
     :     'REDISPLAY        : Redisplay this screen of list',
     :     'FORWARD    %f[n] : Go to next (or n ahead) screen of list',
     :     'BACK             : Go to previous screen of list',
     :     'NUMBER %F[number] : Select line from list'/

      distab = .false.

      keep = .false.
*
* loop through list of lines to be identified
*
      call par_wruser('L I N E   I D E N T I F I C A T I O N',pstat)
      call par_wruser('-------------------------------------',pstat)
      location=0
      j=0
      jlast=-10

* Get virtual memory
*  PTR1     LINE_COUNT (d)
*  DWAVES   LINE_COUNT (d)
*  PTR3     400        (d)
*  CENT     LINE_COUNT (d)

      call dsa_get_work_array(line_count,'double',ptr1,slot,status)
      call dsa_get_work_array(line_count,'double',dwaves,slot2,status)
      call dsa_get_work_array(400,'double',ptr3,slot3,status)
      call dsa_get_work_array(line_count,'double',cent,slot4,status)
      more = status.eq.SAI__OK

* Loop on more lines to identify

      do while(more)

* Search for first "unknown" line

        find = .true.
        oldj = j
        do while(find)
          j = j + 1
          if(j.eq.oldj) then
            find = .false.
            more = .false.
          else if(j.gt.line_count) then
            j = 0
          else if(line_name(j).eq.'UNKNOWN   ') then
            find = .false.
          end if
        end do

        ok=.not.more

        if(more) then
          write(chars,'(''Identify line number '',i2)')j
          call par_wruser(chars(:23),pstat)

* Display line, then ask user for wavelength, giving the user the value
* interpolated from the other lines and that from the fit (max 3rd
* order) to the other lines, if possible.

          if(line_count.ge.1) then

*     If any lines already identified, fill array cent with their peak
*     positions, to use for guessing wavelengths of newly located lines.

            call fill_cent(%VAL(CNF_PVAL(cent)),line_count,sdata,
     :                     %VAL(CNF_PVAL(d_vsptr)),wavdim,left,right,
     :                     line_name,ncent,wavelength,
     :                     %VAL(CNF_PVAL(dwaves)))
          end if
          call get_peak(dline,sdata,%VAL(CNF_PVAL(d_vsptr)),wavdim,
     :                  left(j),right(j))
          if(ncent.gt.2) then

* AJH Removed in de-nagging
*            qwave = get_wave(%VAL(CNF_PVAL(cent),ncent,dline,
*     :         %VAL(CNF_PVAL(ptr1),%VAL(CNF_PVAL(dwaves)),
*     :         %VAL(CNF_PVAL(ptr3)))
*
*     next line inserted
             qwave = real(dline)
          else
             qwave = real(dline)
          end if
          if_qwave = qwave.gt.1.0e-20

* Continue asking until a satisfactory identification is found.

          halfwid=200*(sdata(2)-sdata(1))
          keep = j.eq.jlast
          istop=min(ids,NLSET)
        end if

*     Loop on current line satisfactorily identified

        do while(.not.ok)

*       We don't yet have a line id to confirm!

          confirm = .false.

*       Display line profile

*
* See if already satisfactory display on screen
*
          if (.not.keep) then
            call display_line(sdata,%VAL(CNF_PVAL(d_vsptr)),wavdim,
     :           left(j),right(j),xlabel,xunits,zunits,halfwid)
          endif
          jlast=j

          keep = .true.

*       Are we to display tables?

          if(distab) then
            oldloc=location
            do i=1,istop,2
              ip1=min((i+1),istop)
*
*           fill in output buffer
*
              do k = i, ip1
                location = location+1
                if(location.gt.ids) location = 1
                numb(k) = location
              end do
*
*           write the output buffer to screen in pairs
*
              write(chars,'(2(i5,1x,f10.4,2x,a,3x))')numb(i),
     :           work_table(numb(i)),work_name(numb(i)),numb(ip1),
     :           work_table(numb(ip1)),work_name(numb(ip1))
              call par_wruser(chars,pstat)
            end do
            tndict = NDICT
          else
            tndict = 6
          end if

*     max number of values to accept

          ndef = 0
          if(if_qwave) ndef = 1

*      Menu:
*      WIDTH,NEXT,DISPLAY,ID,QUIT,REDISPLAY,FORWARD,BACK,NUMBER

          value = qwave
          call qmenu('Identification Menu',dict,tndict,ndef,value,
     :         chars,key,nval,status)

*     abort or quit

          key = key - 1
          if((status.ne.SAI__OK).or.(key.eq.OPT_QUIT)) then
            ok = .true.
            more = .false.
          else if(key.eq.OPT_WAVE) then
            if(ids.gt.0) then
              wavelength(j)=arfind2(work_table,ids,value,work_name,
     :            line_name(j))
              confirm = .true.
            else
              call par_wruser('No line lists loaded',pstat)
            end if
          else if(key.eq.OPT_WIDTH) then
            if(nval.eq.1) then
              halfwid = value*0.5
            else
              halfwid=200*(sdata(2)-sdata(1))
            end if
            keep = .false.
          else if(key.eq.OPT_NEXT) then
            ok = .true.
          else if(key.eq.OPT_DISP) then
            distab = .not.distab

*     ID without use of tables

          else if(key.eq.OPT_ID) then
            wavelength(j) = value
            line_name(j) = chars(:10)
            confirm = .true.

*     forward

          else if(key.eq.OPT_FORW) then
            if(nval.eq.1) then
              ival = nint(value) - 1
            else
              ival = 0
            end if
            oldloc = location+ival*NLSET
            if(oldloc.gt.ids) oldloc=1

*     Back

          else if(key.eq.OPT_BACK) then
            oldloc=oldloc-NLSET
            if (oldloc.le.0) oldloc=((ids-1)/NLSET)*NLSET + 1
          else if(key.eq.OPT_NUM) then
            it=1
            find = .true.
            ival = nint(value)
            do while(find)
              if(numb(it).eq.ival) then

*         line has been found, numb(it) = ival

                line_name(j)=work_name(ival)
                wavelength(j)=work_table(ival)
                find=.false.
                confirm = .true.

              else if(it.eq.istop) then
*
*       test if end of screen reached
*
                find=.false.
                call par_wruser('This line is not on the screen'
     :                  ,pstat)
              else
                it=it+1
              end if
            end do
          end if
          location = oldloc

*       If we have a line id which we need to confirm, then do so now

          if(confirm) then
            len1 = 0
            call chr_fill(' ',chars)
            call chr_putr(wavelength(j),chars,len1)
            len1 = len1 + 1
            call chr_appnd(line_name(j),chars,len1)
            len1 = len1 + 1
            call chr_putc('ok?',chars,len1)
            ok = par_quest(chars(:len1),.true.)

*         If we quit then the line would still be effectively
*         identified, so we must "unidentify" it!

            if(.not.ok) then
              wavelength(j) = -1
              line_name(j) = 'UNKNOWN   '
            end if
          end if

*   While .not.ok

        end do

* While More

      end do

      call dsa_free_workspace(slot4,status)
      call dsa_free_workspace(slot3,status)
      call dsa_free_workspace(slot2,status)
      call dsa_free_workspace(slot,status)

      end
