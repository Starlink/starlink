      subroutine pick_lines(tram1,tram2,line_count,limit,nopts,curkey,
     :     yval,xval,auto)
*+
* Name:
*    PICK_LINES

* Invocation:
*    CALL PICK_LINES(TRAM1,TRAM2,LINE_COUNT,LIMIT,NOPTS,CURKEY,YVAL,
*                XVAL,AUTO)

* Purpose:
*  Locate lines with a cursor

* Description:
*    PICK_LINES allows tramline positions to be defined.
*    Used in ARC2D to choses arclines and in the LONGSLIT
*    analysis package.
*    Certain keys on the key board have special meanings,
*    which can be used to control the program. In particular :-
*
*     - E : Exit
*     - I : Ignore this line
*     - F : Scroll forwards
*     - B : Scroll backwards
*     - C : Change the scale of the display
*     - W : Display the whole spectrum
*     - Z : Zoom in
*     - O : Zoom out
*     - A : Automatic line finding
*     - Any thing else          - Include this line
* Arguments:
*    LIMIT = INTEGER (Given)
*        Maximum number of lines that can be identified
*    NOPTS = INTEGER (Given)
*        Number of options to allow, these are as follows:
*                       1 - Help
*                       2 - Delete
*                       3 - End
*                       4 - Ignore
*                       5 - Change scale
*                       6 - Zoom in
*                       7 - Zoom out
*                       8 - Whole
*                       9 - Back
*                       10 - Forward
*    AUTO = LOGICAL (Given)
*        If automatic line location allowed
*    LINE_COUNT = INTEGER (Given and returned)
*        Number of lines identified
*    TRAM1(LIMIT) = REAL ARRAY (Returned)
*        Left limits of lines
*    TRAM2(LIMIT) = REAL ARRAY (Returned)
*        Right limits of lines
*    CURKEY = INTEGER (Returned)
*        Options, as given in NOPTS above and
*                        10 - Automatic line finding
*    YVAL = REAL (Returned)
*        Y value of cursor hit (for cscale)
*    XVAL = REAL (Returned)
*        X value of cursor hit (for cscale)
*
* Global variables:
*    BLANK_PEN = LOGICAL (Returned)
*        If a blank pen is defined
*
* Subroutines and functions called :
*   PGPAGE          : Erase screen
*   GR_CURSR        : Cursor routine
*   PAR_WRUSER      : Writes a character string to the screen

* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then Durham

* History:
*   TNW: 13/6/88. To exit if no more room for lines, rather than when try to
*        identify next. Tidied up a bit.
*   TNW 20/7/88 to use tnw_sgstx and some optimisation
*   TNW: 14/11/89 Tidied, newlin loop and gr_curin added.
*   TNW: 3/90 PGPLOT version
*   TNW: 15/4/91 Made so if no segments then segment options not available
*   TNW: 17/5/91 Bug fix-correct value of curkey if tables full.
*   TNW: 12/8/92 Name change
*   TNW: 2/8/93 Improve initial cursor positioning
*   TNW: 6/8/93 Whole option from segments now just another segment.
*   TNW: 11/8/93 Bug fix, also centre cursor if out of range on X or Y.
*   TNW: 26/8/93 Allow user to ask for help, and write this to graphics
*       screen (or window), rather than always writing a list of options to
*       the alpha screen.
*   TNW: 8/12/93 Bug fix-if slots filled should exit, not ask for help!
*-
      implicit none
      include 'SAE_PAR'
      integer line_count
      integer limit,status,curkey
      integer nopts

* character

      character*3 segment
      character key,chr_upper
      character*29 note,chars*18

* logical

      logical scroll,auto
      logical gen_similar

* common

      include 'gr_inc'

* real

      real x(2),dummy,yval,xval
      real tram1(limit),tram2(limit)

      real xpos,ypos,xmin,xmax,ymin,ymax

* Integer

      integer newlin
      integer lout,i,j,olcnt
      integer IGNORE,DELETE
      parameter (DELETE = 2, IGNORE=4)
      integer keylet,pgcurse
      character*11 cursor
      save xpos,ypos
      data cursor/'A?DEIYZOWBF'/
      data xpos,ypos/0.0,0.0/

      call pgqwin(xmin,xmax,ymin,ymax)

*  If Cursor outside range, then put it in middle of plot

      if((xpos.lt.xmin).or.(xpos.gt.xmax)) xpos = 0.5 * (xmin + xmax)
      if((ypos.lt.ymin).or.(ypos.gt.ymax)) ypos = 0.5 * (ymin + ymax)

      call gr_spen(1)
      write(segment,'(i3)') line_count
      note='No of lines identified = '//segment
      call pgmtext('T',1.0,0.0,0.0,note)

* inquire if cursor available

      status=0
      call gr_curin(3,status)
      scroll = status.ne.SAI__OK
*
* Since lines are only accepted if an ignore or END command are
* not given below it is possible that
* on entry to this routine the counter LINE_COUNT can be
* zero .ie no lines identified yet. Hence set up a dummy counter
* for prompting

      do while(.not.scroll)
         lout=line_count+1
         write(chars,'(a,i4)') 'Locate Line :',lout
         call par_wruser(chars,status)

*                        1ST TRAM LINE.

         newlin = 1
         do while(newlin.lt.3)
            olcnt = line_count
            if(newlin.eq.2) then
               call par_wruser('2nd tram line',status)
            end if
            call sync
            status = pgcurse(xpos,ypos,key) - 1
            x(newlin) = xpos
            key = chr_upper(key)
            if(auto) then
               keylet = index(cursor(:nopts+1),key)
               if(keylet.eq.1) then
                  keylet = 11
               else if(keylet.ne.0) then
                  keylet = keylet - 1
               endif
            else
               keylet = index(cursor(2:nopts+1),key)
            endif

* See which key has been hit.

            curkey = 0

*     ignore

            if(keylet.eq.IGNORE) then
               call par_wruser('line ignored',status)
               newlin = 0
            else if(keylet.eq.DELETE) then

*    Delete a line

               i = 1
               do while(i.le.line_count)
                  if((tram1(i).lt.xpos).and.(tram2(i).gt.xpos)) then
                     line_count = line_count - 1
                     call pgtext((tram1(i)+tram2(i))*0.5,0.5 * (ymin +
     :                    ymax),'D')
                     do j = i, line_count
                        tram1(j) = tram1(j+1)
                        tram2(j) = tram2(j+1)
                     enddo
                     tram1(line_count+1) = 0.0
                     tram2(line_count+1) = 0.0
                  endif
                  i = i + 1
               enddo
               newlin = 0
            else if(keylet.ne.0) then
               scroll=.true.

*    CURKEY:
*        3 - End
*        4 - Ignore
*        5 - Change scale
*        6 - Zoom in
*        7 - Zoom out
*        8 - Whole
*        9 - Back
*       10 - Forward

               curkey = keylet
               yval = ypos
               xval = xpos
            end if

            if(newlin.eq.2) then

* Check for legality.

               if (gen_similar(x(2),x(1))) then
                  call par_wruser('Line Ignored-tram lines Equal !',
     :                 status)
               else
                  if ( line_count .lt. limit ) then

* Order so that CHAN2 > CHAN1.

                     if(x(1).gt.x(2)) then
                        dummy = x(1)
                        x(1) = x(2)
                        x(2) = dummy
                     end if
                     call gr_vline(x(1))
                     call gr_vline(x(2))
                     line_count = line_count+1
                     tram1(line_count) = x(1)
                     tram2(line_count) = x(2)
                  else
                     call par_wruser
     :                    ('Identification slots full-line ignored'
     :                    ,status)
                  end if
                  if ( line_count .ge. limit ) then


*           We have no more room, but the user may wish to delete lines.

                     call par_wruser('Identification slots full', status
     :                    )
                  end if

*       trams not equal

               end if

*     newlin.eq.2

            end if
            if(olcnt.ne.line_count) then

* overwrite line counter and segment id with blank pen if possible

               if(blank_pen) then
                  call gr_spen(0)
                  call pgmtext('T',1.0,0.0,0.0,note)
               end if
               call gr_spen(1)
               write(segment,'(i3)') line_count
               note='No of lines identified = '//segment
               call pgmtext('T',1.0,0.0,0.0,note)
               call gr_spen(3)
            endif
            newlin = newlin + 1
            status = SAI__OK
            if(scroll) newlin = 10

*   newlin.lt.3

         end do

* not scroll

      end do
      call gr_spen(1)
      end
