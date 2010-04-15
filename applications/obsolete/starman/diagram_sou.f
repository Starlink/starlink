CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DIAGRAM -- Takes magnitude tables and plots HR or 2-col diagram
C
C   a j penny                 dao           1988-04-25

      subroutine diagram ( ierradam )

      implicit none

      integer     ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_diagram

      call starman_end ( ierradam )

      end


C  This is DIASUBG.FOR
C
C  It contains:-
C
C DI_OPEN   Open device
C DI_CLOSE  Close device
C DI_PLOT   Plot points
C DI_GCUR   Use cursor to pick spots
C DI_LINE   Puts lines on the display
C DI_CLEAR  Clear display


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_OPEN -- Open device
C
C   a j penny                            dao      1988-04-25

      subroutine di_open ()
      implicit none
      include 'diagram.inc'
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      if ( GD_DISPOP ) then
         call printo ( 'ERROR: Device already open' )
      else
         call gd_open ( istat )
         if ( GD_DISPOP ) then
            GOTLIMS = .false.
            DONEAXIS = .false.
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_CLOSE -- Close device
C
C   a j penny                            dao      1988-04-25

      subroutine di_close ()
      implicit none
      include 'diagram.inc'
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GD_DISPOP ) then
         call printo ( 'ERROR: Device not open' )
      else
         call gd_close
         GOTLIMS = .false.
         DONEAXIS = .false.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_PLOT -- Plot points
C
C   a j penny                            dao      1988-04-25

      subroutine di_plot ()
      implicit none
      include 'diagram.inc'
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'
C--
      character*4 textb(2)
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GD_DISPOP ) call di_open				!Is device open?
      if ( .not.GD_DISPOP ) return

      if ( .not.LOADED(NSET) ) call di_load				!Loaded data?

      if ( .not.GOTLIMS ) call di_plims 				!Get limits

      if ( .not.DONEAXIS ) then						!Draw axes
         write ( textb(1), '(a1,''-'',a1)'  ) COL(3), COL(4)
         if ( KSTART.eq.1 ) then
            write ( textb(2), '(a1,''-'',a1)' ) COL(1), COL(2)
         else
            write ( textb(2), '(a1)' ) COL(2)
         endif
         call gd_dobox ( DXLIM(1), DXLIM(2), textb(1),
     +                   DYLIM(1), DYLIM(2), textb(2), ' ', 0 )
      endif
      DONEAXIS = .true.

      call gd_opts ( XX, YY, ZZ, TOTSTARS, .true., DOTNUM, SYMB(NSET))	!Plot stars


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_GCUR -- Use cursor to pick spots
C
C    a j penny                      dao               1988-04-24

      subroutine di_gcur ( kopt )
      implicit none
      include 'diagram.inc'
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'

      integer   kopt		!i: Option (1=cursor;2=cursor + nearest)
C--
      logical     loop
      real        xa, ya, rd, xmin, ymin, dd, xw, yw, dx, dy
      character   ch*1, rname*20, text*72
      integer     j, k, pgcurse, jmin
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GD_DISPOP ) then					!Is device open?
         call printo ( 'ERROR: Device not open' )
         return
      endif

      call printo ('Position cursor and hit keyboard to get positions')
      call printo ('Place cursor to left of origin and input to exit' )

      if ( KSTART.eq.1 ) then						!List of positions heading
         write ( text, '(''    '',a1,''-'',a1,''      '',a1,''-'',a1)')
     +           COL(1), COL(2), COL(3), COL(4)
      else
         write ( text, '(''    '',a1,''       '',a1,''-'',a1)' )
     +           COL(2), COL(3), COL(4)
      endif
      if ( kopt.eq.2 ) then						!Nearest star
         if ( KSTART.eq.1 ) then
            write ( text(19:),'(''     Star    '',16x,a1,''-'',a1,
     +      ''    '',3x,a1,''-'',a1)' ) COL(1),COL(2),COL(3),COL(4)
         else
            write ( text(17:),'(''     Star    '',16x,a1,''    '',
     +              3x,a1,''-'',a1)' )COL(2), COL(3), COL(4)
         endif
      endif
      call printo ( text )

      loop = .true.							!Get and type posns
      do while ( loop )
         k = pgcurse ( xa, ya, ch )
         if ( xa.ge.min(DXLIM(1),DXLIM(2)) .and.
     +        xa.le.max(DXLIM(1),DXLIM(2)) ) then
            write ( text, '(1x,f7.3,2x,f7.3)' ) ya, xa
            if ( kopt.eq.2 ) then
               rd = 1.0e10
               jmin = 0
               xmin = 0.0
               ymin = 0.0
               xw = DXLIM(2) - DXLIM(1)
               yw = DYLIM(2) - DYLIM(1)
               do j = 1, TOTSTARS
                  if ( ZZ(j) ) then
                     dx = xa - XX(j)
                     dy = ya - YY(j)
                     dd = (dx*dx/(xw*xw)) + (dy*dy/(yw*yw))
                     if ( dd.lt.rd ) then
                        rd = dd
                        jmin = j
                        xmin = XX(j)
                        ymin = YY(j)
                     endif
                  endif
               enddo
               if ( jmin.eq.0 ) then
                  write ( text(27:), '(''    No valid stars'')' )
               else
                  rname = ANAME(KSTART,jmin)
                  write ( text(20:), '(a20,2x,f7.3,2x,f7.3)' )
     +                  rname, ymin, xmin
               endif
            endif
            call printo ( text )
         else
            loop = .false.
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_LINE -- Puts lines on the display
C
C   a j penny                    dao             1988-04-25

      subroutine di_line ()

      implicit none
      include 'diagram.inc'
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'
C--
      real      x, y, dx, dy
      logical   more, loop
      character ch*1, text*72
      integer   k, kopt, kst, tbxv, tby, kcol1, kcol2, iptb, kdef,
     +          istat

      integer pgcurse
      external pgcurse

      integer nthelp
      parameter ( nthelp=9 )
      character*68 thelp(nthelp)
      data (thelp(k),k=1,nthelp) /
     + 'Place lines on graph:-' ,
     + '    ' ,
     + 'Method of inputing (and then plotting) the line:-' ,
     + 'Option    Function' ,
     + '------    --------' ,
     + 'Cursor    Use the cursor to mark line' ,
     + 'Keyboard  Input line by keyboard entry' ,
     + 'Return    Return to main Program' ,
     + 'Table     Get line from a table' /
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GD_DISPOP ) then					!Is device open?
         call printo ( 'ERROR: Device not open' )
         return
      endif

      kdef = 2
      more = .true.
      do while ( more )

         call get_job ( 'TYLINE', 'cursor:table:keyboard:return', 	!Get sort of line
     +                            kopt, kdef, thelp, nthelp )
         if ( ST_FAILED ) return
         kdef = kopt

         if ( kopt.eq.1 ) then						!Use cursor to define line

            if ( KSTART.eq.1 ) then					!List of positions heading
           write ( text, '(''    '',a1,''-'',a1,''    '',a1,''-'',a1)' )
     +              COL(1), COL(2), COL(3), COL(4)
            else
               write ( text, '(''    '',a1,''     '',a1,''-'',a1)' )
     +                         COL(2), COL(3), COL(4)
            endif
            call printo ( text )

            kst = 0
            loop = .true.
            do while ( loop )
               k = pgcurse ( x, y, ch )
               if ( x.lt.DXLIM(1) ) then
                  loop = .false.
               else
                  write ( text, '(1x,f7.3,2x,f7.3)' ) y, x
                  call printo ( text )
                  if ( kst.eq.0 ) then
                     call pgmove ( x, y )
                     kst = 1
                  endif
                  call pgbbuf
                  call pgdraw ( x, y )
                  call pgebuf
               endif
            enddo
         endif

         if ( kopt.eq.2 ) then						!Use a file to define line

            call optabr ( 'INTAB', iptb, tbxv, tby, .false., istat )
            if ( ST_FAILED ) return
            kcol1 = 1
            kcol2 = 2
            call get2i ( 'PCOLUMN', kcol1, kcol2, .true., 1, tbxv-5 )
            if ( ST_FAILED ) return
            kcol1 = kcol1 + 5
            kcol2 = kcol2 + 5
            dx = 0.0
            dy = 0.0
            call get2r ( 'OFFSETS', dx, dy, .true., -1.0e10, 1.0e10 )
            if ( ST_FAILED ) return

            call pgbbuf
            do k = 1, tby
               call copr1 ( %val(iptb), tbxv, tby, kcol1, k, x )
               x = x + dx
               call copr1 ( %val(iptb), tbxv, tby, kcol2, k, y )
               y = y + dy
               if ( k.eq.1 ) call pgmove ( x, y )
               call pgdraw ( x, y )
            enddo
            call pgebuf
            call canpar ( 'INTAB' )
         endif

         if ( kopt.eq.3 ) then						!Use keyboard
            x = DXLIM(1)
            y = DYLIM(1)
            loop = .true.
            kst = 0
            do while ( loop )
               call get2r ( 'XYKEY', x, y, .true., -1.0e10, 1.0e10 )
               if ( ST_FAILED ) return
               if ( x.lt.DXLIM(1) ) then
                  loop = .false.
               else
                  if ( kst.eq.0 ) then
                     call pgmove ( x, y )
                     kst = 1
                  endif
                  call pgbbuf
                  call pgdraw ( x, y )
                  call pgebuf
               endif
            enddo
         endif

         if ( kopt.eq.4 ) more = .false. 						!No line

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_CLEAR -- Clear display
C
C    a j penny                      dao               1988-04-24

      subroutine di_clear ()
      implicit none
      include 'diagram.inc'
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GD_DISPOP ) then						!Is device open?
         call printo ( 'ERROR: Device not open' )
         return
      endif

      DONEAXIS = .false.
      call pgpage


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   DIAGRAM.FOR
C
C   Contains:-
C
C T_DIAGRAM     Perform the DIAGRAM program function
C DI_OPTION_SETUP  Put out help panel help on chosen option
C DI_COLCODE    Get the colour codes for the wanted colours
C DI_SETUP      Load the default parameters
C DI_COLDECODE  Decode colour character to file no
C DI_GSET       Get a new set of files
C DI_MAGLIMS    Get magnitude limit of a file
C DI_NUMGOOD    Get number of good measures needed
C DI_SYMB       Change plot symbol (PGPLOT code)
C DI_PRANGE     Allowed plot range
C DI_ZERO       Get zero points for this file
C DI_COLEQ      Get colour eqn for file N
C DI_LOAD       Load magnitudes, etc from files into data
C DI_OUT        Outputs data to file
C DI_PLIMS      Get plot limits


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_DIAGRAM -- Perform the DIAGRAM program function
C
C   a j penny                 dao           1988-04-25

      subroutine t_diagram

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'
      include 'ST_DS_PANEL_INC'
C--
      integer k, ierr
      character*12 ktopt
      logical loop, easycmd, doexit

      integer nthelp1
      parameter ( nthelp1=4 )
      character*68 thelp1(nthelp1)
      data (thelp1(k),k=1,nthelp1) /
     + 'Option   Function',
     + '------   --------',
     + 'Cmd      Plot a Colour-magnitude diagram',
     + 'Twocol   Plot a Colour-colour diagram' /

Cbegin


      if ( ST_FAILED ) return

      call di_setup							!Set up parameters
      if ( ST_FAILED ) return

      call get1b ( 'EASYCMD', easycmd, .true. )
      if ( easycmd ) then
         KSTART = 2
         call di_colcode
         call di_gset
         call di_plot
         call type_hchoice
         call get1b ( 'EXIT', doexit, .true. )
         if ( doexit ) return
         call di_option_setup ( ktopt, 1, .true. )
         if ( ST_FAILED ) return
         call choice_panel_sw
      else
         call get_job ( 'TYPE', 'cmd:twocol', k, 1, thelp1, nthelp1 )	!Get if HR or Two-colour
         if ( ST_FAILED ) return
         KSTART = 1							! diagram
         if ( k.eq.1 ) KSTART = 2
         call di_colcode						!Get colour codes
         call type_hchoice
         call di_option_setup ( ktopt, 1, .true. )
         if ( ST_FAILED ) return
      endif

      loop = .true.							!Do the work
      do while ( loop )

         call di_option_setup ( ktopt, 1, .false. )
         call get_choice ( ktopt, 1 )					!Get choice
         if ( ST_FAILED ) return

         if ( ktopt.eq.'getdata' ) call di_gset				!Open a set of files

         if ( ktopt.eq.'plot' ) call di_plot				!Display points

         if ( ktopt.eq.'colour_eqn' ) call di_coleq			!Change colour corrns

         if ( ktopt.eq.'clear' ) call di_clear				!Clear display

         if ( ktopt.eq.'zero_point' ) call di_zero			!Change zero points

         if ( ktopt.eq.'store' ) call di_out				!Output result

         if ( ktopt.eq.'open' ) call di_open				!Open device

         if ( ktopt.eq.'mag_limits' ) call di_maglims 			!Change magnitude limits

         if ( ktopt.eq.'setnumber' ) then				!Change set number
                           call pargi ( NTOTSET )
                           call printd ( '%d sets open so far' )
                           if ( NSET.ne.0 ) call get1i ( 'SET', NSET,
     +                                             NSET, 1, NTOTSET )
                           if ( ST_FAILED ) return
                           endif

         if ( ktopt.eq.'numgood' ) call di_numgood			!Number of good estimates

         if ( ktopt.eq.'plot_range' ) call di_prange			!Allowed plot range

         if ( ktopt.eq.'symbol' ) call di_symb				!Plot symbols

         if ( ktopt.eq.'dotnumber' ) then				!Change numbering the plotted points
                           if ( DOTNUM ) then
                              call printo ( 'Points were numbered' )
                              call printo ( 'They are not now' )
                           else
                              call printo ( 'Points were not numbered')
                              call printo ( 'They are now' )
                           endif
                           DOTNUM = .not.DOTNUM
                           endif

         if ( ktopt.eq.'line' ) call di_line				!Put lines on plot

         if ( ktopt.eq.'cursor' ) call di_gcur ( 1 )			!Type positions on plot picked by cursor

         if ( ktopt.eq.'nearest' ) call di_gcur ( 2 )			!Type positions of cursor and nearest star

         if ( ktopt.eq.'panel' ) call choice_panel_sw			!Change panel/keyboard input choice

         if ( ktopt.eq.'plot_limits' ) call di_plims 			!Change plot limits

         if ( ktopt.eq.'close' ) call di_close				!Close graph

         if ( ktopt.eq.'exit' ) loop = .false. 				!Exit from program

         if ( ST_FAILED ) return

      enddo

      if ( DOPANEL ) call ds_p_close ( ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_OPTION_SETUP -- Put out help panel help on chosen option
C
C   alan penny                        ral              1990-01-31

      subroutine di_option_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      character*12   ktopt              !o: Chosen option
      integer        set_num		!i: Number of option set
      logical        koutside		!i: Are we outside the option loop?
C--
      integer j,k

      integer opt_num
      parameter ( opt_num=20 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'clear', 'Clear plot',
     + 'Clear the plotted diagram (the programme does not forget about',
     + 'the input table).',
     + ' ', ' ', ' ', ' '/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'open', 'Open the plot device ',
     + 'Open a device for the plot. This uses the PGPLOT package. You',
     + 'put in the GKS name of the device. A standard device is ',
     + '-xwindows- for opening a window on an X-terminal. To find the',
     + 'GKS name of a device, type -ask-.',
     + 'For a device which makes a file, you must press the close',
     + 'button to close the device, before plotting the file.'/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'close', 'Close the plot device ',
     + 'One may need to close the plot output display. It is a',
     + 'PGPLOT -device-, and one might want to swop between screen ',
     + '(xwindows) and printer, or one might want to get the ',
     + 'output from printer (this file only becomes available when',
     + 'the device is -closed-). ',
     + '[All plot options open a device, if it is closed.]'/

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.',
     + ' ', ' ', ' '/

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'exit', 'Exit from this program',
     + 'Exit from this program. Any windows open are closed, and any',
     + 'files open are closed.',
     + ' ', ' ', ' ', ' '/

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'line', 'Put line onto graph.',
     + 'A solid line is drawn on the graph. Method choice (repeated):-',
     + '    1) Reading in a table of XY positions.       (-table-)',
     + '    2) Typing in a series of XY positions.      (-keyboard-)',
     + '    3) Cursor input a series of XY positions. (-cursor-)',
     + '    4) End of this option.                         (-return-)',
     + '[2) and 3) - end by inputting a posn to the left of the axis.]'/

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'cursor', 'Get XY positions from graph ',
     + 'The cursor is placed in the plot, and a mouse button pressed.',
     + 'The location of that position is typed out, and the user can',
     + 'repeat for another position.',
     + ' ',
     + 'Exit from this option by selecting a position to the left of',
     + 'the plot (but still within the window).'/

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'colour_eqn', 'Change colour equation applied to input colours',
     + 'Each input -colour- (i.e. a single waveband) has a',
     + 'colour equation applied to it before plotting or storage.',
     + 'This asks you for the colour constant (default zero) in the ',
     + 'equation to be applied to each colour. The Equation is:- ',
     + ' Used Colour = Input colour + Const * (col(3-4)) + Zero point',
     + '[col(3-4) is the (input) colour in the plot in X direction.]'/

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'plot', 'Plot the data as points on the graph ',
     + 'The data already loaded are plotted as points on the graph.',
     + '(A plot device is opened if not open.)',
     + ' ', ' ', ' ', ' ' /

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'dotnumber', 'Set whether numbers are plotted with points',
     + ' ',
     + 'Toggle between two states of plotting the data. The first ',
     + 'is just plotting the points. The second additionally adds ',
     + 'the number of each point in the input table. ',
     + ' ', ' '/

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'mag_limits', 'Star Magnitude ranges allowed to be plotted',
     + ' ',
     + 'This sets the range in input magnitudes that plotted stars are',
     + 'allowed to have. You can set these limits for all the input ',
     + 'magnitudes. Or you can let all the stars be plotted.',
     + 'The defaults are just outside the minimum and maximum of all',
     + 'the data. '/

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'numgood', 'Number of good measures for plotting a star ',
     + ' ',
     + 'This sets the number of good measures a star must have',
     + 'before it can be plotted. These ranges apply to the present',
     + 'data set. You can also set them to apply to all sets',
     + ' ',
     + 'The default is the present value, start value = 1.'/

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'plot_limits', 'Set the range of the graph axes in X and Y',
     + ' ',
     + 'This sets the range of the scales on the X and Y axes of',
     + 'the plot. ',
     + ' ',
     + 'The defaults are from a bit below the minimum of all the',
     + 'data to a bit above the maximum. '/

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'plot_range', 'Set the range of data in X and Y to be plotted',
     + 'This puts the range in X and Y that data are allowed to be',
     + 'plotted in. Data points falling outside the permitted XY box',
     + 'will not be plotted. These ranges apply to the present data',
     + 'set. You can also set them to apply to all sets.',
     + 'The defaults are just outside the minimum and maximum of all',
     + 'the data. '/

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'setnumber', 'Data set these setups apply to ',
     + ' ',
     + 'As you can input a number of data sets, you can set a ',
     + 'different a different set of the setup values (col eqn, zero',
     + 'point, plot limits, etc) to each set. The default is the last',
     + 'set of data entered. With this option you can set it to any of',
     + 'the previously entered sets. '/

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'symbol', 'Set the symbol plotted as points ',
     + 'You can set the type of the sybol plotted for each point.',
     + 'The options are the integers of the PGPLOT codes 1 -128.',
     + 'This code is set for the present set. You also have the option',
     + 'to set this symbol for all data sets.',
     + ' ',
     + 'The default is the present set code, at first 1 (a dot).'/

      data opt_text(17),opt_head(17),(opt_help(j,17),j=1,6) /
     + 'zero_point', 'Change zero points applied to input colours',
     + 'Each input -colour- (i.e. a single waveband) has a',
     + 'zero point applied to it before plotting or storage. ',
     + 'This asks you for the zero point (default zero) in the ',
     + 'equation to be applied to each colour. The Equation is:- ',
     + ' Used Colour = Input colour + Const * (col(3-4)) + Zero point',
     + '[col(3-4) is the (input) colour in the plot in X direction.]'/

      data opt_text(18),opt_head(18),(opt_help(j,18),j=1,6) /
     + 'getdata', 'Get the tables with the magnitudes in them',
     + ' ',
     + 'Get the tables with the magnitudes in them. There can be',
     + 'up to four input colours - but these may be in four separate',
     + 'tables or in less, with more than one colour in a single ',
     + 'table. Only enough tables are asked for to get the required ',
     + 'colours.'/

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'store', 'Store the present plotted data set as a table file ',
     + ' ',
     + 'The present data set is stored in a table. The XY positions',
     + 'may be stored. Then the cmd or two-colour diagram values are',
     + 'stored. Then the input magnitudes and the number of good',
     + 'measures are stored.',
     + ' ' /

      data opt_text(20),opt_head(20),(opt_help(j,20),j=1,6) /
     + 'nearest', 'Get cursor XY positions and nearest star',
     + 'The cursor is placed in the plot, and a mouse button pressed.',
     + 'The location of that position is typed out, and the user can',
     + 'repeat for another position. The position and name of the',
     + 'nearest star is the current set is put out.',
     + 'Exit from this option by selecting a position to the left of',
     + 'the plot (but still within the window).'/

      character*50 title, option
      integer ncode
      data title, option, ncode / 'Diagram', 'OPTION', 1 /

      integer def_x, def_y
      parameter ( def_x=5 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'getdata', 'plot', 'panel', 'cursor' /

      integer sect_num
      parameter ( sect_num=6 )
      character*10 sect_head(sect_num)
      data sect_head / 'ACTIONS', 'FILES', 'SETS', 'SETUPS',
     +                 'GRAPH', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'plot:line:cursor:nearest' /
      data sect_text(2) / 'getdata:store' /
      data sect_text(3) / 'setnumber' /
      data sect_text(4) / 'colour_eqn:dotnumber:mag_limits:numgood:
     +                     plot_limits:plot_range:symbol:zero_point' /
      data sect_text(5) / 'clear:close:open' /
      data sect_text(6) / 'panel:exit' /

      integer help_num
      parameter ( help_num=5 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,help_num) /
     + '                                 ' ,
     + 'When using the cursor to find magnitudes/colours of positions' ,
     + 'on the graph, place the cursor on the desired location, and' ,
     + 'press any keyboard key. To end, place the cursor to the left ' ,
     + 'of the origin and press any key.' /


Cbegin


      if ( ST_FAILED ) return

      call setup_option ( ktopt, set_num, koutside,
     +                    sect_num, sect_text, sect_head,
     +                    title, option, ncode,
     +                    1, opt_num, opt_text,
     +                    1, opt_head,
     +                    1, opt_help,
     +                    1, help_num, help_text,
     +                    1, def_x, def_y, def_text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_COLCODE -- Get the colour codes for the wanted colours
C
C   a j penny                     dao              1988-05-01

      subroutine di_colcode

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      integer j, k
      character*7 colcode
      character*1 cv(7), cva
      data cv / 'U', 'B', 'B', 'V', 'V', 'B', 'V' /
Cbegin


      if ( ST_FAILED ) return

      if ( KSTART.eq.1 ) then
         call printo ( 'Colours are plotted as 1-2 versus 3-4' )
      else
         call printo ( 'Colours are plotted as 2 versus 3-4' )
      endif

      do k = KSTART, 4
         call pargi ( k )
         call printd ( 'Colour %d has character code = ?(a1)' )
         j = k
         if ( KSTART.eq.2 ) j = k + 3
         write ( cva, '(i1)' ) k
         colcode = 'COLCOD'//cva
         call get1c ( colcode, cva, cv(j), .true. )
         if ( ST_FAILED ) return
         COL(k) = cva
         KFILE(k) = 0
         do j = KSTART, k
            if ( COL(k).eq.COL(j) .and. KFILE(k).eq.0 ) KFILE(k) = j
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_SETUP -- Load the default parameters
C
C     a j penny                 stsci             1988-04-24

      subroutine di_setup

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      integer j
Cbegin


      if ( ST_FAILED ) return

      call ds_sdef ( 2, 16 )

      KSTART = 1
      DOTNUM = .false.
      NSET = 0
      NTOTSET = 0
      TOTSTARS = 0

      call azeroi ( KFILE,        4 )
      call azeroi ( KCOLUMN,  4*MAXSET )
      call azeroi ( KNCOLUMN, 4*MAXSET )
      call azeroi (   TBYS,     MAXSET )
      call azeroi (  TBXVS,   4*MAXSET )
      call azeror ( COLCOR,   4*MAXSET )
      call azeror ( ZEROP,    4*MAXSET )
      call azeror (     XX,   MAXSTARS )
      call azeror (     YY,   MAXSTARS )
      call amovki ( -1,        NUMG, 4*MAXSET )
      call azerob ( ALLNUMG )
      call amovki ( 1,         SYMB,   MAXSET )
      call amovkr ( 49.0,    RAWMAX, 4*MAXSET )
      call amovkr ( 0.1,     RAWMIN, 4*MAXSET )
      call amovkr ( 1.0e10,   XXMAX,   MAXSET )
      call amovkr ( -1.0e10,  XXMIN,   MAXSET )
      call amovkr ( 1.0e10,   YYMAX,   MAXSET )
      call amovkr ( -1.0e10,  YYMIN,   MAXSET )
      call azerob ( LOADED,   MAXSET )
      call azerob ( OPENED,   4*MAXSET )
      call azerob ( ZZ, MAXSTARS )
      do j = 1, 4
         COL(j) = ' '
      enddo

      PLOTRANGE(1) = -1.0e10
      PLOTRANGE(2) = 1.0e10
      PLOTRANGE(3) = -1.0e10
      PLOTRANGE(4) = 1.0e10

      DXLIM(1) = -1.0
      DXLIM(2) = 2.0
      DYLIM(1) = 25.0
      DYLIM(2) = 10.0
      GOTLIMS  = .false.
      GOTMLIMS = .false.
      DONEAXIS = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_COLDECODE -- Decode colour character to file no
C
C   a j penny                 dao           1988-04-25

      subroutine di_coldecode ( ctext, kcol )

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'

      character*(*) ctext	!i: Colour name
      integer kcol		!o: Colour posn in array
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      if ( NTOTSET.eq.0 ) then
         call printo ( 'ERROR: No data sets read in' )
         return
      endif

      do k = 1, 4
         if ( COL(k).eq.ctext(1:1) ) kcol = k
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_GSET -- Get a new set of files
C
C   a j penny                             dao          1988-04-24

      subroutine di_gset

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      integer iv, i, istat, ipt, j, k, ka, tbxv, tbya, tby, ja, jj, jk
      logical ok
      character cva*1, tcolumn*7, tncol*5, file*72, text*72
Cbegin


      if ( ST_FAILED ) return

      if ( NTOTSET.eq.MAXSET ) then
         call pargi ( MAXSET )
         call printd ( 'ERROR: Maximum no (%d) of sets loaded already' )
         return
      endif
      NTOTSET = NTOTSET + 1
      NSET = NTOTSET

      do k = KSTART, 4							!Open the input files
         if ( KFILE(k).eq.k ) then

            call printo ( ' ' )
            call pargc ( COL(k) )
            call printd ( 'Input file for colour %c' )

            ok = .false.
            do while ( .not.ok )
               ok = .true.
               ka = k + (NSET-1)*4
               if ( ka.ge.10 ) then
                   write ( file, '(''IN'',i2)' ) ka
               else
                   write ( file, '(''IN'',i1)' ) ka
               endif
               call optabr ( file, ipt, tbxv, tbya, .false., istat )
               if ( ST_FAILED ) return
               ok = .true.
               if ( istat.eq.1 ) then
                  call printo ( 'ERROR: Bad file - try again' )
                  ok = .false.
               elseif ( istat.eq.3 ) then
                  if ( k.eq.KSTART .and. NSET.eq.1 ) then
                     call printo ( 'ERROR: Bad file - try again' )
                     ok = .false.
                  else
                     call printo ('WARNING: Table has name of '//
     +                            'a table entered before' )
                     if ( NSET.eq.1 ) then
                        call printo('         Taken as last file -')
                        ipt  = IPF(k-1,NSET)
                        tbya = TBYS(NSET)
                        tbxv = TBXVS(k-1,NSET)
                     else
                        call printo ('         Taken as file in'//
     +                               ' previous set -' )
                        ipt  = IPF(k,NSET-1)
                        tbya = TBYS(NSET-1)
                        tbxv = TBXVS(k,NSET-1)
                     endif
                     call printo ('          not necessarily '//
     +                            'the right one')
                     ok = .true.
                  endif
               elseif ( tbxv.lt.6 .or. tbya.le.0 ) then
                  call printo ( 'ERROR: Bad file - try again' )
                  ok = .false.
               endif

               if ( ok ) then
                  OPENED(ka) = .true.
                  if ( k.eq.KSTART ) then
                     tby = tbya
                     if ( istat.ne.3 ) then
                        call gtdesc ( file, 'TITLE', ATITLE,
     +                                ' ', iv, i )
                     else
                        ATITLE = ' '
                     endif
                  else
                     if ( tbya.ne.tby ) then
                        call printo (
     +                       'ERROR: File wrong length - try again' )
                        ok = .false.
                     endif
                  endif
               endif

            enddo

            jk = 3
            ja = 0
            call findhead ( file, (tbxv-5), 'magnitude', .false.,jj)
            if ( jj.ne.0 ) ja = jj
            call findhead ( file, (tbxv-5), 'magnitudes',.false.,jj)
            if ( jj.ne.0 ) ja = jj
            call findhead ( file, (tbxv-5), 'mag', .false., jj )
            if ( jj.ne.0 ) ja = jj
            call findhead ( file, (tbxv-5), 'magn', .false., jj )
            if ( jj.ne.0 ) ja = jj
            call findhead ( file, (tbxv-5), 'mags', .false., jj )
            if ( jj.ne.0 ) ja = jj
            call findhead ( file, (tbxv-5), 'magns', .false., jj )
            if ( jj.ne.0 ) ja = jj
            if ( ST_FAILED ) return
            if ( ja.ne.0 ) then
               jk = ja
               call gthead ( file, jk, text, istat )
               call pargc ( text )
               call pargi ( jk )
               call printd (
     +              'Magnitude header - %c - found in column %d' )
            else
               call printo ( 'No Magnitude header found. It is '//
     +                       'commonly in column 3' )
            endif

            write ( cva, '(i1)' ) k
            tcolumn = 'COLUMN'//cva
            call get1i ( tcolumn, KCOLUMN(k,NSET), jk, 1, (tbxv-5) )
            if ( ST_FAILED ) return

            jk = 4
            ja = 0
            call findhead ( file, (tbxv-5), 'number', .false.,jj)
            if ( jj.ne.0 ) ja = jj
            call findhead ( file, (tbxv-5), 'numbers',.false.,jj)
            if ( jj.ne.0 ) ja = jj
            call findhead ( file, (tbxv-5), 'num', .false., jj )
            if ( jj.ne.0 ) ja = jj
            call findhead ( file, (tbxv-5), 'nums', .false., jj )
            if ( jj.ne.0 ) ja = jj
            call findhead ( file, (tbxv-5), 'numb', .false., jj )
            if ( jj.ne.0 ) ja = jj
            call findhead ( file, (tbxv-5), 'numbs', .false., jj )
            if ( jj.ne.0 ) ja = jj
            if ( ST_FAILED ) return
            if ( ja.ne.0 ) then
               jk = ja
               call gthead ( file, jk, text, istat )
               call pargc ( text )
               call pargi ( jk )
               call printd (
     +              'Number header - %c - found in column %d' )
            else
               call printo ( 'No Number header found. It is '//
     +                       'commonly in column 4' )
            endif

            tncol = 'NCOL'//cva
            call get1i ( tncol, KNCOLUMN(k,NSET), jk, 0, (tbxv-5) )
            if ( ST_FAILED ) return
            if ( KNCOLUMN(k,NSET).ne.0 .and. .not.ALLNUMG )
     +                                         NUMG(k,NSET) = 0

            IPF(k,NSET)   = ipt
            TBYS(NSET)    = tby
            TBXVS(k,NSET) = tbxv

         endif

      enddo

      do k = KSTART, 4
         j = KFILE(k)
         KCOLUMN(k,NSET) = KCOLUMN(j,NSET)
         KNCOLUMN(k,NSET) = KNCOLUMN(j,NSET)
         IPF(k,NSET)   = IPF(j,NSET)
         TBXVS(k,NSET) = TBXVS(j,NSET)
      enddo

      call printo ( ' ' )
      call pargi ( tby )
      call printd ( 'Total number of stars in these tables: %d ' )
      call pargi ( NSET )
      call printd ( 'Data is put into set number: %d ' )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_MAGLIMS -- Get magnitude limit of a file
C
C     a j penny                     dao                1988-04-25

      subroutine di_maglims ()

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      real rv
      integer j, k, kt
      logical doall
Cbegin


      if ( ST_FAILED ) return

      if ( NTOTSET.eq.0 ) then
         call printo ( 'ERROR: No data sets read in' )
         return
      endif

      call get1b ( 'ALL_SETS', doall, .true. )
      if ( ST_FAILED ) return

      kt = NSET
      if ( doall ) kt = 1
      do k = KSTART, 4
         if ( KFILE(k).eq.k ) then

            call pargc ( COL(k) )
            call printd ( 'Colour %c maximum magnitude?' )
            rv = RAWMAX(k,kt)
            call get1r ( 'HILIM', rv, rv, -1.0e10, 1.0e10 )
            if ( ST_FAILED ) return
            if ( rv.ne.RAWMAX(k,kt) ) LOADED(kt) = .false.
            RAWMAX(k,kt) = rv
            if ( doall ) then
               do j = 1, NTOTSET
                  if ( rv.ne.RAWMAX(k,j) ) LOADED(j) = .false.
                  RAWMAX(k,j) = rv
               enddo
            endif

            call pargc ( COL(k) )
            call printd ( 'Colour %c minimum magnitude?' )
            rv = RAWMIN(k,kt)
            call get1r ( 'LOLIM', rv, rv, -1.0e10, 1.0e10 )
            if ( ST_FAILED ) return
            if ( rv.ne.RAWMIN(k,kt) ) LOADED(kt) = .false.
            RAWMIN(k,kt) = rv
            if ( doall ) then
               do j = 1, NTOTSET
                  if ( rv.ne.RAWMIN(k,j) ) LOADED(j) = .false.
                  RAWMIN(k,j) = rv
               enddo
            endif

         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_NUMGOOD -- Get number of good measures needed
C
C     a j penny                     dao                1988-04-25

      subroutine di_numgood ()

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      integer j, k, kt, iv
Cbegin


      if ( ST_FAILED ) return

      if ( NTOTSET.eq.0 ) then
         call printo ( 'ERROR: No data sets read in' )
         return
      endif

      call get1b ( 'ALL_SETS', ALLNUMG, .true. )
      if ( ST_FAILED ) return

      kt = NSET
      if ( ALLNUMG ) kt = 1
      do k = KSTART, 4
         if ( KFILE(k).eq.k ) then

            call pargc ( COL(k) )
            call printd ( 'Colour %c no of good magnitudes needed?' )
            iv = NUMG(k,kt)
            iv = max(iv,0)
            call get1i ( 'NUMGOOD', iv, iv, 0, 100000 )
            if ( ST_FAILED ) return
            if ( iv.ne.NUMG(k,kt) ) LOADED(kt) = .false.
            NUMG(k,kt) = iv
            if ( ALLNUMG ) then
               do j = 1, NTOTSET
                  if ( iv.ne.NUMG(k,j) ) LOADED(j) = .false.
                  NUMG(k,j) = iv
               enddo
            endif

         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_SYMB -- Change plot symbol (PGPLOT code)
C
C     a j penny                     dao                1988-04-25

      subroutine di_symb ()

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      integer iv
      logical doall
Cbegin


      if ( ST_FAILED ) return

      call get1i ( 'SYMBOL', iv, SYMB(NSET), 1, 128 )
      call get1b ( 'ALL_SETS', doall, .true. )
      if ( ST_FAILED ) return

      SYMB(NSET) = iv
      if ( doall ) call amovki ( iv, SYMB, MAXSET )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_PRANGE -- Allowed plot range
C
C     a j penny                     dao                1988-04-25

      subroutine di_prange ()

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      real xva, xvb,yva, yvb
      logical doall
Cbegin


      if ( ST_FAILED ) return

      if ( NTOTSET.eq.0 ) then
         call printo ( 'ERROR: No data sets read in' )
         return
      endif

      xva = XXMIN(NSET)
      xvb = XXMAX(NSET)
      yva = YYMIN(NSET)
      yvb = YYMAX(NSET)
      call get2r ( 'XRANGE', xva, xvb, .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return
      call cswopr ( xva, xvb )
      call get2r ( 'YRANGE', yva, yvb, .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return
      call cswopr ( yva, yvb )

      XXMIN(NSET) = xva
      XXMAX(NSET) = xvb
      YYMIN(NSET) = yva
      YYMAX(NSET) = yvb
      LOADED(NSET) = .false.

      call get1b ( 'ALL_SETS', doall, .true. )
      if ( ST_FAILED ) return

      if ( doall ) then
         call amovkr ( xva, XXMIN, MAXSET )
         call amovkr ( xvb, XXMAX, MAXSET )
         call amovkr ( yva, YYMIN, MAXSET )
         call amovkr ( yvb, YYMAX, MAXSET )
         call azerob ( LOADED,   MAXSET )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_ZERO -- Get zero points for this file
C
C   a j penny                 dao           1988-04-25

      subroutine di_zero ()

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      integer k
      real rv
Cbegin


      if ( ST_FAILED ) return

      if ( NTOTSET.eq.0 ) then
         call printo ( 'ERROR: No data sets read in' )
         return
      endif

      call printo ( ' ' )
      call printo ( 'Change input magnitude zero points' )
      do k = KSTART, 4
         if ( KFILE(k).eq.k ) then
            call printo ( ' ' )
            call pargc ( COL(k) )
            call pargc ( COL(k) )
            call pargc ( COL(k) )
            call pargc ( COL(3) )
            call pargc ( COL(4) )
            call pargc ( COL(k) )
            call printd (
     +         '   %c (used) = %c + K%c*(%c-%c) + ZP%c (input) ' )
            call pargc ( COL(k) )
            call printd ( '   What value of ZP%c to use?' )
            rv = ZEROP(k,NSET)
            call get1r ( 'ZEROP', rv, rv, -1.0e10, 1.0e10 )
            if ( ST_FAILED ) return
            if ( rv.ne.ZEROP(k,NSET) ) LOADED(NSET) = .false.
            ZEROP(k,NSET) = rv
         else
            ZEROP(k,NSET) = ZEROP(KFILE(k),NSET)
         endif
      enddo
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_LOAD -- Load magnitudes, etc from files into data
C
C   a j penny                 dao           1988-04-25

      subroutine di_load ( )

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      real    c1, c2, c3, c4, c34, rv
      integer j, ja, jj, k
      character*20 rname
Cbegin


      if ( ST_FAILED ) return

      if ( NTOTSET.eq.0 ) then
         call printo ( 'ERROR: No data sets read in' )
         return
      endif

      if ( TBYS(NSET).gt.MAXSTARS ) then
         call pargi ( TBYS(NSET) )
         call pargi ( MAXSTARS )
         call printd ( 'ERROR: %d stars - too many. Only %d allowed' )
         call pargi ( MAXSTARS )
         call printd ( '        so only first %d loaded' )
      endif

      TOTSTARS = min(MAXSTARS,TBYS(NSET))

      do k = KSTART, 4
         j = KFILE(k)
         ja = KCOLUMN(j,NSET) + 5
         do jj = 1, TOTSTARS
            call copr1 ( %val(IPF(j,NSET)), TBXVS(j,NSET),
     +                   TBYS(NSET), ja, jj, rv )
            ADATA(k,jj) = rv
            call namegt ( %val(IPF(j,NSET)), TBXVS(j,NSET),
     +                   TBYS(NSET), jj, rname )
            ANAME(k,jj) = rname
         enddo
      enddo

      do k = KSTART, 4
         if ( KNCOLUMN(k,NSET).ne.0 ) then
            ja = KNCOLUMN(j,NSET) + 5
            do jj = 1, TOTSTARS
              call copr1 ( %val(IPF(j,NSET)), TBXVS(j,NSET),
     +                     TBYS(NSET), ja, jj, rv )
              FNUMFG(k,jj) = rv
            enddo
         else
            do jj = 1, TOTSTARS
               FNUMFG(k,jj) = 1.0
            enddo
         endif
      enddo

      do k = 1, TOTSTARS

         XX(k) = 0.0
         YY(k) = 0.0
         ZZ(k) = .true.
         do j = KSTART, 4
            if ( ADATA(j,k).gt.RAWMAX(j,NSET) ) ZZ(k) = .false.
            if ( ADATA(j,k).lt.RAWMIN(j,NSET) ) ZZ(k) = .false.
            if (  NUMG(j,NSET).ne.-1 ) then
               if ( nint(FNUMFG(j,k)).lt.NUMG(j,NSET) ) ZZ(k) = .false.
            endif
         enddo

         if ( ZZ(k) ) then
            c3 = ZEROP(3,NSET) + ADATA(3,k)
            c4 = ZEROP(4,NSET) + ADATA(4,k)
            c34 = c3 - c4
            if ( KSTART.eq.1 ) c1 = ZEROP(1,NSET) + ADATA(1,k) +
     +                              COLCOR(1,NSET)*c34
            c2 = ZEROP(2,NSET) + ADATA(2,k) + COLCOR(2,NSET)*c34
            c3 = c3 + COLCOR(3,NSET)*c34
            c4 = c4 + COLCOR(3,NSET)*c34
            XX(k) = c3 - c4
            if ( KSTART.eq.1 ) then
               YY(k) = c1 - c2
            else
               YY(k) = c2
            endif
         endif

         if ( XX(k).lt.XXMIN(NSET) ) ZZ(k) = .false.
         if ( XX(k).gt.XXMAX(NSET) ) ZZ(k) = .false.
         if ( YY(k).lt.YYMIN(NSET) ) ZZ(k) = .false.
         if ( YY(k).gt.YYMAX(NSET) ) ZZ(k) = .false.


      enddo

      call azerob ( LOADED,   MAXSET )
      LOADED(NSET) = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_OUT -- Outputs data to file
C
C   a j penny                 dao           1988-04-25

      subroutine di_out ()

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      integer ipo, istat, j, k, kp, kxy, tbxvout, tbyout
      real rv
      logical doxy
      character text*72
      character*4 hval(12)
Cbegin


      if ( ST_FAILED ) return

      if ( NTOTSET.eq.0 ) then
         call printo ( 'ERROR: No data sets read in' )
         return
      endif

      if ( .not.LOADED(NSET) ) call di_load

      call get1b ( 'STOREXY', doxy, .true. )
      if ( ST_FAILED ) return

      tbxvout = 8
      if ( doxy ) tbxvout = tbxvout + 2
      do k = 1, 4
         if ( KFILE(k).eq.k ) then
            tbxvout = tbxvout + 1
            if ( NUMG(k,NSET).ne.-1 ) tbxvout = tbxvout + 1
         endif
      enddo
      tbyout = TBYS(NSET)

      call optabw ( 'OUT', ipo, tbxvout, tbyout, .false., istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call charln ( ATITLE, k )
      if ( k.eq.0 ) ATITLE = 'Output from DIAGRAM'
      call get1c ( 'TITLE', text, ATITLE, .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT', 'TITLE', text )

      kp = 0
      if ( doxy ) then
         kp = kp + 1
         call pthead ( 'OUT', kp, 'X', istat )
         kp = kp + 1
         call pthead ( 'OUT', kp, 'Y', istat )
      endif

      if ( KSTART.eq.1 ) then
         write ( hval(kp), '(a1,''-'',a1)' ) COL(1), COL(2)
      else
         write ( hval(kp), '(a1)' ) COL(2)
      endif
      kp = kp + 1
      call pthead ( 'OUT', kp, hval, istat )
      write ( hval(kp), '(a1,''-'',a1)' ) COL(3), COL(4)
      kp = kp + 1
      call pthead ( 'OUT', kp, hval, istat )

      do k = KSTART, 4
         if ( KFILE(k).eq.k ) then
            kp = kp + 1
            write ( hval(kp), '(a1)' ) COL(k)
            if ( NUMG(k,NSET).ne.-1 ) then
               kp = kp + 1
               write ( hval(kp), '(''n'',a1)' ) COL(k)
            endif
            call pthead ( 'OUT', kp, hval, istat )
         endif
      enddo

      kxy = 5
      if ( doxy ) kxy = 7
      do k = 1, TBYS(NSET)
         call coprr ( %val(IPF(KSTART,NSET)), TBXVS(KSTART,NSET),
     +                TBYS(NSET), 1, kxy,  k, k, %val(ipo), tbxvout,
     +                tbyout, 1, k )
         call cop1r ( YY(k), %val(ipo), tbxvout, tbyout, kxy+1, k )
         call cop1r ( XX(k), %val(ipo), tbxvout, tbyout, kxy+2, k )
         rv = 0.0
         if ( ZZ(k) ) rv = 1.0
         call cop1r ( rv, %val(ipo), tbxvout, tbyout, kxy+3, k )
         kp = kxy + 3
         do j = KSTART, 4
            if ( KFILE(j).eq.j ) then
               kp = kp + 1
               call cop1r ( ADATA(j,k), %val(ipo), tbxvout, tbyout,kp,k)
               if ( NUMG(j,NSET).ne.-1 ) then
                  kp = kp + 1
                  rv = FNUMFG(j,k)
                  call cop1r ( rv, %val(ipo), tbxvout, tbyout, kp, k )
               endif
            endif
         enddo
      enddo

      call canpar ( 'OUT' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_COLEQ -- Get colour eqn
C
C   a j penny                      dao             1988-04-24

      subroutine di_coleq ()

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      integer k
      real rv, rva
Cbegin


      if ( ST_FAILED ) return

      if ( NTOTSET.eq.0 ) then
         call printo ( 'ERROR: No data sets read in' )
         return
      endif

      call printo ( ' ' )
      call printo ( 'Colour equations: Changes to constants:-' )	!Get colour eqn
      do k = KSTART, 4
         if ( KFILE(k).eq.k ) then
            call printo ( ' ' )
            call pargc ( COL(k) )
            call printd ( 'Colour Equation for colour %c :' )
            call pargc ( COL(k) )
            call pargc ( COL(k) )
            call pargc ( COL(k) )
            call pargc ( COL(3) )
            call pargc ( COL(4) )
            call pargc ( COL(k) )
            call printd (
     +        '    %c (used) =  %c + K%c*(%c-%c) + ZP%c (input) ' )
            call pargc ( COL(k) )
            call printd ( '    What value of -K%c- to use ?' )
            rva = COLCOR(k,NSET)
            call get1r ( 'COLCOR', rv, rva, -1.0e10, 1.0e10 )
            if ( ST_FAILED ) return
            if ( rv.ne.COLCOR(k,NSET) ) LOADED(NSET) = .false.
            COLCOR(k,NSET) = rv
         else
            COLCOR(k,NSET) = COLCOR(KFILE(k),NSET)
         endif
      enddo
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DI_PLIMS -- Get plot limits
C
C   a j penny                 dao           1988-04-25

      subroutine di_plims ( )

      implicit none
      include 'diagram.inc'
      include 'STARMAN_INC'
C--
      integer k
      real    amina, amaxa, ax(2), ay(2)
      logical some
      real trunc_e
      external trunc_e
Cbegin


      if ( ST_FAILED ) return

      if ( NTOTSET.eq.0 ) then
         call printo ( 'ERROR: No data sets read in' )
         return
      endif

      if ( .not.LOADED(NSET) ) call di_load				!Check mags loaded

      if ( .not.GOTMLIMS ) then
         amina = 1.0e10							!Get vertical axis range
         amaxa = -1.0e10
         some = .false.
         do k = 1, TOTSTARS
            if ( ZZ(k) ) then
               some = .true.
               amaxa = max(amaxa,YY(k))
               amina = min(amina,YY(k))
            endif
         enddo
         amina = trunc_e(amina,2)
         amaxa = trunc_e(amaxa,2)
         if ( .not.some ) then
            call printo ( ' No valid stars' )
            if ( KSTART.eq.1 ) then
               call pargc ( COL(1) )
               call pargc ( COL(2) )
               call pargr ( amina )
               call pargr ( amaxa )
               call printd (
     +         ' %c - %c limits taken as:  Min = %f  ; Max = %f' )
            else
               call pargc ( COL(2) )
               call pargr ( amina )
               call pargr ( amaxa )
               call printd (
     +         ' %c limits taken as:  Min = %f  ; Max = %f' )
            endif
         else
            if ( KSTART.eq.1 ) then
               ay(1) = amina - 0.15
               ay(2) = amaxa + 0.15
               call pargc ( COL(1) )
               call pargc ( COL(2) )
               call pargr ( amina )
               call pargr ( amaxa )
               call printd ( ' %c - %c data: Min = %f  ; Max = %f' )
            else
               ay(1) = amina - 0.5
               ay(2) = amaxa + 1.5
               call pargc ( COL(2) )
               call pargr ( amina )
               call pargr ( amaxa )
               call printd ( ' %c data: Min = %f  ; Max = %f' )
            endif
         endif
      else
         ay(1) = DYLIM(2)
         ay(2) = DYLIM(1)
      endif

      if ( KSTART.eq.1 ) then
         call pargc ( COL(1) )
         call pargc ( COL(2) )
         call printd ( ' Enter %c - %c plot limits' )
      else
         call pargc ( COL(2) )
         call printd ( ' Enter %c plot limits' )
      endif
      call get2r ( 'DEVLIMY', ay(1), ay(2), .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return
      DYLIM(1) = ay(2)
      DYLIM(2) = ay(1)

      if ( .not.GOTMLIMS ) then
         amina = 30000.0
         amaxa = -30000.0
         some = .false.
         do k = 1, TOTSTARS
            if ( ZZ(k) ) then
               some = .true.
               amaxa = max(amaxa,XX(k))
               amina = min(amina,XX(k))
            endif
         enddo
         amina = trunc_e(amina,2)
         amaxa = trunc_e(amaxa,2)
         if ( .not.some ) then
            call printo ( 'No valid stars' )
            call pargc ( COL(3) )						!Get horizontal axis range
            call pargc ( COL(4) )
            call pargr ( amina )
            call pargr ( amaxa )
            call printd (
     +          ' %c - %c limits taken as:  Min = %f  ; Max = %f' )
         else
            call pargc ( COL(3) )						!Get horizontal axis range
            call pargc ( COL(4) )
            call pargr ( amina )
            call pargr ( amaxa )
            call printd ( ' %c - %c data: Min = %f  ; Max = %f' )
         endif
         ax(1) = amina - 0.15
         ax(2) = amaxa + 0.15
      else
         ax(1) = DXLIM(1)
         ax(2) = DXLIM(2)
      endif

      call pargc ( COL(3) )						!Get horizontal axis range
      call pargc ( COL(4) )
      call printd ( ' Enter %c - %c plot limits' )
      call get2r ( 'DEVLIMX', ax(1), ax(2), .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return
      DXLIM(1) = ax(1)
      DXLIM(2) = ax(2)


      GOTLIMS = .true.
      GOTMLIMS = .true.


      end


