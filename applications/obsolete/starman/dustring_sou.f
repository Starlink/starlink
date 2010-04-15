CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DUSTRING -- (Program) Remove dust ring shadows from image
C
C         A J Penny                RAL             1990-10-14

      subroutine dustring ( ierradam )

      implicit none

      integer    ierradam         !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_dustring

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  DUSTRING.FOR
C
C  Contains:-
C
C T_DUSTRING    Remove/add dust ring shadows from image
C DU_OPTION_SETUP  Set up option choices
C DU_SDEF       Set up defaults
C DU_ADDZ       Add/subtract a dustring from image
C DU_ADDCZ      Cursor Add/subtract a dustring from image
C DU_TBSTORE    Store rings table in a file
C DU_TBAPPLY    Apply all rings from table
C DU_TBLOAD     Load noted rings table from a file
C DU_TBLIST     List out the noted rings
C DU_TBUSE      Set use of ring in table
C DU_GPROF      Get profile
C DU_LEVEL      Type out local level
C DU_TBPUT      Put defined ring into table
C DU_TBGET      Get defined ring from table
C DU_SHOW       Type out defined parameters of ring
C DU_IMSTORE    Store 'cleaned' image
C DU_NEWIM      Get new image
C DU_IMLOAD     Load image to work space
C DU_OPDISP     Open display



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_DUSTRING -- Remove/add dust ring shadows from image
C
C         A J Penny                RAL                     1990-10-15

      subroutine t_dustring ( )

      implicit none
      include 'dustring.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'
      include 'ST_DS_PANEL_INC'
C--
      logical loop
      integer ierr, kx, ky, kb, iv, istat
      character*12 ktopt, ktoptc
Cbegin


      call du_sdef							!Set up start
      if ( ST_FAILED ) return

      call type_hchoice
      call du_option_setup ( ktopt, 1, .true. )
      if ( ST_FAILED ) return
      ktoptc = ' '
      if ( ST_FAILED ) return
      loop = .true.
      do while ( loop .and. .not.ST_FAILED )

         if ( ktopt.eq.'false' ) ktopt = ktoptc

         call du_option_setup ( ktopt, 1, .false. )			!Get choice
         call get_choice ( ktopt, 1 )
         if ( ST_FAILED ) return

         if ( ktopt.eq.'tb_store' ) call du_tbstore 			!Write list to file

         if ( ktopt.eq.'profile' ) call du_gprof			!Profile

         if ( ktopt.eq.'tb_put' ) call du_tbput				!Put present ring into table

         if ( ktopt.eq.'im_store' ) call du_imstore 			!Store 'cleaned' image

         if ( ktopt.eq.'position' ) call get2r  ( 'POSN', XP, YP, 	!Get position
     +                                   .true., -1.0e10, 1.0e10 )

         if ( ktopt.eq.'bk_auto' ) then				!Change use of ring centre as background
                           if ( BACKAUTO ) then
                              BACKAUTO = .false.
                              call printo ( 'Box at centre of ring '//
     +            'not automatically used and loaded into background' )
                           else
                              BACKAUTO = .true.
                              call printo ( 'Box at centre of ring '//
     +            'automatically used and loaded into background' )
                           endif
                           endif

         if ( ktopt.eq.'bk_size' ) then				!Change level getting box size
                           call pargi ( ISIDE )
                           call printd ( 'Present box side = %d' )
                           ISIDE = (5.0/8.0)*RIN
                           ISIDE = max(3,min(ISIDE,NX,NY))
                           call pargi ( ISIDE )
                           call printd ( 'Suggested box side = %d' )
                           iv = min(NX,NY)
                           call get1i ( 'SIDE', ISIDE, ISIDE, 3, iv )
                           if ( ST_FAILED ) return
                           endif

         if ( ktopt.eq.'bk_ground' ) call get1r ('BACK', BACK, BACK,	!Set profile background level
     +                                            -1.0e10, 1.0e10 )

         if ( ktopt.eq.'bk_level' ) call du_level ( %val(IPW) )	!Find local Level

         if ( ktopt.eq.'bk_load' ) then				!Change use of last level as background
                           if ( BACKLOAD ) then
                              BACKLOAD = .false.
                              call printo ( 'Determined level not '//
     +                                      'loaded into background' )
                           else
                              BACKLOAD = .true.
                              call printo ( 'Determined level loaded '//
     +                                      'into background' )
                           endif
                           endif

         if ( ktopt.eq.'show' ) call du_show				!Show ring profile, posn, background

         if ( ktopt.eq.'panel' ) call choice_panel_sw			!Switch between panel and keyboard

         if ( ktopt.eq.'exit' ) loop = .false. 				!Exit

         if ( ktopt.eq.'display' .and. GOTIMAGE ) then			!Display image
                           call du_opdisp ( ierr )
                           if ( ST_FAILED ) return
                           call ds_dodisp ( %val(IPW), NX, NY, 'REAL',
     +                                       DSKVRANGE, IMTITLE )
                           DISPLAYED = .true.
                           endif

         if ( ktopt.eq.'flash' .and. GOTIMAGE ) then			!Flash up image
                           call du_opdisp ( ierr )
                           if ( ST_FAILED ) return
                           call ds_doflash ( %val(IPW), NX, NY, 'REAL',
     +                                       DSKVRANGE, IMTITLE )
                           DISPLAYED = .true.
                           endif

         if ( ktopt.eq.'add' ) call du_addz ( %val(IPW), 1, istat )	!Add ring

         if ( ktopt.eq.'subtract' ) call du_addz ( %val(IPW), 2, istat)	!Subtract ring

         if ( ktopt.eq.'cursor' ) call du_addcz ( %val(IPW) )		!Add/subtract ring with cursor posn

         if ( ktopt.eq.'zoom' .or. ktopt.eq.'reset' .or.
     +        ktopt.eq.'cvalues' ) then
            if ( .not.DISPLAYED ) then
               call printo ( 'ERROR: No image displayed' )
            else
               if ( ktopt.eq.'zoom' ) call ds_zoom ( .true., 0, 0 )	!Pan

               if ( ktopt.eq.'reset' ) call ds_zoom ( .true., 1, 0 )	!Reset zoom

               if ( ktopt.eq.'cvalues' ) call ds_gtcur ( .true., kx, 	!Get cursor posn, image values
     +                                                  ky, kb, ierr )
            endif
         endif

         if ( ktopt.eq.'clear' .and. OPDISP ) then        	        !Clear display
                                        call ds_erase
                                        DISPLAYED = .false.
                                        endif

         if ( ktopt.eq.'close' .and. OPDISP ) then 			!Close display
                                        call ds_close ( ierr )
                                        OPDISP = .false.
                                        DISPLAYED = .false.
                                        endif

         if ( ktopt.eq.'im_get' ) call du_newim ( ierr )		!Get new image

         if ( ktopt.eq.'open' ) call du_opdisp ( ierr )                 !Open display

         if ( ktopt.eq.'tb_apply' ) call du_tbapply			!Apply table of rings

         if ( ktopt.eq.'tb_list' ) call du_tblist			!List table of noted rings

         if ( ktopt.eq.'tb_get' ) call du_tbget				!Load present ring from table

         if ( ktopt.eq.'tb_load' ) call du_tbload			!Load table from file

         if ( ktopt.eq.'tb_use' ) call du_tbuse				!Change use of ring in table

         if ( ktopt.eq.'im_reload' .and. GOTIMAGE ) call du_imload 	!Reload input image into working image

         if ( ktopt.eq.'tb_clear' ) then				!Clear list of rings
                                   NRINGS = 0
                                   call azeror ( RES, TBX*MAXRINGS )
                                   endif

      enddo

      call ds_close ( ierr )
      call ds_p_close ( ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_OPTION_SETUP -- Set up option choices
C
C   alan penny                        ral              1990-01-31

      subroutine du_option_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      character*12   ktopt              !i: Chosen option
      integer set_num                   !i: Code for set of options
      logical  koutside                 !i: Is this called from outside loop?
C--
      integer j, k

      integer opt_num
      parameter ( opt_num=32 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'clear', 'Clear screen' ,
     + 'Clear the display screen (the programme does not forget about',
     + 'the input image).',
     + ' ', ' ', ' ', ' '/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'close', 'Close the display screen',
     + 'Close the display screen (the programme does not forget about',
     + 'the input image).',
     + ' ', ' ', ' ', ' '/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'open', 'Open the display screen',
     + 'Open the display screen, with a size given by keyboard entry.',
     + 'Do not display any image.',
     + ' ', ' ', ' ', ' '/

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'exit', 'Exit from this program',
     + 'Exit from this program. Any windows open are closed, and any',
     + 'files open are closed.',
     + ' ', ' ', ' ', ' '/

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.',
     + ' ', ' ', ' '/

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'im_get', 'Input new image' ,
     + 'This asks for a new image (via the keyboard), and forgets',
     + 'about the present image.', ' ', ' ', ' ', ' '/

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'display', 'Display image',
     + 'Display the present input image. The program asks you for:-' ,
     + '1) The display range (default: +3 and -2 std. devs. from mean)',
     + '2) Whether to -wrap- values outside that range or to show them',
     + '      as at the range ends (default: -no- or last chosen).' ,
     + '3) Where to put the bottom left hand corner of the image' ,
     + '      (default: so as to centre the image in the screen).' /

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'flash', 'Display image in standard way',
     + 'Display the present input image. The programme uses the ',
     + 'standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'reset', 'Reset zoom/pan to unity and centre',
     + 'Reset zoom/pan of the image display to unity and centre.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'zoom', 'Zoom and pan image by using mouse location ',
     + '-Zoom- means zoom around present position of cursor. ' ,
     + '-Pan-  means set present position of cursor to screen centre.' ,
     + '     Left Button twice                      = zoom down by x2' ,
     + '     Centre Button twice                 = zoom up by x2' ,
     + '     Left Button then Centre Button = pan'  ,
     + '     Right button once                        = exit' /

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'cvalues', 'Show the cursor position and image pixel value',
     + 'Show the cursor position and image pixel value.',
     + 'Whilst the cursor is inside the last displayed image in the ',
     + 'the display screen, and that window is -active-, put these',
     + 'values up in the panel window.',
     + ' ', 'Click on the right-hand button to return. ' /

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'add', 'Add a ring of standard profile at position set',
     + ' ',
     + 'This adds a ring of the standard profile at the given',
     + 'position, using the background that has either been ',
     + 'already set, or one determined at this time.',
     + ' ',
     + 'The adding is done to the present working image' /

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'subtract', 'Subtract a ring of set profile at position set',
     + ' ',
     + 'This subtracts a ring of the standard profile at the given',
     + 'position, using the background that has either been ',
     + 'already set, or one determined at this time.',
     + ' ',
     + 'The subtracting is done to the present working image' /

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'cursor', 'Add or subtract a set ring at cursor position',
     + 'This adds or subtracts a ring of the standard profile at ',
     + 'present position of the cursor. It uses the background ',
     + 'that has either been already set, or one determined at ',
     + 'this time. Press a button to perform add or subtract.',
     + ' ',
     + 'Left button subtracts; middle button adds; right buttons ends'/

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'tb_apply', 'Add or subtract rings from the input table',
     + 'Using the table input buttons, a table of positions and ring ',
     + 'profiles can be input. This option applies that table of rings',
     + 'to the image. You choose whether to add or subtract them. ',
     + 'The background is either taken from the table, or from the ',
     + 'local area, depending on the BK_AUTO button option. ',
     + 'The table has a use flag for each ring.' /

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'bk_auto', 'Toggle the method of calculating the local level',
     + ' ',
     + 'A ring profile is a fractional one, as a dust ring intercepts',
     + 'a fraction of the incident light. Thus a local average level',
     + 'has to be determined. This can either be done  automatically',
     + 'from the local level at the centre of the ring, or the user ',
     + 'can feed in a level.'/

      data opt_text(17),opt_head(17),(opt_help(j,17),j=1,6) /
     + 'bk_size', 'Set the size of the area used to find background',
     + 'As explained in the BOX_AUTO button help, a local background',
     + 'has to be found. If this is done automatically, or by the user',
     + 'finding a level, this is done by finding the mean in a square',
     + 'box. The side of the box is set by this option through the ',
     + 'SIDE parameter. The suggested size is 5/8 times the inner',
     + 'radius of the present ring profile.'/

      data opt_text(18),opt_head(18),(opt_help(j,18),j=1,6) /
     + 'bk_ground', 'Input the level for the background ',
     + 'Use the input parameter BACK to set the level of the',
     + 'background.',
     + ' ',
     + 'A ring profile is a fractional one, as a dust ring intercepts',
     + 'a fraction of the incident light. Thus a local level has to ',
     + 'be determined. '/

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'bk_level', 'Measure local level - perhaps load into default',
     + 'Take an area at the present position (input by keyboard or ',
     + 'cursor) and of size given by BK_SIZE button, calculate mean',
     + 'level and optionally set default background level at this. ',
     + 'A ring profile is a fractional one, as a dust ring intercepts',
     + 'a fraction of the incident light. Thus a local level has to ',
     + 'be determined. '/

      data opt_text(20),opt_head(20),(opt_help(j,20),j=1,6) /
     + 'bk_load', 'Toggle between loading background measure and not',
     + 'The background level can be measured with the BK_LEVEL button',
     + 'and if this switch is set, then this measure is loaded as the',
     + 'default background level. It starts off as being set. ',
     + 'A ring profile is a fractional one, as a dust ring intercepts',
     + 'a fraction of the incident light. Thus a local level has to ',
     + 'be determined. '/

      data opt_text(21),opt_head(21),(opt_help(j,21),j=1,6) /
     + 'position', 'Set position via keyboard',
     + ' ',
     + 'The position of a ring is set up via POSN parameter. This is',
     + 'used to apply the ring. It is also used for the measurement',
     + 'of the local background level with the BK_LEVEL button.',
     + ' ', ' '/

      data opt_text(22),opt_head(22),(opt_help(j,22),j=1,6) /
     + 'profile', 'Set the parameters of the ring profile ',
     + 'The ring profile has a number of factors. First the PERCENT ',
     + 'factor which gives the percentage (not fraction) of the depth',
     + 'of the ring relative to the background. Then the inner and',
     + 'outer radii via RIN and ROUT. Then the length of the umbra of',
     + 'the inner and outer edges, the distances over which the shadow',
     + 'goes from full to zero, via SLOPEIN and SLOPEOUT.'/

      data opt_text(23),opt_head(23),(opt_help(j,23),j=1,6) /
     + 'show', 'Print out the ring paramaters and present controls ',
     + ' ',
     + 'Print out the ring paramaters and present controls ',
     + ' ', ' ', ' ', ' '/

      data opt_text(24),opt_head(24),(opt_help(j,24),j=1,6) /
     + 'tb_clear', 'Clear the table of set rings ',
     + ' ',
     + 'This clears the table of set rings.',
     + ' ',
     + 'The program has an internal table of ring positions and ',
     + 'parameters. These can be applied, changed, flagged for use,',
     + 'input and stored' /

      data opt_text(25),opt_head(25),(opt_help(j,25),j=1,6) /
     + 'tb_get', 'Extract a ring from the table, ready for use ',
     + 'Extract a ring from the table, ready for use. This loads ',
     + 'from the table a ring position, background and shape. This ',
     + 'can then be applied or changed',
     + 'The program has an internal table of ring positions and ',
     + 'parameters. These can be applied, changed, flagged for use,',
     + 'input and stored' /

      data opt_text(26),opt_head(26),(opt_help(j,26),j=1,6) /
     + 'tb_list', 'List the present contents of the set rings table',
     + ' ',
     + 'List the present contents of the set rings table',
     + ' ',
     + 'The program has an internal table of ring positions and ',
     + 'parameters. These can be applied, changed, flagged for use,',
     + 'input and stored' /

      data opt_text(27),opt_head(27),(opt_help(j,27),j=1,6) /
     + 'tb_load', 'Load a table of set rings from a file',
     + 'Load the internal table from a file. The file has contents',
     + 'which are the position, background, profile, and use flag.',
     + 'This table is in the style of the table output via TB_STORE',
     + 'The program has an internal table of ring positions and ',
     + 'parameters. These can be applied, changed, flagged for use,',
     + 'input and stored' /

      data opt_text(28),opt_head(28),(opt_help(j,28),j=1,6) /
     + 'tb_put', 'Put present ring parameters into table',
     + ' ',
     + 'Put present ring parameters into table',
     + ' ',
     + 'The program has an internal table of ring positions and ',
     + 'parameters. These can be applied, changed, flagged for use,',
     + 'input and stored' /

      data opt_text(29),opt_head(29),(opt_help(j,29),j=1,6) /
     + 'tb_store', 'Store present table of rings into a disk file ',
     + ' ',
     + 'Store present table of rings into a disk file ',
     + ' ',
     + 'The program has an internal table of ring positions and ',
     + 'parameters. These can be applied, changed, flagged for use,',
     + 'input and stored' /

      data opt_text(30),opt_head(30),(opt_help(j,30),j=1,6) /
     + 'tb_use', 'Set whether to use a particular ring in the table',
     + 'Set whether to use a particular ring in the table when using',
     + 'the TB_APPLY button',
     + ' ',
     + 'The program has an internal table of ring positions and ',
     + 'parameters. These can be applied, changed, flagged for use,',
     + 'input and stored' /

      data opt_text(31),opt_head(31),(opt_help(j,31),j=1,6) /
     + 'im_reload', 'Load original input image into present image ',
     + ' ',
     + 'The original input image is loaded into this program and ',
     + 'changed by adding or subtracting rings. This option loads ',
     + 'the original image back in ',
     + ' ', ' '/

      data opt_text(32),opt_head(32),(opt_help(j,32),j=1,6) /
     + 'im_store', 'Output present image into a disk file',
     + ' ',
     + 'The original input image is loaded into this program and ',
     + 'changed by adding or subtracting rings. This option outputs',
     + 'the present image into an output file.',
     + ' ', ' '/

      character*50 title, option
      integer ncode
      data title, option, ncode / 'Dustring', 'OPTION', 1 /

      integer def_x, def_y
      parameter ( def_x=5 )
      parameter ( def_y=2 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'im_get', 'flash',   'panel', 'cursor',
     +                ' ', 'im_get', 'display', 'panel', 'cursor' /

      integer sect_num
      parameter ( sect_num=7 )
      character*10 sect_head(sect_num)
      data sect_head / '  APPLY ', '   RING', 'BACKGROUND', 'TABLE',
     +                 'IMAGE', 'DISPLAY',  'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1)/ 'cursor:add:subtract:tb_apply' /
      data sect_text(2)/ 'position:profile:show' /
      data sect_text(3)/ 'bk_auto:bk_size:
     +                    bk_ground:bk_level:bk_load' /
      data sect_text(4)/ 'tb_clear:tb_get:tb_list:tb_load:tb_put:
     +                    tb_store:tb_use' /
      data sect_text(5)/ 'im_get:im_reload:im_store' /
      data sect_text(6)/ 'clear:close:cvalues:display:flash:
     +                    open:reset:zoom' /
      data sect_text(7)/ 'panel:exit' /

      integer help_num
      parameter ( help_num=12 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + '             ',
     + '             ',
     + ' Buttons for Cursor add/subtract work:  ',
     + '                         Button 1 = add ' ,
     + '                         Button 2 = subtract ' ,
     + '                         Button 3 = exit ' ,
     + ' Buttons for Zoom work:  Button 1 twice        = zoom /2' ,
     + '                         Button 2 twice        = zoom x2' ,
     + '                         Button 1 and Button 2 = pan' ,
     + '                         Button 3 twice        = exit' /
      data (help_text(k),k=11,help_num) /
     + ' Buttons for Cvalues work:                              ' ,
     + '            X windows - all buttons exit, '/

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


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_SDEF -- Set up defaults
C
C  alan penny                 RAL                           1990-10-15

      subroutine du_sdef ()

      implicit none
      include 'dustring.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'
      include 'ST_DS_GEN_INC'
C--
      integer ierr
Cbegin


      if ( ST_FAILED ) return

      NRINGS   = 0

      XP       = 50.0
      YP       = 50.0
      BACK     = 1.0
      BACKLOAD = .true.
      BACKAUTO = .true.
      SLOPEIN  = 2.0
      SLOPEOUT = 2.0
      RIN      = 8.0
      ROUT     = 12.0
      PERCENT  = 0.5

      ISIDE    = 5
      TSCALE   = 1000.0
      TZERO    = -1000.0

      NX = 1
      NY = 1
      BS = 1.0
      BZ = 0.0
      INVAL = INT_INVALSI
      RINVAL = INT_INVALR
      IMTITLE = ' '

      call gtwrkr ( 'WORK', 1, IPW, ierr )
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      DISPLAYED = .false.
      GOTIMAGE = .false.
      OPDISP = .false.

      call ds_sdef ( 4, 11 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_ADDCZ -- Cursor Add/subtract a dustring from image
C
C  alan penny                     RAL             1990-10-15

      subroutine du_addcz ( im )

      implicit none
      include 'dustring.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real      im(NX,NY)	!i/o: Working image
C--
      logical more
      integer kx, ky, kb, ierr, kn, istat
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIMAGE ) then
         call printo ( 'No image got' )
         return
      endif

      kn = 0
      more = .true.
      do while ( more )

         call ds_gcur ( .true., kx, ky, kb, ierr )

         if ( kb.eq.1 .or. kb.eq.2 ) then
            XP = kx
            YP = ky
            call du_addz ( im, kb, istat )
            if ( istat.eq.0 ) then
               if ( kb.eq.1 ) then
                  kn = kn + 1
                  call pargi ( kn )
                  call printd (
     +                   'Added defined ring: running total = %d' )
               else
                  kn = kn - 1
                  call pargi ( kn )
                  call printd (
     +                   'Subtracted defined ring: running total = %d' )
               endif
            endif
         else
            more = .false.
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_ADDZ -- Add/subtract a dustring from image
C
C  alan penny                     RAL             1990-10-15

      subroutine du_addz ( im, kopt, istat )

      implicit none
      include 'dustring.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real      im(NX,NY)	!i/o: Working image
      integer	kopt		!i:   Flag for: 1=Add;2=subtract
      integer	istat 		!o:   Flag erro: 0=ok:1=not done
C--
      integer lx, ly, lxs, lxe, lys, lye, jx, jy,
     +        ix, iy, kxr(2), kyr(2), ierr
      real    corr, dx, dy, d, dc, am, std, rva, rvb
Cbegin


      if ( ST_FAILED ) return

      istat = 0

      if ( .not.GOTIMAGE ) then
         call printo ( 'No image got' )
         istat = 1
         return
      endif

      if ( BACKAUTO ) then
         kxr(1) = XP - ISIDE/2
         kxr(2) = kxr(1) + ISIDE - 1
         kyr(1) = YP - ISIDE/2
         kyr(2) = kyr(1) + ISIDE - 1
         if ( kxr(1).gt.NX .or. kxr(2).lt.1 .or. kyr(1).gt.NY
     +        .or. kyr(2).lt.1 ) then
            call printo ( '  Cannot measure background here' )
            call printo ( '  - You might try using known background' )
            istat = 1
            return
         endif
         call ranger ( im, NX, NY, kxr, kyr, RINVAL, am, std, ierr )
         if ( ierr.ne.0 ) then
            call printo ( '  Cannot measure background here' )
            call printo ( '  - You might try using known background' )
            istat = 1
            return
         endif
         BACK = BS*am + BZ
      endif

      rva = real(int(XP*100.0))/100.0
      rvb = real(int(YP*100.0))/100.0

      call pargr ( rva )
      call pargr ( rvb )
      call pargr ( BACK )
      call pargi ( ISIDE )
      call pargi ( ISIDE )
      call printd (
     + 'Posn %f %f : Background %f in %d x %d box at centre of ring' )

      lx = 2*(ROUT+SLOPEOUT+2.0)
      ly = lx
      lxs = XP - lx/2
      lxe = lxs + lx
      lys = YP - ly/2
      lye = lys + ly

      if ( lxs.gt.NX .or. lxe.lt.1 .or. lys.gt.NY
     +     .or. lye.lt.1 ) then
          call printo ( '   Cannot apply ring here' )
          istat = 1
          return
      endif

      lxs = min(NX,max(1,lxs))
      lxe = min(NX,max(1,lxe))
      lys = min(NY,max(1,lys))
      lye = min(NY,max(1,lye))

      corr = BACK*(PERCENT/100.0)
      do jy = lys, lye
         dy = jy - YP
         do jx = lxs, lxe
            if ( im(jx,jy).ne.RINVAL ) then
               dx = jx - XP
               d = sqrt(0.0001+dx*dx+dy*dy)
               dc = 0.0
               if ( d.gt.(RIN-SLOPEIN) .and. d.lt.(ROUT+SLOPEOUT) ) then
                  if ( d.ge.RIN .and. d.le.ROUT ) then
                     dc = corr
                  else if ( d.lt.RIN ) then
                     dc = corr*(1.0-abs((d-RIN)/SLOPEIN))
                  else
                     dc = corr*(1.0-abs((d-ROUT)/SLOPEOUT))
                  endif
               endif
               if ( kopt.eq.2 ) then
                  im(jx,jy) = im(jx,jy) + dc/BS
               else
                  im(jx,jy) = im(jx,jy) - dc/BS
               endif
            endif
         enddo
      enddo

      if ( DISPLAYED ) then
         call ds_tiv ( lxs, lys, ix, iy )
         call ds_acim ( im, NX, NY, 'REAL', lxs, lxe, lys, lye, ix, iy,
     +                  .false. )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_TBSTORE -- Store noted rings table in a file
C
C a j penny             stsci                  1987-03-22

      subroutine du_tbstore ( )

      implicit none
      include 'dustring.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer tbvx, ierr, ipo, j
      character title*72
      character*20 texth(TBX)
      data texth / 'X', 'Y', 'BACK', 'PERCENT', 'RIN',
     +             'ROUT', 'SLOPEIN', 'SLOPEOUT', 'USE' /
Cbegin


      if ( ST_FAILED ) return

      if ( NRINGS.lt.1 ) then
         call printo ( 'ERROR: No noted rings - so no output' )
         return
      endif

      tbvx = TBX + 5
      call optabw ( 'OUTTAB', ipo, tbvx, NRINGS, .true., ierr )
      if ( ST_FAILED ) return
      if ( ierr.eq.2 ) return

      title = IMTITLE
      if ( title.eq.' ' ) title = 'Rings from DUSTRING'
      call get1c ( 'TITLE', title, title, .true. )
      call ptdesc ( 'OUTTAB', 'TITLE', title )
      if ( ST_FAILED ) return
      do j = 1, TBX
         call pthead ( 'OUTTAB', j, texth(j), ierr )
      enddo

      call coprr ( RES, TBX, MAXRINGS, 1, TBX, 1, NRINGS,
     +             %val(ipo), tbvx, NRINGS, 6, 1 )

      call ident ( %val(ipo), tbvx, NRINGS )

      call canpar ( 'OUTTAB' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_TBLOAD -- Load noted rings table from a file
C
C a j penny             stsci                  1987-03-22

      subroutine du_tbload ( )

      implicit none
      include 'dustring.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer tbvx, ierr, iptb
Cbegin


      if ( ST_FAILED ) return

      call optabr ( 'INTAB', iptb, tbvx, NRINGS, .true., ierr )
      if ( ierr.ne.0 ) return

      if ( tbvx.ne.(5+TBX) ) then
         call printo ( ' ' )
         call printo ( '  ERROR: This table must have nine columns' )
         call printo ( '         Like a table output from DUSTRING' )
         call printo ( ' ' )
         return
      endif
      if ( NRINGS.gt.MAXRINGS ) then
         call printo ( ' ' )
         call printo ( '  ERROR: This table has too many rows' )
         call pargi ( MAXRINGS )
         call printd ( '         Can only have up to %d ' )
         call printo ( ' ' )
         return
      endif

      call coprr ( %val(iptb), tbvx, NRINGS, 6, tbvx, 1, NRINGS,
     +             RES, TBX, MAXRINGS, 1, 1 )

      call canpar ( 'INTAB' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_TBLIST -- List out the noted rings
C
C a j penny             stsci                  1987-03-22

      subroutine du_tblist ( )

      implicit none
      include 'dustring.inc'
      include 'STARMAN_INC'
C--
      integer k
      character text*78, tuse*1, tb*11
      real  x, y, b, p, ri, ro, si, so
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      if ( NRINGS.lt.1 ) then
         call printo ( 'ERROR: No noted rings - so no output' )
         return
      endif

      call printo ( ' ' )
      call printo ( 'Num      X       Y      Back    Percent  '//
     +              '  Rin  Rout  Slopein  Slopeout  Use' )

      do k = 1, NRINGS
         x  = RES(1,k)
         y  = RES(2,k)
         b  = RES(3,k)
         p  = RES(4,k)
         ri = RES(5,k)
         ro = RES(6,k)
         si = RES(7,k)
         so = RES(8,k)
         tuse = 'Y'
         if ( RES(9,k).gt.0.5 ) tuse = 'N'
         x = trunc(x,4)
         y = trunc(y,4)
         call rvtext ( b, tb )
         p = trunc(p,3)
         ri = trunc(ri,3)
         ro = trunc(ro,3)
         si = trunc(si,3)
         so = trunc(so,3)
         write ( text, '(1x,i4,2f8.2,1x,a11,f8.3,4f7.2,6x,a1)' )
     +           k, x, y, tb, p, ri, ro, si, so, tuse
         call printo ( text )
      enddo
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_GPROF -- Get profile
C
C alan penny                   RAL                    1990-10-15

      subroutine du_gprof ( )

      implicit none
      include 'dustring.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call get1r (  'PERCENT',  PERCENT,  PERCENT, -1.0e8, 1.0e8 )
      call get1r (      'RIN',      RIN,      RIN,    0.0, 1.0e8 )
      call get1r (     'ROUT',     ROUT,     ROUT,    RIN, 1.0e8 )
      call get1r (  'SLOPEIN',  SLOPEIN,  SLOPEIN,    0.0, 1.0e8 )
      call get1r ( 'SLOPEOUT', SLOPEOUT, SLOPEOUT,    0.0, 1.0e8 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_LEVEL -- Get local level
C
C alan penny                  RAL                        1990-1015

      subroutine du_level ( im )

      implicit none
      include 'dustring.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real     im(NX,NY)		!i: Work image
C--
      integer kxr(2), kyr(2), ierr
      real    am, std
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIMAGE ) then
         call printo ( 'No image got' )
         return
      endif

      kxr(1) = XP - ISIDE/2
      kxr(2) = kxr(1) + ISIDE - 1
      kyr(1) = YP - ISIDE/2
      kyr(2) = kyr(1) + ISIDE - 1

      call ranger ( im, NX, NY, kxr, kyr, RINVAL, am, std, ierr )
      am = BS*am + BZ
      std = BS*std

      call printo ( ' ' )
      call pargr ( XP )
      call pargr ( YP )
      call printd ( 'Position : %f %f ' )
      call pargi ( ISIDE )
      call pargi ( ISIDE )
      call pargr ( am )
      call pargr ( std )
      call printd ( 'Box size %dx%d : Level = %f  : Std Dev = %f')
      if ( BACKLOAD ) then
         BACK = am
         call printo ( 'Background loaded with this level' )
      else
         call printo ( 'Background not loaded with this level' )
      endif
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_TBPUT -- Note defined ring
C
C   alan penny                    RAL                 1990-10-15

      subroutine du_tbput ()

      implicit none
      include 'dustring.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( NRINGS.eq.MAXRINGS ) then
         call pargi ( MAXRINGS )
         call printd (
     +        'Already stored the maximum number of rings - %d' )
         call printo (
     +     'You will have to store and clear these before adding more' )
         return
      endif

      NRINGS = NRINGS + 1

      RES(1,NRINGS) = XP
      RES(2,NRINGS) = YP
      RES(3,NRINGS) = BACK
      RES(4,NRINGS) = PERCENT
      RES(5,NRINGS) = RIN
      RES(6,NRINGS) = ROUT
      RES(7,NRINGS) = SLOPEIN
      RES(8,NRINGS) = SLOPEOUT
      RES(9,NRINGS) = 0.0

      call pargi ( NRINGS )
      call printd ( 'Defined Ring stored in table in row: %d ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_TBAPPLY -- Apply all rings from table
C
C   alan penny                    RAL                 1990-10-15

      subroutine du_tbapply ()

      implicit none
      include 'dustring.inc'
      include 'STARMAN_INC'
C--
      integer k, istat
      character ktopt*3
Cbegin


      if ( ST_FAILED ) return

      if ( NRINGS.eq.0 ) then
         call printo ( 'No rings in table yet' )
         return
      endif

      call get_option ( 'DO_ADD', 'add:sub', 1, ktopt, 'add', ' ', 0 )
      if ( ST_FAILED ) return

      if ( BACKAUTO ) then
         call printo ( 'Local backgrounds used' )
      else
         call printo ( 'Backgrounds in table used' )
      endif

      do k = 1, NRINGS
         if ( RES(9,k).lt.0.5 ) then

            XP       = RES(1,k)
            YP       = RES(2,k)
            BACK     = RES(3,k)
            PERCENT  = RES(4,k)
            RIN      = RES(5,k)
            ROUT     = RES(6,k)
            SLOPEIN  = RES(7,k)
            SLOPEOUT = RES(8,k)
            if ( ktopt.eq.'add' ) then
               call du_addz ( %val(IPW), 1, istat )
            else
               call du_addz ( %val(IPW), 2, istat )
            endif

            if ( istat.eq.0 ) then
               call pargi ( k )
               call printd ( 'Ring %d applied' )
            endif

         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_TBGET -- Get defined ring from table
C
C   alan penny                    RAL                 1990-10-15

      subroutine du_tbget ()

      implicit none
      include 'dustring.inc'
      include 'STARMAN_INC'
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      if ( NRINGS.eq.0 ) then
         call printo ( 'No rings in table yet' )
         return
      endif

      call get1i ( 'NUMBER', k, 1, 0, NRINGS )

      if ( k.lt.1 .or. k.gt.NRINGS ) then
         call printo ( 'Ring parameters not got from table' )
         return
      endif

      XP       = RES(1,k)
      YP       = RES(2,k)
      BACK     = RES(3,k)
      PERCENT  = RES(4,k)
      RIN      = RES(5,k)
      ROUT     = RES(6,k)
      SLOPEIN  = RES(7,k)
      SLOPEOUT = RES(8,k)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_TBUSE -- Set use of ring in table
C
C   alan penny                    RAL                 1990-10-15

      subroutine du_tbuse ()

      implicit none
      include 'dustring.inc'
      include 'STARMAN_INC'
C--
      integer k
      logical inuse, use
Cbegin


      if ( ST_FAILED ) return

      if ( NRINGS.eq.0 ) then
         call printo ( 'No rings in table yet' )
         return
      endif

      call get1i ( 'NUMBER', k, 1, 1, NRINGS )

      inuse = .true.
      if ( RES(9,k).gt.0.5 ) inuse = .false.
      call get1b ( 'USE', use, inuse )

      RES(9,k) = 0.0
      if ( .not.use ) RES(9,k) = 1.0


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_SHOW -- Type out defined parameters of ring
C
C   alan penny                    RAL                 1990-10-15

      subroutine du_show ()

      implicit none
      include 'dustring.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      call pargr ( RIN )
      call pargr ( ROUT )
      call pargr ( SLOPEIN )
      call pargr ( SLOPEOUT )
      call pargr ( PERCENT )
      call pargr ( BACK )
      call pargr ( XP )
      call pargr ( YP )
      call printo ( 'Defined Ring profile is:-' )
      call printd ( 'Inner Radius   = %f   : Outer Radius = %f' )
      call printd ( 'Inner Slope    = %f   : Outer Slope  = %f' )
      call printd ( 'Percentage Dip = %f   : Background   = %f' )
      call printo ( 'Defined Ring position is:-' )
      call printd ( 'X Posn         = %f   : Y Posn        = %f' )
      if ( BACKAUTO ) then
         call printo ( 'Local backgrounds used' )
      else
         call printo ( 'Backgrounds setup used' )
      endif
      if ( BACKLOAD ) then
         call printo ( 'Determined level loaded into background' )
      else
         call printo ( 'Determined level not loaded into background' )
      endif

      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_IMSTORE -- Store 'cleaned' image
C
C alan penny                      RAL                      1990-10-15

      subroutine du_imstore ( )

      implicit none
      include 'dustring.inc'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'
      include 'STARMAN_INC'
C--
      character*30 title
      integer ipo, ierr
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIMAGE ) then
         call printo ( 'No image got' )
         return
      endif

      call opimzw ( 'OUT', IMTYPE, ipo, NX, NY, .true., ierr )
      if ( ierr.ne.0 ) return

      call icopdes ( 'IN', 'OUT', ierr )
      title = IMTITLE
      if ( title.eq.' ' ) title = 'Output from DUSTRING'
      call get1c ( 'TITLE', title, title, .true. )
      call ptdesc ( 'OUT', 'TITLE', title )

      if ( IMTYPE.eq.'SHORT' ) then
         call azchtrs ( %val(IPW), RINVAL, INT_MINSR, INT_MAXSR,
     +                  %val(ipo), INVAL, NX*NY )
      else
         call amovr ( %val(IPW), %val(ipo), NX*NY )
      endif

      call canpar ( 'OUT' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_NEWIM -- Get new image
C
C    a j penny                    ral         1990 jan

      subroutine du_newim ( ierr )

      implicit none
      include 'dustring.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      integer ierr		!o: Error flag
C--
      integer ierra
Cbegin


      if ( ST_FAILED ) return

      if ( GOTIMAGE ) call canpar ( 'IN' )
      call wrkcan ( 'WORK' )

      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .true., ierr )		!Get image
      if ( ierr.ne.0 ) then
         call gtwrkr ( 'WORK', 1, IPW, ierra )
         if ( ierra.ne.0 ) ST_FAILED = .true.
         NX = 1
         NY = 1
         GOTIMAGE = .false.
         return
      endif
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE,ierra)	!Image scalers

      call gtwrkr ( 'WORK', NX*NY, IPW, ierra )
      call du_imload							!Load to work space

      DSNXS = 1								!Display area
      DSNXE = NX
      DSNYS = 1
      DSNYE = NY

      if ( .not.GOTIMAGE ) then						!Area of interest
         NXS =  1
         NXE = NX
         NYS = 1
         NYE = NY
      else
         NXS = min(NXS,NX)
         NXE = min(NXE,NX)
         NYS = min(NYS,NY)
         NYE = min(NYE,NY)
      endif

      if ( GOTIMAGE ) call ds_gtcomf ( 1 )				!Get image display size compression
      GOTIMAGE = .true.
      DSKVRANGE = 0

      DISPLAYED = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_IMLOAD -- Load image to work space
C
C    a j penny                    ral         1990 jan

      subroutine du_imload ( )

      implicit none
      include 'dustring.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( IMTYPE.eq.'SHORT' ) then					!Move to work space
         call azchtsr ( %val(IPIM), INVAL, %val(IPW), RINVAL, NX*NY )
      else
         call amovr ( %val(IPIM), %val(IPW), NX*NY )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DU_OPDISP -- Open display
C
C alan penny                    ral                  1990-06-16

      subroutine du_opdisp ( ierr )

      implicit none
      include 'dustring.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer	ierr		!o: Error flag (0=ok;1=bad)
C--
Cbegin


      if ( ST_FAILED ) return

      if ( OPDISP ) return

      call ds_gtype ( ierr )						!Get type of display
      if ( ierr.ne.0 ) return

      call ds_init ( IMTITLE, 0, ierr )					!Open display
      if ( ierr.ne.0 ) return
      OPDISP = .true.

      call ds_gtcomf ( 1 )							!Get image display size compression


      end

