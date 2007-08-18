CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHOICE.INC
C
C  Attributes for 'choice' s/rs
C
C   alan penny                  RAL                 1993 Oct

      integer   KLEF_X				! No of times defaults asked for
      integer   KLEF_Y				! Last Y Posn in defaults array
      logical   LOOPT_HELP			!
      logical   ISBHELP				! Was last button press the button-help?
      logical   WASBHELP 			! Last button was help on
      logical   CH_AUTOSTART			! Use a starting standard set of options automatically?

      character*70 KTDEF_HELP			!
      character*12 TBHELP			! Button help asked for

      integer         CH_SECT_NUM		! No of sections
      integer         CH_MAX_SECT_NUM
      parameter       ( CH_MAX_SECT_NUM = 15 )
      character*200   CH_SECT_TEXT(CH_MAX_SECT_NUM)	! Options in sections
      character*10    CH_SECT_HEAD(CH_MAX_SECT_NUM)	! Section headers

      integer         CH_NCODE         		! Number of Panel Option List
                                        	!  (usually=1; if diff from
                                        	!  previous entry, load new panel)
      character*50    CH_TITLE         		! Title of panel
      character*50    CH_OPTION			! Option parameter name

      integer         CH_OPT_NUM            	! No of options
      integer         CH_MAX_OPT_NUM
      parameter       ( CH_MAX_OPT_NUM = 100 )
      character*12    CH_OPT_TEXT(CH_MAX_OPT_NUM)
      character*68    CH_OPT_HEAD(CH_MAX_OPT_NUM)
      character*68    CH_OPT_HELP(6,CH_MAX_OPT_NUM)

      integer         CH_HELP_NUM	 	! No of lines of help text
      integer         CH_MAX_HELP_NUM
      parameter       ( CH_MAX_HELP_NUM = 200 )
      character*68    CH_HELP_TEXT(CH_MAX_HELP_NUM)

      integer         CH_DEF_X            	! No of default options
      integer         CH_DEF_Y            	! No of poss default paths
      integer         CH_MAX_DEF_X
      parameter       ( CH_MAX_DEF_X = 20 )
      integer         CH_MAX_DEF_Y
      parameter       ( CH_MAX_DEF_Y = 5 )
      character*12    CH_DEF_TEXT(CH_MAX_DEF_X,CH_MAX_DEF_Y)

C      logical         dum_cho_1, dum_cho_2, dum_cho_3		! to make 4*n logicals


      common / xxchoice  / KLEF_X, KLEF_Y, CH_DEF_X, CH_DEF_Y, 
     +                     CH_SECT_NUM, CH_NCODE, CH_OPT_NUM, 
     +                     CH_HELP_NUM

      common / xxchoiceb / LOOPT_HELP, ISBHELP, WASBHELP, CH_AUTOSTART

      common / xxchoicec / KTDEF_HELP, TBHELP, CH_TITLE, CH_OPTION,
     +                     CH_SECT_TEXT, CH_SECT_HEAD, 
     +                     CH_DEF_TEXT, 
     +                     CH_OPT_TEXT, CH_OPT_HEAD, CH_OPT_HELP, 
     +                     CH_HELP_TEXT
