C--------------------------------------------------------------------------
C
C                   S X G P G P L O T . I N C
C
C--------------------------------------------------------------------------
C
C   Include file for SXGPGPLOT.FOR
C   Contains definitions of common blocks.
*
*   24-JUL-1998 (BLY):
*      Modified colour table arrays from 0:255 to 0:256 to cater
*      for GKS/PGPLOT colour range.
C

C  Device selected

      INTEGER   IDEV             !   Device for plotter output
      LOGICAL   DEVINIT          !  .TRUE. if device initialized
      LOGICAL   ITERM            !  .TRUE. if device is a terminal
      LOGICAL   DUAL             !  .TRUE. if device is dual-screen
      LOGICAL   COLOUR           !  .TRUE. if device can do colour
      LOGICAL   HARDCOPY         !  .TRUE. if device can do hardcopy
      CHARACTER OUTFILE*80       !   PGPLOT hardcopy output file

      COMMON /SMDEV/ IDEV, DEVINIT, ITERM, DUAL, COLOUR,
     &                     HARDCOPY, OUTFILE

C  Characteristics of plot device

      REAL     XSIZED,YSIZED     !  screen size of selected dev.
      LOGICAL  GREYOK            !  This graphics package draws greyscales
      LOGICAL  COLOROK           !  This graphics package has colour drivers

      COMMON /SMDSPEC/ XSIZED, YSIZED, GREYOK, COLOROK

C  Current virtual co-ordinates and windows

      REAL     ULL,UHH,VLL,VHH  !  Current world coordinates of window
      REAL     XLL,XHH,YLL,YHH  !  Current 'mm' virtual window
      INTEGER  IXL,IXH,IYL,IYH  !  Corresponding absolute (dev coord) window

      COMMON /SMWINDOW/ ULL,UHH,VLL,VHH,XLL,XHH,YLL,YHH,IXL,IXH,IYL,IYH

C  Colour tables

      REAL     RED(0:256)       !  Red component of colour table, real in (0,1)
      REAL     GREEN(0:256)     !  Gn  component of colour table, real in (0,1)
      REAL     BLUE(0:256)      !  Bl  component of colour table, real in (0,1)

      COMMON /SMCOLOURS/ RED, GREEN, BLUE


C  Fonts and other information

      INTEGER  FONT             !  Current default FONT
      REAL     EXPAND           !  Current expansion
      REAL     XTICK,  YTICK    !  X and Y axis tick intervals
      REAL     XSTICK, YSTICK   !  X and Y axis subtick intervals
      LOGICAL  XSEX,   YSEX     !  True for sexagisimal labelling
      INTEGER  NDECX,  NDECY    !  Number of decimal places for X & Y sex labels

      COMMON /SMMISC/   FONT, EXPAND, XTICK, YTICK, XSTICK, YSTICK,
     &                  XSEX, YSEX,   NDECX, NDECY

      SAVE   /SMDSPEC/
      SAVE   /SMWINDOW/
      SAVE   /SMCOLOURS/
      SAVE   /SMMISC/

C--------------------------------------------------------------------------
