C-----------------------------------------------------------------------
C     PLOT2D.INC
C     Include file defining limits of current cube in "real" units
C     RP - 22/12/87
C-----------------------------------------------------------------------
      REAL*8    MAP_RA               ! RA of map centre
      REAL*8    MAP_DEC              ! Dec of map centre

      INTEGER*4 IOFF(3)              ! Offsets to start of window in cube
      INTEGER*4 NAX(3)               ! Length of window

      REAL*4    PFAC(3)              ! Axis scaling factors
      REAL*4    PF1(3)               ! Residual channels at start of axis
      REAL*4    PF2(3)               ! Residual channels at end of axis
      REAL*4    CBEG(3)              ! Cube limits
      REAL*4    CEND(3)              ! Cube limits

      LOGICAL*4 CHANGE_PLOT          ! Use plot axes set interactively
      REAL*4    XLIM(2)              ! Current plot limits
      REAL*4    YLIM(2)              ! Current plot limits
      REAL*4    AX1REQ               ! Requested X-axis length, this plot
      REAL*4    AX2REQ               ! Requested Y-axis length, this plot
      REAL*4    AXLENX               ! Current X-axis length
      REAL*4    AXLENY               ! Current Y-axis length
      REAL*4    PLOTLIMS(4)          ! Current limits of useable plot area

      INTEGER*4 NAXX
      INTEGER*4 NAXY                 ! (Smoothed) map array dimensions
      INTEGER*4 NMAPS                ! Total # of map panels
      INTEGER*4 NXMAP
      INTEGER*4 NYMAP                ! # of map panels in X & Y
      INTEGER*4 LXPIX
      INTEGER*4 LYPIX                ! # of image points per pixel

      INTEGER*4 INDEX_PTR            ! Virtual address of INDEX array

      CHARACTER MAP_NAME*40          ! Title of map

      COMMON /PLOT2D/
     &       MAP_RA, MAP_DEC,
     &       IOFF,   NAX,   PFAC,   PF1,    PF2,
     &       CBEG,   CEND,  CHANGE_PLOT,    XLIM,   YLIM,   AXLENX,
     &       AXLENY, NAXX,  NAXY,   NXMAP,  NYMAP,  LXPIX,  LYPIX,
     &       INDEX_PTR,     NMAPS,  AX1REQ, AX2REQ, PLOTLIMS,
     &       MAP_NAME

      SAVE   /PLOT2D/
