*----------------------------------------------------
*
*     GSD_VAR.INC
*    Include file to define GSD Scan file parameters
*
*----------------------------------------------------

      REAL*8    DDATE              ! Date of observation
      REAL*8    DUT                ! Universal time of observation
      REAL*8    DECDG              ! Declination (degrees)
      REAL*8    RADG               ! R.A. (degrees)
      REAL*8    DX                 ! Map cell size in X
      REAL*8    DY                 ! Map cell size in Y
      REAL*8    SCAN_ANGLE         ! Scan angle (degrees)
      REAL*8    AZIMUTH            ! Azimuth (degrees)
      REAL*8    ELEVATION          ! Elevation (degrees)
      REAL*8    INT_TIME
      REAL*8    V2Y                ! P.A. of map Y-axis from N
      REAL*8    X2Y                ! P.A. of map x-axis from y-axis
      REAL*8    XMAP_OFF           ! x-offset of map centre in RUNS
      REAL*8    YMAP_OFF           ! y-offset of map centre in RUNS
      REAL*8    VRAD               ! LSR velocity (km/s)

      REAL*4    PHIST(2,129*129)   ! Pointing history table
      REAL*4    XGC                ! X grid-coordinate of start of map
      REAL*4    YGC                ! Y grid-coordinate of start of map

      INTEGER*4 IXNP
      INTEGER*4 IYNP
      INTEGER*4 IFLY      ! on the fly?
      INTEGER*4 LSPC
      INTEGER*4 NCI       ! # cycles per integration
      INTEGER*4 NP        ! # phases stored in file
      INTEGER*4 NRC       ! # backend receiver channels (quadrants)
      INTEGER*4 NRF       ! # frontend receiver channels
      INTEGER*4 NGSDSPEC  ! Actual # spectra in file
      INTEGER*4 PH_DIMVALS(2) ! Dimensions for Pointing history table
      INTEGER*4 PPC       ! # phases per cycle
      INTEGER*4 SCAN_NO   ! # Original GSD scan #

      LOGICAL   XPOS
      LOGICAL   YPOS

      CHARACTER VREF*16            ! Radial velocity reference (Telluric etc)
      CHARACTER VDEF*16            ! Radial velocity transformation law (RAD,OPT)
      CHARACTER SCAN_DIR*16
      CHARACTER SCAN_TYPE*16
      CHARACTER OBSERVER*16
      CHARACTER SOURCE_NAME*32
      CHARACTER LOCAL_COSYS*16
      CHARACTER TELESCOPE*16
      CHARACTER BACKEND*16

      COMMON /GSD_VAR/
     &                 DDATE,        DUT,
     &                 DECDG,        RADG,
     &                 DX,           DY,         SCAN_ANGLE,
     &                 AZIMUTH,      ELEVATION,  
     &                 INT_TIME,     V2Y,        X2Y, 
     &                 XMAP_OFF,     YMAP_OFF,   VRAD,
     &                 XPOS,         YPOS,
     &                 IXNP,         IYNP,       IFLY,
     &                 NRF,          NRC,        LSPC, 
     &                 PPC,          NCI,        NP,
     &                 NGSDSPEC,
     &                 PH_DIMVALS,   PHIST,
     &                 XGC,          YGC,
     &                 SCAN_DIR,     SCAN_TYPE,
     &                 SCAN_NO,      OBSERVER,   SOURCE_NAME,
     &                 LOCAL_COSYS,  TELESCOPE,
     &                 VREF,         VDEF,       BACKEND
      SAVE   /GSD_VAR/

*  Common block for derived parameters (those not stored directly
*  in GSD files)

      REAL*8    INT_TIME_LAST
      INTEGER*4 NSPEC_FULL
      INTEGER*4 NNSPEC
      INTEGER*4 NO_NEWSPEC

      COMMON /GSD_VAR2/ INT_TIME_LAST, NSPEC_FULL, NNSPEC, NO_NEWSPEC
      SAVE   /GSD_VAR2/
