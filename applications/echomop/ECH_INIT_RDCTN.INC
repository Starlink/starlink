*  Include file for data structure definition.
      INTEGER MAX_SKY_LINES ! Maximum number of sky lines per echellogram.
      PARAMETER ( MAX_SKY_LINES = 100 )

      INTEGER MAX_POLY      ! Maximum order when doing polynomial fitting.
      PARAMETER ( MAX_POLY = 10 )

      INTEGER MX2D_POLY     ! Maximum order used when doing 2-D fitting.
      PARAMETER ( MX2D_POLY = MAX_POLY * MAX_POLY )

      INTEGER HIST_BINS     ! No of bins in histograms of raw objects
      PARAMETER ( HIST_BINS = 1000 )

      INTEGER CHAR_DEFAULT  ! Size of character fields. Default is 80.
      PARAMETER ( CHAR_DEFAULT = 80 )

      INTEGER MX2D_PNTS     ! Maximum number of points used to do the
                            ! 2-D chebyshev distortion fitting.
      PARAMETER ( MX2D_PNTS = 2000 )

      INTEGER PFL_SAMPLING  ! Max number of elements used to sample profiles
                            ! in the spatial direction.
      PARAMETER ( PFL_SAMPLING = 200 )

      INTEGER NX            ! X dimension of input images. REQUIRED.
      INTEGER NY            ! Y dimension of input images. REQUIRED.
      INTEGER NUM_ORDERS    ! Number of echelle orders. REQUIRED
      INTEGER NUM_OBJECTS   ! Number of object spectra. Defaults to 1.

      LOGICAL DISTORTION    ! TRUE if a 2-D distortion fit is to be done.
                            ! Defaults to FALSE.
      LOGICAL DATA_QUALITY  ! TRUE if a data quality 'image' is to be produced.
                            ! Defaults to FALSE.
      LOGICAL SKY_INFO      ! TRUE is data on the 'sky' is to be included.
                            ! Default is TRUE.
      LOGICAL COSMIC_INFO   ! TRUE if data on the cosmic rays to be included.
                            ! Default is TRUE.

      COMMON / ECH_INIT_RDCTN / NX, NY, NUM_ORDERS, NUM_OBJECTS,
     :       DISTORTION, DATA_QUALITY, SKY_INFO, COSMIC_INFO
