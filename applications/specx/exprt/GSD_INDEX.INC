      INTEGER*4  GSD__SZINDEX
      PARAMETER (GSD__SZINDEX = 2)

      INTEGER*4 IND_BKE(GSD__SZINDEX)     ! Backend name
      INTEGER*4 IND_OBS(GSD__SZINDEX)     ! Observer name
      INTEGER*4 IND_TEL(GSD__SZINDEX)     ! Telescope name
      INTEGER*4 IND_SNAME1(GSD__SZINDEX)  ! Source name (first 16 chars)
      INTEGER*4 IND_SNAME2(GSD__SZINDEX)  ! Source name (second 16 chars)
      INTEGER*4 IND_SCAN(GSD__SZINDEX)    ! Scan number
      INTEGER*4 IND_DAT(GSD__SZINDEX)     ! Date of observation
      INTEGER*4 IND_UT(GSD__SZINDEX)      ! Universal time
      INTEGER*4 IND_VRAD(GSD__SZINDEX)    ! Source radial velocity
      INTEGER*4 IND_VRADTEL(GSD__SZINDEX) ! Telescope radial velocities
      INTEGER*4 IND_VREF(GSD__SZINDEX)    ! Source radial velocity reference
      INTEGER*4 IND_VDEF(GSD__SZINDEX)    ! Source radial velocity trans'n law
      INTEGER*4 IND_INTIME(GSD__SZINDEX)  ! Integration time
      INTEGER*4 IND_NRC(GSD__SZINDEX)     ! Number of backend receiver channels
      INTEGER*4 IND_LSPC(GSD__SZINDEX)    ! No of points per receiver spectrum
      INTEGER*4 IND_RA(GSD__SZINDEX)      ! Right Ascension (degrees)
      INTEGER*4 IND_DEC(GSD__SZINDEX)     ! Declination (degrees)
      INTEGER*4 IND_AZ(GSD__SZINDEX)      ! Azimuth (degrees)
      INTEGER*4 IND_ELEV(GSD__SZINDEX)    ! Elevation (degrees)
      INTEGER*4 IND_NRF(GSD__SZINDEX)     ! Number of frontend receiver channels
      INTEGER*4 IND_DATA(GSD__SZINDEX)    ! Data array
      INTEGER*4 IND_TSYS(GSD__SZINDEX)    ! System temperatures
      INTEGER*4 IND_TREC(GSD__SZINDEX)    ! Receiver temperatures
      INTEGER*4 IND_TSKY(GSD__SZINDEX)    ! Zenith sky temperatures
      INTEGER*4 IND_TTEL(GSD__SZINDEX)    ! Telescope temperatures
      INTEGER*4 IND_RF(GSD__SZINDEX)      ! Rest frequency (GHz)
      INTEGER*4 IND_FR(GSD__SZINDEX)      ! Frequency resolution (MHz)
      INTEGER*4 IND_CF(GSD__SZINDEX)      ! Centre frequency (GHz)
      INTEGER*4 IND_LO(GSD__SZINDEX)      ! LO frequency (GHz)
      INTEGER*4 IND_IF(GSD__SZINDEX)      ! IF frequency (GHz)
      INTEGER*4 IND_SD(GSD__SZINDEX)      ! Scan_direction
      INTEGER*4 IND_ST(GSD__SZINDEX)      ! Scan type
      INTEGER*4 IND_FLY(GSD__SZINDEX)     ! on-the-fly?
      INTEGER*4 IND_XNP(GSD__SZINDEX)     ! # of x-points &
      INTEGER*4 IND_YNP(GSD__SZINDEX)     ! # of y-points in map
      INTEGER*4 IND_XGC(GSD__SZINDEX)     ! X Start point in RUNS
      INTEGER*4 IND_YGC(GSD__SZINDEX)     ! Y start point in RUNS
      INTEGER*4 IND_XPOS(GSD__SZINDEX)    ! X increments in a positive sense
      INTEGER*4 IND_YPOS(GSD__SZINDEX)    ! Y increments in a positive sense
      INTEGER*4 IND_NP(GSD__SZINDEX)      ! # of spectra filed
      INTEGER*4 IND_PPC(GSD__SZINDEX)     ! # phases per cycle
      INTEGER*4 IND_NCI(GSD__SZINDEX)     ! # cycles per integration
      INTEGER*4 IND_LSC(GSD__SZINDEX)     ! Local system coord code (C*2)
      INTEGER*4 IND_XMAP_OFF(GSD__SZINDEX)! Map centre offset (x) (RUNS)
      INTEGER*4 IND_YMAP_OFF(GSD__SZINDEX)! Map centre offset (y) (RUNS)
      INTEGER*4 IND_DX(GSD__SZINDEX)      ! Map cell size in x (arcsec)
      INTEGER*4 IND_DY(GSD__SZINDEX)      ! Map cell size in y (arcsec)
      INTEGER*4 IND_MSA(GSD__SZINDEX)     ! Map scanning angle (degrees)
      INTEGER*4 IND_V2Y(GSD__SZINDEX)     ! P.A. of map y-axis from N
      INTEGER*4 IND_X2Y(GSD__SZINDEX)     ! P.A. of map x_axis from y-axis
      INTEGER*4 IND_PHIST(GSD__SZINDEX)   ! Pointing history table
