C                     FLAGCOMM.INC
C                     ------------
C     4 November 1996 (Tim Jenness) - Expanded GSDNAME to 32 characters
C                                     and reduced CHFLGD to 100
C     19 September 2000 (Alan Chipperfield) - removed unused CLIPVAL
C
C     This is the include file for the /FLAGS/ common blocks.
C     Contains (128+128+128+128+32) I*4 words
C
      INTEGER*4 GSD_SCAN                      ! READ-GSD-DATA
      INTEGER*4 IPSEQ,ISEQ0                   ! Plotting
      INTEGER*4 ILOUT                         ! SET-LIST-FILE
      INTEGER*4 IBLRPT(4)                     ! REMOVE-LINEAR-BASELINE
      INTEGER*4 IXCMAP                        ! CHANNEL-MAPS
      INTEGER*4 NBIN,NSTART                   ! BIN-DATA
      INTEGER*4 NS1,NS2,NXSSK                 ! FIND-SKEW
      INTEGER*4 IXLOW,IXHIGH,NXSII            ! FIND-INT-INTENSITY
      INTEGER*4 IEDVER,IEDASK                 ! FORMAT-EDIT-COMMANDS
      INTEGER*4 NXSFM                         ! FIND-MOMENTS
      INTEGER*4 NXSSS                         ! SHIFT-SPECTRUM
      INTEGER*4 NC1,NC2                       ! FIND-CENTROID
      INTEGER*4 IC1,IC2                       ! SET-CHANNELS
      INTEGER*4 IDC1,IDC2                     ! DROP-CHANNELS
      INTEGER*4 ISHIFT                        ! TRUNCATE-SPECTRUM
      INTEGER*4 ISMTH                         ! SMOOTH
      INTEGER*4 IPUT,IGET                     ! STORE, RECALL
      INTEGER*4 IPEN,IMODEL                   ! For plotting
      INTEGER*4 MAXITS,IER,NPR,IBLP(16),ISBFLG! Baseline removal
      INTEGER*4 ITH,ITC                       ! Calibration temp flags
      INTEGER*4 MASK(8), NMASK, IREST,  NXS   ! For X-axis scaling
      INTEGER*4 ITICKX,ITICKY,NINTSX,NINTSY,IDEV  ! For drawing plot axes
      INTEGER*4 LINK(3),NCONT                 ! For contour mapping
      INTEGER*4 INTERP_X,INTERP_Y             ! For SMOOTH-MAP
      INTEGER*4 IFSSP(10), NFSSP              ! For FIND-SPECTRUM-STAT
      INTEGER*4 NCSET                         ! Number of manual contours
      INTEGER*4 LTNEG,LTZ,LTPOS               ! Line types for contours.
      INTEGER*4 IPDEV                         ! Hardcopy device number
      INTEGER*4 OLDPLP(4)                     ! Dummy!
      INTEGER*4 INFILE, OUTFILE               ! Pointers to files.
      INTEGER*4 ITDEV                         ! Terminal device number
      INTEGER*4 COLOUR_TABLE                  ! Colour table to use for maps
      INTEGER*4 LWEIGHT                       ! Line weight for 1-D plots
      INTEGER*4 IPLP(6)                       ! Order of maps in PLOT-LINE-PARS
      INTEGER*4 ICOLOR                        ! Colour for line plotting
      INTEGER*4 I4FLGD(19)                    ! To make 128 total

      COMMON /I4FLAGS/
     &          GSD_SCAN,IPSEQ,  ISEQ0,   ILOUT,   
     &          IBLRPT,  IXCMAP, NBIN,    NSTART,  NS1,
     &          NS2,     NXSSK,  IXLOW,   IXHIGH,  NXSII,   IEDVER,
     &          IEDASK,  NXSFM,  NXSSS,   NC1,     NC2,     IC1,
     &          IC2,     IDC1,   IDC2,    ISHIFT,  ISMTH,   IPUT,
     &          IGET,    IPEN,   IMODEL,  MAXITS,  IER,
     &          NPR,     IBLP,   ISBFLG,  ITH,     ITC,     MASK,
     &          NMASK,   IREST,  NXS,     ITICKX,  ITICKY,  NINTSX,
     &          NINTSY,  IDEV,   LINK,    NCONT,   INTERP_X,INTERP_Y,
     &          IFSSP,   NFSSP,  NCSET,   LTNEG,   LTZ,     LTPOS,
     &          IPDEV,   OLDPLP, INFILE,  OUTFILE, ITDEV, COLOUR_TABLE,
     &          LWEIGHT, IPLP,   ICOLOR,  I4FLGD
      SAVE  /I4FLAGS/

      REAL*4    FACT                          ! MULTIPLY-SPECTRUM
      REAL*4    DIV                           ! DIVIDE-SPECTRUM
      REAL*4    PNC,PC                        ! CONVOLVE-SPECTRUM
      REAL*4    ACHAN                         ! SHIFT-SPECTRUM
      REAL*4    OFF                           ! OFFSET-SPECTRUM
      REAL*4    XSR1,XSR2,SRTOL               ! REMOVE-SPIKES
      REAL*4    XREF,XGAP,XINT                ! FIND-MOMENTS
      REAL*4    VAL                           ! SET-CHANNELS
      REAL*4    XBAR                          ! FIND-SKEW
      REAL*4    TOL                           ! Baseline removal
      REAL*4    THOT,TAMB,TCOLD               ! Calibration temperatures
      REAL*4    XFAC(8)                       ! X-axis increment in curr. units
      REAL*4    XLEN, YLEN, CHARHT, XST, XEND,
     &          YST,  YEND                    ! For drawing plot axes
      REAL*4    PBEG(3), PEND(3)              ! 2-D plot limits (used)
      REAL*4    QBEG(3), QEND(3)              ! 2-D plot limits (set)
      REAL*4    CONT0, CONTI                  ! Contour levels
      REAL*4    AX1LEN,  AX2LEN               ! 2-D plot size
      REAL*4    FWHM, WMAX                    ! Interpolation function for cube
      REAL*4    THETA                         ! 2-D plot rotation angle
      REAL*4    ZZERO, ZWID                   ! CHANNEL-MAP
      REAL*4    CLEVELS(16)                   ! Contour levels, manually set
      REAL*4    BADPIX_VAL                    ! Bad pixel value for CONTOUR etc
      REAL*4    MAP_TOL                       ! Is spectrum close enough to grid
      REAL*4    TLIM(2)                       ! TA* limits for GRID-MAP
      REAL*4    BSIZE                         ! Beam size (FWHM, arcsec) for 2d
      REAL*4    FSS_AV, FSS_VAR, FSS_SD       ! Results from FIND-SPEC-STAT
      REAL*4    FRQCOEFF(6)                   ! Frequency corrections for AOS etc
      REAL*4    XCHAN                         ! Value of centroid for FIND-CENTR
      REAL*4    GREYLIM(2)                    ! Greyscale max and min.
      REAL*4    VELOUT                        ! Velocity in new frame for display
      REAL*4    VWIDHM                        ! FWHM of gaussian for convolving
                                              ! maps in velocity dimension
      REAL*4    VFNMAX                        ! Max offset to convolve vel to.
      REAL*4    C5START, C5ROTAT, C5EXP       ! For colour 5 spiral.
      REAL*4    CLIPLIM(2)                    ! For CLIP-SPECTRUM
      REAL*4    TMAX, VMAX                    ! For FIND-MAX
      REAL*4    TOTINT                        ! For FIND-INT-INT
      REAL*4    MAPLIMITS(2)                  ! Greyscale max and min actually used
      REAL*4    R4FLGD(21)                    ! To give 128 total.

      COMMON /R4FLAGS/
     &          FACT,    DIV,
     &          PNC,     PC,      ACHAN,   OFF,      XSR1,
     &          XSR2,    SRTOL,   XREF,    XGAP,     XINT,
     &          VAL,     XBAR,    TOL,
     &                            THOT,    TAMB,     TCOLD,
     &          XFAC,    XLEN,    YLEN,    CHARHT,   XST,
     &          XEND,    YST,     YEND,    PBEG,     PEND,
     &          QBEG,    QEND,
     &          CONT0,   CONTI,   AX1LEN,  AX2LEN,   FWHM,
     &          WMAX,    THETA,   ZZERO,
     &          ZWID,    CLEVELS, BADPIX_VAL,        MAP_TOL,
     &          TLIM,    BSIZE,   FSS_AV,  FSS_VAR,  FSS_SD,
     &          FRQCOEFF,XCHAN,   GREYLIM, VELOUT,   VWIDHM,
     &          VFNMAX,  C5START, C5ROTAT, C5EXP,    CLIPLIM,
     &          TMAX,    VMAX,    TOTINT,  MAPLIMITS,R4FLGD
      SAVE  /R4FLAGS/

      REAL*8    COSLAT                        ! Cosine of observatory latitude
      REAL*8    ALAT                          ! Observatory latitude (deg)
      REAL*8    ALONG                         ! Observatory longitude (deg)
      REAL*8    TIMCOR                        ! Obs hours behind GMT
      REAL*8    FCEN(8)                       ! Centre frequencies, each quad.
      REAL*8    FIRST_IF                      ! Frequency of first I.F. (GHz)
      REAL*8    REF_FREQ                      ! Reference frequency for SETX
      REAL*8    R8FLGD(52)                    ! To give 64 total

      COMMON /R8FLAGS/
     &          COSLAT,   ALAT,     ALONG,   TIMCOR,  FCEN,
     &          FIRST_IF, REF_FREQ, R8FLGD
      SAVE   /R8FLAGS/

      CHARACTER ITTLP*2                       ! SET-TERMINAL-PLOT
      CHARACTER CDUMMY*2,  FILNAM*8, FILNMX*20  ! MONGO plots
      CHARACTER NAMEFD*20                     ! DUMP, RESTART
      CHARACTER ICHAR*76                      ! Plotting
      CHARACTER OBSTIT*20                     ! Observatory title
      CHARACTER NAMEMP*80                     ! For contour mapping
      CHARACTER CDUMMY2*8                     ! 
      CHARACTER GSDNAME*32                    !GSD observation file name prefix
      CHARACTER SIDEBAND*1                    ! Current sideband (U/L)
      CHARACTER XAXIS_UNITS*6                 ! X-axis units
      CHARACTER XAXIS_NAME*10                 ! X-axis name
      CHARACTER SAVE_FILE*40                  ! Filename of output file
      CHARACTER YAXIS_NAME*16                 ! Y-axis name for 1-D plot
      CHARACTER YAXIS_UNITS*16                ! Y-axis units for 1-D plots
      CHARACTER TERMDEV*16                    ! Terminal device name
      CHARACTER PRINTDEV*16                   ! Hardcopy device name
      CHARACTER TTNN*16                       ! Terminal line for graphics
      CHARACTER VEL_DEF*3                     ! Velocity definition (RAD/OPT/REL)
      CHARACTER VEL_REF*4                     ! Velocity frame for display.
      CHARACTER CHFLGD*100                    ! To give 512 chars total.

      COMMON /CHFLAGS/
     &          ITTLP,   CDUMMY, NAMEFD,  FILNAM,
     &          FILNMX,  ICHAR,  OBSTIT,  NAMEMP,  CDUMMY2,
     &          GSDNAME, SIDEBAND, XAXIS_UNITS, XAXIS_NAME,
     &          SAVE_FILE, YAXIS_NAME, YAXIS_UNITS,
     &          TERMDEV, PRINTDEV, TTNN,  VEL_DEF, VEL_REF, 
     &          CHFLGD
      SAVE  /CHFLAGS/

      LOGICAL*4 ANDXLG                        ! Anadex paper longways
      LOGICAL*4 ERASE_PLOT                    ! Erase screen before plotting map
      LOGICAL*4 SMOOTH                        ! Interpolate map?
      LOGICAL*4 INTERACTIVE                   ! Interactive plotting required
      LOGICAL*4 TERMINAL                      ! Plot device is a terminal
      LOGICAL*4 ISUM                          ! Integrated intensity (not av)
      LOGICAL*4 ICAUTO                        ! Contour levels set automatically
      LOGICAL*4 MVPTS                         ! Mark valid points on plot
      LOGICAL*4 DMS                           ! Scales in deg-min-sec in CONTOUR
      LOGICAL*4 ISETSC                        ! Auto scaling in PLOT
      LOGICAL*4 DUMP_STATE                    ! Dump current state to file
      LOGICAL*4 MAP_OPEN                      ! Map is open for plotting etc
      LOGICAL*4 CLEVELS_SET                   ! Contour levels set explicitly
      LOGICAL*4 QUICK_PLOT                    ! Suppress labelling on spectra
      LOGICAL*4 ABS_FREQ                      ! Plot frequencies in GHz.
      LOGICAL*4 REST_REL                      ! Frequencies relative rest freq?
      LOGICAL*4 REPLACE_MAP_DATA              ! Overwrite existing data in map
      LOGICAL*4 HISTOGRAM                     ! Plot to be histogram (not lines)
      LOGICAL*4 SHOW_BEAM                     ! Indicate beam size on map
      LOGICAL*4 FCORRECT                      ! Apply correction to freq axis
      LOGICAL*4 PLOTGREY                      ! Plot greyscales
      LOGICAL*4 PLOTCONT                      ! Plot contours
      LOGICAL*4 AUTOGREY                      ! Set grey scales automatically
      LOGICAL*4 OVERCONT                      ! Overlay contours on greyscale map
      LOGICAL*4 SECTOR_OFFSET                 ! Offset sectors in MERGE
      LOGICAL*4 CHANGE_FRAME                  ! Change output velocity frame
      LOGICAL*4 SHOW_HEADER                   ! Show spectrum header in 1-D plots
      LOGICAL*4 SHOW_2DHEAD                   ! Show spectrum header in 1-D plots
      LOGICAL*4 FREEX                         ! Auto scaling on plot X-axis
      LOGICAL*4 FREEY                         ! Auto scaling on plot Y-axis
      LOGICAL*4 LOGFLG(2)                     ! To give 32 total

      COMMON /LFLAGS/
     &          ANDXLG,   ERASE_PLOT, SMOOTH,        INTERACTIVE,
     &          TERMINAL, ISUM,       ICAUTO,        MVPTS,
     &          DMS,      ISETSC,     DUMP_STATE,    MAP_OPEN,
     &          CLEVELS_SET,          QUICK_PLOT,    ABS_FREQ,
     &          REST_REL, REPLACE_MAP_DATA,          HISTOGRAM,
     &          SHOW_BEAM,            FCORRECT,      PLOTGREY,
     &          PLOTCONT, AUTOGREY,   OVERCONT,      SECTOR_OFFSET,
     &          CHANGE_FRAME,         SHOW_HEADER,   SHOW_2DHEAD,
     &          FREEX,    FREEY,      LOGFLG
      SAVE  /LFLAGS/

C-----------------------------------------------------------------------
