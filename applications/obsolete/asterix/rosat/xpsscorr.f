*+  XPSSCORR - Converts PSS XRT source counts into a flux
      SUBROUTINE XPSSCORR( STATUS )
*
*    Description :
*
*     Converts the output from a PSS run on a ROSAT XRT image for:
*                   PSPC:
*                         Vignetting as a function of position and energy
*                         The effect of the wires
*                         The exposure time (converts to c/s)
*                   HRI:
*                         Exposure time.
*                         Vignetting
*
*    Environment parameters :
*
*     INPUT = UNIV(R)
*        Input source search results file
*     ENERGY = REAL(R)
*        Mean photon energy in keV
*     RESPFILE = CHAR(R)
*        Name of detector matrix file
*     EFFILE = CHAR(R)
*        Name of effective area file
*     WIRES  = LOGICAL(R)
*        Add in factor for the wire absorption ?
*
*    Method :
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     Richard Saxton    (LTVAD::RDS)
*     David J. Allan    (BHVAD::DJA)
*
*    History :
*
*     29 Jul 91 : Original (RDS)
*      9 Mar 92 : V1.5-4 Mods (RDS)
*      2 Jun 92 : V1.6-0 Extra parameter SSO_GETPAR0L. Also handles multiple
*                        flux error levels (DJA)
*      2 Jun 92 : V1.6-1 Changed to use standard cal. files (RDS)
*      2 Feb 93 : V1.6-4 Fixed a bug in the call to XRTCORR_GETENERGY
*     20 Feb 93 : V1.6-5 Works on US exposure maps
*     19 Apr 93 : V1.6-6 Works on HRI data (RDS)
*     17 May 93 : V1.6-7 Optional parameter for the wire correction added (RDS)
*     14 Dec 93 : V1.7-0 Correctly default CALDIR on UNIX (DJA)
*     21 Feb 94 : V1.7-1 Correct div. by zero error when Exposure time is zero (RJV)
*     23 Nov 94 : V1.7-2 Vignetting correction for HRI (RJV)
*      8 Feb 95 : V1.8-0 Corrected bug in types of MDATE and MSWITCH.
*                        HEAD.DET was also not being set up! (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions:
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) FELOC   ! Locator to input FLUX field
      CHARACTER*(DAT__SZLOC) ILOC    ! Locator to PSS source file
      CHARACTER*(DAT__SZLOC) HLOC    ! Locator to PSS source file header
      CHARACTER*(DAT__SZLOC) RLOC    ! Locator to response file
      CHARACTER*(DAT__SZLOC) ELOC    ! Locator to effective areas
      CHARACTER*(DAT__SZLOC) EXLOC   ! Locator to exposure map
      CHARACTER*(DAT__SZLOC) BILOC   ! Locator to the instrument box
      CHARACTER*80 RFILE             ! Name of response file
      CHARACTER*80 EFILE             ! Name of eff. area file
      CHARACTER*80 EXPFIL            ! Name of exposure map
      CHARACTER*80 CALDIR            ! Name of XRTCAL directory
      CHARACTER*20 DET               ! Name of detector (e.g. PSPCB or C)
      CHARACTER*1 MODE               ! Operation mode 'C' or 'E'

      REAL        MEAN_EN            ! Photon energy used in PSS (keV)
      REAL        EXPOS              ! Exposure time of searched image

      INTEGER     CFPTR, ECFPTR      ! Pointers to corrected flux and errors
      INTEGER     FPTR, EFPTR        ! Pointers to flux and errors
      INTEGER     EDIMS(3)           ! Error dimensions
      INTEGER     ENDIM              ! Error dimensionality
      INTEGER     DDIM(2)            ! Error dimensionality
      INTEGER     MDATE,MSWITCH ! MJDs
      INTEGER     NED                ! Number of error items per source
      INTEGER     NELEV              ! Number of flux error levels
      INTEGER     NSRC               ! Number of sources
      INTEGER     XPTR, YPTR         ! Pointers to source image coordinates
      INTEGER     NEDIM              ! Number of exposure map dims
      INTEGER     EXDIM(DAT__MXDIM)  ! Dimensions of exposure array
      INTEGER     EXPTR              ! Pointer to exposure map data
      INTEGER     EAPTR1             ! Pointer to exposure map axis 1
      INTEGER     EAPTR2             ! Pointer to exposure map axis 2
      LOGICAL     ERRORS             ! Flux errors present in input?
      LOGICAL     INPRIM             ! Is input primitive ?
      LOGICAL     OK                 ! Dataset ok?
      LOGICAL     SYM_ERRORS         ! Symmetric flux errors?
      LOGICAL     LWIRE              ! Correct for wires ?
*
*    Version :
*
      CHARACTER*30 VERSION
        PARAMETER (VERSION = 'XPSSCORR version 1.8-0')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT
      CALL SSO_INIT( STATUS )

*    Associate input dataset
      CALL USI_ASSOCI( 'INPUT', 'UPDATE', ILOC, INPRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Check input ok
      CALL SSO_VALID( ILOC, OK, STATUS )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Dataset is not a results file!', STATUS )
        GOTO 999
      END IF

*    Get number of sources
      CALL SSO_GETNSRC( ILOC, NSRC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Error reading no. of sources from file',
     :                                                      STATUS )
        GOTO 999
      END IF

*    Map image coordinates
      CALL SSO_MAPFLD( ILOC, 'X_CORR', '_REAL', 'READ', XPTR, STATUS )
      CALL SSO_MAPFLD( ILOC, 'Y_CORR', '_REAL', 'READ', YPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Error reading X and Y positions '/
     :                                  /'from file', STATUS )
        GOTO 999
      END IF

*    Map flux
      CALL SSO_MAPFLD( ILOC, 'FLUX', '_REAL', 'READ', FPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Error reading source fluxes '/
     :                              /'from file', STATUS )
        GOTO 999
      END IF

*    Symmetric source parameter errors?
      NED = 1
      CALL SSO_GETPAR0L( ILOC, 1, 'SYMMETRIC', SYM_ERRORS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        ERRORS = .FALSE.
        CALL ERR_ANNUL( STATUS )

      ELSE

*      One data item for symmetric errors, two for asymmetric
        IF ( .NOT. SYM_ERRORS ) NED = 2

*      Get errors if present
        CALL SSO_CHKFLDERR( ILOC, 'FLUX', ERRORS, STATUS )
        IF ( ERRORS ) THEN
          CALL SSO_MAPFLDERR( ILOC, 'FLUX', '_REAL', 'READ', EFPTR,
     :                                                     STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            ERRORS = .FALSE.
          END IF
        END IF

      END IF
*
*    Create and map corrected flux
      CALL SSO_CREFLD( ILOC, 'CFLUX', '_REAL', STATUS )
      CALL SSO_MAPFLD( ILOC, 'CFLUX', '_REAL', 'WRITE', CFPTR, STATUS )
      CALL SSO_PUTFITEM0C( ILOC, 'CFLUX', 'UNITS', 40, 'count/sec',
     :                                                     STATUS )

*    Create output errors if present in input
      IF ( ERRORS ) THEN

*      Locate input errors and get dimensions
        CALL SSO_LOCFLD( ILOC, 'FLUX', FELOC, STATUS )
        CALL CMP_SHAPE( FELOC, 'ERROR', DAT__MXDIM, EDIMS, ENDIM,
     :                                                   STATUS )
        CALL DAT_ANNUL( FELOC, STATUS )

*      Number of levels
        IF ( SYM_ERRORS ) THEN
          IF ( ENDIM .EQ. 1 ) THEN
            NELEV = 1
          ELSE
            NELEV = EDIMS(1)
          END IF
        ELSE
          IF ( ENDIM .EQ. 2 ) THEN
            NELEV = 1
          ELSE
            NELEV = EDIMS(2)
          END IF
        END IF

*      Create output errors
        CALL SSO_CREFLDERR( ILOC, 'CFLUX', '_REAL', NED, NELEV, STATUS )
        CALL SSO_MAPFLDERR( ILOC, 'CFLUX', '_REAL', 'WRITE', ECFPTR,
     :                                                      STATUS )

      ELSE
*
*    map two dynamic arrays if no errors
        DDIM(1) = NED
        DDIM(2) = NSRC
        CALL DYN_MAPR(2, DDIM, EFPTR, STATUS)
        CALL DYN_MAPR(2, DDIM, ECFPTR, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('** Error: allocating dynamic memory **')
           GOTO 999
        ENDIF
*
        NELEV = NSRC
*
      ENDIF
*
* Read the detector name from the header
*    Get locator to the instrument box
      CALL HDX_FIND( ILOC, 'BOOK(1).MORE.ASTERIX.INSTRUMENT',
     &                                             BILOC, STATUS )

*    Get detector type i.e. PSPC or HARI
      CALL CMP_GET0C(BILOC, 'DETECTOR', DET, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('*Error reading instrument type*')
         GOTO 999
      ENDIF
*
      CALL DAT_ANNUL( BILOC, STATUS )
*
*    Is it an HRI observation ?
      IF ( INDEX(DET, 'HRI') .NE. 0) THEN

*      Locate HEADER object to get exposure time
         CALL HDX_FIND( ILOC, 'BOOK(1).MORE.ASTERIX.HEADER',
     :                                             HLOC, STATUS )
         CALL CMP_GET0R( HLOC, 'EXPOSURE_TIME', EXPOS, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
           CALL ERR_REP( ' ', '! Error reading exposure time', STATUS )
           GOTO 999
         END IF

*      Apply the exposure correction
         CALL XPSSCORR_HRI( NSRC, %VAL(XPTR),%VAL(YPTR),
     :                             %VAL(FPTR), ERRORS, NED*NELEV,
     :          %VAL(EFPTR), EXPOS, %VAL(CFPTR), %VAL(ECFPTR), STATUS )
*
      ELSE

*    Ask if a correction for the wires is required
         CALL USI_GET0L('WIRES', LWIRE, STATUS)
*
*    Get operation mode from the user. 'C' to calculate the exposure
*    from the vignetting and 'E' to use the exposure map
         CALL USI_GET0C('MODE', MODE, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
         CALL CHR_UCASE(MODE)
*
*    Calculatiuon mode:
         IF (MODE .EQ. 'C') THEN
*
*      Ask user which energy was used in PSS
            CALL USI_GET0R( 'ENERGY', MEAN_EN, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999

*      Locate HEADER object to get exposure time
            CALL HDX_FIND( ILOC, 'BOOK(1).MORE.ASTERIX.HEADER',
     :                                             HLOC, STATUS )
            CALL CMP_GET0R( HLOC, 'EXPOSURE_TIME', EXPOS, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              CALL ERR_REP(' ', '! Error reading exposure time', STATUS)
              GOTO 999
            END IF

*     Get cal directory and use this to set default for response
            CALL XRT_CALDEF( CALDIR, STATUS )
            IF (STATUS .NE. SAI__OK) THEN
              CALL MSG_PRNT('Warning: XRT cal directory not found')
              CALL ERR_ANNUL( STATUS )
            ENDIF
            RFILE = CALDIR(1:CHR_LEN(CALDIR)) // 'drmpspc'
            CALL USI_DEF0C('RESPFILE', RFILE, STATUS)

*     Get detector response matrix name
            CALL USI_GET0C('RESPFILE', RFILE, STATUS)
*
            IF (STATUS .NE. SAI__OK) GOTO 999
*
*     Open response matrix
            CALL HDS_OPEN(RFILE,'READ',RLOC,STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETC('RFILE', RFILE)
               CALL MSG_PRNT('Error opening ^RFILE')
               GOTO 999
            ENDIF
*
*     Get effective area filename
*   Sometimes it doesn't say if it is PSPC-B or C, but just says PSPC. In
*   this case the only way to tell is from the observation date.
*   Before Jan 26th 1991 is 'C' and after is 'B'.
*     Test if the detector string is ok.
            IF ( (INDEX(DET, 'PSPCB') .EQ. 0) .AND.
     &              (INDEX(DET, 'PSPCC') .EQ. 0) ) THEN
*
*        Get the observation date as an MJD (use MJDs to compare dates)
               CALL CMP_GET0I(HLOC, 'BASE_MJD', MDATE, STATUS)
*
               IF (STATUS .NE. SAI__OK) THEN
                  CALL MSG_PRNT('** Error reading BASE_MJD **')
                  CALL MSG_PRNT('** Can not determine detector type **')
                  GOTO 999
               ENDIF
*
*        Convert 26th Jan 1991 to an MJD
               CALL CONV_YMDMJD(1991, 1, 26, MSWITCH)
*
*        Compare the MJD of the observation to the switch over point
               IF (MDATE .LE. MSWITCH) THEN
                  DET = 'PSPCC'
               ELSE
                  DET = 'PSPCB'
               ENDIF
*
            ENDIF
*
            CALL DAT_ANNUL( HLOC, STATUS )

*     Select the default efective area file (dependent on the date of the
*     observation)
*        Find the XRTCAL directory
            CALL XRT_CALDEF(CALDIR, STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('Warning: XRT cal directory not found')
               CALL ERR_ANNUL(STATUS)
            ENDIF
*
            IF (INDEX(DET, 'PSPCB') .NE. 0) THEN
               EFILE = '$XRTCAL/pspcb_eff'
               CALL USI_DEF0C('EFFILE', EFILE, STATUS)
            ELSEIF (INDEX(DET, 'PSPCC') .NE. 0) THEN
               EFILE = '$XRTCAL/pspcc_eff'
               CALL USI_DEF0C('EFFILE', EFILE, STATUS)
            ENDIF
*
*     Get effective area filename from the user
            CALL USI_GET0C('EFFILE', EFILE, STATUS)
*
*     Open effective areas file
            CALL HDS_OPEN(EFILE, 'READ', ELOC, STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETC('EFF', EFILE)
               CALL MSG_PRNT('Error opening effective area file ^EFF')
               GOTO 999
            ENDIF
*
*      Calculate the correction factors
           CALL XPSSCORR_CALC( RLOC, ELOC, NSRC, %VAL(XPTR), %VAL(YPTR),
     :              %VAL(FPTR), ERRORS, NED*NELEV, %VAL(EFPTR), MEAN_EN,
     :                 EXPOS, LWIRE, %VAL(CFPTR), %VAL(ECFPTR), STATUS )
*
*      Close the cal files
            CALL HDS_CLOSE(ELOC, STATUS)
*
*    Otherwise use the exposure map
         ELSEIF (MODE .EQ. 'E') THEN
*
*      Get exposure map filename from the user
            CALL USI_GET0C('EXPMAP', EXPFIL, STATUS)
*
*     Open effective areas file
            CALL HDS_OPEN(EXPFIL, 'READ', EXLOC, STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETC('EFF', EXPFIL)
               CALL MSG_PRNT('Error opening exposure map file ^EFF')
               GOTO 999
            ENDIF
*
*     Map the data array for the exposure map and get the axis values
            CALL BDA_CHKDATA(EXLOC, OK, NEDIM, EXDIM, STATUS)
*
            CALL BDA_MAPDATA(EXLOC, 'READ', EXPTR, STATUS)
*
*     Map the axes - NB: have to assume that the axis units are half arcsecs
            CALL BDA_MAPAXVAL(EXLOC, 'READ', 1, EAPTR1, STATUS)
            CALL BDA_MAPAXVAL(EXLOC, 'READ', 2, EAPTR2, STATUS)
*
            IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('** Error reading exposure map **')
               GOTO 999
            ENDIF
*
*    Use the exposure map to generate the exposure times
            CALL XPSSCORR_EXP( EXDIM(1), EXDIM(2), %val(EXPTR),
     :           %val(EAPTR1), %val(EAPTR2), NSRC, %VAL(XPTR),
     :           %VAL(YPTR), %VAL(FPTR), ERRORS, NED*NELEV,
     :           %VAL(EFPTR), MEAN_EN,
     :           EXPOS, LWIRE, %VAL(CFPTR), %VAL(ECFPTR), STATUS )
*
         ENDIF
      ENDIF
*
*    Release dataset
      CALL SSO_RELEASE( ILOC, STATUS )

999   CALL AST_CLOSE(STATUS)
      CALL AST_ERR(STATUS)

      END



*+  XPSSCORR_CALC - Calculate the correction for each source
      SUBROUTINE XPSSCORR_CALC(RLOC, ELOC, NSRC, XPOS, YPOS, FLUX,
     :                 GOT_ERRORS, NED, EFLUX, MEAN_EN, EXPOS, LWIRE,
     :                                        CFLUX, ECFLUX, STATUS )
*    Description :
*    Deficiencies :
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     23-Jul-1991 original
*      2-Feb-1993 fixed a bug in call to XRTCORR_GETENERGY
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*     <specification of FORTRAN structures>
*    Import :
      CHARACTER*(DAT__SZLOC) RLOC    ! Locator to response file
      CHARACTER*(DAT__SZLOC) ELOC    ! Locator to effective areas

      INTEGER NSRC                ! Number of sources
      REAL XPOS(NSRC)             ! X offset of sources
      REAL YPOS(NSRC)             ! Y offset of sources
      REAL FLUX(NSRC)             ! Source flux
      LOGICAL GOT_ERRORS          ! Flux errors there
      INTEGER NED                 ! Flux error items per source
      REAL EFLUX(NED,*)           ! Flux errors
      REAL MEAN_EN                ! Energy used in source search (keV)
      REAL EXPOS                  ! Exposure time (seconds)
      LOGICAL LWIRE               ! Correct for wire absorption ?
*    Import-Export :
*    Export :
      REAL CFLUX(NSRC)            ! Flux conversion factors
      REAL ECFLUX(NED,*)          ! Converted errors
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      RECORD /CORR/ HEAD                   ! Header structure
      INTEGER ELP                          ! Loop over flux errors
      INTEGER SLP                          ! Loop over sources
      INTEGER NENERGY                      ! No. of energies
      INTEGER EPNTR                        ! Pointer to energy array
      INTEGER DUMMY                        ! Pointer to dummy array
      REAL VZERO                           ! Effective area at field centre
      REAL VSING                           ! Effective area at source position
      REAL WFACTOR                         ! Wire correction factor
      REAL CFACTOR                         ! Total correction factor
      REAL EDUM
      LOGICAL VFLAG                        ! Was vignetting correction ok ?
*    Local data :
*-
*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

* Calculate the correction factor for the thin and thick wires
* Note a source partially hidden behind the central ring or one of the
* spokes will not be fully corrected by this routine.
      IF (LWIRE) THEN
         WFACTOR = 1.0 / 0.79
      ELSE
         WFACTOR = 1.0
      ENDIF
*
* Read in trial energies from the response matrix
      CALL XRTCORR_GETENERGY(RLOC, 1, 1.0, NENERGY, EPNTR, EDUM, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Map a dummy array
      CALL DYN_MAPR(1, NENERGY, DUMMY, STATUS)
*
* Find the effective area for this energy in the field centre
      HEAD.DET = 'PSPC'
      HEAD.OFFAX=0.0
*
      CALL XRT_VIGNET(HEAD, ELOC, NENERGY, %val(EPNTR), MEAN_EN,
     &                 .FALSE., 1, %val(DUMMY), VZERO, VFLAG, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Loop over each source
      DO SLP=1,NSRC
*
*      Calculate off axis angle in arcminutes
         HEAD.OFFAX = SQRT( XPOS(SLP)**2 + YPOS(SLP)**2 ) * 60.
*
*      Calc. effective area for this source
         CALL XRT_VIGNET(HEAD, ELOC, NENERGY, %val(EPNTR), MEAN_EN,
     &                 .FALSE., 1, %val(DUMMY), VSING, VFLAG, STATUS)

         IF (STATUS.NE.SAI__OK) GOTO 999
*
*     Calc. total flux correction for this source
         CFACTOR = VSING / VZERO * WFACTOR / EXPOS
*
*      Find corrected flux
        CFLUX(SLP) = CFACTOR * FLUX(SLP)

*      Set errors if present
        IF ( GOT_ERRORS ) THEN
          DO ELP = 1, NED
            ECFLUX(ELP,SLP) = EFLUX(ELP,SLP)*CFACTOR
          END DO
        END IF

      END DO

999   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'from XPSSCORR_CALC', STATUS )
      END IF

      END

*+  XPSSCORR_EXP - Calculate the correction for each source
      SUBROUTINE XPSSCORR_EXP(EDIM1, EDIM2, EXDATA, AX1, AX2, NSRC,
     :                 XPOS, YPOS, FLUX, GOT_ERRORS, NED, EFLUX,
     :                 MEAN_EN, EXPOS, LWIRE, CFLUX, ECFLUX, STATUS )
*    Description :
*    Deficiencies :
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     23-Jul-1991 original
*     20-Feb-1993 updated to handle US format exposure map files. These
*                 have axes in degrees rather than the MPE format
*                 of arcsecs/2
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Import :
      INTEGER EDIM1,EDIM2         ! Dimensions of Exposure array
      REAL EXDATA(EDIM1,EDIM2)    ! Exposure map data array
      REAL AX1(EDIM1)             ! Exposure 1st axis values (arcsec/2)
      REAL AX2(EDIM2)             ! Exposure 2nd axis values (arcsec/2)
      INTEGER NSRC                ! Number of sources
      REAL XPOS(NSRC)             ! X offset of sources
      REAL YPOS(NSRC)             ! Y offset of sources
      REAL FLUX(NSRC)             ! Source flux
      LOGICAL GOT_ERRORS          ! Flux errors there
      INTEGER NED                 ! Flux error items per source
      REAL EFLUX(NED,*)           ! Flux errors
      REAL MEAN_EN                ! Energy used in source search (keV)
      REAL EXPOS                  ! Exposure time (seconds)
      LOGICAL LWIRE               ! Correct for the wires ?
*    Import-Export :
*    Export :
      REAL CFLUX(NSRC)            ! Flux conversion factors
      REAL ECFLUX(NED,*)          ! Converted errors
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER LPX,LPY             ! Loop over exposure map pixels
      INTEGER XPIX,YPIX           ! Pixel position of source in expo map
      INTEGER CNT
      INTEGER ELP,SLP
      REAL XWID,YWID              ! Widths of expo map pixels (arcsec/2)
      REAL OFFAX                  ! Off-axis position (arcmins)
      REAL PSF                    ! 90% radius (degs)
      REAL PIXRAD                 !    "       (EXPO MAP PIXELS)
      REAL SUM                    ! Total exposure in source pixels
      REAL EXPO_TIM               ! Mean exposure time for source
      REAL WFACTOR                ! Correction factor for the thin/thick wires
      REAL XSTART,YSTART          ! Start of exposure map axes
*    Local data :
*-
*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Calculate the correction factor for the thin and thick wires
      IF (LWIRE) THEN
         WFACTOR = 1.0 / 0.79
      ELSE
         WFACTOR = 1.0
      ENDIF

*    Loop over each source
      DO SLP=1,NSRC

*      Calculate off axis angle in arcminutes
        OFFAX = SQRT( XPOS(SLP)**2 + YPOS(SLP)**2 ) * 60.

*      Find the 90% radius at this off-axis angle in degrees
        CALL XRAD90_INT(OFFAX, PSF, STATUS)


*      Find the radius in exposure map pixels.
*      MPE exposure maps have pixels in arcsecs/2, US files have
*      degrees RA and DEC ! Work in units of arcsecs/2 here.
*      Calculate the pixel width in arcsecs/2
        XWID = (AX1(EDIM1) - AX1(1)) / REAL(EDIM1-1)
        YWID = (AX2(EDIM2) - AX2(1)) / REAL(EDIM2-1)
*      If XWID is < 1, then its probably US data so convert from
*      degrees to arcsec/2
        IF (ABS(XWID) .LT. 1.0) THEN
          XWID = XWID * 7200.
          YWID = YWID * 7200.
          XSTART = (AX1(1) - AX1((EDIM1-1)/2)) * 7200.
          YSTART = (AX2(1) - AX2((EDIM2-1)/2)) * 7200.
        ELSE
          XSTART = AX1(1)
          YSTART = AX2(1)
        END IF
        PIXRAD = ABS(NINT(PSF*7200./XWID))

*      Find the exposure map pixel of this source
        XPIX = (-XPOS(SLP) * 7200. - XSTART) / XWID + 1
        YPIX = (-YPOS(SLP) * 7200. - YSTART) / YWID + 1
        SUM=0.0
        CNT=0
*

        DO LPX=(XPIX-PIXRAD),(XPIX+PIXRAD)
          DO LPY=YPIX-PIXRAD,YPIX+PIXRAD
            IF (SQRT ( REAL((XPIX-LPX)**2 + (YPIX-LPY)**2) )
     :                                          .LE. PIXRAD) THEN
              SUM = SUM + EXDATA(LPX,LPY)
              CNT = CNT + 1
            ENDIF
          ENDDO
        ENDDO
        EXPO_TIM = SUM / REAL(CNT)

*      Find corrected flux, assuming exposure time none zero
        IF (EXPO_TIM .GT. 0.0) THEN
          CFLUX(SLP) = FLUX(SLP) * WFACTOR / EXPO_TIM
        ELSE
          CFLUX(SLP) = -9999.0
        END IF

*      Set errors if present
        IF ( GOT_ERRORS ) THEN
          DO ELP = 1, NED
            IF (EXPO_TIM.GT.0.0) THEN
              ECFLUX(ELP,SLP) = EFLUX(ELP,SLP) * WFACTOR / EXPO_TIM
            ELSE
              ECFLUX(ELP,SLP) = -9999.0
            ENDIF
          END DO
        END IF

      END DO

*    Tidy up
 999  IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'from XPSSCORR_EXP', STATUS )
      END IF

      END


*+  XPSSCORR_HRI - Calculate the correction for each HRI source
      SUBROUTINE XPSSCORR_HRI(NSRC,XPOS,YPOS, FLUX, GOT_ERRORS, NED,
     :                          EFLUX,EXPOS, CFLUX, ECFLUX, STATUS )
*    Description :
*    Deficiencies :
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     19-Apr-1993 original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*     <specification of FORTRAN structures>
*    Import :
      INTEGER NSRC                ! Number of sources
      REAL XPOS(NSRC),YPOS(NSRC)  ! Source positions
      REAL FLUX(NSRC)             ! Source flux
      LOGICAL GOT_ERRORS          ! Flux errors there
      INTEGER NED                 ! Flux error items per source
      REAL EFLUX(NED,*)           ! Flux errors
      REAL EXPOS                  ! Exposure time (seconds)
*    Import-Export :
*    Export :
      REAL CFLUX(NSRC)            ! Flux conversion factors
      REAL ECFLUX(NED,*)          ! Converted errors
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER SLP,ELP
      REAL OFFAX
      REAL QE,VIG
      REAL CFACTOR
*    Local data :
*-
*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop over each source
      DO SLP=1,NSRC

*  get off-axis angle
        OFFAX=SQRT(XPOS(SLP)**2 + YPOS(SLP)**2)

*  get vignetting and quantum efficiency factors
        CALL XRT_HRIQE(OFFAX,QE)
        CALL XRT_HRIVIG(OFFAX,VIG)

*  total correction factor
        CFACTOR=1.0/QE/VIG/EXPOS

*      Find corrected flux
        CFLUX(SLP) = FLUX(SLP) * CFACTOR

*      Set errors if present
        IF ( GOT_ERRORS ) THEN
          DO ELP = 1, NED
            ECFLUX(ELP,SLP) = EFLUX(ELP,SLP) * CFACTOR
          END DO
        END IF

      END DO

      END
