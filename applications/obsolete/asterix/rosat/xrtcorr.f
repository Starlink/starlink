*+  XRTCORR - Corrects XRT datafiles for vignetting effects etc..
      SUBROUTINE XRTCORR( STATUS )
*    Description :
*     Corrects ROSAT XRT files for:
*                         Dead time effects
*                         The filter support structure
*                         Vignetting as a function of position and energy
*                         Point spread function (fn of position and energy)
*    Environment parameters :
*      OVER     LOGICAL  Overwrite the input file ?
*      INPUT    UNIV     Name of input file
*      OUTPUT   UNIV     Name of output file
*      ENERGY   REAL     Hidden parameter - the mean energy of the
*                        photons incident on the detector keV (default 0.2)
*      DCORR    LOGICAL  Perform the dead time correction ?
*      VCORR    LOGICAL  Perform the vignetting correction ?
*      PCORR    LOGICAL  Perform the point spread correction ?
*      WCORR    LOGICAL  Perform the wire correction ?
*    Method :
*     Applies the following corrections dependent on file type:
*
*      Spectra:
*         Corrects for dead time and wire structure. Writes the correction
*         for vignetting and point spreading into an energy indexed
*         array in the datafile. This will be subsequently picked up
*         by XRTRESP which will modify the detector response matrix for
*         the off-axis position of the source.
*
*      Time series:
*         Corrects for dead time and wire structure. Also corrects
*         for vignetting and point spreading using a mean energy.
*
*      Images:
*         Corrects for dead time only.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton    (LTVAD::RDS)
*    History :
*     26-6-1990   original
*     1-5-1991    Version 1.4-3 with exposure corrections for
*                 time series fixed
*     3-5-1991    Version 1.4-4 - uses quality array to generate useful
*                 error messages during dead time correction
*    21-5-1991    Version 1.4-5 - corrects radial bins and uses the
*                 new SORT structure
*     1-7-1991    No longer performs the wire correction for spectra as this is
*                 included in the effective area correction. The
*                 correction for time series is reduced to 1/0.79 - V1.4-6.
*     25-2-1992   Cocked up the previous wire correction. V1.4-7 includes
*                 the wcorr of 1.0/0.79 1.4-6 gave 1.0/0.72
*     24-6-1992   Various minor additions V1.4-8
*    30-11-1992   Does vignetting corrections to images and spectral
*                 images by default. V1.4-9
*     7-1-1993    Allows user to override the of-axis angles for vignetting
*                 correction V1.6-0
*     7-4-1993    Now performs basic corrections on HRI data V1.6-2
*     9-6-1993    Defaults the .FITS extension for US files - V1.6-3
*     6-1-1994    Handles Rationalised data format and writes the
*                 livetime structure V1.6-4
*    21-2-1994    Uses RDF filenaming convention V1.6-5
*    24-Apr-1994  (v1.7-0) for new release of asterix
*     4 Jul 94    V1.7-1  HRI quantum efficiency and vignetting (RJV)
*    20 Nov 94    V1.7-2  HRI deadtime (RJV)
*    15 Feb 95    V1.8-0  Option not to do exposure correction (RJV)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      RECORD /CORR/ HEAD                  ! Header information
*
      LOGICAL OVER                        ! Overwrite the input file ?

      CHARACTER*(DAT__SZLOC) LOC1         ! Locator to input file
      CHARACTER*(DAT__SZLOC) LOCOUT       ! Locator to output file
      CHARACTER*(DAT__SZLOC) RLOC         ! Locator to the response matrix
      CHARACTER*(DAT__SZLOC) ELOC         ! Locator to the eff. area file
      CHARACTER*(DAT__SZLOC) ILOC         ! Locator to the instrument box
      CHARACTER*(DAT__SZLOC) LLOC         ! Locator to the livetime structure
      CHARACTER*80 CALDIR                 ! Dir for XRT cal files
      CHARACTER*80 RFILE                  ! Response matrix file
      CHARACTER*80 EFILE                  ! Eff. area file
      LOGICAL INPRIM                      ! Is input file a primitive array ?
      LOGICAL THERE                       ! Is HDS object present ?
      INTEGER DPNTR                       ! Pointer to reordered data array
      INTEGER TDPNTR                      ! Pointer to original data array
      INTEGER VPNTR                       ! Pointer to reordered variance array
      INTEGER TVPNTR                      ! Pointer to original variance array
      INTEGER QPNTR                       ! Pointer to reordered QUALITY array
      INTEGER TQPNTR                      ! Pointer to original quality array
      INTEGER EPNTR                       ! Pointer to trial energies array
      INTEGER CDPNTR                      ! Pointer to dead time corrections
      INTEGER CPPNTR                      ! Pointer to point spread corrections
      INTEGER CWPNTR                      ! Pointer to wire corrections
      INTEGER CVPNTR                      ! Pointer to vig. corrections
      INTEGER CIVPNTR                     ! Pointer to image vig. corrections
      INTEGER TCVPTR                      ! Pointer to single vig. array
      INTEGER CTPNTR                      ! Pointer to thresholding corrections
      INTEGER DXPNTR                      ! Pointer to exposure time corrections
      INTEGER SXPNTR                      ! Pointer to start exptime corrections
      INTEGER EXPNTR                      ! Pointer to end exptime corrections
      INTEGER TEPNTR                      ! Pointer to product of all energy
*                                         ! dependent corections
      INTEGER DQPNTR                      ! Pointer to Timebin QUALITY array
      INTEGER LPNTR                       ! Pointer to workspace
      INTEGER EPHPTR                      ! Pointer to energy of PH chans.
      INTEGER EAXPTR                      ! Pointer to PH chan axis array.
      INTEGER PVSING                      ! Pointer to single energy vig. corrs.
      INTEGER DIMS(DAT__MXDIM)            ! Dimensions of reordered array
      INTEGER RDIMS(DAT__MXDIM)           ! Dimensions of original array
      INTEGER VCDIM(3)                    ! Dims of vig. correction array
      INTEGER ORDER(7)                    ! Order of axes:
*                                         ! 1=X, 2=Y, 3=T 4=Corr PH
      INTEGER REORD(7)                    ! Original order of axes
      INTEGER NX,NY,NT,NP,NR              ! Dimensions of various axes
      INTEGER NENERGY                     ! Number of trial energies
      INTEGER NWIRE                       ! Number of wire corrs.
      INTEGER NTHRESH                     ! No. of thresholding corrections
      INTEGER NLINES                      ! Number of lines of history text
      CHARACTER*80 PATH(8)                ! History text
      REAL MEAN_ENERGY                    ! Mean photon energy received at det.
      REAL PSING                          ! Point spread correction for mean en.
      REAL VSING                          ! Single vignetting correction
      LOGICAL LDCORR                      ! Does user want to perform dtime cor?
      LOGICAL LVCORR                      ! Does user want to perform vignetting
      LOGICAL LPCORR                      ! Does user want to perform PSF corr ?
      LOGICAL LWCORR                      ! Does user want to perform wires corr
      LOGICAL LECORR
      LOGICAL VFLAG                       ! Performing vignetting corrections ?
      LOGICAL DFLAG                       ! Performing dead time corrections ?
      LOGICAL LHRI                        ! HRI data ?
      BYTE MASK                           ! BADBITS mask
      INTEGER LP
*    Local data :
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'XRTCORR version 1.8-0')
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
      CALL MSG_PRNT(VERSION)
*
      CALL AST_INIT
*
* Should input file be overwritten ?
      CALL USI_GET0L('OVER',OVER,STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
      IF (OVER) THEN
*
         CALL USI_ASSOCI('INP','UPDATE',LOC1,INPRIM,STATUS)
*
*   Clone an output locator
         CALL DAT_CLONE(LOC1,LOCOUT,STATUS)
*
      ELSE
*
         CALL USI_ASSOC2('INP','OUT','READ',LOC1,LOCOUT,
     &                                               INPRIM,STATUS)
*
         IF (INPRIM) THEN
            CALL MSG_PRNT('Primitive arrays must be corrected in'/
     &              /' their original data file: use the OVER switch')
            GOTO 999
         ENDIF
*
*   Copy all components from old file into new file
         CALL HDX_COPY(LOC1,LOCOUT,STATUS)
*
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Read header information into a structured array
      CALL XRTCORR_GETHEAD(LOCOUT, HEAD, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*    Find dimensions of data array and map it. The data array is reordered
*    in this routine so that the axes are in the order X,Y,Time,corrected
*    amplitude. If variance or quality arrays are not found they are
*    created. Also gets further XRT specific header info.
      CALL XRTCORR_GETDATA(LOCOUT, HEAD, DIMS, ORDER, DPNTR,
     &             TDPNTR, VPNTR, TVPNTR, QPNTR, TQPNTR, MASK, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*    Set axes dimension values
      NX=DIMS(1)
      NY=DIMS(2)
      NT=DIMS(3)
      NP=DIMS(4)
      NR=DIMS(5)
*
* Is it HRI data ?
      IF (INDEX(HEAD.DET, 'HRI') .NE. 0) THEN
         LHRI = .TRUE.
      ELSE
         LHRI = .FALSE.
      ENDIF
*
*
*    Set default to standard matrix
      CALL XRT_CALDEF(CALDIR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Warning: XRT cal directory not found')
         CALL ERR_ANNUL(STATUS)
      ENDIF
*
      IF (.NOT. LHRI) THEN
*   Response matrix not yet available for HRI
         RFILE = CALDIR(1:CHR_LEN(CALDIR)) // 'drmpspc'
         CALL USI_DEF0C('RESPFILE', RFILE, STATUS)
*
*   Get detector response matrix name
         CALL USI_GET0C('RESPFILE', RFILE, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Open response matrix
         CALL HDS_OPEN(RFILE,'READ',RLOC,STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC('RFILE', RFILE)
            CALL MSG_PRNT('Error opening ^RFILE')
            GOTO 999
         ENDIF

      ENDIF
*
*   Get effective area filename for PSPC
*   Select the default efective area file (dependent on the date of the
*   observation and detector)
      IF (.NOT.LHRI) THEN

        IF (INDEX(HEAD.DET, 'PSPCB') .NE. 0) THEN
           EFILE = CALDIR(1:CHR_LEN(CALDIR)) // 'pspcb_eff'
           CALL USI_DEF0C('EFFILE', EFILE, STATUS)
        ELSEIF (INDEX(HEAD.DET, 'PSPCC') .NE. 0) THEN
           EFILE = CALDIR(1:CHR_LEN(CALDIR)) // 'pspcc_eff'
           CALL USI_DEF0C('EFFILE', EFILE, STATUS)
        ENDIF
*
*   Get effective area filename from the user
        CALL USI_GET0C('EFFILE', EFILE, STATUS)
*
*   Open effective areas file
        CALL HDS_OPEN(EFILE, 'READ', ELOC, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_SETC('EFF', EFILE)
           CALL MSG_PRNT('Error opening effective area file ^EFF')
           GOTO 999
        ENDIF

      ENDIF
*
*
*    Get rootname of calibration files
      CALL USI_GET0C('RTNAME', HEAD.RTNAME, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Find out if this file was produced from US format data
      CALL BDA_LOCINSTR(LOCOUT, ILOC, STATUS)
      CALL DAT_THERE(ILOC, 'RAWDATA', THERE, STATUS)
*
      IF (THERE) THEN
         CALL CMP_GET0C(ILOC, 'RAWDATA', HEAD.ORIGIN, STATUS)
      ELSE
         HEAD.ORIGIN = 'OMD'
      ENDIF
*
*  If status has gone bad - reset and assume raw data is MPE data
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_ANNUL(STATUS)
         HEAD.ORIGIN = 'OMD'
      ENDIF
*
*    Map an array to take the energies at each pulse height channel
      CALL DYN_MAPR(1, NP, EPHPTR, STATUS)
*
*    Map the axis array if present
      IF (NP .GT. 1) THEN
         CALL BDA_MAPAXVAL(LOCOUT, 'READ', ORDER(4), EAXPTR, STATUS)
      ELSE
         CALL DYN_MAPR(1, 1, EAXPTR, STATUS)
      ENDIF
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error mapping dynamic axis array')
         GOTO 999
      ENDIF
*
*    Read in trial energies from the response matrix
      IF (.NOT. LHRI) THEN
         CALL XRTCORR_GETENERGY(RLOC, NP,
     &        %val(EAXPTR), NENERGY, EPNTR, %val(EPHPTR), STATUS)
      ELSE
         NENERGY = 1
      ENDIF
*
*    If the file doesn't contain pulse height information assume
*    a mean photon energy. This should be a constant which may
*    be overridden on the command line.
      IF (NP .EQ. 1) THEN
*
         CALL USI_GET0R('ENERGY', MEAN_ENERGY, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
*    Write the MEAN ENERGY into the PH energy array
         CALL ARR_COP1R(1,MEAN_ENERGY, %val(EPHPTR),STATUS)

      ENDIF
*
*    Map deadtime dynamic array
      CALL DYN_MAPR(1,NT,CDPNTR,STATUS)
*
*    Map point spread correction dynamic array
      CALL DYN_MAPR(1,NENERGY,CPPNTR,STATUS)
*
*    Map wire correction dynamic array, one element for now
      NWIRE=1
      CALL DYN_MAPR(1,NWIRE,CWPNTR,STATUS)
*
*    Map vignetting correction dynamic array - this has different
*    dimensionality for an image
      IF (NX .EQ. 1) THEN
         VCDIM(1) = NENERGY
         VCDIM(2) = NR
         CALL DYN_MAPR(2,VCDIM,CVPNTR,STATUS)
*
      ELSE
         VCDIM(1) = NX
         VCDIM(2) = NY
         VCDIM(3) = NP
         CALL DYN_MAPR(3,VCDIM,CIVPNTR,STATUS)
      ENDIF
*
*    Map vig. correction array - for one radial bin or image pixel
      CALL DYN_MAPR(1,NENERGY,TCVPTR,STATUS)
*
*    Map single energy vignetting correction factor array
      CALL DYN_MAPR(1,NR,PVSING,STATUS)
*
*    Map threshold correction dynamic array, one element for now
      NTHRESH=1
      CALL DYN_MAPR(1,NTHRESH,CTPNTR,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining dynamic space')
         GOTO 999
      ENDIF
*
*    Calculate correction factors
*    Dead time:
      CALL USI_GET0L('DCORR', LDCORR, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
      IF (LDCORR) THEN
*
*      Generate quality array for each time bin
         CALL DYN_MAPB(1,NT,DQPNTR,STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error obtaining dynamic memory')
            GOTO 999
         ENDIF
*
         CALL XRTCORR_TBINQUAL(NX, NY, NT, NP, NR, MASK,
     &                            %val(QPNTR), %val(DQPNTR))


*     Calc the dead times

         IF (LHRI) THEN
           CALL XRTCORR_DTIME_HRI(HEAD,NT,%val(DQPNTR),%val(CDPNTR),
     &                                                  DFLAG,STATUS)
         ELSE
           CALL XRTCORR_DTIME_PSPC(HEAD,NT,%val(DQPNTR),%val(CDPNTR),
     &                                                  DFLAG,STATUS)
         ENDIF
      ELSE
*
*      If dead time corrections not wanted, set array to 1.0
         CALL ARR_INIT1R(1.0, NT, %val(CDPNTR),STATUS)
      ENDIF
*
*    Vignetting:
      CALL USI_GET0L('VCORR', LVCORR, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*    Calculate the vignetting correction if required
      IF (LVCORR) THEN
*
         IF (NX .EQ. 1 .AND. NY .EQ. 1) THEN
*
*       calc. separate vignetting correction for each radial bin
            CALL XRTCORR_GETVIG(HEAD, ELOC, NENERGY, %val(EPNTR),
     &               MEAN_ENERGY, NR, NP, %val(TCVPTR), VSING,
     &                    %val(CVPNTR), %val(PVSING), VFLAG, STATUS)
*
         ELSE
*
*       calc. vignetting correction for each image/spectral bin
            CALL XRT_IMVIG(HEAD, ELOC, NENERGY, %val(EPNTR),
     &               NX, NY, NP, %val(EPHPTR), .TRUE.,
     &                    %val(CIVPNTR), VFLAG, STATUS)
*
         ENDIF
*
*    Otherwise set the correction to 1.0
      ELSE
*
         CALL ARR_INIT1R(1.0, NR, %val(PVSING),STATUS)
*
         IF (NX .EQ. 1 .AND. NY .EQ. 1) THEN
            CALL ARR_INIT1R(1.0, NENERGY*NR, %val(CVPNTR),STATUS)
         ELSE
            CALL ARR_INIT1R(1.0, NX*NY*NP, %val(CIVPNTR),STATUS)
         ENDIF
*
         VFLAG = .FALSE.
*
      ENDIF
*
*    PSF:  Of course images have no PSF correction
      CALL USI_GET0L('PCORR', LPCORR, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*    If we are correcting radial bins or an annular box - point spread
*    corrections are undefined - so ban them !!
      IF (NR .GT. 1 .OR. HEAD.SHAPE .EQ. 'A') LPCORR = .FALSE.
*
*    Calculate PSF correction if wanted
      IF (LPCORR .AND. .NOT. LHRI) THEN
*
         IF (NX .EQ. 1 .AND. NY .EQ. 1) THEN
            CALL XRTCORR_PSF(HEAD, NENERGY, %val(EPNTR), MEAN_ENERGY,
     &                                NP, %val(CPPNTR), PSING, STATUS)
         ELSE
            PSING=1.0
         ENDIF
*
*    Otherwise set the correction to 1.0
      ELSE
*
         CALL ARR_INIT1R(1.0, NENERGY, %val(CPPNTR),STATUS)
         PSING = 1.0
*
      ENDIF
*
*    Wires:
*    The wire correction is included in the effective area file so
*    spectra do not have to be wired.
      IF (NP .EQ. 1) THEN
*
         CALL USI_GET0L('WCORR', LWCORR, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
      ELSE
         LWCORR = .FALSE.
      ENDIF
*
*    Calculate wire correction if wanted
      IF (LWCORR .AND. NX .EQ. 1 .AND. NY .EQ. 1
     &                           .AND. .NOT. LHRI) THEN
         CALL XRTCORR_WIRES(NWIRE, %val(CWPNTR))
      ELSE
*
*    Set wire corrections to one
         CALL ARR_INIT1R(1.0, NWIRE, %val(CWPNTR),STATUS)
*
      ENDIF
*
*    Thresholding:
      CALL XRTCORR_THRESH(NP, MEAN_ENERGY, NTHRESH, %val(CTPNTR))
*

*    Exposure correction:
      CALL USI_GET0L('ECORR',LECORR,STATUS)

      IF (LECORR) THEN

* Map a dynamic array to hold the exposure & time values for each time bin.
        CALL DYN_MAPR(1, NT, DXPNTR, STATUS)
        CALL DYN_MAPD(1, NT, SXPNTR, STATUS)
        CALL DYN_MAPD(1, NT, EXPNTR, STATUS)
*
* Map some workspace
        CALL DYN_MAPD(1, MAXRNG*2, LPNTR, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error obtaining dynamic memory')
           GOTO 999
        ENDIF
*
*  Calculate the exposure in each time bin.
        CALL XRTCORR_CALCEXP(HEAD, NT, %val(LPNTR), %val(SXPNTR),
     &                             %val(EXPNTR), %val(DXPNTR), STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
* Write the livetime values
        CALL BDA_CRELIVE(LOCOUT, STATUS)
        CALL BDA_LOCLIVE(LOCOUT, LLOC, STATUS )
        CALL DAT_NEW (LLOC, 'ON',  '_DOUBLE', 1, NT, STATUS )
        CALL DAT_NEW (LLOC, 'OFF', '_DOUBLE', 1, NT, STATUS )
        CALL DAT_NEW (LLOC, 'DURATION', '_REAL', 1, NT, STATUS )

        CALL CMP_PUT1D (LLOC, 'ON', NT, %val(SXPNTR), STATUS)
        CALL CMP_PUT1D (LLOC, 'OFF', NT, %val(EXPNTR), STATUS)
        CALL CMP_PUT1R (LLOC, 'DURATION', NT, %val(DXPNTR), STATUS)
*       CALL DAT_ANNUL (LLOC, STATUS)
*
      ELSE

        CALL DYN_MAPR(1, NT, DXPNTR, STATUS)
        CALL ARR_INIT1R(1.0,NT,%val(DXPNTR),STATUS)

      ENDIF


* correct data array, variance and set quality bad if no
* exposure in a given bin.
      CALL XRTCORR_DOIT(LOCOUT, HEAD, NX, NY, NT, NP, NR, NWIRE,
     &          NTHRESH, %val(CDPNTR), %val(PVSING), PSING,
     &          %val(CIVPNTR), %val(CTPNTR), %val(CWPNTR), %val(DXPNTR),
     &          %val(DPNTR), %val(VPNTR), %val(QPNTR),STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Reorder the dataset if necessary
      IF (ORDER(1) .NE. 1 .OR. ORDER(2) .NE. 2 .OR.
     &         ORDER(3) .NE. 3 .or. ORDER(4) .NE. 4) THEN
*
         DO LP=1,7
            REORD(ORDER(LP)) = LP
            RDIMS(ORDER(LP)) = DIMS(LP)
         ENDDO
*
         CALL XRT_AXSWAP_R(DIMS, %val(DPNTR), REORD, RDIMS,
     &                                     %val(TDPNTR), STATUS)
         CALL XRT_AXSWAP_R(DIMS, %val(VPNTR), REORD, RDIMS,
     &                                     %val(TVPNTR), STATUS)
         CALL XRT_AXSWAP_B(DIMS, %val(QPNTR), REORD, RDIMS,
     &                                     %val(TQPNTR), STATUS)
*
      ENDIF
*
* Change the normalised flag to true on the time axis - if it exists
      IF (NT .GT. 1) THEN
         CALL BDA_PUTAXNORM(LOCOUT, ORDER(3), .TRUE., STATUS)
      ENDIF
*
* Write energy dependent factors to the instrument box within the
* datafile if this is a spectral dataset - but not if it is a spectral
* image, because then the vignetting will have been done or something.
* This area needs reviewing:
      IF (NP .GT. 1 .AND. NX .EQ. 1 .AND. .NOT. LHRI) THEN
*
         CALL DYN_MAPR(1, NENERGY, TEPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping temporary space')
            GOTO 999
         ENDIF
*
         CALL XRTCORR_ENCORR(LOCOUT, NENERGY, NR, %val(CVPNTR),
     &                        %val(CPPNTR), %val(TEPNTR), STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
         CALL DYN_UNMAP(TEPNTR)
*
      ENDIF

      IF (LECORR) THEN
* Change data units to counts / second
        CALL BDA_PUTUNITS(LOCOUT, 'counts / second', STATUS)
        CALL BDA_PUTLABEL(LOCOUT, 'Intensity', STATUS)
      ENDIF
*
* Amend corrections structure
      CALL XRTCORR_WRIPROC(LOCOUT, VFLAG, DFLAG, LECORR, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Write history record
*   Trace path of input data.
      CALL USI_NAMEI(NLINES,PATH,STATUS)
*
      CALL HIST_ADD(LOCOUT, VERSION, STATUS)
*
      CALL HIST_PTXT(LOCOUT, NLINES, PATH, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing history record')
      ENDIF
*
999   CONTINUE
*
      CALL AST_CLOSE(STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTCORR',STATUS)
      ENDIF
*
      END


*+  XRTCORR_CALCEXP - Calculates exposure time in each time bin
      SUBROUTINE XRTCORR_CALCEXP(HEAD, NT, WORK, START, END,
     &                            EXPOS, STATUS)
*    Description :
*     <description of what the subroutine does - for user info>
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Richard Saxton
*    History :
*     1990 - original
*     1-Feb-1993 - reads diferent header depending on which type
*                  of raw data is being used.
*     6-Jan-1994 - handles rationalised files
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
      INCLUDE 'XRTLIB(INC_XRTSRT)'
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Import :
      RECORD /CORR/ HEAD                 !Source file header info
      INTEGER NT                         !Number of time bins
      DOUBLE PRECISION WORK(MAXRAN*2)    !Workspace array
*    Import-Export :
      DOUBLE PRECISION START(NT),END(NT) !Start and end time of a time bin
      REAL EXPOS(NT)                     !Exposure in each time bin
*    Export :
*     <declarations and descriptions for exported arguments>
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      RECORD /XRT_SCFDEF/ SRT            !Sort control structure
      RECORD /XRT_HEAD/ RAWHD            !Raw data header structure
*
      REAL TBIT
      INTEGER TLP,LP2
      CHARACTER*50 VERS                  !SASS processing version date
      INTEGER SPTR,UPTR                  !Pointers to time arrays
      INTEGER NRAW
      DOUBLE PRECISION OVRLAP
*    Local data :
*     <any DATA initialisations for local variables>
*-
* Exposure time is exposure time of the whole file if there is one time bin
      IF (NT .EQ. 1) THEN
*
         EXPOS(1)=HEAD.EXPOS
         START(1)=HEAD.TMIN(1)
         END(1)=HEAD.TMAX(HEAD.NTRANGE)
*
      ELSE
*
         IF (HEAD.ORIGIN .EQ. 'OMD') THEN
*           Read the MPE style header
            SRT.ROOTNAME=HEAD.RTNAME
*
            CALL XRT_RDHEAD(.FALSE., SRT, RAWHD, VERS, STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('Error reading header file')
               GOTO 999
            ENDIF
*
         ELSE
*          Read header infor from rationalised fits data
           CALL RAT_GETXRTHEAD(HEAD.RTNAME, RAWHD, STATUS)
         ENDIF
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Combine the times from the header with the times over which the data
*   was sorted.
*     Create work space to hold the raw selection times
         CALL DYN_MAPD(1, RAWHD.NTRANGE*2, SPTR, STATUS)
*
*     Create the array of user selection times
         CALL DYN_MAPD(1, HEAD.NTRANGE*2, UPTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping dynamic memory')
            GOTO 999
         ENDIF
*
*     Put SASS selection times into one array
         CALL XRT_LIVEWIND(RAWHD.NTRANGE, RAWHD.TSTART,
     &                                 RAWHD.TEND, %val(SPTR))
*
*     Put user selection times into one array
         CALL XRT_LIVEWIND(HEAD.NTRANGE, HEAD.TMIN,
     &                                 HEAD.TMAX, %val(UPTR))
*
*     Merge these with the user selection times in the sort box
         CALL XRT_TIMSET(RAWHD.NTRANGE*2, RAWHD.NTRANGE*2, %val(SPTR),
     &                HEAD.NTRANGE*2, HEAD.NTRANGE*2, %val(UPTR),
     &                MAXRAN*2, NRAW, WORK, OVRLAP)
*
* Calculate the exposure time in each bin
         DO TLP=1,NT
*
            EXPOS(TLP) = 0.0
*
*    Find upper and lower values of this time bin
            START(TLP) = HEAD.TMIN(1) + HEAD.TSCALE(1) * (TLP-1)
            END(TLP) = HEAD.TMIN(1) + HEAD.TSCALE(1) * TLP
*
*    Loop over each time segment from the input file header
            DO LP2=1,NRAW
*
*      Calculate the contribution of this time range to this time bin
               TBIT = MIN( WORK(LP2*2), END(TLP) ) -
     &                       MAX( WORK(1+2*(LP2-1)), START(TLP) )
*
               EXPOS(TLP) = EXPOS(TLP) + MAX(0.0, TBIT)
*
            ENDDO
*
D            WRITE(3,*)EXPOS(TLP)
         ENDDO
*
      ENDIF

 999  IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTCORR_CALCEXP',STATUS)
      ENDIF
*
      END

*+  XRTCORR_DTIME_PSPC - Calculates dead time correction per time bin
      SUBROUTINE XRTCORR_DTIME_PSPC(HEAD,NT,QUAL,DCORR,DFLAG,STATUS)
*    Description :
*     Calculates the dead time correction for each time bin in the input
*     datafile. Finds the mean event rate received by the detector
*     in each time bin and then calculates the dead time for each time
*     bin.
*    History :
*     13-Nov-1990   original (LTVAD::RDS)
*      2-May-1991   uses quality of each time bin to generate error
*                   messages (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
      INCLUDE 'XRTLIB(INC_XRTSRT)'
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Status :
      INTEGER STATUS
*    Import :
      RECORD /CORR/ HEAD            ! Header from file
      RECORD /XRT_HEAD/ XRTHDR      ! header to get origin
      RECORD /XRT_SCFDEF/ SRT         ! Sort control structure
      INTEGER NT                    ! Number of time bins in input file
      BYTE QUAL(NT)                 ! Quality of each time bin
*    Import-Export :
*    Export :
      REAL DCORR(NT)                ! Dead time correction factor (>1.0)
      LOGICAL DFLAG                 ! Have dead time corrections been calc'd ?
*    Functions :
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
*    Local variables :
      CHARACTER*20 EXT              ! filename extension
      CHARACTER*20 COL              ! HDS column/array name
      CHARACTER*132 ERFILE          ! Eventrate HDS file
      CHARACTER*(DAT__SZLOC) ERLOC,TLOC
      CHARACTER*(DAT__SZLOC) ALOC1,ALOC2,ALOC3
      INTEGER NTIMES                         !Number of els. in eventrate file
      INTEGER EV_TPNTR                       !Eventrate time array
      INTEGER EV1_PNTR,EV2_PNTR,EV3_PNTR     !Eventrate arrays
      INTEGER W1PNTR,W2PNTR,W3PNTR,W4PNTR    !Workspace pointers
      CHARACTER*80 VERS
*-

* Get data origin
      IF (HEAD.ORIGIN.EQ.'OMD') THEN
         SRT.ROOTNAME = HEAD.RTNAME
         CALL XRT_RDHEAD(.FALSE.,SRT,XRTHDR,VERS,STATUS)
      ELSE
         CALL RAT_GETXRTHEAD(HEAD.RTNAME,XRTHDR,STATUS)
      ENDIF
* Open eventrate file
      CALL RAT_HDLOOKUP(XRTHDR,'EVRATE','EXTNAME',EXT,STATUS)
      ERFILE = HEAD.RTNAME(1:CHR_LEN(HEAD.RTNAME))//EXT
*
      CALL HDS_OPEN(ERFILE, 'READ', ERLOC, STATUS)
*
* If file coundnt be opened set dead time correction to 1.0
      IF (STATUS .NE. SAI__OK) THEN
*
         CALL MSG_SETC('ERFILE', ERFILE)
         CALL MSG_PRNT('Error opening eventrate file ^ERFILE ')
*
      ELSE
*
*   Read eventrates as a function of time
         CALL RAT_HDLOOKUP(XRTHDR,'EVRATE','TIME',COL,STATUS)
         CALL DAT_FIND(ERLOC, COL, TLOC, STATUS)
         CALL DAT_SIZE(TLOC, NTIMES, STATUS)
*
*   Map the time array
         CALL DAT_MAPR(TLOC, 'READ', 1, NTIMES, EV_TPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping eventrate TIME array')
         ENDIF
*
*   Map the eventrates
         CALL RAT_HDLOOKUP(XRTHDR,'EVRATE','A2_AL',COL,STATUS)
         CALL DAT_FIND(ERLOC, COL, ALOC1, STATUS)
         CALL DAT_MAPR(ALOC1, 'READ', 1, NTIMES, EV1_PNTR, STATUS)
*
         CALL RAT_HDLOOKUP(XRTHDR,'EVRATE','XACC',COL,STATUS)
         CALL DAT_FIND(ERLOC, COL, ALOC2, STATUS)
         CALL DAT_MAPR(ALOC2, 'READ', 1, NTIMES, EV2_PNTR, STATUS)
*
         CALL RAT_HDLOOKUP(XRTHDR,'EVRATE','XTRANSM',COL,STATUS)
         CALL DAT_FIND(ERLOC, COL, ALOC3, STATUS)
         CALL DAT_MAPR(ALOC3, 'READ', 1, NTIMES, EV3_PNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping eventrate AC_EV_RATE array')
         ENDIF
*
*  Map two work arrays
         CALL DYN_MAPR(1, NT, W1PNTR, STATUS)
         CALL DYN_MAPR(1, NT, W2PNTR, STATUS)
         CALL DYN_MAPR(1, NT, W3PNTR, STATUS)
         CALL DYN_MAPI(1, NT, W4PNTR, STATUS)
*
*  Calculate the correction for each time bin
         CALL XRTCORR_DTIME_PSPC_DOIT(HEAD, NTIMES, %val(EV_TPNTR),
     &         %val(EV1_PNTR), %val(EV2_PNTR), %val(EV3_PNTR), NT,
     &         QUAL, %val(W1PNTR), %val(W2PNTR), %val(W3PNTR),
     &                                 %val(W4PNTR), DCORR, STATUS)

         CALL DYN_UNMAP(W1PNTR,STATUS)
         CALL DYN_UNMAP(W2PNTR,STATUS)
         CALL DYN_UNMAP(W3PNTR,STATUS)
         CALL DYN_UNMAP(W4PNTR,STATUS)

      ENDIF
*
      IF (STATUS .NE. SAI__OK) THEN
*
         STATUS=SAI__OK

         CALL ARR_INIT1R( 1.0, NT, DCORR, STATUS )
*
         CALL MSG_PRNT('not performing dead time correction')
*
         DFLAG=.FALSE.
*
      ELSE
         DFLAG=.TRUE.
      ENDIF

 999  IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTCORR_DTIME_PSPC',STATUS)
      ENDIF
*
      END

*+  XRTCORR_DTIME_PSPC_DOIT - Calcs. dead time correction for each time bin
      SUBROUTINE XRTCORR_DTIME_PSPC_DOIT(HEAD,NTIMES,EVTIM,A1LL,AXE,
     &                                   AEXE,NT,QUAL,TOTA1LL,TOTAXE,
     &                                    TOTAEXE,COUNT,DCORR,STATUS)
*    Description :
*         Calculates the dead time correction for each time bin.
*      At present a fixed correction of 250 microseconds per event is
*      multiplied by the average event rate in each time bin to
*      calculate the dead time correction array
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'               !Quality values
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ HEAD               !Header structure for input file
*
      INTEGER NTIMES                   !Number of eventrate records
      REAL EVTIM(NTIMES)               !Times of event rates
      REAL A1LL(NTIMES)                !Eventrates
      REAL AXE(NTIMES)                 !Eventrates
      REAL AEXE(NTIMES)                !Eventrates
      INTEGER NT                       !Number of time bins in input file
      BYTE QUAL(NT)                    !Quality of each time bin
      REAL TOTA1LL(NT)                 !Workspace for total events in timebin
      REAL TOTAXE(NT)                  !Workspace for total events in timebin
      REAL TOTAEXE(NT)                 !Workspace for total events in timebin
      INTEGER COUNT(NT)                !Workspace to normalise event total
*    Import-Export :
      REAL DCORR(NT)                   !Dead time correction array
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
      REAL DEADTP
        PARAMETER (DEADTP=250.0)
*    Local variables :
      INTEGER LP,TBIN
      INTEGER NOCORR                           !No of bins with bad evrate data
      INTEGER GQERR                            !No of bins with no evrate data
      REAL A1LL_AV,AXE_AV,AEXE_AV              ! Average events per time bin
      REAL FLIVE,FLIVE1,FLIVE2
      REAL DTOT
      REAL ETIM                                ! S/C time in U.T.
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN

*    Initialise arrays
      CALL ARR_INIT1R( 0.0, NT, TOTA1LL, STATUS )
      CALL ARR_INIT1R( 0.0, NT, TOTAXE, STATUS )
      CALL ARR_INIT1R( 0.0, NT, TOTAEXE, STATUS )
      CALL ARR_INIT1I( 0, NT, COUNT, STATUS )
*
* Loop over each event rate time bin and calculate total number of events
* received during each datafile time bin
      DO LP=1,NTIMES
*
*   Convert the eventrate time into UT.
         ETIM = EVTIM(LP) - HEAD.SCBASE
*
         IF (ETIM .GE. HEAD.TMIN(1) .AND.
     &       ETIM .LE. HEAD.TMAX(HEAD.NTRANGE)) THEN
*
*      Calculate time axis bin number
            TBIN = INT( (ETIM - HEAD.TMIN(1)) / HEAD.TSCALE(1) + 1.0 )
            TBIN = MIN(TBIN, NT)
*
*      Add up eventrates for this time bin
            TOTA1LL(TBIN) = TOTA1LL(TBIN) + A1LL(LP)
            TOTAXE(TBIN) = TOTAXE(TBIN) + AXE(LP)
            TOTAEXE(TBIN) = TOTAEXE(TBIN) + AEXE(LP)
            COUNT(TBIN) = COUNT(TBIN) + 1
         ENDIF
*
      ENDDO
*
* Loop over each time bin
      DTOT = 0.0
      NOCORR = 0
      GQERR = 0
*
      DO LP=1,NT
*
*   If eventrates were recorded in this time bin
         IF (COUNT(LP) .NE. 0) THEN
*
            A1LL_AV = TOTA1LL(LP) / COUNT(LP)
            AXE_AV = TOTAXE(LP) / COUNT(LP)
            AEXE_AV = TOTAEXE(LP) / COUNT(LP)
*
            CALL XRTCORR_LIVTIM(A1LL_AV,DEADTP,AXE_AV,AEXE_AV,FLIVE1,
     &                          FLIVE2,FLIVE,STATUS)
*
            IF (STATUS .EQ. SAI__OK) THEN
               DCORR(LP) = 1.0 / (FLIVE)
            ELSE
               DCORR(LP) = 1.0
               NOCORR = NOCORR + 1
               STATUS = SAI__OK
            ENDIF
*
*    If no eventrate data was available for this time then set the
*    correction value to 1.0 and output a warning
         ELSE
*
            DCORR(LP) = 1.0
*
*      If quality of this bin is good - increment error counter
            IF (QUAL(LP) .EQ. QUAL_GOOD) GQERR = GQERR + 1

         ENDIF
*
         DTOT = DTOT + DCORR(LP)
*
      ENDDO
*
*   If some times were outside the range of the eventrate data output
*   a warning message.
      IF (GQERR .GT. 0) THEN
         CALL MSG_SETI('N', GQERR)
         CALL MSG_PRNT('Warning: insufficient eventrate data '/
     &           /'to calculate dead time correction for ^N timebins')
         CALL MSG_PRNT('Correction for these bins has been set to 1.0')
      ENDIF
*
*  If the eventrates were zero for some time bins then say so
      IF (NOCORR .GT. 0) THEN
         CALL MSG_SETI('N', NOCORR)
         CALL MSG_PRNT('Warning: bad eventrate data for ^N timebins')
         CALL MSG_PRNT('Correction for these bins has been set to 1.0')
      ENDIF
*
      CALL MSG_SETR('DMEAN', DTOT/REAL(NT))
      CALL MSG_PRNT('Mean dead time correction : ^DMEAN')
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTCORR_DTIME_PSPC_DOIT',STATUS)
      ENDIF
*
      END



*+  XRTCORR_DTIME_HRI - Calculates dead time correction per time bin
      SUBROUTINE XRTCORR_DTIME_HRI(HEAD, NT, QUAL, DCORR, DFLAG, STATUS)
*    Description :
*     Calculates the dead time correction for each time bin in the input
*     datafile. Finds the mean event rate received by the detector
*     in each time bin and then calculates the dead time for each time
*     bin.
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
      INCLUDE 'XRTLIB(INC_XRTSRT)'
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Status :
      INTEGER STATUS
*    Import :
      RECORD /CORR/ HEAD            ! Header from file
      RECORD /XRT_HEAD/ XRTHDR      ! header to get origin
      RECORD /XRT_SCFDEF/ SRT         ! Sort control structure
      INTEGER NT                    ! Number of time bins in input file
      BYTE QUAL(NT)                 ! Quality of each time bin
*    Import-Export :
*    Export :
      REAL DCORR(NT)                ! Dead time correction factor (>1.0)
      LOGICAL DFLAG                 ! Have dead time corrections been calc'd ?
*    Functions :
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
*    Local variables :
      CHARACTER*20 EXT              ! filename extension
      CHARACTER*20 COL              ! HDS column/array name
      CHARACTER*132 ERFILE          ! Eventrate HDS file
      CHARACTER*(DAT__SZLOC) ERLOC,TLOC,PSLOC
      INTEGER NTIMES                         !Number of els. in eventrate file
      INTEGER T_PNTR                         !Eventrate time array
      INTEGER PS_PNTR                        !Primary eventrate array
      INTEGER W1PNTR,W2PNTR                  !Workspace pointers
      CHARACTER*80 VERS
*-

* Get data origin
      IF (HEAD.ORIGIN.EQ.'OMD') THEN
         SRT.ROOTNAME = HEAD.RTNAME
         CALL XRT_RDHEAD(.FALSE.,SRT,XRTHDR,VERS,STATUS)
      ELSE
         CALL RAT_GETXRTHEAD(HEAD.RTNAME,XRTHDR,STATUS)
      ENDIF
* Open eventrate file
      CALL RAT_HDLOOKUP(XRTHDR,'EVRATE','EXTNAME',EXT,STATUS)
      ERFILE = HEAD.RTNAME(1:CHR_LEN(HEAD.RTNAME))//EXT
*
      CALL HDS_OPEN(ERFILE, 'READ', ERLOC, STATUS)
*
* If file coundnt be opened set dead time correction to 1.0
      IF (STATUS .NE. SAI__OK) THEN
*
         CALL MSG_SETC('ERFILE', ERFILE)
         CALL MSG_PRNT('Error opening eventrate file ^ERFILE ')
*
      ELSE
*
*   Read eventrates as a function of time
         CALL RAT_HDLOOKUP(XRTHDR,'EVRATE','TIME',COL,STATUS)
         CALL DAT_FIND(ERLOC, COL, TLOC, STATUS)
         CALL DAT_SIZE(TLOC, NTIMES, STATUS)
*
*   Map the time array
         CALL DAT_MAPR(TLOC, 'READ', 1, NTIMES, T_PNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping eventrate TIME array')
         ENDIF
*
*   Map the eventrates
         CALL RAT_HDLOOKUP(XRTHDR,'EVRATE','PS_VALID',COL,STATUS)
         CALL DAT_FIND(ERLOC, COL, PSLOC, STATUS)
         CALL DAT_MAPR(PSLOC, 'READ', 1, NTIMES, PS_PNTR, STATUS)
*
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping eventrate  array')
         ENDIF
*
*  Map two work arrays
         CALL DYN_MAPR(1, NT, W1PNTR, STATUS)
         CALL DYN_MAPI(1, NT, W2PNTR, STATUS)
*
*
*  Calculate the correction for each time bin
         CALL XRTCORR_DTIME_HRI_DOIT(HEAD, NTIMES, %val(T_PNTR),
     &         %val(PS_PNTR), NT,QUAL,%val(W1PNTR),%val(W2PNTR),
     &                                              DCORR,STATUS)
*
         CALL DYN_UNMAP(W1PNTR,STATUS)
         CALL DYN_UNMAP(W2PNTR,STATUS)

      ENDIF
*
      IF (STATUS .NE. SAI__OK) THEN
*
         STATUS=SAI__OK

         CALL ARR_INIT1R( 1.0, NT, DCORR, STATUS )
*
         CALL MSG_PRNT('not performing dead time correction')
*
         DFLAG=.FALSE.
*
      ELSE
         DFLAG=.TRUE.
      ENDIF

 999  IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTCORR_DTIME_HRI',STATUS)
      ENDIF
*
      END



*+  XRTCORR_DTIME_HRI_DOIT - Calcs. dead time correction for each time bin
      SUBROUTINE XRTCORR_DTIME_HRI_DOIT(HEAD,NTIMES,EVTIM,RATE,
     &                                      NT,QUAL,TOTAL,COUNT,
     &                                              DCORR,STATUS)
*    Description :
*         Calculates the dead time correction for each time bin.
*      At present a fixed correction of 0.85 milliseconds per event is
*      multiplied by the average event rate in each time bin to
*      calculate the dead time correction array
*    Authors :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'               !Quality values
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ HEAD               !Header structure for input file
*
      INTEGER NTIMES                   !Number of eventrate records
      REAL EVTIM(NTIMES)               !Times of event rates
      REAL RATE(NTIMES)                !Eventrates
      INTEGER NT                       !Number of time bins in input file
      BYTE QUAL(NT)                    !Quality of each time bin
      REAL TOTAL(NT)                   !Workspace for total events in timebin
      INTEGER COUNT(NT)                !Workspace to normalise event total
*    Import-Export :
      REAL DCORR(NT)                   !Dead time correction array
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
      REAL DEADTP
        PARAMETER (DEADTP=0.85/1.0E3)
*    Local variables :
      INTEGER LP,TBIN
      INTEGER NOCORR                           !No of bins with bad evrate data
      INTEGER GQERR                            !No of bins with no evrate data
      REAL AVRATE                              ! Average events per time bin
      REAL DTOT
      REAL ETIM                                ! S/C time in U.T.
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN

*    Initialise arrays
      CALL ARR_INIT1R( 0.0, NT, TOTAL, STATUS )
      CALL ARR_INIT1I( 0, NT, COUNT, STATUS )
*
* Loop over each event rate time bin and calculate total number of events
* received during each datafile time bin
      DO LP=1,NTIMES
*
*   Convert the eventrate time into UT.
         ETIM = EVTIM(LP) - HEAD.SCBASE
*
         IF (ETIM .GE. HEAD.TMIN(1) .AND.
     &       ETIM .LE. HEAD.TMAX(HEAD.NTRANGE)) THEN
*
*      Calculate time axis bin number
            TBIN = INT( (ETIM - HEAD.TMIN(1)) / HEAD.TSCALE(1) + 1.0 )
            TBIN = MIN(TBIN, NT)
*
*      Add up eventrates for this time bin
            TOTAL(TBIN) = TOTAL(TBIN) + RATE(LP)
            COUNT(TBIN) = COUNT(TBIN) + 1
         ENDIF
*
      ENDDO
*
* Loop over each time bin
      DTOT = 0.0
      NOCORR = 0
      GQERR = 0
*
      DO LP=1,NT
*
*   If eventrates were recorded in this time bin
         IF (COUNT(LP) .NE. 0) THEN
*
            AVRATE = TOTAL(LP) / REAL(COUNT(LP))

            DCORR(LP) = 1.0 + AVRATE*DEADTP

*
*    If no eventrate data was available for this time then set the
*    correction value to 1.0 and output a warning
         ELSE
*
            DCORR(LP) = 1.0
*
*      If quality of this bin is good - increment error counter
            IF (QUAL(LP) .EQ. QUAL__GOOD) GQERR = GQERR + 1

         ENDIF
*
         DTOT = DTOT + DCORR(LP)
*
      ENDDO
*
*   If some times were outside the range of the eventrate data output
*   a warning message.
      IF (GQERR .GT. 0) THEN
         CALL MSG_SETI('N', GQERR)
         CALL MSG_PRNT('Warning: insufficient eventrate data '/
     &           /'to calculate dead time correction for ^N timebins')
         CALL MSG_PRNT('Correction for these bins has been set to 1.0')
      ENDIF
*
*  If the eventrates were zero for some time bins then say so
      IF (NOCORR .GT. 0) THEN
         CALL MSG_SETI('N', NOCORR)
         CALL MSG_PRNT('Warning: bad eventrate data for ^N timebins')
         CALL MSG_PRNT('Correction for these bins has been set to 1.0')
      ENDIF
*
      CALL MSG_SETR('DMEAN', DTOT/REAL(NT))
      CALL MSG_PRNT('Mean dead time correction : ^DMEAN')
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTCORR_DTIME_HRI_DOIT',STATUS)
      ENDIF
*
      END




*+XRTCORR_ENCORR   Writes energy dependent correction factors into file
      SUBROUTINE XRTCORR_ENCORR(LOCOUT, NENERGY, NR, RVCORR, PCORR,
     &                                               TOT_EN, STATUS)
*    Description :
*     Multiplies the vignetting and point spread corrections for each
*     energy and writes the result to a structure:
*            MORE.ASTERIX.INSTRUMENT.ENCORR
*    History :
*     19-Nov-1990   original (LTVAD::RDS)
*     22-May-1991   modified to handle radial bins
*    Type definitions :
      IMPLICIT NONE
*    Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      CHARACTER*(DAT__SZLOC) LOCOUT        ! Locator to output file
      INTEGER NENERGY                      ! Number of trial energies
      INTEGER NR                           ! Number of radial bins
      REAL RVCORR(NENERGY,NR)              ! Vignetting correction
      REAL PCORR(NENERGY)                  ! Point spread correction
*    Import-Export :
      REAL TOT_EN(NENERGY)                 ! Temp array to hold total corr.
*    Export :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) ILOC          ! Locator to instrument box
      CHARACTER*(DAT__SZLOC) ELOC          ! Locator to error corrections
      CHARACTER*(DAT__SZLOC) CLOC          ! Locator to CELL
      INTEGER RLP,ELP
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Get locator to instrument box in output file
      CALL BDA_LOCINSTR(LOCOUT, ILOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error getting locator to instrument box '/
     &                /'in output file')
         GOTO 999
      ENDIF
*
* Create energy corrections array structure
      CALL DAT_NEW(ILOC, 'ENCORR', 'Correction', 1, NR, STATUS)
      CALL DAT_FIND(ILOC, 'ENCORR', ELOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error creating energy corrections array')
         GOTO 999
      ENDIF
*
* Loop over radial bins
      DO RLP = 1,NR
*
D        WRITE(*,*) RLP
*   Calculate total energy dependent correction factor
         DO ELP=1,NENERGY
            TOT_EN(ELP) = RVCORR(ELP,RLP) * PCORR(ELP)
         ENDDO
*
*   Get locator to this cell
         CALL DAT_CELL(ELOC, 1, RLP, CLOC, STATUS)
*
*   Write total correction array to output file
         CALL HDX_PUTR(CLOC, 'DATA_ARRAY', NENERGY, TOT_EN, STATUS)
*
*   Annul cell locator
         CALL DAT_ANNUL(CLOC, STATUS)
*
      ENDDO
*
*   Annul corrections locator
      CALL DAT_ANNUL(ELOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing energy correction array')
      ENDIF

 999  IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTCORR_ENCORR',STATUS)
      ENDIF

      END

*+  XRTCORR_DOIT - apply corrections to an XRT data array
      SUBROUTINE XRTCORR_DOIT(LOC, HEAD, NX, NY, NT, NP, NR,
     &                    NWIRE, NTHRESH, DCORR, VCORR, PCORR, VIMCOR,
     &                    TCORR, WCORR, EXPOS, DATA, VAR, QUAL, STATUS)
*    Description :
*      Applies corrections to an XRT data array and exposure corrects.
*      Rem: at present most of these corrections are just one element.
*      hence NWIRE and NTHRESH will be 1.
*    History :
*     13-Nov-1990   original (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Structure definitions
      INCLUDE 'XRTLIB(INC_CORR)'
*    Status :
      INTEGER STATUS
*    Import :
      CHARACTER*(DAT__SZLOC) LOC      ! Locator to output file
      RECORD /CORR/ HEAD              ! Header info. for file
      INTEGER NX,NY,NT,NP,NR          ! Dimensions of data array
      INTEGER NWIRE                   ! Dimensions of wire factor
      INTEGER NTHRESH                 ! Dimensions of thresholding factor
      REAL DCORR(NT)                  ! Dead time correction
      REAL TCORR(NTHRESH)             ! Thresholding correction
      REAL PCORR                      ! Point spread correction
      REAL VIMCOR(NX,NY,NP)           ! IMAGE vignetting correction
      REAL VCORR(NR)                  ! Non-image vignetting correction
      REAL WCORR(NWIRE)               ! Wires correction
      REAL EXPOS(NT)                  ! Exposure times
*    Import-Export :
      REAL DATA(NX,NY,NT,NP,NR)       ! Data array
      REAL VAR(NX,NY,NT,NP,NR)        ! Variance array
      BYTE QUAL(NX,NY,NT,NP,NR)       ! Quality array
*    Export :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) HLOC     ! Locator to header block
      REAL TOT_CORR                   ! Total correction factor for this bin
      INTEGER PLP,TLP,XLP,YLP,RLP

*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

* Apply corrections to each bin
      DO RLP=1,NR
        DO PLP=1,NP
           DO TLP=1,NT
D                  WRITE(2,*)DCORR(TLP),EXPOS(TLP)
             DO YLP=1,NY
                DO XLP=1,NX
*
                  IF (NX .GT. 1) VCORR(RLP) = VIMCOR(XLP,YLP,PLP)
*
*       Check exposure time is non-zero
                  IF (EXPOS(TLP) .GT. 0.0 .AND.
     &               QUAL(XLP,YLP,TLP,PLP,RLP) .EQ. QUAL_GOOD) THEN
*
                     TOT_CORR = DCORR(TLP) * TCORR(1) * PCORR *
     &                           VCORR(RLP) * WCORR(1) / EXPOS(TLP)
*
                     DATA(XLP,YLP,TLP,PLP,RLP) =
     &                     DATA(XLP,YLP,TLP,PLP,RLP) * TOT_CORR
*
                     VAR(XLP,YLP,TLP,PLP,RLP) =
     &                  VAR(XLP,YLP,TLP,PLP,RLP) * TOT_CORR * TOT_CORR
*
                  ELSEIF (QUAL(XLP,YLP,TLP,PLP,RLP) .EQ.
     &                                          QUAL_GOOD) THEN
*
                     QUAL(XLP,YLP,TLP,PLP,RLP) = QUAL_BAD
*
                  ENDIF

*    Set variance good if quality has been set bad. This is to avoid the
*    plots looking stupid. The t-series analysis routines wont use these
*    pixels anyway.
                  IF (QUAL(XLP,YLP,TLP,PLP,RLP) .NE. QUAL_GOOD) THEN
                     VAR(XLP,YLP,TLP,PLP,RLP) = 0
                  ENDIF
*
               ENDDO
             ENDDO
           ENDDO
        ENDDO
      ENDDO
*
* Write in an effective exposure value into the header, unless the file
* was a time series
      IF (NT .EQ. 1) THEN
         CALL BDA_LOCHEAD(LOC, HLOC, STATUS)
         CALL HDX_PUTR(HLOC, 'EFF_EXPOSURE', 1,
     &                           EXPOS(1)/DCORR(1), STATUS)
      ENDIF

 999  IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTCORR_DOIT',STATUS)
      ENDIF
*
      END

*+XRTCORR_GETDATA    Maps the data array and reorders if necessary
      SUBROUTINE XRTCORR_GETDATA(LOCIN, HEAD, ODIMS, ORDER, DPNTR,
     &              TDPNTR, VPNTR, TVPNTR, QPNTR, TQPNTR, MASK, STATUS)
*    Description :
*      Maps the data_array and puts it in the axis order, X,Y,Time,CPHA
*      Then accesses header information.
*    Environment parameters :
*      AXTYPE       INTEGER      The axis type:
*                                     X=1, Y=2, Time=3, CORR_PH=4,
*                                     RADIAL=5, OTHER=6
*    Method :
*     The data, variance and quality arrays are mapped at first and if
*     necessary, they are reordered so that the dimensions are in the
*     order X, Y, Time, CORR_PH, RADIAL. If the variance array is not
*     present in
*     the datafile a dummy array is created and set to the value of
*     the data array
*     Similarly if Quality is not already present, it is created and set to
*     zero.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     1-Feb-1990    original  (LTVAD::RDS)
*    21-MAY-1991    caters for radial bins
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      CHARACTER*(DAT__SZLOC) LOCIN       ! Locator to input fle
*    Import-Export :
      RECORD /CORR/ HEAD
*    Export :
      INTEGER ODIMS(DAT__MXDIM)          ! Dimensions of array
      INTEGER ORDER(7)                   ! The order of the new axes
      INTEGER DPNTR                      ! Pointer to the data array
      INTEGER TDPNTR                     ! Pointer to initially mapped array
      INTEGER VPNTR                      ! Pointer to the variance array
      INTEGER TVPNTR                     ! Pointer to initially mapped array
      INTEGER QPNTR                      ! Pointer to the quality array
      INTEGER TQPNTR                     ! Pointer to initially mapped array
      BYTE MASK                          ! BADBITS value
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER DIMS(DAT__MXDIM)           ! Dimensions of original array
      INTEGER NDIM,LP
      LOGICAL OK              ! Is axis array present and regular ?
      CHARACTER*40 LABEL                 ! Axis label
      CHARACTER*(DAT__SZLOC) HLOC  ! Locator to header ans instrument box
      INTEGER VNDIM, VDIMS(DAT__MXDIM)   ! Variance dimensions
      INTEGER AXTYPE                     ! Axis type
      INTEGER NELS                       ! Tot size of array
      INTEGER QNDIM,QDIMS(DAT__MXDIM)    ! Quality dimensions
      LOGICAL LVAR                       ! Are variances present in input file?
      LOGICAL LQUAL                      ! Is quality present in input file?
      REAL BASE                          ! Axis base values
      INTEGER DUM(DAT__MXDIM)            ! Dummy dimensions
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Initialise dimension arrays
      DO LP=1,7
         DIMS(LP)=1
         ODIMS(LP)=1
      ENDDO
*
* Initaialise the order array
      DO LP=1,5
         ORDER(LP)=0
      ENDDO
*
      DO LP=6,7
         ORDER(LP)=LP
      ENDDO
*
* Check data_array is present
      CALL BDA_CHKDATA(LOCIN, OK, NDIM, DIMS, STATUS)
*
      IF (STATUS .NE. SAI__OK .OR. .NOT. OK) THEN
          CALL MSG_PRNT('Error accessing data array')
          STATUS=SAI__ERROR
          GOTO 999
      ENDIF
*
      IF (NDIM .GT. 5) THEN
          CALL MSG_PRNT('Cant correct files with more than 4 dims.')
          STATUS=SAI__ERROR
          GOTO 999
      ENDIF
*
* Calc size of array
      NELS=DIMS(1)*DIMS(2)*DIMS(3)*DIMS(4)*DIMS(5)
*
* Map the data array
      CALL BDA_MAPDATA(LOCIN, 'UPDATE', TDPNTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('Error mapping data array')
          GOTO 999
      ENDIF
*
* Check variance array is present
      CALL BDA_CHKVAR(LOCIN, LVAR, VNDIM, VDIMS, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Map variance array if present
      IF (LVAR) THEN
*
         CALL BDA_MAPVAR(LOCIN, 'UPDATE', TVPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping variance array')
            GOTO 999
         ENDIF
*
      ELSE
*
* Variance array not present: create array in output file and set to
* same value as data array.
         CALL BDA_CREVAR(LOCIN, NDIM, DIMS, STATUS)
*
         CALL BDA_MAPVAR(LOCIN, 'WRITE', TVPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error creating variance array')
            GOTO 999
         ENDIF
*
* Set var. array to data array values - if data=0 then set variance to
* be one photon.
         CALL DTA_VARDATA(DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     &        DIMS(5), DIMS(6), DIMS(7), %val(TDPNTR), %val(TVPNTR))
*
      ENDIF
*
* Quality:
      CALL BDA_CHKQUAL(LOCIN, LQUAL, QNDIM, QDIMS, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Map quality array if present
      IF (LQUAL) THEN
*
         CALL BDA_MAPQUAL(LOCIN, 'UPDATE', TQPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping quality array')
            GOTO 999
         ENDIF
*
*   Get mask
         CALL BDA_GETMASK(LOCIN, MASK, STATUS)
*
      ELSE
*
* Quality array not present: create array in output file and set to zero
         CALL BDA_CREQUAL(LOCIN, NDIM, DIMS, STATUS)
*
         CALL BDA_MAPQUAL(LOCIN, 'WRITE', TQPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error creating quality array')
            GOTO 999
         ENDIF
*
*  Zero array
         CALL ARR_INIT1B(0, NELS, %val(TQPNTR),STATUS)
*
*  Create mask in output file
         MASK=QUAL_MASK
         CALL BDA_PUTMASK(LOCIN, MASK, STATUS)
*
      ENDIF
*
* Find which axis is which
      DO LP=1,NDIM
*
         CALL BDA_GETAXLABEL(LOCIN, LP, LABEL, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error reading axis label')
            GOTO 999
         ENDIF
*
         CALL CHR_UCASE(LABEL)
*
*   The order array contains the axis positions of the X,Y and TIME axes
         IF ( INDEX(LABEL(1:1),'X') .NE. 0 ) THEN
            ORDER(1)=LP
            ODIMS(1)=DIMS(LP)
         ELSEIF ( INDEX(LABEL(1:1),'Y') .NE. 0 ) THEN
            ORDER(2)=LP
            ODIMS(2)=DIMS(LP)
         ELSEIF ( INDEX(LABEL(1:1),'T') .NE. 0 .OR.
     &                    INDEX(LABEL,'TIM') .NE. 0 ) THEN
            ORDER(3)=LP
            ODIMS(3)=DIMS(LP)
         ELSEIF ( INDEX(LABEL,'CORR') .NE. 0 ) THEN
            ORDER(4)=LP
            ODIMS(4)=DIMS(LP)
         ELSEIF ( INDEX(LABEL,'RAD') .NE. 0 ) THEN
            ORDER(5)=LP
            ODIMS(5)=DIMS(LP)
         ELSE
*
*   Axis is not recognised, ask for type
            CALL MSG_SETC('LAB', LABEL)
            CALL MSG_PRNT('Axis label not recognised ^LAB')
*
            CALL USI_GET0I('AXTYPE', AXTYPE, STATUS)
*
            IF (STATUS .NE. SAI__OK) GOTO 999
*
            CALL USI_CANCL('AXTYPE', STATUS)
*
            IF (AXTYPE .LT. 1 .OR. AXTYPE .GT. 5) THEN
               CALL MSG_PRNT('*Only X,Y,time, Corr amplitude, '/
     &                      /'or radial axes are allowed*')
               STATUS=SAI__ERROR
               GOTO 999
            ELSE
               ORDER(AXTYPE)=LP
               ODIMS(AXTYPE)=DIMS(LP)
            ENDIF
*
         ENDIF
*
      ENDDO
*
* Check each order axis has been set
      IF (ORDER(1) .EQ. 0) THEN
         ORDER(1) = MAX( ORDER(2), ORDER(3), ORDER(4), ORDER(5) ) + 1
      ENDIF
*
      IF (ORDER(2) .EQ. 0) THEN
         ORDER(2) = MAX( ORDER(1), ORDER(3), ORDER(4), ORDER(5) ) + 1
      ENDIF
*
      IF (ORDER(3) .EQ. 0) THEN
         ORDER(3) = MAX( ORDER(1), ORDER(2), ORDER(4), ORDER(5) ) + 1
      ENDIF
*
      IF (ORDER(4) .EQ. 0) THEN
         ORDER(4) = MAX( ORDER(1), ORDER(2), ORDER(3), ORDER(5) ) + 1
      ENDIF
*
      IF (ORDER(5) .EQ. 0) THEN
         ORDER(5) = 5
      ENDIF
*
* Reorder the array so that the axes are in the order X,Y,T,CORR_PHA if nec.
      IF (ORDER(1) .NE. 1 .OR. ORDER(2) .NE. 2 .OR.
     &               ORDER(3) .NE. 3 .OR. ORDER(4) .NE. 4) THEN
*
*  Create mapped array to hold the reordered array
         CALL DYN_MAPR(5, ODIMS, DPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error obtaining temporary space')
            GOTO 999
         ENDIF
*
         CALL XRT_AXSWAP_R(DIMS, %val(TDPNTR), ORDER, ODIMS,
     &                                      %val(DPNTR), STATUS)
*
*  Do the same for variances if necessary
         CALL DYN_MAPR(5, ODIMS, VPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error obtaining temporary space')
            GOTO 999
         ENDIF
*
         CALL XRT_AXSWAP_R(DIMS, %val(TVPNTR), ORDER, ODIMS,
     &                                      %val(VPNTR), STATUS)
*
*  and Quality:
         CALL DYN_MAPB(5, ODIMS, QPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error obtaining temporary space')
            GOTO 999
         ENDIF
*
         CALL XRT_AXSWAP_B(DIMS, %val(TQPNTR), ORDER, ODIMS,
     &                                      %val(QPNTR), STATUS)
*
      ELSE
         DPNTR=TDPNTR
         VPNTR=TVPNTR
         QPNTR=TQPNTR
      ENDIF
*
* Access header info from the datafile
      CALL BDA_LOCHEAD(LOCIN, HLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('Error getting locator to header structure')
          GOTO 999
      ENDIF
*
* Get the sort ranges
      CALL XRT_GETSORT(LOCIN, HEAD, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Set scale factors for X, Y ranges
      IF (ODIMS(1) .EQ. 1) THEN
         HEAD.XSCALE = 2.0 * HEAD.XOUTER / ODIMS(1)
         HEAD.YSCALE = 2.0 * HEAD.YOUTER / ODIMS(2)
      ELSE
*
* Get pixel widths from the axes
         CALL BDA_GETAXVAL(LOCIN, ORDER(1), BASE, HEAD.XSCALE,
     &                                                 DUM, STATUS)
         CALL BDA_GETAXVAL(LOCIN, ORDER(2), BASE, HEAD.YSCALE,
     &                                                 DUM, STATUS)
      ENDIF
*
      HEAD.TSCALE(1) = (HEAD.TMAX(HEAD.NTRANGE) - HEAD.TMIN(1))
     &                                                 / ODIMS(3)
*
      IF (HEAD.NTRANGE .EQ. 2) HEAD.TSCALE(2)=HEAD.TSCALE(1)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRTCORR_GETDATA',STATUS)
      ENDIF
*
      END

*+  XRTCORR_GETENERGY - Reads trial energies from the detector response file
      SUBROUTINE XRTCORR_GETENERGY(RLOC, NP, PAX, NENERGY,
     &                                    EPNTR, EPHBIN, STATUS)
*    Description :
*    Bugs :
*    Authors :     R.D.Saxton   (LTVAD::RDS)
*    History :
*     16-Nov-1990  original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
*    Import :
      CHARACTER*(DAT__SZLOC) RLOC               !Locator to response file
      INTEGER NP                                !Number of PHA bins
      REAL PAX(NP)                              !Centre of each PH bin
*    Export :
      INTEGER NENERGY                           !Number of trial energies
      INTEGER EPNTR                             !Pointer to the energy array
      REAL EPHBIN(NP)                           !Energies of each PH bin
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
          EXTERNAL CHR_LEN
*    Local constants :
      INTEGER MAXHEAD
         PARAMETER (MAXHEAD=1000)
*    Local variables :
      CHARACTER*90 FHEAD(MAXHEAD)               !Array for header records
      CHARACTER*(DAT__SZLOC) MLOC,FLOC
      LOGICAL OK
      INTEGER NDIM,DMX_DIM(DAT__MXDIM)
      INTEGER NHEAD                             !Number of header records
      INTEGER NPHA_OUT                          !No. of PH bins in resp file.
      INTEGER EBWPTR,ENBPTR,EPBPTR              !Pointers
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find the size of the response array
      CALL BDA_CHKDATA(RLOC, OK, NDIM, DMX_DIM, STATUS)
*
      IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error accessing detector response array')
         GOTO 999
      ELSEIF (NDIM .NE. 2) THEN
         CALL MSG_PRNT('Response array does not have 2 dimensions')
         GOTO 999
      ENDIF
*
* Set number of ENERGY bins in the response file. NB: This relies
* on the PHA axis being first.
      NPHA_OUT=DMX_DIM(1)
      NENERGY=DMX_DIM(2)
*
*  Read the FITS header lines in this datafile
      CALL DAT_FIND(RLOC, 'MORE', MLOC, STATUS)
      CALL DAT_FIND(MLOC, 'FITS', FLOC, STATUS)
      CALL DAT_SIZE(FLOC, NHEAD, STATUS)
*
*  Check that the character array has been declared large enough
      IF (NHEAD .GT. MAXHEAD) THEN
         CALL MSG_PRNT('The FITS header is longer than allowed - '/
     &                /'refer to author of XRTCORR')
         GOTO 999
      ENDIF
*
*  Read in the FITS header
      CALL DAT_GETC(FLOC, 1, NHEAD, FHEAD, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error reading FITS structure in input file')
         GOTO 999
      ENDIF
*
* Map an array to hold the energy boundaries
      CALL DYN_MAPR(1, NENERGY, EPNTR, STATUS)
*
* Map arrays to hold the PH energy values
      CALL DYN_MAPR(1, NENERGY, EBWPTR, STATUS)
      CALL DYN_MAPR(1, NENERGY+1, ENBPTR, STATUS)
      CALL DYN_MAPR(1, NPHA_OUT, EPBPTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error mapping temporary space')
         GOTO 999
      ENDIF
*
* Get the energy values for all 256 PHA channels
      CALL XRT_RDRESPHEAD(MAXHEAD, FHEAD, NHEAD, NPHA_OUT, NENERGY,
     &           %val(EBWPTR), %val(ENBPTR), %val(EPBPTR), STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Read in the energies
      CALL XRTCORR_GETENERGY_INT(MAXHEAD, FHEAD, NHEAD, NENERGY,
     &     NPHA_OUT, %val(EPBPTR), NP, PAX, EPHBIN, %val(EPNTR), STATUS)

 999  IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTCORR_GETENEGY',STATUS)
      ENDIF
*
      END

*+  XRTCORR_GETENERGY_INT
      SUBROUTINE XRTCORR_GETENERGY_INT(MAXHEAD, FHEAD, NHEAD, NENERGY,
     :               NPHA_OUT, EPHALL, NP, PAX, EPHBIN, ENERGY, STATUS)
*    Description :
*     <description of what the subroutine does>
*    History :
*     date:  original (institution::username)
*    Type definitions :
      IMPLICIT NONE
*
      INCLUDE 'SAE_PAR'
      INTEGER STATUS
*    Import :
      INTEGER MAXHEAD
      CHARACTER*90 FHEAD(MAXHEAD)
      INTEGER NHEAD
      INTEGER NENERGY
      INTEGER NPHA_OUT
      REAL EPHALL(NPHA_OUT)             ! Energy of each PH bin
      INTEGER NP                        ! Number of PH bins
      REAL PAX(NP)                      ! PH VALUE OF EACH BIN
*    Import-Export :
      REAL EPHBIN(NP)                   ! Energy of each PH axis bin
      REAL ENERGY(NENERGY)
*    Export :
*     <declarations and descriptions for exported arguments>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER COUNT                    !Number of header record
      INTEGER LEFT
      LOGICAL JUMPOUT                  !Leave loop ?
      LOGICAL LEN                      !Energies found ?
      INTEGER ECLP,I,LP
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop over each header record
      COUNT=0
      JUMPOUT=.FALSE.
*
      DO WHILE (.NOT. JUMPOUT)
*
         COUNT=COUNT+1
*
*   Check for energy value record
         IF (INDEX (FHEAD(COUNT), 'ENERGY''') .NE. 0) THEN
*
*     Read in the various energies - note this relies on their being 5
*     records per column.
            DO ECLP=1,INT(NENERGY/5.0)

               COUNT=COUNT+1

               READ(FHEAD(COUNT)(10:90), *)
     &                        (ENERGY((ECLP-1)*5+I), I=1,5)
            ENDDO
*
            LEFT=NENERGY-INT(NENERGY/5.0)*5
*
            IF (LEFT .GT. 0) THEN

               COUNT=COUNT+1

               READ(FHEAD(COUNT)(10:90), *)
     &                        (ENERGY((ECLP-1)*5+I), I=1,LEFT)

            ENDIF
*
            LEN=.TRUE.
            JUMPOUT=.TRUE.
*
         ENDIF
*
         IF (COUNT .GE. NHEAD) JUMPOUT=.TRUE.
*
      ENDDO
*
* Check energies were found
      IF (.NOT. LEN) THEN
         CALL MSG_PRNT('Error reading trial energies from DRPM header')
         STATUS=SAI__ERROR
      ENDIF
*
* Set the energy values in each PHA axis bin
      IF (NP .GT. 1) THEN
         DO LP=1,NP
            EPHBIN(LP) = EPHALL(PAX(LP))
         ENDDO
      ENDIF
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTCORR_GETENERGY_INT', STATUS)
      ENDIF
*
      END

*+XRTCORR_GETHEAD    Gets header info. from the input datafile
      SUBROUTINE XRTCORR_GETHEAD(LOCIN, HEAD, STATUS)
*    Description :
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     19-NOV-1990    original  (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      CHARACTER*(DAT__SZLOC) LOCIN       ! Locator to input fle
*    Import-Export :
      RECORD /CORR/ HEAD
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) HLOC,ILOC   ! Locator to header ans instrument box
      CHARACTER*(DAT__SZLOC) ALOC,PLOC   ! Locator to ASTERIX and PROC boxes
      LOGICAL LBARY
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Access header info from the datafile
      CALL BDA_LOCAST(LOCIN, ALOC, STATUS)
      CALL BDA_LOCHEAD(LOCIN, HLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('Error getting locator to header structure')
          GOTO 999
      ENDIF
*
* Check whether this file has been barycentrically corrected
      CALL DAT_FIND(ALOC, 'PROCESSING', PLOC, STATUS)
      CALL DAT_THERE(PLOC, 'BARY_CORR_DONE', LBARY, STATUS)
*
      IF (LBARY) THEN
         CALL CMP_GET0L(PLOC, 'BARY_CORR_DONE', LBARY, STATUS)
      ENDIF
*
      IF (LBARY) THEN
         CALL MSG_PRNT('** Barycentric corrections have been '/
     &    /'performed on this file - so the corrections applied to '/
     &    /'each time bin will almost certainly be wrong - '/
     &    /'refer to author **')
      ELSEIF (STATUS .NE. SAI__OK) THEN
         CALL ERR_ANNUL(STATUS)
      ENDIF
*
      CALL DAT_ANNUL(PLOC, STATUS)
*
* Get exposure time
      CALL CMP_GET0R(HLOC, 'EXPOSURE_TIME', HEAD.EXPOS, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining exposure time from datafile')
         GOTO 999
      ENDIF
*
* Get locator to the instrument box
      CALL BDA_LOCINSTR(LOCIN, ILOC, STATUS)
*
* Get detector type
      CALL CMP_GET0C(ILOC, 'DETECTOR', HEAD.DET, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error accessing DETECTOR object')
         GOTO 999
      ENDIF
*
* Read in the base time in spacecraft clock units
      CALL CMP_GET0R(ILOC, 'SC_BASE', HEAD.SCBASE, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error reading base s/c clock time')
         GOTO 999
      ENDIF
*
* Get filter status if PSPC image
      IF (INDEX (HEAD.DET, 'PSPC') .NE. 0) THEN
         CALL CMP_GET0C(ILOC, 'FILTER', HEAD.FILTER, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error accessing FILTER object')
            GOTO 999
         ENDIF
*
      ENDIF

 999  IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRTCORR_GETHEAD',STATUS)
      ENDIF
*
      END

*+  XRTCORR_GETVIG - Gets vignetting corections
      SUBROUTINE XRTCORR_GETVIG(HEAD, ELOC, NENERGY, ENERGY,
     &             MEAN_ENERGY, NR, NP, VCORR, VSING, RVCORR, RVSING,
     &             VFLAG, STATUS)
*    Description :
*    Deficiencies :
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     23-MAY-1991 original
*      7-Jan-1993 allow the user the option of overriding the default
*                 off-axis angle for each radial bin, if centred on
*                 the optical axis
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ HEAD                   ! Header structure
      CHARACTER*(DAT__SZLOC) ELOC          ! Locator to eff. area file
      INTEGER NENERGY                      ! No. of energies
      REAL ENERGY(NENERGY)                 ! Trial energies
      REAL MEAN_ENERGY                     ! Average energy
      INTEGER NR                           ! Number of radial bins
      INTEGER NP                           ! Number of PH bins
*    Import-Export :
      REAL VCORR(NENERGY)                  ! Vig. correction at single R bin
      REAL VSING                           ! Mean single correction
*    Export :
      REAL RVCORR(NENERGY,NR)              ! Vignetting corrections
      REAL RVSING(NR)                      ! Mean correction for each R bin
      LOGICAL VFLAG                        ! Have VIG corrections been calc'd ?
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER RLP,ELP
      INTEGER IVAR,NCHAR
      CHARACTER*7 PARAM
      CHARACTER*2 CRLP
*    Local data :
*     <any DATA initialisations for local variables>
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set the prompt for the offaxis parameter if there is only 1 radial
*   bin
      IF (NR .EQ. 1) CALL USI_PROMT('OFFAX1','Off-axis angle'/
     &           /' to use for the source box', STATUS)

*   Loop over each radial bin
      DO RLP=1,NR
*
*      Calculate off axis angle
         CALL XRT_OFFAX(NR, RLP, HEAD)
*
*      If the box is circular and centred on the optical axis, the
*      angle to use for the vignetting correction depends on the
*      distribution of counts in the box. By default the correction
*      is calculated at a point midway between the inner and outer
*      radii. Allow the user the option of overriding this.
c         IF (HEAD.SHAPE .EQ. 'C' .AND. ABS(HEAD.XCENT) .LT. 0.001
c     &                .AND. ABS(HEAD.YCENT) .LT. 0.001) THEN
            IVAR = MIN(RLP,10)
            CALL CHR_ITOC(IVAR, CRLP, NCHAR)
            PARAM = 'OFFAX' // CRLP(1:NCHAR)
*
            CALL USI_DEF0R(PARAM, HEAD.OFFAX, STATUS)
            CALL USI_GET0R(PARAM, HEAD.OFFAX, STATUS)

            CALL USI_CANCL(PARAM, STATUS)
*
            IF (STATUS .NE. SAI__OK) GOTO 999
c         ENDIF

*
*      Calc. vignetting correction for each energy at this off axis angle.
         CALL XRT_VIGNET(HEAD, ELOC, NENERGY, ENERGY, MEAN_ENERGY,
     &                 .TRUE., NP, VCORR, VSING, VFLAG, STATUS)
*
*      Copy the corrections for this radial bin into the large
*      vignetting corrections array
         DO ELP=1,NENERGY
            RVCORR(ELP,RLP) = VCORR(ELP)
         ENDDO
*
*      Copy the single correction into the single correction array
         RVSING(RLP) = VSING
*
      ENDDO                    ! End of radial bin loop

 999  IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTCORR_GETVIG',STATUS)
      ENDIF

      END

*+ XRTCORR_LIVTIM - Calculate livetime for PSPC
      SUBROUTINE XRTCORR_LIVTIM(A1LL,DEADTP,AXE,AEXE,FLIVE1,
     &                                         FLIVE2,FLIVE,STATUS)
C
CC  Calculates PSPC livetime factor from Count Rates and Deadtime Param.
C
C************************ FFORM VERSION 1.2 ************ DD-MMM-YY HH:MM
C
CA  author : GRH        date: 13-MAR-1990 09:02
CU  update : IZZO       date: 04-SEP-1990 16:15 Implementation in EXSAS
C
CT  status: not tested
C
C   general description:
CG  The PSPC livetime factor, a value between 0 and 1) which has to be
CG  multiplied to the exposure time to obtain the effective live
CG  exposure time, is calculated from a product of two values:
CG
CG  FLIVE1 using the input A1-lower-level-discriminator count rate
CG  (A1LL) [cts/s] and the deadtime factor (DEADTP) [musec] according
CG  to the recipe in the TN-ROS-ME-ZA00-025. The deadtime parameter
CG  DEADTP, which is actually a function of mean energy and PSPC,
CG  should be specified from outside as a parameter. (Suggested value:
CG  250.0
CG
CG  FLIVE2 from the ratio between the accepted and evaluated X-ray
CG  event rate (AEXE) and the accepted X-ray event rate (AXE). A
CG  difference between those two indicates loss of events in the
CG  telemetry stream because of a busy readout.
C
C   call_var.          type I/O description
CP  A1LL                R4  I   EE-A1LL count rate from HK-data [cts/s]
CP  AXE                 R4  I   EE-AXE  count rate from HK-data [cts/s]
CP  AEXE                R4  I   EE-AEXE  count rate from HK-data [cts/s]
CP  DEADTP              R4  I   Deadtime Parameter (ca. 190-260 [musec])
CP  FLIVE1              R4    O PSPC Livetime Factor (between 0 and 1)
CP  FLIVE2              R4    O ER Livetime Factor (between 0 and 1)
CP  FLIVE               R4    O Ttotal Livetime Factor (between 0 and 1)
CP  STATUS                I     O = 0 no error
CP                              = 1 negative square root ARG (FLIVE1=1)
CP                              = 1 denominator = 0 (FLIVE2=1)
CP                              = 3
C
C   include_block_name          description
CI  R$COMMON:CGENL.CMN          general parameter common block
C
C   routines_called    type     description
CR  HFLAG               R       output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description
CX
C
C***********************************************************************
C
C   variables   meaning
C   ARG    argument under square root
C
      IMPLICIT NONE
C
      REAL          A1LL,AXE,AEXE,ARG,DEADTP,FLIVE1,FLIVE2,FLIVE
      INTEGER       STATUS
      CHARACTER*8   RTNAME
C
      DATA          RTNAME /'LIVTIM'/
C
      STATUS=0
C
C     FIRST: calculate PSPC livetime FLIVE1
C
      ARG = 2.0E-6 * A1LL * DEADTP
C
C     Check for error condition
C
      IF(ARG.LT.0.OR.ARG.GT.1)THEN
        STATUS = 1
        FLIVE1 = 1.
C        WRITE(6,*) ' *** Unreasonable count rate or deadtime ***'
      ELSE
        FLIVE1=SQRT(1.0-ARG)
      ENDIF
C
C     SECOND: calculate ER livetime FLIVE2
C     check for error condition
C
      IF(AXE.EQ.0)THEN
        STATUS = 1
        FLIVE2 = 1.
C        WRITE(6,*) ' *** EE-AXE count rate = 0 ! ***'
      ELSE
        FLIVE2=AEXE/AXE
      ENDIF
C
C     THIRD: multiply the two values
C
      FLIVE = FLIVE1 * FLIVE2
C
      END

*+XRTCORR_PSF   Calculates point spread corrections
      SUBROUTINE XRTCORR_PSF(HEAD, NENERGY, ENERGY, MEAN_EN,
     &                               NP, PCORR, PSING, STATUS)
*    Description :
*     Calculates the correction for a series of energies for the fraction
*     of counts from the source which are spread outside the source box.
*    History :
*     13-Nov-1990   original (LTVAD::RDS)
*     19-Feb-1991   Correction factors inverted for NP > 1 (LTVAD::RDS)
*     21-May-1991   Uses elliptical boxes
*    Type definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Status :
      INTEGER STATUS
*    Import :
      RECORD /CORR/ HEAD               !Header record
      INTEGER NENERGY                  !Number of trial energies
      REAL ENERGY(NENERGY)             !Trial energies
      REAL MEAN_EN                     !Mean photon energy
      INTEGER NP                       !Number of pulse height bins in file
*    Import-Export :
*    Export :
      REAL PCORR(NENERGY)              !Point spread corrections
      REAL PSING                       !Single PSF correction to apply
*                                      !to data array. This is 1.0 for files
*                                      !containing spectral info or the value
*                                      !of the correction for a mean photon en.
*    Functions:
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
*    Local constants :
*    Local variables :
      REAL FWHM                       ! Full width half max of the PSF (arcsec)
      REAL ALPHA
      REAL RAD                        ! Radius of box (arcsec)
      REAL PTOT,PMEAN
      INTEGER ELP
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

* Calculate mean box radius
      RAD = (HEAD.XOUTER + HEAD.YOUTER) * 3600. / 2.0
*
* Use mean photon energy if not a spectral file
      IF (NP .EQ. 1) THEN
*
*   Calculate PSF FWHM
         CALL XRT_FWHM(HEAD.OFFAX, MEAN_EN, FWHM)
*
         ALPHA = 4.0 * LOG(2.0) / ( FWHM * FWHM )
*
         PSING = 1.0 / (1.0 - EXP( - ALPHA * RAD * RAD ))
*
         PMEAN = PSING
*
      ELSE
*
         PTOT = 0.0
*
*  Loop over each energy
         DO ELP=1,NENERGY
*
*   Calculate PSF FWHM
            CALL XRT_FWHM(HEAD.OFFAX, ENERGY(ELP), FWHM)
*
            ALPHA = 4.0 * LOG(2.0) / ( FWHM * FWHM )
*
            PCORR(ELP) = 1.0 - EXP( - ALPHA * RAD * RAD )
*
            PTOT = PTOT + 1.0 / PCORR(ELP)
*
         ENDDO
*
*  Set the single multiplication factor to 1
         PSING = 1.0
*
         PMEAN = PTOT / NENERGY
*
      ENDIF
*
      CALL MSG_SETR('PCOR', PMEAN)
      CALL MSG_PRNT('Mean scattering correction : ^PCOR')
*
      END

*+  XRTCORR_TBINQUAL - Calculates quality for each time bin
      SUBROUTINE XRTCORR_TBINQUAL(NX, NY, NT, NP, NR,
     &                                  MASK, IQUAL, OQUAL)
*    Description :
*     Sets the quality of each time bin to zero if any quality in that
*     time slice is good or to one otherwise.
*    History :
*     3-may-1991   original (LTVAD::RDS)
*    Type definitions :
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*    Structure definitions :
*    Import :
      INTEGER NX,NY,NT,NP,NR        ! Dimensions of input quality array
      BYTE MASK                     ! BADBITS mask
      BYTE IQUAL(NX,NY,NT,NP,NR)    ! Input Quality array
*    Import-Export :
*    Export :
      BYTE OQUAL(NT)                ! Quality of each time bin
*    Local constants :
*    Local variables :
      INTEGER TLP,XLP,YLP,PLP,RLP
*
      DO TLP=1,NT
*
*   Set output quality bad
         OQUAL(TLP) = QUAL_BAD
*
         DO XLP=1,NX
           DO YLP=1,NY
            DO PLP=1,NP
             DO RLP=1,NR
*
*     If this bin is good - set output quality good
               IF ( (IQUAL(XLP,YLP,TLP,PLP,RLP) .AND. MASK) .EQ. 0) THEN
                  OQUAL(TLP) = QUAL_GOOD
               ENDIF
*
             ENDDO
            ENDDO
           ENDDO
         ENDDO
      ENDDO
*
      END

*+  XRTCORR_THRESH - Calculates thresholding correction
      SUBROUTINE XRTCORR_THRESH(NP, MEAN_ENERGY, NTHRESH, TCORR)
*    Description :
*     Calculates the correction due to genuine source counts being
*     lost due to the detector descriminator rejecting low pulse height
*     counts. If there is no pulse height information in the file
*     e.g. a time series, a mean energy needs to be used to calculate
*     the thresholding factor. How you calculate the thresholding fn
*     is anybodys guess at the moment.
*    History :
*     13-Nov-1990   original (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER NP                    ! Number of CORR_PH chans in input file
      REAL MEAN_ENERGY              ! Mean photon energy (keV)
      INTEGER NTHRESH               ! Number of threshold correction bins
*    Import-Export :
*    Export :
      REAL TCORR(NTHRESH)           ! Threshold correction factor (>1.0)
*    Local constants :
*    Local variables :
      INTEGER LP
*-
*   If the file contains no energy information a mean photon energy
*   must be assumed
      IF (NP .EQ. 1) THEN
*
*   Calculate thresholding factor
         DO LP=1,NTHRESH
C            TCORR(LP) = fn(MEAN_ENERGY)
            TCORR(LP) = 1.0
         ENDDO
*
      ELSE
*
         DO LP=1,NTHRESH
            TCORR(LP) = 1.0                      ! for now
         ENDDO
*
      ENDIF
*
      END

*+XRTCORR_WIRES   Calculates transmission loss due to wires
      SUBROUTINE XRTCORR_WIRES(NWIRE, WCORR)
*    Description :
*     Calculates the correction due to photon absorption by the window
*     support structure.
*    History :
*     20-Nov-1990   original (LTVAD::RDS)
*     8-SEP-1991    correction is now 1.0/0.79  (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER NWIRE                 ! Number of correction bins
*    Import-Export :
*    Export :
      REAL WCORR(NWIRE)             ! Correction due to wire absorption
*    Local constants :
*    Local variables :
      INTEGER LP
*-
*   Currently assume that the correction is time independent, position
*   independent and energy independent and I belive in Santa Claus
      DO LP=1,NWIRE
*
         WCORR(LP) = 1.0 / 0.79
*
      ENDDO
*
      CALL MSG_SETR('WCORR', WCORR(1))
      CALL MSG_PRNT('Mean wire correction : ^WCORR')
*
      END

*+XRTCORR_WRIPROC   Updates the processing box of the output file
      SUBROUTINE XRTCORR_WRIPROC(LOCOUT, VFLAG, DFLAG, EFLAG, STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     27-2-1990     original
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Structure definitions :
*    Import :
      CHARACTER*(DAT__SZLOC) LOCOUT                ! Locator to output file
      LOGICAL VFLAG                                ! Vignetting correction ?
      LOGICAL DFLAG                                ! Dead time correction ?
      LOGICAL EFLAG                                ! Exposure corrrection ?
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) ALOC                  ! Locator to ASTERIX box
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Get locator to the Asterix box
      CALL BDA_LOCAST(LOCOUT, ALOC, STATUS)
*
* Write in dead time value
      IF (EFLAG) THEN
        CALL HDX_PUTL(ALOC, 'PROCESSING.CORRECTED.EXPOSURE',
     &                                         1, .TRUE., STATUS)
      ENDIF
*
* Write in dead time value
      IF (DFLAG) THEN
        CALL HDX_PUTL(ALOC, 'PROCESSING.CORRECTED.DEAD_TIME',
     &                                         1, DFLAG, STATUS)
      ENDIF
*
* Write in vignetting value
      IF (VFLAG) THEN
        CALL HDX_PUTL(ALOC, 'PROCESSING.CORRECTED.VIGNETTING',
     &                                         1, VFLAG, STATUS)
      ENDIF
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing processing structure')
         CALL ERR_ANNUL(STATUS)
      ENDIF

      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRTCORR_WRIPROC',STATUS)
      ENDIF
*
      END
