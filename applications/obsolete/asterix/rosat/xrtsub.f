*+  XRTSUB - Subtracts an XRT background from an XRT source file
      SUBROUTINE XRTSUB(STATUS)
*    Description :
*     Takes an XRT source and background file and subtracts them.
*    Environment parameters :
*     SOURCE         UNIV             Name of file containing source data
*     BCKGND         UNIV             Name of file containing bckgnd data
*     OUTPUT         UNIV             Name of file containing output data
*     ROOTNAME       CHAR             Rootname of calibration files
*     PCORR          LOGICAL          Calc. position correction ?
*    Method :
*     Get names for source,bckgnd and output files
*     Check axes are consistent between the two files
*     Check the time and sumsig ranges are the same
*     Create a subtraction array, containing NT * NE elements where
*       NT is the number of time els in the source file, and NE
*       is no of corrected PH els.
*     Sum the background counts over each X,Y pixel to produce the
*       subtraction array
*     Normalise this array by the different areas of the source and bkgnd box
*     Correct for the differing instrument response at the box positions.
*     Subtract the subtraction array from the source array
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton   (LTVAD::RDS)
*    History :
*     5-Jun-1990   original
*    10-Feb-1991   now produces a variance array with the formula
*                  VAR = SRC + BGD * CFACTOR**2   (LTVAD::RDS)
*    16-may-1991   Supports the new SORT box - will optionally
*                  create a background image    (LTVAD::RDS)
*    20-May-1991   Handles radial images    - version 1.4-3
*    29-Jun-1991   Subtracts ordinary X,Y images  - version 1.4-4
*    16-Sep-1991   Subtract spectral images - version 1.4-5
*    13-Apr-1992   Attempt to estimate the particle background
*                  prior to subtraction - version 1.4-6
*     5-May-1992   Uses the background QUALITY values in calculating
*                  the background. Also in calc. the area - Version 1.6-1
*    25-Jun-1992   Optionally produces a background spectrum at the
*                  source position for max. likelihood - Version 1.6-2
*    18-Aug-1992   Uses a single parameter to ask for a background spectrum
*                  or background image etc.. at the source position. Ver 1.6-3
*     8-Sep-1992   Fixed a bug initialising SUBD and SUBV  Ver 1.6-4
*    18-Sep-1992   Fixed a bug with particle position corrections Ver 1.6-5
*    10-Nov-1992   Included reordering of the background model when
*                  applicable Ver 1.6-6
*    10-Dec-1992   Fixed a bug in XRTSUB_PARTCNT_MVR which caused
*                  MEAN_MVR=10**31 if _EVR data was incomplete - V1.6-7
*    22-Dec-1992   Fixed a bug in XRTSUB_POSCORR_SPEC which was giving
*                  position corrections for the wrong energy when subtracting
*                  spectra. V1.6-8
*     7-Jan-1993   Now links to XRTSUB_GETDATA rather than XRT_GETDATA,
*                  this fixes a problem with underestimation of the off-axis
*                  angle of a circular box around the optical axis - V1.6-9
*     5-Apr-1993   Basic support for HRI data - V1.6-10
*     6-May-1993   Updated particle model - V1.6-11
*     9-Jun-1993   Now defaults the .FITS extension in US files - V1.6-12
*    17-Sep-1993   V1.6-13 : bug in FX_ fixed (RJV)
*     9-Jan-1993   No longer prompts for a FITS file. V1.6-14
*    24-Feb-1994   Uses RDF convention filename (INC_RDF) V1.6-15 (LTVAD::JKA)
*    24-Apr-1994   (v1.7-0) for new asterix release
*    15-Aug-1994   V1.7-1 Fix when NOVR=0 in XRTSUB_PARTCNT (DJA)
*    11 Jan 1996   V1.8-2 ADI port (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :

      INCLUDE 'XRTLIB(INC_CORR)'
*
      RECORD /CORR/ SHEAD, BHEAD                  ! Contains auxiliary info.
*                                                 ! for source and bckgnd files
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
          EXTERNAL CHR_LEN
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*(DAT__SZLOC) RLOC                 ! Locator to response file
      CHARACTER*(DAT__SZLOC) ELOC                 ! Locator to effective areas
      CHARACTER*80 RFILE                          ! Name of response file
      CHARACTER*80 EFILE                          ! Name of eff. area file
      CHARACTER*80 PFILE                          ! Name of particle file
      CHARACTER*80 CALDIR                         ! Dir. for XRT cal. files
      REAL AMULT                                  ! Area multiplication
*
*      Pointers:
      INTEGER SDPNTR,SVPNTR,SQPNTR                ! Source data, var and qual.
      INTEGER BDPNTR,BVPNTR,BQPNTR                ! Backgnd data, var and qual
      INTEGER TSDPNTR,TSVPNTR,TSQPNTR             ! Reordered source arrays
      INTEGER TBDPNTR,TBVPNTR,TBQPNTR             ! Reordered bckgnd arrays
      INTEGER PDPNTR                              ! Pointer to particle file
      INTEGER SUBD,SUBV                           ! Pointers to subtraction
*                                                 ! data and variance
      INTEGER OSUBD,OSUBV                         ! Pntrs to bckgnd model arrays
      INTEGER AXPNTR                              ! PHA axis
      INTEGER EPHPTR                              ! Energies of PH chans.
      INTEGER PCPNTR                              ! Array of corrections
*                                                 ! due to the diff. in instr.
      INTEGER PPAR1                               ! Pointer to internal parts.
      INTEGER PPAR2                               ! Pointer to Al flour.
      INTEGER PPAR3                               ! Pointer to external parts.
      INTEGER PPE1,PPE2,PPE3                      ! Pointers to particle arrays
      INTEGER APNTR                               ! Correction factor for diff.
*                                                 ! areas of bckgnd and srce box
*                                                 ! response at box positions.
      INTEGER PBDSUM,PBVSUM                       ! Tot back. data and variance
      INTEGER SORD(DAT__MXDIM), BORD(DAT__MXDIM)  ! Order of the mapped
*                                                 ! source and bkgnd arrays
      INTEGER REORD(DAT__MXDIM)                   ! Original order of source
      INTEGER SDIM(DAT__MXDIM)                    ! Dimensions of source data
      INTEGER BDIM(DAT__MXDIM)                    ! Dimensions of bckgnd data
      INTEGER RDIM(DAT__MXDIM)                    ! Dimensions of reordered arr.
      INTEGER NSIZE                               ! Total no. of data elements
      INTEGER SID,BID,OID,MID,PFID
      LOGICAL SLVAR                               ! variances in source file?
      LOGICAL BLVAR                               ! variances in bckgnd file?
      LOGICAL LHRI                                ! Is it an HRI observation ?
      LOGICAL LPART                               ! Take particles into account
      LOGICAL PCORR                               ! Perform the position
      LOGICAL SREORD,BREORD                       ! Is src/bck data reordered
*                                                 ! correction on the bkgnd ?
      REAL BAREA                                  ! Area of background box
      REAL SVDEF                                  ! Default source variance
      REAL BVDEF                                  ! Default bckgnd variance
      INTEGER LP
      INTEGER			IFILES			! Input file info
      BYTE SMASK, BMASK                           ! Quality masks
      LOGICAL SLQUAL, BLQUAL, LBMOD
*    Local data :
*     <any DATA initialisations for local variables>
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'XRTSUB  Version 1.8-2')
*-
      CALL AST_INIT
*
      CALL MSG_PRNT(VERSION)

* Get source file
      CALL USI_ASSOC( 'SOURCE', 'BinDS', 'READ', SID, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 999

* Get name of background file
      CALL USI_ASSOC( 'BCKGND', 'BinDS', 'READ', BID, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 999

* Get name of output file
      CALL USI_CLONE( 'SOURCE', 'OUT', 'BinDS', OID, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 999

* Check source file is XRT and get pointing position
      CALL XRTSUB_GETPOS( OID, SHEAD, STATUS )
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Read source data from copied input file
      CALL XRTSUB_GETDATA( OID, 'UPDATE', SHEAD, SDIM, SORD,
     &                  SDPNTR, TSDPNTR, SLVAR, SVPNTR, TSVPNTR,
     &                  SLQUAL, SQPNTR, TSQPNTR, SMASK, SREORD, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Find the number of elements in the input data array
      NSIZE=SDIM(1)*SDIM(2)*SDIM(3)*SDIM(4)*SDIM(5)
*
* Get pointing position of background file
      CALL XRTSUB_GETPOS( BID, BHEAD, STATUS )
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
      CALL XRTSUB_GETDATA( BID, 'READ', BHEAD, BDIM, BORD, BDPNTR,
     &                TBDPNTR, BLVAR, BVPNTR, TBVPNTR,
     &                BLQUAL, BQPNTR, TBQPNTR, BMASK, BREORD, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Calculate the background area
      CALL XRT_CALCAREA(BHEAD, BLQUAL, BDIM(1), BDIM(2), BDIM(3),
     &                 BDIM(4), BDIM(5), %val(BQPNTR), BMASK, BAREA)
*
* Check source and background observations are compatible  i.e. taken over
* the same time and sumsig ranges
      CALL XRTSUB_COMPAT(SHEAD, SDIM, BHEAD, BDIM, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Find if the data is HRI or PSPC
      IF (INDEX(SHEAD.DET, 'HRI') .NE. 0) THEN
         LHRI = .TRUE.
      ELSE
         LHRI = .FALSE.
      ENDIF
*
* Get PSPC cal if not hri
      IF (.NOT. LHRI) THEN
*
*   Get detector response matrix name
*       Set default to standard matrix
         CALL XRT_CALDEF(CALDIR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Warning: XRT cal directory not found')
            CALL ERR_ANNUL(STATUS)
         ENDIF
*
         RFILE = CALDIR(1:CHR_LEN(CALDIR)) // 'drmpspc'
         CALL USI_DEF0C('RESPFILE', RFILE, STATUS)
*
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
*
*   Get effective area filename
*   Select the default efective area file (dependent on the date of the
*   observation)
         IF (INDEX(SHEAD.DET, 'PSPCB') .NE. 0) THEN
            EFILE = CALDIR(1:CHR_LEN(CALDIR)) // 'pspcb_eff'
            CALL USI_DEF0C('EFFILE', EFILE, STATUS)
         ELSEIF (INDEX(SHEAD.DET, 'PSPCC') .NE. 0) THEN
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
*
      ENDIF
*
* Get rootname of calibration files
      CALL USI_GET0C('RTNAME', SHEAD.RTNAME, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
      BHEAD.RTNAME = SHEAD.RTNAME
*
* Give the option of producing an output background file, which is
* the extrapolated background counts in the source box
      CALL USI_CLONE( 'SOURCE', 'BGMODEL', 'BinDS', MID, STATUS )

*   "!" means dont produce a file
      IF (STATUS .EQ. PAR__NULL) THEN
         CALL ERR_ANNUL(STATUS)
         LBMOD = .FALSE.
      ELSE
         LBMOD = .TRUE.
      ENDIF
*
*   Create subtraction data array and variance array of size no of time elements
*    times no of energy chan elements times no. of radial bins.
      CALL DYN_MAPR(1, NSIZE, SUBD, STATUS)
      CALL DYN_MAPR(1, NSIZE, SUBV, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining temporary space')
         GOTO 999
      ENDIF
*
*   Zero the background image data arrays
      CALL ARR_INIT1R(0.0, NSIZE, %val(SUBD),STATUS)
      CALL ARR_INIT1R(0.0, NSIZE, %val(SUBV),STATUS)
*

*    Create area normalisation factor array. One for each radial bin
      CALL DYN_MAPR(1, SDIM(5), APNTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining temporary space')
         GOTO 999
      ENDIF

*
*    Calculate the difference in areas between the source and background boxes.
      CALL XRTSUB_BACKNORM(SHEAD, BHEAD, SDIM(1), SDIM(5),
     &                                      BAREA, %val(APNTR))

*
*    Calculate the difference in instrument response at the source and bkgnd
*    box positions. This will be energy dependent.
*    First create a dynamic array to hold these corrections
      CALL DYN_MAPR(1, BDIM(4)*SDIM(5), PCPNTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining virtual memory')
         GOTO 999
      ENDIF
*
* If not an image and not an HRI observation
      IF (SDIM(1) .EQ. 1 .AND. SDIM(2) .EQ. 1 .AND. .NOT. LHRI) THEN
*
*   Give the user the option of not performing the position correction
         CALL USI_GET0L('PCORR', PCORR, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
      ELSE
         PCORR = .FALSE.
      ENDIF
*
      IF (PCORR) THEN
*

*   Calculate the position correction array
         CALL XRTSUB_POSCORR( SID, SHEAD, BHEAD, RLOC, ELOC, SORD(4),
     &                           BDIM(4), SDIM(5), %val(PCPNTR), STATUS)
*
      ELSE
*
*   Fill the array with ones
         CALL ARR_INIT1R(1.0, BDIM(4)*SDIM(5), %val(PCPNTR),STATUS)
*
      ENDIF
*
*

*   Create arrays to hold particle values
      CALL DYN_MAPR(1, BDIM(3)*BDIM(4), PPAR1, STATUS)
      CALL DYN_MAPR(1, BDIM(3)*BDIM(4), PPAR2, STATUS)
      CALL DYN_MAPR(1, BDIM(3)*BDIM(4), PPAR3, STATUS)
      CALL DYN_MAPR(1, BDIM(4), PPE1, STATUS)
      CALL DYN_MAPR(1, BDIM(4), PPE2, STATUS)
      CALL DYN_MAPR(1, BDIM(4), PPE3, STATUS)
*

      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error mapping dynamic space')
         GOTO 999
      ENDIF
*

* Ask user if he/she wants to correct for particles
      IF (.NOT. LHRI) THEN
         CALL USI_GET0L('PARTICLES', LPART, STATUS)
      ELSE
         LPART = .FALSE.
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*

* Get the particle counts at this position
      IF (LPART) THEN
         CALL XRTSUB_PARTCNT( BID, BHEAD, BAREA, BDIM(3), BDIM(4),
     &           %val(PPAR1), %val(PPAR2), %val(PPAR3), STATUS)
*

* Allow the user to produce a particle spectrum if desired
         CALL USI_CREAT( 'PART_FILE', ADI__NULLID, PFID, STATUS )

*   "!" means dont produce a file
         IF (STATUS .EQ. PAR__NULL) THEN
            CALL ERR_ANNUL(STATUS)
         ELSE

*
           CALL BDI_LINK( 'Spectrum', 1, BDIM(4), 'REAL', PFID, STATUS )

*      Create data array and axes
            CALL BDI_MAPR( PFID, 'Data', 'WRITE', PDPNTR, STATUS )

*      Copy spectral axis from background file
            CALL BDI_AXCOPY( BID, BORD(4), ' ', PFID, 1, STATUS )
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('** Error producing particle spectrum'/
     &                      /' file **')
               GOTO 999
            ENDIF
*
*      Produce the particle data array
            CALL XRTSUB_PARWRITE(BDIM(3), BDIM(4), %val(PPAR1),
     &                    %val(PPAR2), %val(PPAR3), %val(PDPNTR))
*
         ENDIF
*
      ELSE
*
*      Set particle counts to zero
         CALL ARR_INIT1R(0.0, BDIM(3)*BDIM(4), %val(PPAR1),STATUS)
         CALL ARR_INIT1R(0.0, BDIM(3)*BDIM(4), %val(PPAR2),STATUS)
         CALL ARR_INIT1R(0.0, BDIM(3)*BDIM(4), %val(PPAR3),STATUS)
*
      ENDIF
*
*
* Get the default variances from the environment
      CALL USI_GET0R('SDEFVAR', SVDEF, STATUS)
      CALL USI_GET0R('BDEFVAR', BVDEF, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* There are two subtraction methods. If the source file is not an image
* then each source bin is subtracted individually. However, if the source
* file is a straight image then a background image of the same size is
* created by extrapolating the counts from the background box over the
* whole of the source area by folding through the vignetting function.
*   If source file is not an image
      IF (SDIM(1) .EQ. 1 .AND. SDIM(2) .EQ. 1) THEN
*
*      Fill the subtraction array using the background counts
         CALL XRTSUB_FILLSUB(SHEAD, BHEAD, BDIM(1), BDIM(2), BDIM(3),
     &        BDIM(4), SDIM(5), %val(BDPNTR), %val(BVPNTR),
     &        %val(BQPNTR), BMASK,
     &        %val(APNTR), %val(PCPNTR), LPART, %val(PPAR1),
     &        %val(PPAR2), %val(PPAR3), BVDEF, %val(SUBD), %val(SUBV))
*
*   Perform the subtraction
         CALL XRTSUB_DOIT(SDIM(1), SDIM(2), SDIM(3), SDIM(4), SDIM(5),
     &        %val(SUBD), %val(SUBV), SVDEF, %val(SDPNTR), %val(SVPNTR))
*
*   Unmap dynamic arrays
         CALL DYN_UNMAP(PCPNTR,STATUS)
      ELSE
*
*   Create temporary background arrays
         CALL DYN_MAPR(1, BDIM(4), PBDSUM, STATUS)
         CALL DYN_MAPR(1, BDIM(4), PBVSUM, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error obtaining dynamic memory')
            GOTO 999
         ENDIF
*

*   Sum up the original background data in each PHA bin
         CALL XRTSUB_IMDATA(BHEAD, BDIM(1), BDIM(2), BDIM(3), BDIM(4),
     &         BDIM(5), %val(BDPNTR), %val(BVPNTR), %val(BQPNTR),
     &         BMASK, LPART, %val(PPAR1),
     &         %val(PPAR2), %val(PPAR3), %val(APNTR), %val(PBDSUM),
     &         %val(PBVSUM), %val(PPE1), %val(PPE2), %val(PPE3))
*

*   If the background file has got spectral information calculate the energy
*   of the centre of each PH bin.
*    First map the PHA array
         IF (BDIM(4) .GT. 1) THEN
            CALL BDI_AXMAPR( BID, BORD(4), 'Data', 'READ', AXPNTR,
     :                       STATUS)
	    CALL DYN_MAPR(1,BDIM(4),EPHPTR,STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('Error mapping PHA axis array')
               GOTO 999
            ENDIF

*     Convert to energies
            IF (.NOT. LHRI) CALL XRTSUB_PHA2EN(RLOC, BDIM(4),
     &                          %val(AXPNTR), %val(EPHPTR), STATUS)
*
         ELSE
            CALL DYN_MAPR(1,1,EPHPTR,STATUS)
         ENDIF
*
*   Normalise the background minimum variance value by the
*   area correction factor
         CALL ARR_COP1R( 1, %val(APNTR), AMULT, STATUS )
         BVDEF = BVDEF * AMULT**2
*
*   Produce the background image from the background file and
*   the vignetting function
         IF (LHRI) THEN
            CALL XRTSUB_HRICBIM(SHEAD, BHEAD, ELOC, SDIM(1), SDIM(2),
     &         SDIM(4), BDIM(4), %val(PBDSUM), %val(PBVSUM),
     &         %val(PPE1), %val(PPE2), %val(PPE3), %val(EPHPTR),
     &         BVDEF, %val(SUBD), %val(SUBV), STATUS)
         ELSE
            CALL XRTSUB_CBIMAGE(SHEAD, BHEAD, ELOC, SDIM(1), SDIM(2),
     &         SDIM(4), BDIM(4), %val(PBDSUM), %val(PBVSUM),
     &         %val(PPE1), %val(PPE2), %val(PPE3), %val(EPHPTR),
     &         BVDEF, %val(SUBD), %val(SUBV), STATUS)
         ENDIF
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
         CALL DYN_UNMAP(EPHPTR,STATUS)
         CALL DYN_UNMAP(PBDSUM,STATUS)
         CALL DYN_UNMAP(PBVSUM,STATUS)
*
*   Calculate the background subtracted image
         CALL XRTSUB_SUBIMAGE(SDIM(1), SDIM(2), SDIM(4), %val(SUBD),
     &                %val(SUBV), SVDEF, %val(SDPNTR), %val(SVPNTR),
     &                                                        STATUS)
*
      ENDIF

*
*  Produce an extrapolated background file if requested
      IF (LBMOD) THEN

*      Map the data and variance from the extrapolated background file
         CALL BDI_MAPR( MID, 'Data', 'UPDATE', OSUBD, STATUS )
         CALL BDI_MAPR( MID, 'Variance', 'UPDATE', OSUBV, STATUS )
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('** Error producing extrapolated backgnd'/
     &                      /' file **')
            GOTO 999
         ENDIF
*
      ENDIF
*
* Reorder the dataset if necessary
      IF (SREORD) THEN
*
         DO LP=1,7
            REORD(SORD(LP)) = LP
            RDIM(SORD(LP)) = SDIM(LP)
         ENDDO
*
         CALL XRT_AXSWAP_R(SDIM, %val(SDPNTR), REORD, RDIM,
     &                                     %val(TSDPNTR), STATUS)
*
*  Reorder variance array
         CALL XRT_AXSWAP_R(SDIM, %val(SVPNTR), REORD, RDIM,
     &                                     %val(TSVPNTR), STATUS)
*
*  If the source file has been reordered, then any background model must
*  also be reordered.
         IF (LBMOD) THEN
*
            CALL XRT_AXSWAP_R(SDIM, %val(SUBD), REORD, RDIM,
     &                                     %val(OSUBD), STATUS)
*
*     Reorder variance array
            CALL XRT_AXSWAP_R(SDIM, %val(SUBV), REORD, RDIM,
     &                                     %val(OSUBV), STATUS)
*
         ENDIF
*
      ELSE
*
*
*    Copy the dynamic background arrays into the output file
         IF (LBMOD) THEN
            CALL ARR_COP1R( NSIZE, %val(SUBD), %val(OSUBD), STATUS)
            CALL ARR_COP1R( NSIZE, %val(SUBV), %val(OSUBV), STATUS)
         ENDIF
*
      ENDIF
*
* Unmap background arrays
      CALL DYN_UNMAP(SUBD,STATUS)
      CALL DYN_UNMAP(SUBV,STATUS)
*
* Set the background subtracted flag
      CALL PRF_SET( OID, 'BGND_SUBTRACTED', .TRUE., STATUS )
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error setting BGND_SUBTRACTED flag true')
         GOTO 999
      ENDIF
*
* Set a reference to the background spectrum - if it has been created
      IF (LBMOD .AND. SDIM(4) .GT. 1) THEN
        CALL FRI_PUT( OID, 'BGND', MID, STATUS )
        IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('Error writing reference to background '/
     &                   /'spectrum')
        ENDIF
      ENDIF

* Update the history structure
*   Trace path of input data.
      CALL USI_NAMES( 'I', IFILES, STATUS )
      CALL HSI_ADD(OID, VERSION, STATUS)
*
      CALL HSI_PTXTI(OID, IFILES, .FALSE., STATUS )
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing history record')
      ENDIF
*
* Update the background model file
      IF (LBMOD) THEN
*
*      Add history structure to the extrapolated background file
         CALL HSI_ADD(MID, VERSION, STATUS)
*
         CALL HSI_PTXTI(MID, IFILES, .TRUE., STATUS)
*
*   Change title of the background image
         CALL BDI_PUT0C( MID, 'Title', 'Extrapolated background file',
     &                                            STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error adding HISTORY record to '/
     &                   /'background model file')
         ENDIF
*
      ENDIF
*
*   Release all the files
      CALL USI_ANNUL('SOURCE',STATUS)
      CALL USI_ANNUL('BGKGND',STATUS)
      CALL USI_ANNUL('OUT',STATUS)

      IF (LBMOD) THEN
        CALL USI_ANNUL('BGMODEL',STATUS)
      ENDIF

*
* Unmap the dynamic arrays
      CALL DYN_UNMAP(PPAR1,STATUS)
      CALL DYN_UNMAP(PPAR2,STATUS)
      CALL DYN_UNMAP(PPAR3,STATUS)
      CALL DYN_UNMAP(PPE1,STATUS)
      CALL DYN_UNMAP(PPE2,STATUS)
      CALL DYN_UNMAP(PPE3,STATUS)
*
999   CONTINUE
*
* Close the eff. area and response files
      IF (.NOT. LHRI) CALL DAT_ANNUL(ELOC, STATUS)
*
* Close down the Asterix common blocks
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END





*+XRTSUB_BACKNORM   Finds factor to multiply background data by.
      SUBROUTINE XRTSUB_BACKNORM(SHEAD, BHEAD, SNX, RDIM,
     &                                        BAREA, CFACTOR)
*    Description :
*     The ratio of the source and background box areas is used to
*     calculate a normalisation factor for the background count rate.
*     Any hotspots in either box will be taken into account when
*     calculating the respective box areas. Since photons from hotspot
*     regions are excluded in the sorting the actual box area is reduced.
*    Method :
*    History :
*     6-Jun-1990     original (LTVAD::RDS)
*    20-May-1991     now handles radial bins  (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'QUAL_PAR'
*    Structure definition :
      INCLUDE 'XRTLIB(INC_CORR)'
      INCLUDE 'INC_XRTHEAD'
*    Import :
      RECORD /CORR/ SHEAD                 ! Header structure for source data
      RECORD /CORR/ BHEAD                 ! Header structure for bckgnd data
*
      INTEGER SNX                         ! Number of X bins in source array
      INTEGER RDIM                        ! Number of radial bins in src. array
      REAL BAREA                          ! Area of the background region
*    Import-Export :
*    Export :
      REAL CFACTOR(RDIM)                  ! Correction factor between background
*                                         ! flux in the source and bckgnd boxes
*    Local constants :
      REAL PI
         PARAMETER (PI=3.1415926535)
*    Local variables :
      REAL SAREA                          ! Size of source box
      REAL XINC,RXINN,RXOUT               ! Used in calculating off axis
      REAL YINC,RYINN,RYOUT               !    angle.
      INTEGER RLP                         ! Radial bin loop
* Local data :
*-
*
* Calculate a correction factor for the difference in size of the
* background box to the size of the source image pixel or box.
*
*   Loop over each radial bin
      DO RLP=1,RDIM
*
*      Get area of source file element.
*       If more than one X,Y element then area is one pixel.
         IF (SNX .GT. 1) THEN
*
            SAREA = ABS( SHEAD.XSCALE * SHEAD.YSCALE )
*
         ELSE
*
*       Calculate inner and outer radii for this radial bin
            XINC = (SHEAD.XOUTER - SHEAD.XINNER) / REAL(RDIM)
            RXINN = SHEAD.XINNER + XINC * REAL(RLP-1.0)
            RXOUT = RXINN + XINC
*
            YINC = (SHEAD.YOUTER - SHEAD.YINNER) / REAL(RDIM)
            RYINN = SHEAD.YINNER + YINC * REAL(RLP-1.0)
            RYOUT = RYINN + YINC
*
            IF (SHEAD.SHAPE(1:1) .EQ. 'R') THEN
*
*      Square box:
               SAREA = 4.0 * ( RXOUT * RYOUT - RXINN * RYINN )
*
            ELSEIF (INDEX ('CAE', SHEAD.SHAPE(1:1)) .NE. 0) THEN
*
*      Circle, ellipse or annulus
               SAREA = PI * ( RXOUT * RYOUT - RXINN * RYINN )
*
            ENDIF
*
         ENDIF
*
*   Convert source box area into square arcmins
         SAREA = SAREA * 3600.
*
*   Subtract any hotspot regions within the source box. Only HRI at present
C         IF (INDEX(HEAD.DET, 'HRI') .NE. 0) THEN
*
*     Attempt to open hotspot file and read in arrays
C            CALL XRT_RDHOTSPOT(SHEAD.RTNAME, SPOT_DEF, STATUS)
*
C            IF (STATUS .NE. SAI__OK) THEN

C               CALL MSG_PRNT('Error opening hotspot file')
*
C            ELSE
*
*       Calculate source and background areas taken up with hotspots
*              DO THIS AT SOME STAGE.
*
C            ENDIF
C         ENDIF
*
         CFACTOR(RLP) = SAREA / BAREA
D        write(*,*) ' Norm. factor = ',	cfactor(RLP), SAREA, BAREA
*
      ENDDO
*
      END

*+XRTSUB_CBIMAGE  -  Produce image
      SUBROUTINE XRTSUB_CBIMAGE(SHEAD, BHEAD, ELOC, NX, NY, NE, BNE,
     &                    BPHOT, BVAR, PE1, PE2, PE3, EPHA, VVAL,
     &                    BIDATA, BIVAR, STATUS)
*    Description :
*     This routine calculates a background image from a background value
*     and the vignetting function. If the source file is a spectral image,
*     a background spectral image will be created; in this case the bckgnd
*     file MUST have the same number of spectral bins as the source file.
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*       Richard Saxton     (LTVAD::RDS)
*    History :
*    20-Jun-1991     original
*    16-Sep-1991     now subtracts spectral images   (LTVAD::RDS)
*    15-Apr-1992     handles particles    (LTVAD::RDS)
*    28-Sep-1992     no longer calls XRT_PARTS but includes the code
*                    for efficiency reasons  (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ SHEAD                  ! Header info for source file
      RECORD /CORR/ BHEAD                  ! Header info for bckgnd file
      CHARACTER*(DAT__SZLOC) ELOC          ! Locator to eff. area file
      INTEGER NX,NY,NE                     ! Dimensions of data array
      INTEGER BNE                          ! Size of bckgnd energy axis
      REAL BPHOT(BNE)                      ! Total photons in background area
      REAL BVAR(BNE)                       ! Background variance
      REAL PE1(BNE)                        ! Internal particles in bckgnd area
      REAL PE2(BNE)                        ! Al. particles in bckgnd area
      REAL PE3(BNE)                        ! External particles in bckgnd area
      REAL EPHA(BNE)                       ! Rough Energies of PHA bin cents.
      REAL VVAL                            ! Default value for background
*                                          ! variance
*    Import-Export :
      REAL BIDATA(NX,NY,NE)                ! Background image data array
      REAL BIVAR(NX,NY,NE)                 ! Background image variance

*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      INTEGER MAXANG
         PARAMETER (MAXANG=60)            ! Max. no. of offset calibrations
*    Local variables :
      REAL ANGLE(MAXANG)                  ! Angles of EFFAR cal.
      REAL REFF(MAXANG)                   ! Effective areas as a function of
*                                         ! off-axis angle for a single energy
      REAL XCENT,YCENT,XSCALE,YSCALE      ! X and Y axis values
      REAL XOFF,YOFF,AXOFF                ! Offsets from centre
      REAL RESP                           ! Eff. area for this pixel
      REAL BRESP                          ! Eff. area for background file
      REAL AVFACT                         ! Average positional correction factor
      REAL PARTOT                         ! Total particle counts
      REAL BPART                          ! Tot. parts at a given position
      INTEGER NANG                        ! Number of off-axis angles
      INTEGER LPX,LPY,TOP,LPO,LPE
      INTEGER EDIM                        ! Energy index
      REAL CFACT                          ! Ratios of position dep. factors
      REAL FRAC
      LOGICAL DETB
*    Local data :
*-
* If no spectral information in the background file get mean energy
* the environment.
      IF ( BNE .EQ. 1 ) THEN
*
         CALL USI_GET0R('ENERGY', EPHA(1), STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
      ENDIF
*
*  Set X,Y axis values
      XCENT = SHEAD.XCENT*60.
      YCENT = SHEAD.YCENT*60.
      XSCALE = SHEAD.XSCALE*60.
      YSCALE = SHEAD.YSCALE*60.
*
* Set a logical depending on whether the PSPC detebtor is PSPCB or PSPCC
      IF (INDEX(SHEAD.DET, 'PSPCB') .NE. 0) THEN
         DETB = .TRUE.
      ELSE
         DETB = .FALSE.
      ENDIF
*
* Set the energy index to one if the source file was a single image
      IF (NE .EQ. 1) EDIM =1
*
* Loop over each energy
      DO LPE=1,BNE
*
*   Set the energy index if source file is a spectral_image
         IF (NE .GT. 1) EDIM=LPE
*
*   Get array of effective area as a function of position
         CALL XRTSUB_CBIMAGE_GETEFF(ELOC, EPHA(LPE), NANG, ANGLE,
     &                                                REFF, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
* Calculate the effective area of the background file
         TOP=0

         DO LPO=1,NANG
*
           IF (BHEAD.OFFAX .LT. ANGLE(LPO)) THEN
              TOP = LPO
              FRAC = (BHEAD.OFFAX-ANGLE(LPO-1)) /
     &                          (ANGLE(LPO)-ANGLE(LPO-1))
              GOTO 100
           ENDIF
*
         ENDDO
*
100      CONTINUE
*
*   Calc. the response
         IF (TOP .GT. 1) THEN
*
            BRESP = REFF(TOP-1) + (REFF(TOP) - REFF(TOP-1)) * FRAC
*
         ELSEIF (TOP .EQ. 1) THEN
*
            BRESP = REFF(1)
*
*    Outside the cal info.
         ELSEIF (TOP .EQ. 0) THEN
            BRESP = REFF(NANG) + (BHEAD.OFFAX - ANGLE(NANG)) /
     &                (ANGLE(NANG) - ANGLE(NANG-1)) *
     &                    (REFF(NANG)-REFF(NANG-1))
         ENDIF
*
*   Calculate the eff. area in each image pixel
         DO LPY=1,NY
            DO LPX=1,NX
*
*   Calculate the off-axis angle of this pixel
               XOFF = XCENT + (NX / 2.0 - LPX + 0.5) * XSCALE
               YOFF = YCENT - (NY / 2.0 - LPY + 0.5) * YSCALE
*
               AXOFF = SQRT( XOFF**2 + YOFF**2 )
*
*     Calculate the response at this offset relative to the response at
*     the position of the background file
               TOP = 0
*
               DO LPO=1,NANG
*
                  IF (AXOFF .LT. ANGLE(LPO)) THEN
                     TOP = LPO
                     FRAC = (AXOFF-ANGLE(LPO-1)) /
     &                           (ANGLE(LPO)-ANGLE(LPO-1))
                     GOTO 200
                  ENDIF
*
               ENDDO
*
200            CONTINUE
*
*   Calc. the response
               IF (TOP .GT. 1) THEN
*
                  RESP = REFF(TOP-1) + (REFF(TOP) - REFF(TOP-1)) * FRAC
*
               ELSEIF (TOP .EQ. 1) THEN
*
                  RESP = REFF(1)
*
*  Outside the cal info.
               ELSEIF (TOP .EQ. 0) THEN
                  RESP = REFF(NANG) + (AXOFF - ANGLE(NANG)) /
     &                (ANGLE(NANG) - ANGLE(NANG-1)) *
     &                    (REFF(NANG)-REFF(NANG-1))
*
*      Need to ensure that RESP doesn't go negative
                  RESP = MAX(RESP, 0.0)
*
               ENDIF
*
*  Calculate the particle count at this position
CC               CALL XRT_PARTS(DETB, BHEAD.OFFAX, AXOFF, PE1(LPE),
CC     &                           PE2(LPE), PE3(LPE), BPART)
*    Instead of calling the subroutine hardwire the code to save time
*    This MUST be kept in line with the XRT_PARTS subroutine.
*            Detector B :
               IF (DETB) THEN
                  CFACT =(1.02E-4 + 3.3E-5 * exp(- (AXOFF - 20.6)**2
     &                    / 12.8 )) / (1.02E-4 + 3.3E-5 *
     &                         exp(- (BHEAD.OFFAX - 20.6)**2 / 12.8))
*            Detector C :
               ELSE
                  CFACT = (8.42E-5 + 3.95E-7 * AXOFF) /
     &                          (8.42E-5 + 3.95E-7 * BHEAD.OFFAX)
               ENDIF
*
               BPART = PE1(LPE) * CFACT + PE2(LPE) + PE3(LPE)
*
*     Set the data point and variance value
               BIDATA(LPX,LPY,EDIM) = BIDATA(LPX,LPY,EDIM) +
     &                           BPHOT(LPE) * RESP / BRESP + BPART
*
*     Calculate mean position correction factor for variances
               PARTOT = PE1(LPE) + PE2(LPE) + PE3(LPE)
*
               IF (PARTOT .GT. 0 .AND. (BPHOT(LPE)+BPART).GT.0.0) THEN
                  AVFACT = ( BPHOT(LPE) * RESP/BRESP + BPART *
     &                         BPART/PARTOT ) / (BPHOT(LPE)+BPART)
               ELSE
                  AVFACT = RESP / BRESP
               ENDIF
*
*     Calc. variance
               IF (BVAR(LPE) .GE. VVAL .OR. NE .EQ. 1) THEN
                  BIVAR(LPX,LPY,EDIM) = BIVAR(LPX,LPY,EDIM) +
     &                                       BVAR(LPE) * AVFACT**2
               ELSE
                  BIVAR(LPX,LPY,EDIM) = BIVAR(LPX,LPY,EDIM) +
     &                                            VVAL * AVFACT**2
               ENDIF
            ENDDO
         ENDDO
*
      ENDDO
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSUB_CBIMAGE',STATUS)
      ENDIF
*
      END

*+XRTSUB_CBIMAGE_GETEFF  -  Get effective area array as a fn. of position
      SUBROUTINE XRTSUB_CBIMAGE_GETEFF(ELOC, ENERGY, NEFF, ANGLE,
     &                           REFF, STATUS)
*    Description :
*    Environment parameters :
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*       Richard Saxton     (LTVAD::RDS)
*    History :
*    20-Jun-1991     original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
*    Local constants :
      INTEGER MAXANG
         PARAMETER (MAXANG=60)             ! Max. no. of offset calibrations
*    Import :
      CHARACTER*(DAT__SZLOC) ELOC          ! Locator to eff. area file
      REAL ENERGY                          ! Mean photon energy
*    Import-Export :
*    Export :
      INTEGER NEFF                         ! Number of angles
      REAL ANGLE(MAXANG)                   ! Angles of effective areas
      REAL REFF(MAXANG)                    ! Effective areas as a function of
*                                          ! off-axis angle for a single energy
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
*    Local variables :
      CHARACTER*(DAT__SZLOC) OALOC     !Locator to off axis array
      CHARACTER*(DAT__SZLOC) EELOC     !Locator to energy array
      CHARACTER*(DAT__SZLOC) RELOC     !Locator to effective area arrays
      CHARACTER*(DAT__SZLOC) CLOC      !Locator to energy element
      CHARACTER*10 STRING              !Name of effective area array
      CHARACTER*4 CI
*
      INTEGER EEPTR                    !Pointer to energy array
      INTEGER NEN                      !Number of test energies
      INTEGER EINDEX                   !Position in energy array of energy
      INTEGER LP,NCHAR
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Read in the array of off-axis angles from the effective areas file
      CALL DAT_FIND(ELOC, 'OFF_ANGLE', OALOC, STATUS)
      CALL DAT_SIZE(OALOC, NEFF, STATUS)
*
      CALL DAT_GET1R(OALOC, MAXANG, ANGLE, NEFF, STATUS)
*
      CALL DAT_ANNUL(OALOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error reading off axis angles from EFFAR file')
         GOTO 999
      ENDIF
*
* Read in the array of energies from the file
      CALL DAT_FIND(ELOC, 'ENERGY', EELOC, STATUS)
      CALL DAT_SIZE(EELOC, NEN, STATUS)
*
      CALL DAT_MAPR(EELOC, 'READ', 1, NEN, EEPTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error reading energy array from EFFAR file')
         GOTO 999
      ENDIF
*
* Find the position in the arrays of the energy chosen
      CALL XRTSUB_GETEFF_INDEX(ENERGY, NEN, %val(EEPTR), EINDEX)
*
* Now write the response to this energy at each off-axis angle into
* the output array
      DO LP=1,NEFF
*
         CALL CHR_ITOC(LP, CI, NCHAR)
         STRING = 'EFFAR_' // CI
*
*  Get the eff area at this energy at this off-axis angle
         CALL DAT_FIND(ELOC, STRING, RELOC, STATUS)
         CALL DAT_CELL(RELOC, 1, EINDEX, CLOC, STATUS)
         CALL DAT_GETR(CLOC, 0, 0, REFF(LP), STATUS)
*
*  Annul locators
         CALL DAT_ANNUL(RELOC, STATUS)
         CALL DAT_ANNUL(CLOC, STATUS)
*
      ENDDO
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error reading effective area arrays')
         GOTO 999
      ENDIF
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP('from XRTSUB_CBIMAGE_GETEFF')
         GOTO 999
      ENDIF
*
      END

*+XRTSUB_COMPAT   Checks if source and background data are compatible
      SUBROUTINE XRTSUB_COMPAT(SHEAD, SDIM, BHEAD, BDIM, STATUS)
*    Description :
*    Environment parameters :
*    Method :
*      If More than one source time bin
*         If (start and end times of file or no of timbins are not the same)
*            Status = bad
*         endif
*      else
*         If (start and end times not the same)
*            Print a warning
*         endif
*      endif
*
*      If (PH channel range is not the same .or. different number of bins)
*          Status = bad
*       endif
*
*      If (Energy channel range is not the same .or. different number of bins)
*          Status = bad
*       endif
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (institution::username)
*    History :
*     6-Jun-1990    original (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ SHEAD                    ! Header of source file
*
      INTEGER SDIM(DAT__MXDIM)               ! Dimensions of source data
*
      RECORD /CORR/ BHEAD                    ! Header of background file
*
      INTEGER BDIM(DAT__MXDIM)               ! Dimensions of bckgnd data
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS                         ! Set bad if files rae incompatible
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
*     <declarations for local variables>
*    Local data :
*     <any DATA initialisations for local variables>
*-
*
      IF (STATUS .NE. SAI__OK) RETURN
*
* Check dimensions are ok.
      IF ((SDIM(3) .GT. 1 .OR. SDIM(4) .GT. 1 .OR. SDIM(5) .GT. 1)
     &    .AND. (SDIM(3) .NE. BDIM(3) .OR. SDIM(4) .NE. BDIM(4))) THEN
*
         CALL MSG_PRNT('*Error: Source and background file '/
     &                   /'dimensions are incompatible*')
         STATUS=SAI__ERROR
         GOTO 999
      ENDIF
*
* Check if the source file has more than one time bin
      IF (SDIM(3) .GT. 1) THEN
*
*   Check if source and background files have compatible time axes
         IF (SHEAD.TMIN(1) .NE. BHEAD.TMIN(1)
     &       .OR. SHEAD.TSCALE(1) .NE. BHEAD.TSCALE(1)) THEN
*
            CALL MSG_PRNT('*Error: Source and background file Time '/
     &                   /'axes are incompatible*')
            STATUS=SAI__ERROR
            GOTO 999
         ENDIF
*
      ELSE
*
*   Check if source and background files have been taken over different times
         IF (SHEAD.TMIN(1) .NE. BHEAD.TMIN(1) .OR.
     &    SHEAD.TMAX(SHEAD.NTRANGE) .NE. BHEAD.TMAX(BHEAD.NTRANGE)) THEN
*
            CALL MSG_PRNT('Warning: Source and background files '/
     &                   /'have been taken over different TIME range')
         ENDIF
*
      ENDIF
*
* Check if the source file has more than one PH bin
      IF (SDIM(4) .GT. 1) THEN
*
*   Check if source and background files have compatible corrected PH axes
         IF (SHEAD.PMIN(1) .NE. BHEAD.PMIN(1)
     &       .OR. SHEAD.PSCALE .NE. BHEAD.PSCALE) THEN
*
            CALL MSG_PRNT('*Error: Source and background file '/
     &                   /'CORR_PH axes are incompatible*')
            STATUS=SAI__ERROR
            GOTO 999
         ENDIF
*
      ELSE
*
*   Check if source and background files have been taken over different PH range
         IF (SHEAD.PMIN(1) .NE. BHEAD.PMIN(1) .OR.
     &                     SHEAD.PMAX(1) .NE. BHEAD.PMAX(1)) THEN
*
            CALL MSG_PRNT('Error: Source and background files '/
     &                 /'have been taken over different CORR_PH range')
            STATUS=SAI__ERROR
            GOTO 999
         ENDIF
*
      ENDIF
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSUB_COMPAT',STATUS)
      ENDIF
*
      END

*+XRTSUB_DOIT - Subtracts background data from source data
      SUBROUTINE XRTSUB_DOIT(NX, NY, NT, NE, NR, BCKD,
     &                                  BCKV, SVDEF, SDATA, SVAR)
*    Description :
*      Substracts a previously set up array with dimensions - no of times
*      x no of PH chans. x no of energy chans.
*    History :
*     6-jun-1990    original (LTVAD::RDS)
*     6-FEB-1991    Removed the uncorrected PH axis
*    20-MAY-1991    Updated to handle radial bins  (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER NX,NY,NT,NE,NR           ! Dimensions of source data
      REAL BCKD(NT,NE,NR)              ! Subtraction array
*                                      ! normalised by area
      REAL BCKV(NT,NE,NR)              ! Subtraction variance array
      REAL SVDEF                       ! Default source variance
*    Import-Export :
      REAL SDATA(NX,NY,NT,NE,NR)       ! Source data array
      REAL SVAR(NX,NY,NT,NE,NR)        ! Source variance array
*    Local constants :
*    Local variables :
      INTEGER TLP,XLP,YLP,ELP,RLP
*-
* Loop over each Energy bin, PH bin and timebin
      DO RLP=1,NR
         DO ELP=1,NE
            DO TLP=1,NT
*
               DO YLP=1,NY
                  DO XLP=1,NX
*
                     SDATA(XLP,YLP,TLP,ELP,RLP) =
     &                   SDATA(XLP,YLP,TLP,ELP,RLP) - BCKD(TLP,ELP,RLP)
*
* Check if SVAR needs increasing.
                     IF (SVAR(XLP,YLP,TLP,ELP,RLP) .LT. SVDEF) THEN
                        SVAR(XLP,YLP,TLP,ELP,RLP) = SVDEF
                     ENDIF
*
                     SVAR(XLP,YLP,TLP,ELP,RLP) =
     &                    SVAR(XLP,YLP,TLP,ELP,RLP) + BCKV(TLP,ELP,RLP)
*
                  ENDDO
               ENDDO
*
            ENDDO
         ENDDO
      ENDDO
*
      END

*+XRTSUB_FILLSUB   Creates subtraction array
      SUBROUTINE XRTSUB_FILLSUB(SHEAD, BHEAD, XDIM, YDIM, TDIM, EDIM,
     &       RDIM, BARRAY, BVAR, BQUAL, BMASK, CFACTOR, PFACTOR,
     &       LPART, PART1, PART2, PART3, VVAL, SARRAY, SVAR)
*    Description :
*    Method :
*     Sum the contributions from all pixels in the background area
*     for each output time bin, PH bin and energy bin. Corrects
*     the background counts for the difference in the areas of the source
*     and background boxes and also corrects for the difference in
*     instrument response at the positions of the two boxes.
*    History :
*     6-Jun-1990      original (LTVAD::RDS)
*    10-Feb-1991      creates a background variance array  (LTVAD::RDS)
*    20-May-1991      handles radial bins
*    10-May-1992      handles background quality
*    Type definitions :
      IMPLICIT NONE
*    Global parameters :
      INCLUDE 'QUAL_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ SHEAD, BHEAD             ! Header stuff
      INTEGER XDIM,YDIM,TDIM,EDIM            ! Dimensions of background array
*                                            ! X=no of x elements etc...
      INTEGER RDIM                           ! No of radial bins in source data
      REAL BARRAY(XDIM,YDIM,TDIM,EDIM,1)     ! Background data array
      REAL BVAR(XDIM,YDIM,TDIM,EDIM,1)       ! Background variances
      BYTE BQUAL(XDIM,YDIM,TDIM,EDIM,1)      ! Background quality
      BYTE BMASK                             ! Quality mask
      REAL CFACTOR(RDIM)                     ! Area corr. factor for bckgnd
      REAL PFACTOR(EDIM,RDIM)                ! Position correction factors
      LOGICAL LPART                          ! Use particles ?
      REAL PART1(TDIM,EDIM)                  ! Internal particle rate
      REAL PART2(TDIM,EDIM)                  ! External Al flourescence
      REAL PART3(TDIM,EDIM)                  ! External power law component
      REAL VVAL                              ! Default variance value
*                                            ! all in counts per bin
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      REAL SARRAY(TDIM,EDIM,RDIM)            ! Subtraction array
      REAL SVAR(TDIM,EDIM,RDIM)              ! Subtraction variance array
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER TLP,XLP,YLP,ELP,RLP
      REAL BSUM,VSUM                         ! Background data and var sums
      REAL PARTOT                            ! No. of particles in this bin
      REAL BPHOTON                           ! No. of photons in this bin
      REAL BPART                             ! No. particles after posit. corr.
      REAL AVFACT                            ! Correction factor for variance
      REAL EVSUM                             ! Total events in bckgnd box
      REAL PARTSUM                           ! Total particles in bckgnd box
      REAL PERCNT                            ! Percentage of particles in bckgnd
      LOGICAL DETB                           ! Is it PSPCB or PSPCC ?
*-
      PARTSUM=0.0
      EVSUM=0.0
*
      IF (INDEX(SHEAD.DET, 'PSPCB') .NE. 0) THEN
         DETB = .TRUE.
      ELSE
         DETB = .FALSE.
      ENDIF
*
* Loop over each output time bin, Energy bin and radial bin
      DO RLP = 1,RDIM
*
*    Find the off-axis angle of this radial bin
         CALL XRTSUB_OFFAX(RDIM, RLP, SHEAD)
*
         DO ELP=1,EDIM
            DO TLP = 1,TDIM
*
               BSUM = 0.0
               VSUM = 0.0
*
*      Sum the background contributions within this time
               DO YLP = 1,YDIM
                  DO XLP = 1,XDIM
*
*        Add in this pixel if its quality is good
                     IF ((BQUAL(XLP,YLP,TLP,ELP,1) .AND. BMASK)
     &                            .EQ. QUAL_GOOD) THEN
*
                        BSUM = BSUM + BARRAY(XLP,YLP,TLP,ELP,1)
*
                     ENDIF
                  ENDDO
               ENDDO
*
*     Sum the particles in the background box, for this time and energy
               PARTOT = PART1(TLP,ELP) + PART2(TLP,ELP) + PART3(TLP,ELP)
*
*     Correct photon bckgnd for vignetting
               BPHOTON = (BSUM - PARTOT) * PFACTOR(ELP,RLP)
*
*     Correct particle bckgnd for positional effects
               CALL XRT_PARTS(DETB, BHEAD.OFFAX, SHEAD.OFFAX,
     &            PART1(TLP,ELP), PART2(TLP,ELP), PART3(TLP,ELP), BPART)
*
*     Normalise with the box areas
               SARRAY(TLP,ELP,RLP) = (BPHOTON + BPART) * CFACTOR(RLP)
*
*     Calculate mean position correction factor for variances
               IF (PARTOT .GT. 0 .AND. (BPHOTON+BPART) .NE. 0) THEN
                  AVFACT = (BPHOTON * BPHOTON/(BSUM-PARTOT) +
     &                    BPART * BPART/PARTOT) / (BPHOTON+BPART)
               ELSE
                  AVFACT = PFACTOR(ELP,RLP)
               ENDIF
*
*     Sum variances of good quality pixels
               DO YLP = 1,YDIM
                  DO XLP = 1,XDIM

                     IF ((BQUAL(XLP,YLP,TLP,ELP,1) .AND. BMASK)
     &                                     .EQ. QUAL_GOOD) THEN
*
                        VSUM = VSUM + BVAR(XLP,YLP,TLP,ELP,1)
                     ENDIF
                  ENDDO
               ENDDO
*
*     If variance is below the default - use the default value
               IF (VSUM .LT. VVAL) VSUM=VVAL
*
               SVAR(TLP,ELP,RLP) = VSUM * CFACTOR(RLP) *
     &                                CFACTOR(RLP) * AVFACT * AVFACT
*
*     Sum up total events and particles for info message
               EVSUM = EVSUM + BSUM
               PARTSUM = PARTSUM + PARTOT
*
            ENDDO                        ! Time loop
         ENDDO                        ! Energy loop
      ENDDO                        ! Radial loop
*
* Output useful message
      IF (LPART) THEN
         CALL MSG_SETR('PART', NINT(PARTSUM*10)/10.)
         PERCNT = 100.0 * PARTSUM / EVSUM
         CALL MSG_SETR('PERCNT', NINT(PERCNT*10)/10.)
         CALL MSG_PRNT('Estimated ^PART particles in the background'/
     &                /' box : ^PERCNT percent of total background')
      ENDIF
*
      END

*+XRTSUB_GETDATA    Maps the data array and reorders if necessary
      SUBROUTINE XRTSUB_GETDATA(IFID, MODE, HEAD, ODIMS, ORDER,
     &                      DPNTR,TDPNTR, LVAR, VPNTR, TVPNTR, LQUAL,
     &                         QPNTR, TQPNTR, MASK, LREORD, STATUS)
*    Description :
*      Maps the data_array and puts it in the axis order, X,Y,Time,Energy.
*      Then accesses header information.
*    Environment parameters :
*      AXTYPE       INTEGER    The axis type X=1, Y=2, Time=3, Corr.PH=4
*                                            Radial=5, Other = 6.
*    Method :
*     The data and variance arrays are mapped at first and if necessary,
*     thay are reordered so that the dimensions are in the
*     order X, Y, Time, Ph chan, Radial.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     1-Feb-1990    original  (LTVAD::RDS)
*     6-Feb-1991    removed the uncorrected PH channel from the routine
*    20-May-1991    put in radial bins.
*    29-Jun-1991    fixed variance bug
*    28-Sep-1992    added an argument to say whether the data had been
*                   reordered. No longer reordering if only one axis is
*                   out of place
*     7-Jan-1993    XRT_GETDATA renamed to XRTSUB_GETDATA - off-axis
*                   angle of central circular box is now area weighted.
*    28 Jun 95      Allow BDA to create dummy VARIANCE and QUALITY (rjv)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      INTEGER			IFID			! Input file id
      CHARACTER*(*) MODE		 ! Access mode
*    Import-Export :
      RECORD /CORR/ HEAD
*    Export :
      INTEGER ODIMS(DAT__MXDIM)          ! Dimensions of array
      INTEGER ORDER(7)                   ! The order of the new axes
      INTEGER DPNTR                      ! Pointer to the data array
      INTEGER TDPNTR                     ! Pointer to initially mapped array
      LOGICAL LVAR                       ! Are variances present in input file?
      INTEGER VPNTR                      ! Pointer to the variance array
      INTEGER TVPNTR                     ! Pointer to initially mapped array
      LOGICAL LQUAL                      ! Was quality available ?
      INTEGER QPNTR                      ! Pointer to the quality array
      INTEGER TQPNTR                     ! Pointer to initially mapped array
      BYTE MASK                          ! BADBITS mask
      LOGICAL LREORD                     ! Have the axes been reordered ?
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      REAL RTOAS                         ! Conversion from Radians to arcsecs
         PARAMETER (RTOAS=180.0 * 3600.0 / 3.1415926535)
*    Local variables :
      INTEGER DIMS(DAT__MXDIM)           ! Dimensions of original array
      INTEGER NDIM,LP
      LOGICAL OK                ! Is axis array present and regular ?
      CHARACTER*40 LABEL                 ! Axis label
      CHARACTER*(DAT__SZLOC) HLOC  ! Locator to header ans instrument box
      INTEGER AXTYPE                     ! Axis type
      INTEGER NSIZE                      ! Total no. of data elements
      INTEGER WRONG                      ! Number of out of order axes
      INTEGER WRGAX                      ! AXIS position of wrong axis
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Initialise dimension arrays
      DO LP=1,7
         ODIMS(LP)=1
      ENDDO
*
* Initialise the order array
      DO LP=1,5
         ORDER(LP)=0
      ENDDO
*
      DO LP=6,7
         ORDER(LP)=LP
      ENDDO
*
* Check data_array is present
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, DAT__MXDIM, DIMS, NDIM, STATUS )
      IF (STATUS .NE. SAI__OK .OR. .NOT. OK) THEN
          CALL MSG_PRNT('Error accessing data array')
          STATUS=SAI__ERROR
          GOTO 999
      ENDIF
*
* Initialise dimension arrays
      CALL AR7_PAD( NDIM, DIMS, STATUS )

* Set the size value
      CALL ARR_SUMDIM( NDIM, DIMS, NSIZE )

* Map the data array
      CALL BDI_MAPR( IFID, 'Data', MODE, TDPNTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('Error mapping data array')
          GOTO 999
      ENDIF
*
* Check variance array is present
      CALL BDI_CHK( IFID, 'Variance', LVAR, STATUS )

* If output file then force creation of variance
      IF (.NOT.LVAR.AND.(MODE(1:1).EQ.'W'.OR.MODE(1:1).EQ.'U')) THEN
        CALL BDI_MAPR( IFID, 'Variance', 'WRITE', TVPNTR, STATUS)
        CALL ARR_COP1R(NSIZE,%val(TDPNTR),%val(TVPNTR),STATUS)
      ELSE
        CALL BDI_MAPR( IFID, 'Variance', MODE, TVPNTR, STATUS)
      ENDIF
      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Error mapping variance array')
        GOTO 999
      ENDIF
*
*
* Check if QUALITY is present in file
      CALL BDI_CHK( IFID, 'Quality', LQUAL, STATUS )

* If output file force creation of quality
      IF (.NOT.LQUAL.AND.(MODE(1:1).EQ.'W'.OR.MODE(1:1).EQ.'U')) THEN
        CALL BDI_MAPUB( IFID, 'Quality', 'WRITE', TQPNTR, STATUS)
        CALL ARR_INIT1B(QUAL__GOOD,NSIZE,%val(TQPNTR),STATUS)
        MASK = QUAL__MASK
        CALL BDI_PUT0UB( IFID, 'QualityMask', MASK, STATUS )
      ELSE
        CALL BDI_MAPUB( IFID, 'Quality', MODE, TQPNTR, STATUS)
        CALL BDI_GET0UB( IFID, 'QualityMask', MASK, STATUS )
      ENDIF
      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Error mapping quality array')
        GOTO 999
      ENDIF

*  Find which axis is which
      DO LP=1,NDIM
*
         CALL BDI_AXGETOC( IFID, LP, 'Label', LABEL, STATUS )
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error reading axis label')
            GOTO 999
         ENDIF
*
         CALL CHR_UCASE(LABEL)
*
*   The order array contains the axis positions of the X,Y TIME and
*   corrected PH axes
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
               CALL MSG_PRNT('*Only X,Y,time,Corr. PHA or radial '/
     &                      /'axes are allowed*')
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
* See if reordering is required - this means that two axes are out of place
      WRONG = 0
      DO LP=1,5
         IF (ORDER(LP) .NE. 0 .AND. ORDER(LP) .NE. LP) THEN
            WRONG = WRONG + 1
            WRGAX = LP
         ENDIF
      ENDDO
*
* Check each order axis has been set
      IF (ORDER(1) .EQ. 0) THEN
         ORDER(1) = MAX( ORDER(2), ORDER(3), ORDER(4), ORDER(5) ) + 1
      ENDIF
      IF (ORDER(2) .EQ. 0) THEN
         ORDER(2) = MAX( ORDER(1), ORDER(3), ORDER(4), ORDER(5) ) + 1
      ENDIF
      IF (ORDER(3) .EQ. 0) THEN
         ORDER(3) = MAX( ORDER(1), ORDER(2), ORDER(4), ORDER(5) ) + 1
      ENDIF
      IF (ORDER(4) .EQ. 0) THEN
         ORDER(4) = MAX( ORDER(1), ORDER(2), ORDER(3), ORDER(5) ) + 1
      ENDIF
      IF (ORDER(5) .EQ. 0) THEN
         ORDER(5) = MAX( ORDER(1), ORDER(2), ORDER(3), ORDER(4) ) + 1
      ENDIF
*
* Reorder if 2 or more axes out of place
      IF (WRONG .GT. 1) THEN
*
         LREORD = .TRUE.
*
*    Create mapped array to hold the reordered array
         CALL DYN_MAPR(7, ODIMS, DPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error obtaining temporary space')
            GOTO 999
         ENDIF
*
         CALL XRT_AXSWAP_R(DIMS, %val(TDPNTR), ORDER, ODIMS,
     &                                      %val(DPNTR), STATUS)
*
*     Do the same for variances
         CALL DYN_MAPR(7, ODIMS, VPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error obtaining temporary space')
            GOTO 999
         ENDIF
*
         CALL XRT_AXSWAP_R(DIMS, %val(TVPNTR), ORDER, ODIMS,
     &                                      %val(VPNTR), STATUS)
*     And quality
         CALL DYN_MAPB(7, ODIMS, QPNTR, STATUS)
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
*
*   Set order flag
         LREORD = .FALSE.
*
         DPNTR=TDPNTR
         VPNTR=TVPNTR
         QPNTR=TQPNTR
      ENDIF
*
* Access header info from the datafile
      CALL ADI1_LOCHEAD( IFID, .FALSE., HLOC, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('Error getting locator to header structure')
          GOTO 999
      ENDIF
*
* Get the sort ranges
      CALL XRT_GETSORT( IFID, HEAD, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Xrt_getsort finds a generic mean off-axis angle. When subtracting
* we need to treat a box centred at the optical axis as a special case.
* Mean off axis angle in a circular box centred at 0,0 is area
* weighted and becomes  offaxis angle = sqrt( (outer**2 + inner**2) / 2 )

      IF (HEAD.SHAPE .EQ. 'C' .AND. ABS(HEAD.XCENT) .LT. 0.001
     &                    .AND. ABS(HEAD.YCENT) .LT. 0.001) THEN
*
         HEAD.OFFAX = SQRT(( HEAD.XOUTER**2 + HEAD.XINNER**2 )
     &                                                / 2.0) * 60.
      ENDIF
*
* Set scale factors
      HEAD.XSCALE = 2.0 * HEAD.XOUTER / ODIMS(1)
      HEAD.YSCALE = 2.0 * HEAD.YOUTER / ODIMS(2)
*
      HEAD.PSCALE = (HEAD.PMAX(1) - HEAD.PMIN(1) + 1) / ODIMS(4)
*
      HEAD.TSCALE(1) = (HEAD.TMAX(HEAD.NTRANGE) - HEAD.TMIN(1))
     &                                                 / ODIMS(3)
*
      IF (HEAD.NTRANGE .EQ. 2) HEAD.TSCALE(2)=HEAD.TSCALE(1)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRTSUB_GETDATA',STATUS)
      ENDIF
*
      END

*+XRTSUB_GETEFF_INDEX   Finds the index in the energy array
      SUBROUTINE XRTSUB_GETEFF_INDEX(ENERGY, NEN, EARRAY, EINDEX)
*    Description :
*    History :
*     date:  original (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      REAL ENERGY         ! Mean photon energy
      INTEGER NEN         ! Number of test energies
      REAL EARRAY(NEN)    ! Test energies
*    Import-Export :
*    Export :
      INTEGER EINDEX      ! Position of energy in energy array
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER LP
*-
      DO LP=1,NEN
*
         IF (ENERGY .LT. EARRAY(LP)) GOTO 100
*
      ENDDO
*
100   CONTINUE
*
      EINDEX=LP
*
      END

*+XRTSUB_GETPOS Gets position information from an XRT datafile
      SUBROUTINE XRTSUB_GETPOS( IFID, HEAD, STATUS)
*    Description :
*     Finds the pointing position of the file and checks if its an LE file
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     30-1-1990     original
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      INTEGER			IFID			! Input file id
*    Export :
      RECORD /CORR/ HEAD                           ! Header structure
*    Status :
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) HLOC                  ! Locator to header box
      CHARACTER*10 INSTR                           ! Instrument
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Test if source file is telescope 1
      CALL ADI1_LOCHEAD( IFID, .FALSE., HLOC, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error accessing the header box')
         GOTO 999
      ENDIF
*
      CALL CMP_GET0C(HLOC, 'INSTRUMENT', INSTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Assuming instrument is the XRT')
         CALL ERR_ANNUL(STATUS)
      ELSE
*
         IF ( INSTR(1:1) .NE. 'X' ) THEN
*
            CALL MSG_PRNT('Data is not from the ROSAT XRT')
            STATUS=SAI__ERROR
            GOTO 999
*
         ENDIF
      ENDIF
*
* Get the RA, DEC and position angle of the pointing direction of the
* spacecraft
      CALL CMP_GET0D(HLOC, 'AXIS_RA', HEAD.RA, STATUS)
      CALL CMP_GET0D(HLOC, 'AXIS_DEC', HEAD.DEC, STATUS)
      CALL CMP_GET0D(HLOC, 'POSITION_ANGLE', HEAD.ROLL, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error getting spacecraft pointing direction')
         GOTO 999
      ENDIF

*    Convert to radians
      HEAD.RA = HEAD.RA * MATH__DTOR
      HEAD.DEC = HEAD.DEC * MATH__DTOR
      HEAD.ROLL = HEAD.ROLL * MATH__DTOR

 999  IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRT_GETPOS',STATUS)
      ENDIF
*
      END

*+XRTSUB_HRICBIM  -  Produce image
      SUBROUTINE XRTSUB_HRICBIM(SHEAD, BHEAD, ELOC, NX, NY, NE, BNE,
     &                    BPHOT, BVAR, PE1, PE2, PE3, EPHA, VVAL,
     &                    BIDATA, BIVAR, STATUS)
*    Description :
*     This routine calculates a background image from a background value
*     for the HRI. It needs to be improved to take account of the mirror
*     vignetting and detector efficiency of the HRI and also the
*     particle (non-vignetted) contribution to the background.
*    Environment parameters :
*    Method :
*    Deficiencies :
*     This routine assumes that there is no positional variation of
*     the background in the HRI field of view.
*    Bugs :
*    Authors :
*       Richard Saxton     (LTVAD::RDS)
*    History :
*     6-Apr-1993     original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ SHEAD                  ! Header info for source file
      RECORD /CORR/ BHEAD                  ! Header info for bckgnd file
      INTEGER NX,NY,NE                     ! Dimensions of data array
      INTEGER BNE                          ! Size of bckgnd energy axis
      CHARACTER*(DAT__SZLOC) ELOC          ! Locator to eff. area file
      REAL BPHOT(BNE)                      ! Total photons in background area
      REAL BVAR(BNE)                       ! Background variance
      REAL PE1(BNE)                        ! Internal particles in bckgnd area
      REAL PE2(BNE)                        ! Al. particles in bckgnd area
      REAL PE3(BNE)                        ! External particles in bckgnd area
      REAL EPHA(BNE)                       ! Rough Energies of PHA bin cents.
      REAL VVAL                            ! Default value for background
*                                          ! variance
*    Import-Export :
      REAL BIDATA(NX,NY,NE)                ! Background image data array
      REAL BIVAR(NX,NY,NE)                 ! Background image variance

*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local variables :
      REAL RESP                           ! Eff. area for this pixel
      REAL BRESP                          ! Eff. area for background file
      REAL AVFACT                         ! Average positional correction factor
      INTEGER LPX,LPY,LPE
      INTEGER EDIM                        ! Energy index
*    Local data :
*-
* If no spectral information in the background file get mean energy
* the environment.
C      IF ( BNE .EQ. 1 ) THEN
*
C         CALL USI_GET0R('ENERGY', EPHA(1), STATUS)
*
C         IF (STATUS .NE. SAI__OK) GOTO 999
*
C      ENDIF
*
*  Set X,Y axis values
C      XCENT = SHEAD.XCENT*60.
C      YCENT = SHEAD.YCENT*60.
C      XSCALE = SHEAD.XSCALE*60.
C      YSCALE = SHEAD.YSCALE*60.
*
* Set the energy index to one if the source file was a single image
      IF (NE .EQ. 1) EDIM =1
*
* Loop over each energy
      DO LPE=1,BNE
*
*   Set the energy index if source file is a spectral_image
         IF (NE .GT. 1) EDIM=LPE
*
*   For HRI set no positional corrections
         RESP = 1.0
         BRESP = 1.0
*
         DO LPY=1,NY
            DO LPX=1,NX
*
*     Set the data point and variance value
               BIDATA(LPX,LPY,EDIM) = BIDATA(LPX,LPY,EDIM) +
     &                           BPHOT(LPE) * RESP / BRESP
*
               AVFACT = RESP / BRESP
*
*     Calc. variance
               IF (BVAR(LPE) .GE. VVAL .OR. NE .EQ. 1) THEN
                  BIVAR(LPX,LPY,EDIM) = BIVAR(LPX,LPY,EDIM) +
     &                                       BVAR(LPE) * AVFACT**2
               ELSE
                  BIVAR(LPX,LPY,EDIM) = BIVAR(LPX,LPY,EDIM) +
     &                                            VVAL * AVFACT**2
               ENDIF
            ENDDO
         ENDDO
*
      ENDDO
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSUB_HRICBIM',STATUS)
      ENDIF
*
      END

*+XRTSUB_IMDATA - Prepares background data to be turned into an image array
      SUBROUTINE XRTSUB_IMDATA(BHEAD, NX, NY, NT, NE, NR, BDATA,
     &             BVARIN, BQUAL, BMASK, LPART, PART1, PART2, PART3,
     &             CFACTOR, BPHOT, BVAROUT, PE1, PE2, PE3)
*    Description :
*     <description of what the subroutine does>
*    History :
*     date:  original (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'QUAL_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ BHEAD                     ! Background header info.
      INTEGER NX,NY,NT,NE,NR                  ! Data dimensions
      REAL BDATA(NX,NY,NT,NE,NR)              ! Bckgnd data
      REAL BVARIN(NX,NY,NT,NE,NR)             ! Bckgnd variance
      BYTE BQUAL(NX,NY,NT,NE,NR)              ! Bckgnd quality
      BYTE BMASK                              !   "       "    mask
      LOGICAL LPART                           ! Have particles been calculated ?
      REAL PART1(NT,NE)                       ! Internal particle component
      REAL PART2(NT,NE)                       ! Al. particle component
      REAL PART3(NT,NE)                       ! External particle component
      REAL CFACTOR                            ! Area correction factor
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      REAL BPHOT(NE)                          ! Photons per energy bin
      REAL BVAROUT(NE)                        ! output variance
      REAL PE1(NE)                            ! Internal particles
      REAL PE2(NE)                            ! Al parts.
      REAL PE3(NE)                            ! External parts.
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER ELP,TLP,XLP,YLP,RLP
      REAL PARTOT
      REAL EVSUM,PARTSUM                      ! Total events and particles
      REAL PERCNT                             ! Percentage of bckgnd particles
*-
      EVSUM = 0
      PARTSUM = 0
*
* Collapse the data onto the energy axis
      DO ELP=1,NE
*
       BPHOT(ELP) = 0.0
       BVAROUT(ELP) = 0.0
*
       DO RLP=1,NR
        DO TLP=1,NT
         DO YLP=1,NY
          DO XLP=1,NX
*
*     Sum the photons if good quality pixels
             IF ( (BQUAL(XLP,YLP,TLP,ELP,RLP) .AND. BMASK) .EQ.
     &                            QUAL_GOOD ) THEN
                BPHOT(ELP) = BPHOT(ELP) + BDATA(XLP,YLP,TLP,ELP,RLP)
*
*     Sum variances
                BVAROUT(ELP) = BVAROUT(ELP) +
     &                            BVARIN(XLP,YLP,TLP,ELP,RLP)
             ENDIF
*
          ENDDO
         ENDDO
        ENDDO
       ENDDO
*
*   Sum up total events
       EVSUM = EVSUM + BPHOT(ELP)
*
      ENDDO
*
* Collapse the particles onto the energy axis
      DO ELP=1,NE
*
         PE1(ELP) = 0.0
         PE2(ELP) = 0.0
         PE3(ELP) = 0.0
*
*   Sum the particles and multiply by the area correction factor
         DO TLP=1,NT
            PE1(ELP) = PE1(ELP) + PART1(TLP,ELP) * CFACTOR
            PE2(ELP) = PE2(ELP) + PART2(TLP,ELP) * CFACTOR
            PE3(ELP) = PE3(ELP) + PART3(TLP,ELP) * CFACTOR
         ENDDO
*
         PARTOT = PE1(ELP) + PE2(ELP) + PE3(ELP)
*
*   Sum up the total particles in the background box
         PARTSUM = PARTSUM + PARTOT / CFACTOR
*
*   Calculate the photons in this energy bin
         BPHOT(ELP) = BPHOT(ELP) * CFACTOR - PARTOT
*
*   Calc. variances
         BVAROUT(ELP) = BVAROUT(ELP) * CFACTOR ** 2
*
      ENDDO
*
* Output useful message
      IF (LPART) THEN
         CALL MSG_SETR('PART', NINT(PARTSUM*10)/10.)
         IF (EVSUM .LE. 0.0) THEN
            PERCNT = 100.0
         ELSE
            PERCNT = 100.0 * REAL(PARTSUM) / REAL(EVSUM)
         ENDIF
         CALL MSG_SETR('PERCNT', NINT(PERCNT*10)/10.)
         CALL MSG_PRNT('Estimated ^PART particles in the background'/
     &                /' box : ^PERCNT percent of total background')
      ENDIF
*
      END

*+XRTSUB_OFFAX - Calculates distance in arcmins from centre of field
      SUBROUTINE XRTSUB_OFFAX(RDIM, RLP, HEAD)
*    Description :
*     Calculates the mean off-axis angle of a source/backgnd box.
*     Based on XRT_OFFAX, but with the addition of treating a circular
*     box centred on the optical axis as a special case and weighting it
*     by the area.
*    History :
*     original 6-1-1993  (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      INTEGER RDIM                        ! Number of radial bins in file
      INTEGER RLP                         ! Radial bin wanted
*    Import-Export :
      RECORD /CORR/ HEAD
*    Export :
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      REAL XINC,RXINN,RXOUT               ! Used in calculating off axis
      REAL YINC,RYINN,RYOUT               !    angle.
      REAL OFF1,OFF2,OFF3,OFF4            ! Offset angles at each "corner"
      REAL XPOS1,XPOS2,YPOS1,YPOS2        ! Used in calc of off-axis pos.
*-
*   Calculate the off axis angle of this radial bin
      XINC = (HEAD.XOUTER - HEAD.XINNER) / REAL(RDIM)
      RXINN = HEAD.XINNER + XINC * REAL(RLP-1.0)
      RXOUT = RXINN + XINC
*
      YINC = (HEAD.YOUTER - HEAD.YINNER) / REAL(RDIM)
      RYINN = HEAD.YINNER + YINC * REAL(RLP-1.0)
      RYOUT = RYINN + YINC
*
*   Offset is the average of four offsets
      XPOS1 = HEAD.XCENT + ( RXINN + (RXOUT - RXINN) / 2.0 )
      XPOS2 = HEAD.XCENT - ( RXINN + (RXOUT - RXINN) / 2.0 )
      YPOS1 = HEAD.YCENT + ( RYINN + (RYOUT - RYINN) / 2.0 )
      YPOS2 = HEAD.YCENT - ( RYINN + (RYOUT - RYINN) / 2.0 )
*
      OFF1 = SQRT(XPOS1**2 + HEAD.YCENT**2)
      OFF2 = SQRT(XPOS2**2 + HEAD.YCENT**2)
      OFF3 = SQRT(YPOS1**2 + HEAD.XCENT**2)
      OFF4 = SQRT(YPOS2**2 + HEAD.XCENT**2)
*
*   Off-axis angle is the average offset in arcmins
      HEAD.OFFAX = (OFF1 + OFF2 + OFF3 + OFF4) * 60.0 / 4.0
*
*   Special case for circular box centred at 0,0. Here the average
*   offset is weighted by the area of the box and so the mean
*   offaxis angle = sqrt( (outer**2 + inner**2) / 2 )
      IF (HEAD.SHAPE .EQ. 'C' .AND. ABS(HEAD.XCENT) .LT. 0.001
     &                    .AND. ABS(HEAD.YCENT) .LT. 0.001) THEN
*
         HEAD.OFFAX = SQRT(( RXOUT**2 + RXINN**2 ) / 2.0) * 60.
      ENDIF

      END

*+ XRTSUB_PARTCNT - Estimate the particle count in each time bin.
      SUBROUTINE XRTSUB_PARTCNT( FID, HEAD, AREA, NT, NE, PART1,
     &                                        PART2, PART3, STATUS)
*    Description :
*     Estimates the number of particles likely to be found in the
*     background box in each time bin. Outputs the particle count in
*     a given time bin and energy bin for each of three components of
*     the particle spectrum in cts/arcmin squared
*    Method :
*     Uses analysis in a paper by Plucinsky et al, 1993, Ap. J. sub.
*
*     The particle rate is correlated with the MASTER VETO RATE (MVR)
*     present in the EVENTRATES file. It consists of three separate
*     components, internal, flourescent Al K line and external. Each of
*     these have separate spectral and positional distributions.
*     Most of the components are seen to vary slowly as a function
*     of time. The Plucinsky paper gives different coefficients for
*     the particle parameterisations in four different time slots,
*
*     Jun 1990 - 25 Jan 1991
*     Feb 6 1991 - 31 May 1991
*     1 June 1991 - 11 Oct 91
*     11 Oct 1991 - present
*
*     The linear correlation with MVR is only valid for MVR < 170.
*     Data with MVR>170 are unpredictable and can't be modelled.
*
*     e.g. the MVR correlation for the first two slots are:
*
*     For the principal detector, PSPCC
*            Pp(t) = ( (0.021 + 8.64E-4 * MVR) + (-0.006 + 2.44E-4 * MVR)
*                            + (0.009 + 0.46E-4 * MVR) ) counts/s
*
*     For the secondary detector, PSPCB
*            Ps(t) = ( (0.018 + 8.64E-4 * mvr) + (-0.006 + 2.44E-4 * MVR)
*                            + (0.005 + 0.92E-4 * MVR)  counts/s
*
*    History :
*     15 Nov 1991  original (LTVAD::RDS)
*     15 Apr 1992  uses new values from SLS paper (LTVAD::RDS)
*     25 Apr 1992  swapped order of calculations for effic. (LTVAD::RDS)
*      3 Mar 1993  ovrlap was incorrectly defined as real (RDS)
*     29 Apr 1993  updated to use the new Plucinsky et al . draft.
*    Type definitions :
      IMPLICIT NONE
*    Status :
      INTEGER STATUS
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
      INCLUDE 'INC_XRTHEAD'                ! Gives the MAXRAN constant
*    Import :
      INTEGER			FID			! File identifier
      RECORD /CORR/ HEAD         ! Header stuff
      REAL AREA                  ! Area of the background box (ARCMIN**2)
      INTEGER NT,NE              ! Dimensions
*    Import-Export :
*    Export :
      REAL PART1(NT,NE)          ! Particle count in internal component
      REAL PART2(NT,NE)          ! Particle count in aluminium flourescence
      REAL PART3(NT,NE)          ! Particle count in external component
*                                !   all in counts per bin
*    Functions :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) ILOC  ! Locator to INSTRUMENT structure
      CHARACTER*(DAT__SZLOC) ERLOC ! Locator to _EVR file
      CHARACTER*(DAT__SZLOC) HLOC ! Locator to _EVR file
      CHARACTER*10 RAWDAT        ! Type of rawdata e.g. US, MPE
*
      INTEGER TPNTR              ! Pointer to eventrate times
      INTEGER MPNTR              ! Pointer to Master veto rate data
      INTEGER SPTR               ! Workspace
      INTEGER UPTR               ! Workspace
      INTEGER LPTR               ! Pointer to selection times array
      INTEGER TJPTR             ! Pointer to joined time array

      REAL PC1                   ! Positional factor for internal component
      REAL PC2                   ! Positional factor for external components
*                                !   both give cts/sec per square arcmin
      REAL SC1(256)              ! Spectral factors for internal component
      REAL SC2(256)              ! Spectral factors for Al Flouresc. component
      REAL SC3(256)              ! Spectral factors for external component
      DOUBLE PRECISION TBIN(2)   ! Start and stop time for a given time bin
      REAL P1T,P2T,P3T           ! Temporal factors
      INTEGER ELP,CLP,TLP
      INTEGER FCHAN,LCHAN
      INTEGER NMVTIM,NRAW,NSEL
      INTEGER MSAVE              ! Current position in MVR index
      INTEGER NOVR               ! No. of overlapping time windows
      INTEGER PDATE              ! Particle model time slot position
      DOUBLE PRECISION OVRLAP    ! Duration of time bin
      INTEGER MJD1,MJD2,MJD3     ! MJDs of particle model slot times
      DOUBLE PRECISION BASE_MJD  ! Base MJD of observation
      REAL CHANP
      REAL MVMEAN                ! Mean MVR for this time bin
      REAL AVMVR                 ! Average MVR in the whole array
      LOGICAL THERE              ! Was the raw data object found ?
*-
*
* Get a locator to instrument section
      CALL ADI1_LOCINSTR( FID, .FALSE., ILOC, STATUS)
*
* Get the file origin from the RAWDATA entry
      CALL DAT_THERE(ILOC,'rawdata',THERE,STATUS)
      IF (THERE) THEN
         CALL CMP_GET0C(ILOC, 'RAWDATA', RAWDAT, STATUS)
         CALL CHR_UCASE(RAWDAT)
      ELSE
         CALL MSG_PRNT('Couldn`t find rawdata origins.  Assuming .OMD')
         RAWDAT = 'OMD'
      ENDIF

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*   Create work space to hold the raw selection times
      CALL DYN_MAPD(1, MAXRAN*2, SPTR, STATUS)
*   Get raw selection times from the header file
** THIS BIT HAS BEEN MODIFIED TO COPE WITH MPE-OMD stuff
      IF (RAWDAT .EQ. 'OMD') THEN
         CALL XRT_RAWTIM(HEAD.RTNAME, NSEL, %val(SPTR), STATUS)
      ELSE
         CALL RAT_RAWTIM(HEAD.RTNAME, NSEL, %val(SPTR), STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            CALL MSG_PRNT('Using alternate header (.hdr)')
            CALL XRT_RAWTIM(HEAD.RTNAME, NSEL, %val(SPTR), STATUS)
         ENDIF
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*


* Get master veto rate times and values for the PSPC
      IF (INDEX(HEAD.DET, 'PSPC') .NE. 0) THEN
*
         CALL XRT_GETMVR(RAWDAT, HEAD.RTNAME, ERLOC, NMVTIM,
     &                      TPNTR, MPNTR, AVMVR, STATUS)
*
      ENDIF
*
* Get base_mjd from file header
      CALL ADI1_LOCHEAD( FID, .FALSE., HLOC, STATUS )
      CALL CMP_GET0D(HLOC, 'BASE_MJD', BASE_MJD, STATUS)

* Decide which 'particle model time slot' this observation falls into.
      CALL CONV_YMDMJD(1991, 1, 25, MJD1)
      CALL CONV_YMDMJD(1991, 5, 31, MJD2)
      CALL CONV_YMDMJD(1991, 10, 11, MJD3)
*
      IF (BASE_MJD .LT. DBLE(MJD1)) THEN
         PDATE = 1
      ELSEIF (BASE_MJD .LT. DBLE(MJD2)) THEN
         PDATE = 2
      ELSEIF (BASE_MJD .LT. DBLE(MJD3)) THEN
         PDATE = 3
      ELSE
         PDATE = 4
      ENDIF
*
* Try and get the spacecraft clock basetime - needed to decode the
* MVR times. If MVR data was ok
      IF (STATUS .EQ. SAI__OK) THEN
         CALL CMP_GET0R(ILOC, 'SC_BASE', HEAD.SCBASE, STATUS)
*
*   Output message if this failed
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('** Error getting base SC time **')
         ENDIF
      ENDIF
*
* If error getting MVR data - set the particle count to zero and tell
* the user
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('** Setting particle rates to zero **')
*
         CALL ERR_ANNUL(STATUS)
*
         DO ELP=1,NE
            DO TLP=1,NT
               PART1(TLP,ELP)=0.0
               PART2(TLP,ELP)=0.0
               PART3(TLP,ELP)=0.0
            ENDDO
         ENDDO
*
         GOTO 999
      ENDIF
*
*
*   Create the array of user selection times
      CALL DYN_MAPD(1, HEAD.NTRANGE*2, UPTR, STATUS)
*
*   Make a dynamic array to hold the join of these two arrays
      CALL DYN_MAPD(1, MAXRAN*2, LPTR, STATUS)
*
*   Make a dynamic array to hold the join of this array and each time bin
      CALL DYN_MAPD(1, MAXRAN*2, TJPTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error mapping dynamic memory')
         GOTO 999
      ENDIF
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%
*   Put user selection times into one array
      CALL XRT_LIVEWIND(HEAD.NTRANGE, HEAD.TMIN, HEAD.TMAX, %val(UPTR))
*
*   Merge these with the user selection times in the sort box
      CALL XRT_TIMSET(MAXRAN*2, NSEL, %val(SPTR), HEAD.NTRANGE*2,
     &                HEAD.NTRANGE*2, %val(UPTR), MAXRAN*2, NRAW,
     &                %val(LPTR), OVRLAP)
*
* Calculate the positional correction factors.
* Head.offax should be in arcmins.
      IF (PDATE .EQ. 1) THEN
         PC1 = 8.42E-5 + 3.95E-7 * HEAD.OFFAX
      ELSEIF (PDATE .EQ. 2 .OR. PDATE .EQ. 3) THEN
         PC1 = 9.79E-5 + 3.18E-7 * HEAD.OFFAX
      ELSE
         PC1 = 9.22E-5 + 2.99E-7 * HEAD.OFFAX
      ENDIF
*
* The external components are flat over the field of view
      IF (PDATE .EQ. 4) THEN
         PC2 = 1.08E-4
      ELSE
         PC2 = 1.04E-4
      ENDIF
*
* Loop over each energy bin
      DO ELP=1,NE
*
*   Calculate first and last PH channels in this energy bin ( and add 0.5 )
         FCHAN = HEAD.PMIN(1) + (ELP-1) * HEAD.PSCALE
         LCHAN = HEAD.PMIN(1) + (ELP) * HEAD.PSCALE - 1
*
*   Add the spectral contribution for each PH channel in this bin
         SC1(ELP)=0.0
         SC2(ELP)=0.0
         SC3(ELP)=0.0
*
         DO CLP=FCHAN,LCHAN
*
*     Channels between 8 and 249 have been measured
            IF (CLP .GE. 8 .AND. CLP .LE. 249) THEN
*
               CHANP = CLP + 0.5
*
*         Calculate spectral distribution for internal component
               IF (PDATE .EQ. 1) THEN
                  SC1(ELP) = SC1(ELP) + 1.36 * CHANP **(-1.97) + 0.00340
               ELSEIF (PDATE .EQ. 2) THEN
                  SC1(ELP) = SC1(ELP) + 1.40 * CHANP **(-1.97) + 0.00338
               ELSEIF (PDATE .EQ. 3) THEN
                  SC1(ELP) = SC1(ELP) + 1.61 * CHANP **(-1.97) + 0.00393
               ELSEIF (PDATE .EQ. 4) THEN
                  SC1(ELP) = SC1(ELP) +44.13 * CHANP **(-2.91) + 0.00393
               ENDIF
*
*         Calculate spectral distribution for Al flourescence component
               SC2(ELP) = SC2(ELP) + 0.835 * CHANP**(-0.75) *
     &                    EXP(-0.71551 * (12.247 - SQRT(CHANP)) ** 2)
*
*         Calculate spectral distribution for external flat component
               IF (PDATE .LT. 3) THEN
                  SC3(ELP) = SC3(ELP) + 0.00413
               ELSE
                  SC3(ELP) = SC3(ELP) + 0.00433
               ENDIF
*
            ENDIF
         ENDDO                 ! End of channel loop
      ENDDO                    ! End of energy loop
*
*  Loop over each time bin
      MSAVE=1
      DO TLP=1,NT
*
*      Calculate the exposure time in this time bin
         TBIN(1) = HEAD.TMIN(1) + (TLP-1) * HEAD.TSCALE(1)
         TBIN(2) = HEAD.TMIN(1) + TLP * HEAD.TSCALE(1)
*
*      Find where this overlaps the live_time array
         CALL XRT_TIMSET(2, 2, TBIN, MAXRAN*2, NRAW*2, %val(LPTR),
     &                            MAXRAN*2, NOVR, %val(TJPTR), OVRLAP)

*
*      Trap zero NOVR
         IF ( NOVR .GT. 0 ) THEN
*
*        Search through MVR array for data in these time windows.
           CALL XRTSUB_PARTCNT_MVR(NMVTIM, %val(TPNTR), HEAD.SCBASE,
     &                   %val(MPNTR), MAXRAN*2, NOVR, %val(TJPTR),
     &                                         AVMVR, MSAVE, MVMEAN)
*
*        Calculate the count rate in this time bin
*         Component 1:
           IF (PDATE .EQ. 1) THEN
              P1T = 0.021 + 8.64E-4 * MVMEAN
           ELSEIF (PDATE .EQ. 2) THEN
              P1T = 0.018 + 8.64E-4 * MVMEAN
           ELSEIF (PDATE .EQ. 3) THEN
              P1T = 0.016 + 7.07E-4 * MVMEAN
           ELSEIF (PDATE .EQ. 4) THEN
              P1T = 0.018 + 7.37E-4 * MVMEAN
           ENDIF
*
*         Components 2 and 3:
           IF (PDATE .LT. 4) THEN
              P2T = -0.006 + 2.44E-4 * MVMEAN
           ELSE
              P2T = -0.004 + 2.29E-4 * MVMEAN
           ENDIF
*
           IF (PDATE .LT. 3) THEN
              P3T = 0.013 + 2.86E-4 * MVMEAN
           ELSEIF (PDATE .EQ. 3) THEN
              P3T = 0.010 + 2.44E-4 * MVMEAN
           ELSEIF (PDATE .EQ. 4) THEN
              P3T = 0.007 + 2.21E-4 * MVMEAN
           ENDIF
*
*         Write out the mean master veto rate
           IF (NT .EQ. 1) THEN
             CALL MSG_SETR('MV', INT(MVMEAN*100.0)/100.0)
             CALL MSG_PRNT('Mean master veto rate : ^MV')
           ENDIF

         ELSE
           P1T = 0.0
           P2T = 0.0
           P3T = 0.0

         END IF
*
*       Calculate the particle count in each component in this time bin
*       in this energy bin at this position. Multiply by the width of the
*       time bin (ovrlap) to convert count rate to counts and by the
*       area of the collection zone to convert to actual counts
         DO ELP=1,NE
            PART1(TLP,ELP) = P1T * SC1(ELP) * PC1 * OVRLAP * AREA
            PART2(TLP,ELP) = P2T * SC2(ELP) * PC2 * OVRLAP * AREA
            PART3(TLP,ELP) = P3T * SC3(ELP) * PC2 * OVRLAP * AREA
*
           WRITE(1,*)MVMEAN, SC1(ELP), SC2(ELP), SC3(ELP),
     &           PART1(TLP,ELP),PART2(TLP,ELP),PART3(TLP,ELP),
     &           (PART1(TLP,ELP)+PART2(TLP,ELP)+PART3(TLP,ELP))
*
         ENDDO                 ! Energy bin
      ENDDO              ! Time bin
*
* Unmap dynamic arrays
      CALL DYN_UNMAP(SPTR,STATUS)
      CALL DYN_UNMAP(LPTR,STATUS)
      CALL DYN_UNMAP(TJPTR,STATUS)
      CALL DYN_UNMAP(UPTR,STATUS)

* Close the eventrate file
      CALL DAT_ANNUL(ERLOC, STATUS)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSUB_PARTCNT',STATUS)
      ENDIF
*
      END


*+XRTSUB_PARTCNT_MVR - Calculate Mean MVR in this time bin
      SUBROUTINE XRTSUB_PARTCNT_MVR(NMVTIM, MVTIM, SCBASE, MVR,
     &                   MAXJN, NOVR, TJOIN, AVMVR, MSAVE, MVMEAN)
*    Description :
*     Finds the mean Master Veto Rate in a series of timew windows
*    History :
*     15-Apr 1992  original (LTVAD::RDS)
*     10-Dec 1992  fixed problem occurring when there were insufficient
*                  times in the _EVR file (RDS)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER NMVTIM               ! Number of MVR values
      REAL MVTIM(NMVTIM)           ! Times of MVR values (Spacecraft clock time)
      REAL SCBASE                  ! Base spacecraft clock time
      REAL MVR(NMVTIM)             ! MVR values
      INTEGER MAXJN                ! Dimension of join array
      INTEGER NOVR                 ! No. of time windows
      DOUBLE PRECISION  TJOIN(MAXJN) ! Times of windows
      REAL AVMVR                   ! Average MVR value from the array
*                                  ! used if _EVR file is incomplete
*    Import-Export :
      INTEGER MSAVE                ! Current MVR value
*    Export :
      REAL MVMEAN                  ! Average MVR value
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER MLP,TLP
      REAL OFFTIM                  ! MVR time as an offset from the base.
      REAL MVRTOT
      INTEGER MVCNT
*-
      MVRTOT = 0.0
      MVCNT = 0
*
      DO MLP=MSAVE,NMVTIM
*
         OFFTIM = MVTIM(MLP) - SCBASE
*
         IF (OFFTIM .LT. TJOIN(1)) THEN
*
*            do nothing
*
         ELSEIF(OFFTIM .GT. TJOIN(NOVR*2)) THEN
*
            IF (MLP .GT. 1) MSAVE = MLP - 1
            GOTO 200
*
         ELSE
*
*     Loop over each time window and see if this MVR value is within them
            DO TLP=1,NOVR
*
               IF (OFFTIM .GE. TJOIN(1+(TLP-1)*2) .AND.
     &            (OFFTIM .LE. TJOIN(TLP*2))) THEN
*
*        If within the time windows add to the total
                  MVRTOT = MVRTOT + MVR(MLP)
                  MVCNT = MVCNT + 1
*
*              Save time by jumping out
                  GOTO 100
*
               ENDIF
*
            ENDDO
*
100         CONTINUE
*
         ENDIF
*
      ENDDO
*
* Save the current value
      MSAVE = MLP
*
200   CONTINUE
*
      IF (MVCNT .GE. 1) THEN
*
*  Calculate the mean MVR in this time bin
         MVMEAN = MVRTOT / REAL(MVCNT)
      ELSE
*
*  If no MVR data in this time bin - use the last value or the average if
*  no last value. Neither of these are that accurate but should be ok
*  in practice.
         IF (MVR(MLP) .GT. 0) THEN
            MVMEAN = MVR( MIN (MLP, NMVTIM) )
         ELSE
            MVMEAN = AVMVR
         ENDIF
*
      ENDIF
*
      END

*+XRTSUB_PARWRITE - calculates the total particle rate.
      SUBROUTINE XRTSUB_PARWRITE(NT, NE, P1, P2, P3, PTOT)
*    Description :
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     28-Apr-1992
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER NT               ! Number of time bins
      INTEGER NE               ! Number of energy bins
      REAL P1(NT,NE)           ! 1st particle count
      REAL P2(NT,NE)           ! 2nd particle count
      REAL P3(NT,NE)           ! 3rd particle count
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      REAL PTOT(NE)            ! Total particle counts
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER LPE,LPT
*    Local data :
*     <any DATA initialisations for local variables>
*-
      DO LPE=1,NE
         PTOT(LPE) = 0.0
         DO LPT=1,NT
            PTOT(LPE) = PTOT(LPE) + P1(LPT,LPE) +
     &                          P2(LPT,LPE) + P3(LPT,LPE)
         ENDDO
      ENDDO
*
      END

*+XRTSUB_PHA2EN   Calcs. energies from pulse height values
      SUBROUTINE XRTSUB_PHA2EN(RLOC, EDIM, PHA, ENPHA, STATUS)
*    Description :
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*       Richard Saxton     (LTVAD::RDS)
*    History :
*     5-Jan-1990     original
*    20-May-1991     now handles radial bins
*    23-Apr-1992     response file is opened outside the routine
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
*    Import :
      CHARACTER*(DAT__SZLOC) RLOC         ! locator to response file
      INTEGER EDIM                        ! No. of corr. PH bins in input file
      REAL PHA(EDIM)                      ! Cemtre values of PHA bins
*    Import-Export :
*    Export :
      REAL ENPHA(EDIM)                    ! Energies of these bins
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
          EXTERNAL CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC)	DLOC			! Response data array
      INTEGER DDIM(2)                     ! Dimensions of detector resp. array
      INTEGER NPHA                        ! Number of PH chans in response arr.
      INTEGER NENERGY                     ! Number of energy bins in resp. arr.
      INTEGER TEPNTR,TAPNTR               ! Temp arrays
      INTEGER EPNTR                ! Energy and PH equivalent energy arr.
*                                         ! vignetting corrs. (only if EDIM=1)
      LOGICAL OK
      INTEGER NDIM
*    Local data :
*-
* Find the size of the response array
      CALL DAT_FIND( RLOC, 'DATA_ARRAY', DLOC, STATUS )
      CALL DAT_VALID( DLOC, OK, STATUS )
      CALL DAT_SHAPE( DLOC, 2, DDIM, NDIM, STATUS )
      CALL DAT_ANNUL( DLOC, STATUS )
      IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error accessing response array')
         GOTO 999
      ELSEIF (NDIM .NE. 2) THEN
         CALL MSG_PRNT('Response array does not have 2 dimensions')
         GOTO 999
      ENDIF
*
      NPHA = DDIM(1)
      NENERGY = DDIM(2)
*
* Map an array to contain the energies and temp space
      CALL DYN_MAPR(1,NENERGY+1,TEPNTR,STATUS)
      CALL DYN_MAPR(1,NPHA+1,TAPNTR,STATUS)
      CALL DYN_MAPR(1,NENERGY,EPNTR,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining dynamic memory')
         GOTO 999
      ENDIF
*
* Get energies of the effective area calibration points and the
* equivalent energies of the corrected PH channels of the input file
      CALL XRTSUB_RDRESP(RLOC, NPHA, NENERGY, EDIM, PHA,
     &                  %val(TEPNTR), %val(TAPNTR), %val(EPNTR),
     &                                       ENPHA, STATUS)
*
* Unmap arrays amd close the drmp file
      CALL DYN_UNMAP(TEPNTR,STATUS)
      CALL DYN_UNMAP(TAPNTR,STATUS)
      CALL DYN_UNMAP(EPNTR,STATUS)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSUB_PHA2EN', STATUS)
      ENDIF
*
      END

*+XRTSUB_POSCORR   Calcs. corrections for different box positions
      SUBROUTINE XRTSUB_POSCORR( SFID, SHEAD, BHEAD, RLOC, ELOC,
     &                            EAX, EDIM, RDIM, PFACTOR, STATUS)
*    Description :
*       Calculates the ratio of the instrument response at the
*       positions of the source and background boxes. This is an energy
*       dependent effect and hence the correction factor produced is
*       an array containing this ratio for the energy at the centre of
*       each corrected pulse height bin in the input file.
*       If there is no corrected pulse height info in the datafile
*       (i.e. EDIM=1) then the ratio at the mean photon energy is used.
*       The mean photon energy is obtained from the environment.
*    Environment parameters :
*       ENERGY       REAL      The mean photon energy in keV
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*       Richard Saxton     (LTVAD::RDS)
*    History :
*     5-Jan-1990     original
*    20-May-1991     now handles radial bins
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      INTEGER			SFID			! Source file id
      RECORD /CORR/ SHEAD                 ! Header info for source file
      RECORD /CORR/ BHEAD                 ! Header info for bkgnd file
      CHARACTER*(DAT__SZLOC) RLOC         ! Locator to the response file
      CHARACTER*(DAT__SZLOC) ELOC         ! Effective area file Locator
      INTEGER EAX                         ! Axis number of Corr PH axis
      INTEGER EDIM                        ! No. of corr. PH bins in input file
      INTEGER RDIM                        ! No. of radial bins in input file
*    Import-Export :
*    Export :
      REAL PFACTOR(EDIM,RDIM)             ! Vignetting correction ratios
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
          EXTERNAL CHR_LEN
*    Local constants :
      REAL MCHAN
          PARAMETER (MCHAN = 50)          ! Approximate channel number of mean
*                                         ! energy value (0.2 keV). This number
*                                         ! has a very small effect.
*    Local variables :
      CHARACTER*(DAT__SZLOC)	DLOC			! Response data locator
      REAL MEAN_EN                        ! Mean photon energy
      INTEGER SPNTR,BPNTR                 ! Effective ares as a fn of energy
      INTEGER DDIM(2)                     ! Dimensions of detector resp. array
      INTEGER NPHA                        ! Number of PH chans in response arr.
      INTEGER NENERGY                     ! Number of energy bins in resp. arr.
      INTEGER TEPNTR,TAPNTR               ! Temp arrays
      INTEGER EPNTR, APNTR                ! Energy and PH equivalent energy arr.
      INTEGER PPNTR                       ! Pointer to the PHA axis
      REAL SCORR,BCORR                    ! Single value for source and backgnd
*                                         ! vignetting corrs. (only if EDIM=1)
      LOGICAL VFLAG                       ! vignetting corrections calculated ?
      LOGICAL OK
      INTEGER LP,LP2,RLP,NDIM
*    Local data :
*-

* Find the size of the response array
      CALL DAT_FIND( RLOC, 'DATA_ARRAY', DLOC, STATUS )
      CALL DAT_VALID( DLOC, OK, STATUS )
      CALL DAT_SHAPE( DLOC, 2, DDIM, NDIM, STATUS )
      CALL DAT_ANNUL( DLOC, STATUS )
      IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error accessing response array')
         GOTO 999
      ELSEIF (NDIM .NE. 2) THEN
         CALL MSG_PRNT('Response array does not have 2 dimensions')
         GOTO 999
      ENDIF
*
      NPHA = DDIM(1)
      NENERGY = DDIM(2)
*
* Map an array to contain the energies and temp space
      CALL DYN_MAPR(1,NENERGY+1,TEPNTR,STATUS)
      CALL DYN_MAPR(1,NPHA+1,TAPNTR,STATUS)
      CALL DYN_MAPR(1,NENERGY,EPNTR,STATUS)
      CALL DYN_MAPR(1,NPHA, APNTR,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining dynamic memory')
         GOTO 999
      ENDIF
*
* Map the pulse height channel if available, otherwise use the
* channel of the mean energy.
      IF (EDIM .GT. 1) THEN
         CALL BDI_AXMAPR( SFID, EAX, 'Data', 'READ', PPNTR, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping corrected PH axis')
            GOTO 999
         ENDIF
*
      ELSE
         CALL DYN_MAPR(1,1,PPNTR,STATUS)
         CALL ARR_INIT1R(MCHAN, 1, %val(PPNTR),STATUS)
      ENDIF
*
* Get energies of the effective area calibration points and the
* equivalent energies of the corrected PH channels of the input file
      CALL XRTSUB_RDRESP(RLOC, NPHA, NENERGY, EDIM, %val(PPNTR),
     &                  %val(TEPNTR), %val(TAPNTR), %val(EPNTR),
     &                                        %val(APNTR), STATUS)
*
* Unmap axis pointer
      IF (EDIM .GT. 1) THEN
         CALL BDI_AXUNMAP( SFID, EAX, 'Data', PPNTR, STATUS )
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error unmapping corrected PH axis')
            GOTO 999
         ENDIF
*
      ELSE
         CALL DYN_UNMAP(PPNTR,STATUS)
      ENDIF
*
* Map arrays to hold vignetting correction factors
      CALL DYN_MAPR(1,NENERGY,SPNTR,STATUS)
      CALL DYN_MAPR(1,NENERGY,BPNTR,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining dynamic memory')
         GOTO 999
      ENDIF
*
* Get mean energy from the environment if there isn't a CORR. PH axis
      IF (EDIM .EQ. 1) THEN
         CALL USI_GET0R('ENERGY', MEAN_EN, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
      ENDIF
*
* Get the effective area as a function of energy for the background file
      CALL XRT_VIGNET(BHEAD, ELOC, NENERGY, %val(EPNTR), MEAN_EN,
     &                .FALSE., EDIM, %val(BPNTR), BCORR, VFLAG, STATUS)
*
* Loop over each radial bin
      DO RLP=1,RDIM
*
*   Calculate the off axis angle of this radial bin. Write into SHEAD
         CALL XRTSUB_OFFAX(RDIM, RLP, SHEAD)
*
D      WRITE(*,*) SHEAD.OFFAX
*
*   Get the effective area as a function of energy for the source file
         CALL XRT_VIGNET(SHEAD, ELOC, NENERGY, %val(EPNTR), MEAN_EN,
     &              .FALSE., EDIM, %val(SPNTR), SCORR, VFLAG, STATUS)
*
*   If the above produced an error don't do the correction
         IF (STATUS .NE. SAI__OK) THEN
*
            CALL MSG_PRNT('Error calculating vignetting correction '/
     &       /'factors - not correcting background box to source pos.')
*
*     Set correction factors to 1
            DO LP2=1,RDIM
               DO LP=1,EDIM
                  PFACTOR(LP,LP2)=1.0
               ENDDO
            ENDDO
*
            STATUS = SAI__OK
*
            GOTO 999
         ENDIF
*
*   Calculate the ratios of the effective areas
*      None-spectral file
         IF (EDIM .EQ. 1) THEN
*
*      Calc. the ratio of the correction factors (NB: these are not
*      the effective areas)
            PFACTOR(1,RLP) = BCORR / SCORR
*
            CALL MSG_SETR('PFACT', NINT(PFACTOR(1,RLP)*100)/100.)
            CALL MSG_PRNT('Positional correction factor : ^PFACT')
*
*    Spectral file
         ELSE
*
            CALL XRTSUB_POSCORR_SPEC(NENERGY, %val(EPNTR), EDIM, RDIM,
     &             RLP, %val(APNTR), %val(SPNTR), %val(BPNTR), PFACTOR)
*
         ENDIF
*
      ENDDO                    ! Loop over radial bins
*
* Unmap dynamic arrays
      CALL DYN_UNMAP(TEPNTR,STATUS)
      CALL DYN_UNMAP(TAPNTR,STATUS)
      CALL DYN_UNMAP(EPNTR,STATUS)
      CALL DYN_UNMAP(APNTR,STATUS)
      CALL DYN_UNMAP(SPNTR,STATUS)
      CALL DYN_UNMAP(BPNTR,STATUS)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSUB_POSCORR', STATUS)
      ENDIF
*
      END

*+XRTSUB_POSCORR_SPEC   Finds position corrections for each energy bin
      SUBROUTINE XRTSUB_POSCORR_SPEC(NENERGY, ENERGY, EDIM, RDIM,
     &                               RLP, AXEN, SVIG, BVIG, PFACTOR)
*    Description :
*     Calculates the factor needed to correct the background counts
*     to the position of the source box. This is the ratio of the
*     vignetting corrections for the background and source boxes.
*     A factor is calculated for each corrected PH channel.
*    History :
*     5-Jan-1990   original (LTVAD::RDS)
*    20-Dec-1992   fixed a bug causing the wrong vignetting factor
*                  to be returned. (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER NENERGY                    !Number of energy points for
*                                        !vignetting correction measurements
      REAL ENERGY(NENERGY)               !Energies of vignetting measurements
      INTEGER EDIM                       !Number of corrected PH bins in input
      INTEGER RDIM                       !Number of radial bins in input file
      INTEGER RLP                        !Radial bin number being corrected
      REAL AXEN(EDIM)                    !Equiv. energy of PH bin centres
      REAL SVIG(NENERGY)                 !Source vignetting corrections
      REAL BVIG(NENERGY)                 !Background vignetting corrections
*    Import-Export :
*    Export :
      REAL PFACTOR(EDIM,RDIM)            !Backgound position correction factors
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER ELP,BLP
      REAL PTOT,PMEAN
*-
      PTOT = 0.0
*
*  Loop over each corrected pulse height bin
      DO ELP=1,EDIM
*
*    Loop over each of the test energies
         DO BLP=1,NENERGY-1
*
*       Find the nearest trial energy to the energy of this pulse height bin
            IF ( AXEN(ELP) .GE. ENERGY(BLP)  .AND.
     &           AXEN(ELP) .LE. ENERGY(BLP+1) ) THEN
*
*          Calculate the ratio of the effective areas at this energy
*          Fill the corresponding radial bin of the corrections array
               PFACTOR(ELP,RLP) = SVIG(BLP) / BVIG(BLP)
*
*          Leave inner loop
               GOTO 10
*
            ENDIF
         ENDDO
*
*   If the energy of this PH channel wasn't found, best set the correction
*   to 1.0 and output a message. This is the case for the first 7 channels.
*   which are acknowledged to be bad anyway.
         CALL MSG_SETI('ELP', ELP)
         CALL MSG_PRNT('Warning: Energy of channel ^ELP out of bounds')
*
         PFACTOR(ELP,RLP)=1.0
*
10       CONTINUE
*
*  Sum contributions
         PTOT = PTOT + PFACTOR(ELP,RLP)
*
      ENDDO
*
      PMEAN = PTOT / REAL(EDIM)
*
      CALL MSG_SETR('PFACT', NINT(PMEAN*100)/100.)
      CALL MSG_PRNT('Mean position correction factor : ^PFACT')
*
      END

*+XRTSUB_RDRESP    Reads energy info from the detector matrix file
      SUBROUTINE XRTSUB_RDRESP(RLOC, NPHA, NENERGY, EDIM, PHCENT,
     &             ENERGY_BOUNDS, EPHA_BOUNDS, ENVAL, EPHVAL, STATUS)
*    Description :
*       Reads the response file and gets the trial energies for
*      the effective area file. Also gets the equivalent energy for
*      each PHA file.
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*      Richard Saxton
*    History :
*     6-Jan-1990     original   (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Structure definitions :
*    Import :
      CHARACTER*(DAT__SZLOC) RLOC               !Locator to the response file
      INTEGER NPHA                              !Number of PH chans in resp arr.
      INTEGER NENERGY                           !Number of Energies in resp arr.
      INTEGER EDIM                              !Number of pulse height output
*                                               !bins
      REAL PHCENT(EDIM)                         !Chanel value of PHA bin centres
*    Import/export :
      REAL ENERGY_BOUNDS(NENERGY+1)             !Work array for energy boundarys
      REAL EPHA_BOUNDS(NPHA+1)                  !Work array for all PH boundarys
*    Export :
      REAL ENVAL(NENERGY)                       !Array of trial energies
      REAL EPHVAL(EDIM)                         !Equivalent energies of the
*                                               !PH bin centres
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      INTEGER MAXHEAD
          PARAMETER (MAXHEAD=1000)
*    Local variables :
      CHARACTER*(DAT__SZLOC) MLOC,FLOC          !Locators to the FITS object
      CHARACTER*90 FHEAD(MAXHEAD)               !Array for header records
      INTEGER LP            !Loop counters
      INTEGER TPNTR                             !Pointer to temp workspace
      INTEGER NHEAD                             !Number of header records in
*                                               !FITS structure.
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
*  Read the FITS header lines in this datafile
      CALL DAT_FIND(RLOC, 'MORE', MLOC, STATUS)
      CALL DAT_FIND(MLOC, 'FITS', FLOC, STATUS)
      CALL DAT_SIZE(FLOC, NHEAD, STATUS)
*
*  Check that the character array has been declared large enough
      IF (NHEAD .GT. MAXHEAD) THEN
         CALL MSG_PRNT('The FITS header is longer than allowed - '/
     &                /'refer to author of XRTRESP')
         GOTO 999
      ENDIF
*
      CALL DAT_GETC(FLOC, 1, NHEAD, FHEAD, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error reading FITS structure in response file')
         GOTO 999
      ENDIF
*
*  Map temporary workspace
      CALL DYN_MAPR(1, NENERGY, TPNTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error mapping temporary space')
         GOTO 999
      ENDIF

*  Translate this header array into energy information
      CALL XRT_RDRESPHEAD(MAXHEAD, FHEAD, NHEAD, NPHA, NENERGY,
     &            %val(TPNTR), ENERGY_BOUNDS, EPHA_BOUNDS, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*  Set the energy out values
      DO LP=1,NENERGY
         ENVAL(LP) = ENERGY_BOUNDS(LP) + ( ENERGY_BOUNDS(LP+1)
     &                      - ENERGY_BOUNDS(LP) ) / 2.0
      ENDDO
*
*  Set the PH channel out energy values. Use the closest PH value
*  to the bin centre
      DO LP=1,EDIM
         EPHVAL(LP) = EPHA_BOUNDS( INT( PHCENT(LP) ) )
      ENDDO
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRTSUB_RDRESP',STATUS)
      ENDIF
*
      END

*+XRTSUB_SUBIMAGE  -  Subtracts an XRT image
      SUBROUTINE XRTSUB_SUBIMAGE(NX, NY, NE, BDATA, BVAR,
     &                            SVDEF, SDATA, SVAR, STATUS)
*    Description :
*    History :
*     1-JUN-1991   original (ltvad::rds)
*     20-Sept-1991   now processes spectral images  (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER NX,NY,NE              ! Dimensions of data
      REAL BDATA(NX,NY,NE)          ! Background data array
      REAL BVAR(NX,NY,NE)           ! Background variance array
      REAL SVDEF                    ! Source default variance
*    Import-Export :
      REAL SDATA(NX,NY,NE)          ! Source data array
      REAL SVAR(NX,NY,NE)           ! Source variance array
*    Export :
*     <declarations and descriptions for exported arguments>
*    Status :
      INTEGER STATUS
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER LP1,LP2,LP3
*-

      IF (STATUS.NE.SAI__OK) RETURN



      DO LP3=1,NE
         DO LP2=1,NY
            DO LP1=1,NX
*
               SDATA(LP1,LP2,LP3) = SDATA(LP1,LP2,LP3) -
     &                                          BDATA(LP1,LP2,LP3)
*
* Check if the source variance is below the minimum
               IF (SVAR(LP1,LP2,LP3) .LT. SVDEF)SVAR(LP1,LP2,LP3)=SVDEF
*
               SVAR(LP1,LP2,LP3) = SVAR(LP1,LP2,LP3) +
     &                                          BVAR(LP1,LP2,LP3)
*
            ENDDO
         ENDDO
      ENDDO
*
      END




*+XRT_CALCAREA - Calculate area of a box
      SUBROUTINE XRT_CALCAREA(HEAD, LQUAL, NX, NY, NT, NE,
     &                                    NR, QUAL, MASK, AREA)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global variables :
      INCLUDE 'QUAL_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ HEAD                ! Header structure
      LOGICAL LQUAL                     ! Is quality available ?
      INTEGER NX,NY,NT,NE,NR            ! Dimensions of quality array
      BYTE QUAL(NX,NY,NT,NE,NR)         ! Quality
      BYTE MASK                         ! Quality mask
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      REAL AREA                         ! Box area in square arcmins
*    Local constants :
      REAL PI
        PARAMETER (PI=3.14159265)
*    Local variables :
      INTEGER PCOUNT,LPX,LPY
      REAL XOFF,YOFF                    ! X and Y off axis angle (degs)
      REAL AXOFF                        ! Mean off-axis angle (arcmins)
*-
* Either find the area by counting pixels if possible and wanted or
* calculate the geometric area
      IF (.NOT. LQUAL .OR. (NX.EQ.1 .AND. NY.EQ.1) ) THEN
*
         IF (HEAD.SHAPE(1:1) .EQ. 'R') THEN
*
*   Square box:
            AREA = 4.0 * ( HEAD.XOUTER * HEAD.YOUTER -
     &                     HEAD.XINNER * HEAD.YINNER )
*
         ELSEIF (INDEX ('CAE', HEAD.SHAPE(1:1)) .NE. 0) THEN
*
*   Circle, ellipse or annulus
            AREA = PI * ( HEAD.XOUTER * HEAD.YOUTER -
     &                             HEAD.XINNER * HEAD.YINNER )
*
         ENDIF
*
      ELSE
*
*   Calculate the area by finding the number of good pixels
*   Just take one slice in time and energy.
         PCOUNT = 0
         AXOFF = 0.0
*
         DO LPY=1,NY
            DO LPX=1,NX
*
               IF ( (QUAL(LPX,LPY,1,1,1) .AND. MASK)
     &                                         .EQ. QUAL__GOOD ) THEN
                  PCOUNT = PCOUNT + 1
*
*       Sum the offset angles of each GOOD pixel. (Relies on the X axis
*       increasing from right to lef and Y increasing from bottom to top !)
                  XOFF = HEAD.XCENT - (NX/2.0 - LPX) * HEAD.XSCALE
                  YOFF = HEAD.YCENT + (NY/2.0 - LPY) * HEAD.YSCALE
                  AXOFF = AXOFF + SQRT(XOFF*XOFF + YOFF*YOFF)
*
               ENDIF
*
            ENDDO
         ENDDO
*
*   Check some good pixels were found
         IF (PCOUNT .EQ. 0) THEN
            CALL MSG_PRNT('** Error: No good pixels found **')
            GOTO 999
         ENDIF
*
*   Modify the off-axis value
         HEAD.OFFAX = AXOFF / REAL(PCOUNT) * 60.
*
*   Multiply by the area of a pixel in square degrees.
         AREA = PCOUNT * HEAD.XSCALE * HEAD.YSCALE
*
      ENDIF
*
* Convert area in square degrees into arcmin^2
      AREA = AREA * 3600.
*
999   CONTINUE

      END
