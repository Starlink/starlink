*+  XRTRESP - Creates an energy response structure inside a spectral NDF.
      SUBROUTINE XRTRESP( STATUS )
* Description :
*      Takes an input datafile and a detector response file and puts
*     a response structure into the datafile so that it can be used in the
*     Asterix spectral fitting package.
* Environment parameters :
* Method :
* Deficiencies :
* Bugs :
* Authors :
*       Richard Saxton
* History :
*      21 Aug 1990      original
*      28 May 1991      V1.3-3  supports radial bins  (LTVAD::RDS)
*      27 Jul 1991      V1.3-4  truncates response at 1.0E-12 (LTVAD::RDS)
*      10 Dec 1992      V1.6-1  fixed a 1 bin shift in the energy
*                               for PH channel array (RDS)
*      24 Apr 1994  (v1.7-0) for new asterix release
*       7 Mar 95        V1.8-0  HRI response (RJV)
*      30 Aug 95        V1.8-1  fixed bug that crashed ICL (RJV)
*     15 Jan 1996 V2.0-0 Partial ADI port (DJA)
*
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
* Status :
      INTEGER STATUS
* Function declarations :
      INTEGER CHR_LEN
* Local variables :
      CHARACTER*(DAT__SZLOC) DLOC             ! Locator to response file
      CHARACTER*(DAT__SZLOC) RLOC             ! Locator to response file
      CHARACTER*80 HTEXT(2)                   ! Text for history.
      CHARACTER*132 RFILE                     ! Name of detector response file
      CHARACTER*132 CALDIR                     ! Name of XRT cal. dir
      LOGICAL OK                              ! Is data array in input file ?
      LOGICAL LCONT                           ! Continue execution ?
      INTEGER LIST_DIM                        ! Dimension of list arrays
      INTEGER ELIST_PTR                       ! Pointer to mapped energy list
      INTEGER CLIST_PTR                       ! Pointer to mapped channel list
      INTEGER RESP_PTR                        ! Pointer to mapped response list
      INTEGER CR_PTR                          ! Pointer to corrected responses
      INTEGER DMX_DIM(3)                      ! Dimensions of workspace arrays
      INTEGER DMX_PTR                         ! Pointer to workspace array
*                                             ! boundaries
      INTEGER TOT_PTR                         ! Pointer to workspace array
      INTEGER EB_PTR                          ! Energy bounds array
      INTEGER PB_PTR                          ! PH bounds pointer
      INTEGER W_PTR                           ! Workspace array
      INTEGER EC_PTR                          ! Energy corrections array
      INTEGER EPB_PTR                         ! Pointer to equivalent energy
*                                             ! of the PH boundaries
      INTEGER ELO_PTR			      ! HRI low energy bound
      INTEGER EHI_PTR                         ! HRI hi energy bound
      INTEGER MAT_PTR                         ! HRI matrix
      INTEGER NEN                             ! HRI number of energy bands
      INTEGER NLIST                           ! Length of lists
      INTEGER NPHA_OUT                        ! Number of pulse height chns.
*                                             ! in input datafile
      INTEGER NR                              ! Number of radial bins in input
      INTEGER NDIM                            ! Number of dimensions of data
      INTEGER NPHA                            ! Number of PH chans in response
*                                             ! array
      INTEGER NENERGY                         ! Number of energy channels
      INTEGER L,IFID
      LOGICAL HRI                             ! attach HRI response
      LOGICAL PSPC                            ! attach PSPC response
* Local data :
* Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'XRTRESP Version 2.1-0')
*-
      IF (STATUS.NE.SAI__OK) RETURN
*
      CALL MSG_PRNT(VERSION)

*  Initialise ASTERIX
      CALL AST_INIT
      CALL ADI_REQPKG( 'eresp', STATUS )

*  Get input filename
      CALL USI_ASSOC( 'INP', 'BinDS', 'UPDATE', IFID, STATUS )

*  HRI or PSPC (default)
      CALL USI_GET0L('HRI',HRI,STATUS)
      PSPC=.NOT.HRI
      IF (STATUS .NE. SAI__OK) GOTO 999

*  Set the default for the detector response file
      CALL XRT_CALDEF(CALDIR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Warning: XRT cal directory not found')
         CALL ERR_ANNUL(STATUS)
      ENDIF

      IF (HRI) THEN
        CALDIR = CALDIR(1:CHR_LEN(CALDIR)) // 'hri_drm'
      ELSE
        CALDIR = CALDIR(1:CHR_LEN(CALDIR)) // 'drmpspc'
      ENDIF
      L=CHR_LEN(CALDIR)
      CALL USI_DEF0C('RESPFILE', CALDIR(:L), STATUS)
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

*  HRI response?
      IF (HRI) THEN

        CALL CMP_MAPN(RLOC,'ENERG_LO','_REAL','READ',1,ELO_PTR,NEN,
     :                                                       STATUS)
        CALL CMP_MAPN(RLOC,'ENERG_HI','_REAL','READ',1,EHI_PTR,NEN,
     :                                                       STATUS)
        CALL CMP_MAPN(RLOC,'MATRIX','_REAL','READ',1,MAT_PTR,NEN,
     :                                                       STATUS)

        CALL XRTRESP_WRIENERGY_HRI( IFID, NEN, %VAL(ELO_PTR),
     :                          %val(EHI_PTR),
     :                          %val(MAT_PTR), VERSIOn, STATUS)

        CALL CMP_UNMAP(RLOC,'ENERG_LO',STATUS)
        CALL CMP_UNMAP(RLOC,'ENERG_HI',STATUS)
        CALL CMP_UNMAP(RLOC,'MATRIX',STATUS)

*  Otherwise PSPC
      ELSE IF ( PSPC ) THEN

*    Find the size of the response array
        CALL DAT_FIND( RLOC, 'DATA_ARRAY', DLOC, STATUS )
        CALL DAT_VALID( DLOC, OK, STATUS )
        CALL DAT_SHAPE( DLOC, 2, DMX_DIM, NDIM, STATUS )
        CALL DAT_ANNUL( DLOC, STATUS )
        IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error accessing response array')
           GOTO 999
        ELSEIF (NDIM .NE. 2) THEN
           CALL MSG_PRNT('Response array does not have 2 dimensions')
           GOTO 999
        ENDIF

*    Set number of PHA and ENERGY bins in the response file. NB: This relies
*    on the PHA axis being first.
        NPHA=DMX_DIM(1)
        NENERGY=DMX_DIM(2)

*    Map an array to hold the energy boundaries
        CALL DYN_MAPR(1, NENERGY+1, EB_PTR, STATUS)

*    Map an array to hold the pulse height boundaries in the input file
        CALL DYN_MAPR(1, NPHA+1, PB_PTR, STATUS)

*    Map an array to hold the energy boundaries
        CALL DYN_MAPR(1, NPHA, EPB_PTR, STATUS)

*    Get info from the datafile including the PHA channel boundaries.
        CALL XRTRESP_READFILE( IFID, NPHA, NPHA_OUT, %val(PB_PTR),
     &                                                NR, STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*
* If NR > 10 issue a warning
        IF (NR .GT. 10) THEN
           CALL MSG_SETI('SIZ',NR*3000)
           CALL MSG_PRNT(
     :               'Warning: the output file will be ^SIZ blocks')
*
*    Ask if user wants to continue
           CALL USI_GET0L('CONTINUE', LCONT, STATUS)
*
           IF (STATUS .NE. SAI__OK .OR. .NOT. LCONT) GOTO 999
*
        ENDIF

*    Map an array to hold the full NENERGY * NR energy correction array
        CALL DYN_MAPR( 1, NENERGY*NR, EC_PTR, STATUS )

*    Map workspace
        CALL DYN_MAPR(1, NENERGY, W_PTR, STATUS)
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error obtaining dynamic memory')
           GOTO 999
        ENDIF
*
        CALL XRTRESP_GETCORR( IFID, NENERGY, NR, %val(W_PTR),
     &                                 %val(EC_PTR), STATUS )
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*

* Map energy index, channel index and response lists as temporary 2-d arrays.
        LIST_DIM=NENERGY*NPHA_OUT
*
        CALL DYN_MAPI(1,LIST_DIM,ELIST_PTR,STATUS)
        CALL DYN_MAPI(1,LIST_DIM,CLIST_PTR,STATUS)
        CALL DYN_MAPR(1,LIST_DIM,RESP_PTR,STATUS)
*
* Map temporary array for the detector matrix
        CALL DYN_MAPR(2,DMX_DIM,DMX_PTR,STATUS)

* Map temp array as workspace for XRTRESP_RDRESP
        DMX_DIM(1) = NENERGY
        DMX_DIM(2) = NPHA_OUT
        CALL DYN_MAPR(2,DMX_DIM,TOT_PTR,STATUS)
        IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_REP(' ','Error mapping temporary space',STATUS)
           GOTO 999
        ENDIF

*    Read the response file
        CALL XRTRESP_RDRESP(RLOC,NPHA,NENERGY,NPHA_OUT,%val(DMX_PTR),
     &            %val(TOT_PTR), %val(PB_PTR),
     &            %val(EB_PTR), %val(EPB_PTR), NLIST,
     &            %val(ELIST_PTR), %val(CLIST_PTR), %val(RESP_PTR),
     &                 STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*
* Map workspace array
        CALL DYN_MAPR(1,NLIST,CR_PTR,STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_REP('Error mapping temporary space',STATUS)
           GOTO 999
        ENDIF
*
* Write response structure into the datafile
        CALL XRTRESP_WRIENERGY_PSPC( IFID, NPHA, NENERGY,%val(EB_PTR),
     &        NPHA_OUT,NR,%val(EC_PTR),%val(PB_PTR),%val(EPB_PTR),NLIST,
     &           %VAL(ELIST_PTR), %VAL(CLIST_PTR), %VAL(RESP_PTR),
     &                                    VERSION, %VAL(CR_PTR), STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*
* Unmap dynamic components
        CALL DYN_UNMAP(EB_PTR,STATUS)
        CALL DYN_UNMAP(PB_PTR,STATUS)
        CALL DYN_UNMAP(EPB_PTR,STATUS)
        CALL DYN_UNMAP(EC_PTR,STATUS)
        CALL DYN_UNMAP(W_PTR,STATUS)
        CALL DYN_UNMAP(CR_PTR,STATUS)
        CALL DYN_UNMAP(ELIST_PTR,STATUS)
        CALL DYN_UNMAP(CLIST_PTR,STATUS)
        CALL DYN_UNMAP(RESP_PTR,STATUS)
        CALL DYN_UNMAP(DMX_PTR,STATUS)
        CALL DYN_UNMAP(TOT_PTR,STATUS)

      ENDIF		! HRI or PSPC

*  Write history component
      CALL HSI_ADD( IFID, VERSION, STATUS )
      HTEXT(1)='Set up detector response structure in datafile'
      CALL HSI_PTXT( IFID,1,HTEXT,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error adding history record')
      ENDIF

*  Tidy up
999   CONTINUE
      CALL USI_ANNUL('INP',STATUS)

      CALL HDS_CLOSE(RLOC,STATUS)

      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  XRTRESP_GETCORR - Gets correction arrays from the input file
      SUBROUTINE XRTRESP_GETCORR( FID, NENERGY, NR, EWORK, ECORR,
     :                            STATUS )
*    Description :
*     Reads the corrections array from the input file
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*      Richard Saxton
*    History :
*    28-May-1991   original   (LTVAD::RDS)
*    Parameters :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
*    Import :
      INTEGER			FID			! Input file id
      INTEGER NENERGY                           !Number of trial energies
      INTEGER NR                                !Number of radial bins
*    Import/export :
      REAL EWORK(NENERGY)                       !Corrections array for one bin
*    Export :
      REAL ECORR(NENERGY,NR)                    !Energy corrections array
*
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) ILOC               !Locator to instrument box
      CHARACTER*(DAT__SZLOC) CLOC               !Locator to cell
      CHARACTER*(DAT__SZLOC) ENCLOC             !Locator to energy corrections
      INTEGER RLP,IDUM,ELP
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Get locator to instrument structure and header structure
      CALL ADI1_LOCINSTR( FID, .FALSE., ILOC, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error obtaining locator to instrument info.',
     &                                            STATUS)
         GOTO 999
      ENDIF
*
* Read the energy correction arrays from the instrument box
      CALL DAT_FIND(ILOC, 'ENCORR', ENCLOC, STATUS)
*
* Loop over each radial bin
      DO RLP=1,NR
*
*   Get locator to the correction array for this bin
         CALL DAT_CELL(ENCLOC, 1, RLP, CLOC, STATUS)
*
*   Get the array for this bin
         CALL CMP_GET1R(CLOC, 'DATA_ARRAY', NENERGY, EWORK,
     &                                          IDUM, STATUS)
         CALL DAT_ANNUL(CLOC, STATUS)
*
*   Was array ok ?
         IF (STATUS .NE. SAI__OK) THEN
*
            STATUS = SAI__OK
*
*     Attempt to read old style format.
            CALL CMP_GET1R(ENCLOC, 'DATA_ARRAY', NENERGY, EWORK,
     &                                                IDUM, STATUS)
            IF (STATUS .NE. SAI__OK) GOTO 999
*
         ENDIF
*
*   Write into large corrections array
         DO ELP=1,NENERGY
            ECORR(ELP,RLP) = EWORK(ELP)
         ENDDO
*
      ENDDO
*
      CALL DAT_ANNUL(ENCLOC, STATUS)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('** Error reading energy correction '/
     &                /'array from datafile - exiting XRTRESP **')
      ENDIF
*
      END


*+  XRTRESP_RDRESP - Calculates the spectral response function for this file
      SUBROUTINE XRTRESP_RDRESP(RLOC, NPHA, NENERGY, NPHA_OUT,DMX,
     &               TOT, PHA_BOUNDS, ENERGY_BOUNDS, EPHA_CENTS,
     &               NLIST, ENERGY_LIST, CHAN_LIST, RESP, STATUS)
*    Description :
*       Reads the response file and calculates response arrays.
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*      Richard Saxton
*    History :
*     21-Aug-1990     original   (LTVAD::RDS)
*     10-Dec-1990     Multiplies response by the energy corrections
*                     array      (LTVAD::RDS)
*     28-May-1991     No longer multiplies by energy correction  (LTVAD::RDS)
*     29-July-1991    Truncates response at 1.0E-12 to reduce the
*                     number of response elements (LTVAD::RDS)
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
      INTEGER NPHA_OUT                          !Number of pulse height output
*                                               !bins
      REAL DMX(NPHA,NENERGY)                    !Detector matrix workspace
      REAL TOT(NENERGY,NPHA_OUT)                !Total array workspace
      REAL PHA_BOUNDS(NPHA+1)                   !Boundaries of PHA bins
*    Export :
      REAL ENERGY_BOUNDS(NENERGY+1)             !Boundaries of energy bins
      REAL EPHA_CENTS(NPHA)                     !Equivalent energies of the
*                                               !PH channel CENTRES
      INTEGER NLIST                             !Length of lists per channel
      INTEGER ENERGY_LIST(NENERGY*NPHA_OUT)     !List of energy indices
      INTEGER CHAN_LIST(NENERGY*NPHA_OUT)       !List of PH channel indices
      REAL RESP(NENERGY*NPHA_OUT)               !List of responses
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      INTEGER MAXHEAD
          PARAMETER (MAXHEAD=1000)
*    Local variables :
      CHARACTER*(DAT__SZLOC) MLOC,FLOC          !Locators to the FITS object
      CHARACTER*90 FHEAD(MAXHEAD)               !Array for header records
      INTEGER ENLP,PHLP            !Loop counters
      INTEGER NEWPH                             !Output PHA channel
      INTEGER RDIM(2),DDIM(2)
      INTEGER TPNTR                             !Pointer to temp workspace
      INTEGER NHEAD                             !Number of header records in
*                                               !FITS structure.
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
*  Read the input arrays from the HDS file
      RDIM(1)=NPHA
      RDIM(2)=NENERGY
      CALL CMP_GETNR(RLOC, 'DATA_ARRAY', 2, RDIM, DMX, DDIM, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error reading detector response from file')
         GOTO 999
      ENDIF
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
         CALL MSG_PRNT('Error reading FITS structure in input file')
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
     &            %val(TPNTR), ENERGY_BOUNDS, EPHA_CENTS, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*  Zero the total array
      CALL ARR_INIT1R( 0.0, NENERGY*NPHA_OUT, TOT, STATUS )
*
* The DMX matrix contains individual responses for all PHA channels
* Average these to get the response for each output channel.
*
*   Loop over energy and pulse height channels and sum detector responses
      DO PHLP=1,NPHA
*
*    Check which output pulse height channel this belongs to
         DO NEWPH=1,NPHA_OUT
*
            IF ( REAL(PHLP) .GT. PHA_BOUNDS(NEWPH)
     &               .AND. REAL(PHLP) .LE. PHA_BOUNDS(NEWPH+1) ) THEN
*
*      If this pha channel wanted write it into the TOT array
               DO ENLP=1,NENERGY
                  TOT(ENLP,NEWPH)=TOT(ENLP,NEWPH) + DMX(PHLP,ENLP)
               ENDDO
*
            ENDIF
*
         ENDDO
*
      ENDDO
*
*  The final reponse structure is stored as a sparse matrix. Find non-zero
*  elements and produce lists for output.
      NLIST=0
*
      DO ENLP=1,NENERGY
*
         DO PHLP=1,NPHA_OUT
*
            IF (TOT(ENLP,PHLP) .GT. 1.0E-12) THEN
*
*   Create lists
               NLIST=NLIST+1
               ENERGY_LIST(NLIST)=ENLP
               CHAN_LIST(NLIST) = PHLP
               RESP(NLIST) = TOT(ENLP,PHLP)
*
            ENDIF
*
         ENDDO
*
      ENDDO

 999  IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRTRESP_RDRESP',STATUS)
      ENDIF

      END


*+  XRTRESP_READFILE - Reads information from the input file
      SUBROUTINE XRTRESP_READFILE( FID, NPHA, NPHA_OUT,
     &                                PHA_BOUNDS, NR, STATUS)
*    Description :
*      Obtains the pulse height channel boundaries from the input datafile
*    Method :
*       If critical information cannot be read then status is set bad.
*    Deficiencies :
*    Bugs :
*    Authors :
*      Richard Saxton
*    History :
*    28-Aug-1990    original   (LTVAD::RDS)
*    28-May-1991    doesn't now get the corrections info.   (LTVAD::RDS)
*    Parameters :
*       PH_DIM            Integer              Axis number of PHA chan.
*       ERASE             LOGICAL              Delete energy_resp structure.
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
*    Import :
      INTEGER			FID			! Input file id
      INTEGER NPHA                              !Max number of PH chans.
*    Export :
      INTEGER NPHA_OUT                          !No of PHA output channels
      REAL PHA_BOUNDS(NPHA+1)                   !Array of PHA boundaries
      INTEGER NR                                !Number of radial bins
*
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) ALOC               !Locator to asterix structure
      CHARACTER*10 EXPT                         !Experiment eg. ME or LE

      INTEGER NDIM                              !No of dimensions in data array
      INTEGER DIMS(7)                           !Dimensions of data array
      INTEGER			DCIID			! Detector info
      INTEGER 			PHA_PTR                 ! Pulse height axis
      INTEGER WPTR                              !Pointer to axis widths
      INTEGER PHDIM                             !Axis number of PHA dimension
      INTEGER RADIM                             !Axis number of radial dimension

      LOGICAL THERE                             !Is object present ?
      LOGICAL ERASE                             !Erase energy_resp structure ?
      LOGICAL OK                                !Is the object there ?
      LOGICAL AXOK                              !Is the axis there ?
*-

*  Check inherited global status
      IF (STATUS .NE. SAI__OK) RETURN

*  Locate ASTERIX structure
      CALL ADI1_LOCAST( FID, .FALSE., ALOC, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error obtaining locator to ASTERIX box',
     &                                            STATUS)
         GOTO 999
      ENDIF

*  Detector info
      CALL DCI_GETID( FID, DCIID, STATUS )
      CALL ADI_CGET0C( DCIID, 'Instrument', EXPT, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
*
         CALL ERR_ANNUL(STATUS)
         CALL MSG_PRNT( 'Assuming data is from ROSAT XRT' )

*  Data is definitely not XRT
      ELSE IF ( INDEX( EXPT,'XRT' ) .EQ. 0 ) THEN
        CALL ERR_REP(' ','*Not ROSAT XRT data*',STATUS)
        STATUS=SAI__ERROR
        GOTO 999

      END IF

*  Find number of dimensions in the data array
      CALL BDI_CHK( FID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( FID, DAT__MXDIM, DIMS, NDIM, STATUS )
      IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error accessing data array in input file')
         GOTO 999
      ENDIF

*   Find which axis contains pulse height values
      CALL BDI0_FNDAXC( FID, 'E', PHDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL AXIS_TLIST( FID, NDIM, STATUS )
        CALL USI_GET0I( 'PH_DIM', PHDIM, STATUS )
        IF ( (STATUS .NE. SAI__OK) .OR. (PHDIM .LE. 0)
     :          .OR. (PHDIM.GT.NDIM) ) THEN

        ELSE
          CALL MSG_PRNT('Cant find pulse height axis information')
          STATUS=SAI__ERROR
          GOTO 999

        END IF

      END IF
      NPHA_OUT = DIMS(PHDIM)

*  Find the RADIAL axis if any
      CALL BDI0_FNDAXC( FID, 'R', RADIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        NR = 1
      ELSE
        NR = DIMS(RADIM)
      END IF

*   Get pulse height boundaries. First map the bin centres
      CALL BDI_AXCHK( FID, PHDIM, 'Data', AXOK, STATUS )

*  Map if ok
      IF ( AXOK ) THEN

        CALL BDI_AXMAPR( FID, PHDIM, 'Data', 'READ', PHA_PTR, STATUS )
        IF (STATUS .NE. SAI__OK) THEN
          STATUS=SAI__ERROR
          CALL ERR_REP(' ','Error mapping pulse height axis',STATUS)
          GOTO 999
        ENDIF
*
      ELSE
         CALL MSG_PRNT('Cant find pulse height axis array')
         STATUS=SAI__ERROR
         GOTO 999
      ENDIF

*  Now map the bin widths - if not present BDI will invent them
      CALL BDI_AXMAPR( FID, PHDIM, 'Width', 'READ', WPTR, STATUS )

*  Find the bin boundaries
      CALL XRTRESP_READFILE_BOUNDS(NPHA, NPHA_OUT, %VAL(PHA_PTR),
     &                                 %VAL(WPTR), PHA_BOUNDS)

*  If an energy_response structure exists ask if it should be deleted.
      CALL DAT_THERE(ALOC,'ENERGY_RESP',THERE,STATUS)
*
      IF (THERE) THEN
*
         CALL MSG_PRNT( 'Energy response structure already exists' )
         CALL USI_GET0L('ERASE',ERASE,STATUS)
*
*   Delete it?
         IF (ERASE) THEN
            CALL DAT_ERASE(ALOC,'ENERGY_RESP',STATUS)
         ELSE
*
*   Set status bad to exit from the program
            STATUS=SAI__ERROR
         ENDIF

      END IF

*  Tidy up
 999  IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT('XRTRESP_READFILE',STATUS)
      ENDIF

      END


*+ XRTRESP_READFILE_BOUNDS - Finds boundaries of pulse height channels
      SUBROUTINE XRTRESP_READFILE_BOUNDS(NPHA, NPHA_OUT, PHA_CEN,
     &                                     PHA_WID, PHA_BOUNDS)
*    Description :
*    History :
*     30-MAY-1989    original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global variables:
*    Import :
      INTEGER NPHA                     ! Number of ph bins in response file
      INTEGER NPHA_OUT                 ! Number of ph bins in data file
      REAL PHA_CEN(NPHA_OUT)           ! Centres of pulse height bins
      REAL PHA_WID(NPHA_OUT)           ! Widths of pulse height bins
*    Import-Export :
*    Export :
      REAL PHA_BOUNDS(NPHA+1)          ! Boundaries of pulse height bins
*    Local constants :
*    Local variables :
      INTEGER LP
*-
      DO LP=1,NPHA_OUT
         PHA_BOUNDS(LP)=PHA_CEN(LP)-PHA_WID(LP)/2.0
      ENDDO
      PHA_BOUNDS(NPHA_OUT+1)=PHA_CEN(NPHA_OUT)+PHA_WID(NPHA_OUT)/2.0

      END


*+ XRTRESP_WRIENERGY_PSPC - Writes PSPC energy response
      SUBROUTINE XRTRESP_WRIENERGY_PSPC(FID,NPHA,NENERGY,EBNDS,
     &             NPHA_OUT,NR,ECORR,PHA_BOUNDS,EPHA_CENTS,NLIST,
     &                ELIST,CLIST,RESP,VERSION,CRESP,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*        Richard Saxton
*    History :
*     Jan 10 1989        original           (LTVAD::RDS)
*     Mar 20 1991        calculates EPHA_BOUNDS for the PHA channels in
*                        the input file     (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*    Import :
      INTEGER			FID			! Input file id
      INTEGER NPHA,NENERGY                  ! Number of PH chans and energy
*                                           ! chans in response array
      REAL EBNDS(NENERGY+1)         ! Boundaries of energy bins
      INTEGER NPHA_OUT                      ! No of output PHA channels
      INTEGER NR                            ! No. of radial bins
      REAL ECORR(NENERGY,NR)                ! Energy correction array
      REAL PHA_BOUNDS(NPHA+1)               ! Boundaries of PHA bins
      REAL EPHA_CENTS(NPHA)                 ! Centre energy of PHA bins
      INTEGER NLIST                         ! List lengths for each output chn.
      INTEGER ELIST(NENERGY*NPHA_OUT) ! List of energy indices
      INTEGER CLIST(NENERGY*NPHA_OUT)   ! List of PH channel indices
      REAL RESP(NENERGY*NPHA_OUT)           ! List of responses
      CHARACTER*(*) VERSION                 ! XRTRESP Version message
*    Import/export
      REAL CRESP(NLIST)                     ! Workspace - corrected responses
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      INTEGER MAXEN
        PARAMETER(MAXEN=750)
      INTEGER MAXPHA
        PARAMETER(MAXPHA=300)
*    Local variables :
      INTEGER LP,RLP,LLP
      REAL CSPEC(MAXPHA)                ! Centre value of PHA bins
      INTEGER OEPTR                         ! Pointer to PHA energy bounds
      INTEGER			RMFID			! Reponse object
*-

*  Check inherited global status
      IF (STATUS .NE. SAI__OK) RETURN

*  Ensure work arrays are large enough
      IF (NPHA .GT. MAXPHA) THEN
         CALL MSG_PRNT('** Response array is larger than temp arrays '/
     &                /'- refer to author **')
         GOTO 99
      ENDIF

*  Create response object
      CALL ADI_NEW0( 'AsterixRMF', RMFID, STATUS )

*  Write version id
      CALL ADI_CPUT0C( RMFID, 'Version', VERSION, STATUS )
      CALL ADI_CPUT0I( RMFID, 'NCHAN', NPHA, STATUS )
      CALL ADI_CPUT0I( RMFID, 'NENERGY', NENERGY, STATUS )

*  Energy axis
      CALL ADI_CPUT1R( RMFID, 'Energy', NENERGY+1, EBNDS, STATUS )

*  Channel axis
      CALL ADI_CNEW1R( RMFID, 'Channels', NPHA_OUT+1, STATUS )
      CALL ADI_CMAPR( RMFID, 'Channels', 'WRITE', OEPTR, STATUS )
      CALL XRTRESP_WRIENERGY_SETBND( NPHA, EPHA_CENTS, NPHA_OUT,
     :                         PHA_BOUNDS, %val(OEPTR), STATUS )
      CALL ADI_CUNMAP( RMFID, 'Channels', OEPTR, STATUS )

*  Response sparse indices
      IF ( NLIST .GT. 0 ) THEN
        CALL ADI_CPUT1I( RMFID, 'EnergyIndices', NLIST, ELIST, STATUS )
        CALL ADI_CPUT1I( RMFID, 'ChannelIndices', NLIST, CLIST, STATUS )
      END IF

*  Create channel centres
      DO LP = 1, NPHA_OUT
        CSPEC(LP) = (PHA_BOUNDS(LP) + PHA_BOUNDS(LP+1)) / 2.0
      END DO
      CALL ADI_CPUT1R( RMFID, 'ChannelSpec', NPHA_OUT, CSPEC, STATUS )

*  Loop over each radial bin
      DO RLP = 1, NR

*    Write Response array
        IF ( NLIST .NE. 0 ) THEN

*      Multiply the response array by the corrections array for this radial bin
          DO LLP=1,NLIST
            CRESP(LLP) = RESP(LLP) * ECORR(ELIST(LLP),RLP)
          END DO

*      Write response values
          CALL ADI_CPUT1R( RMFID, 'RMF', NLIST, CRESP, STATUS )

        END IF

*    Write the response
        CALL ERI_PUTIDS( FID, RLP, NR, RMFID, ADI__NULLID, STATUS )

*  Next radial shell
      END DO

*  Tidy up
 99   IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT('XRTRESP_WRIENERGY_PSPC',STATUS)
      END IF
*
      END



*+ XRTRESP_WRIENERGY_HRI - Writes HRI energy response
      SUBROUTINE XRTRESP_WRIENERGY_HRI( FID, NEN, ENERGY_LO,ENERGY_HI,
     &                                    MATRIX,VERSION, STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*        RJV
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*    Structure definitions :
*    Import :
      INTEGER			FID			! Input file id
      INTEGER 			NEN
      REAL ENERGY_LO(*),ENERGY_HI(*)        ! Boundaries of energy bins
      REAL MATRIX(*)
      CHARACTER*(*)		VERSION
*    Import/export
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      INTEGER NMAX
        PARAMETER(NMAX=100)
*    Local variables:
      REAL 			EBNDS(NMAX+1)
      REAL 			CSPEC(1)
      REAL 			CBNDS(2)

      INTEGER 			ELIST(NMAX)
      INTEGER 			CLIST(NMAX)
      INTEGER 			LP
      INTEGER			RMFID			! Reponse object
*-

*  Check inherited global status
      IF (STATUS .NE. SAI__OK) RETURN

*  Check internal store is adequate
      IF ( NEN .GT. NMAX ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'AST_ERR: insufficient storage for matrix',
     :                STATUS )
        GOTO 99
      END IF

*  Calculate energy and channel centres
      DO LP=1,NEN
        ELIST(LP) = LP
        CLIST(LP) = LP
        EBNDS(LP) = ENERGY_LO(LP)
      END DO
      EBNDS(NEN+1) = ENERGY_HI(NEN)
      CSPEC(1) = 1.0
      CBNDS(1) = ENERGY_LO(1)
      CBNDS(2) = ENERGY_HI(NEN)

*  Create response object
      CALL ADI_NEW0( 'AsterixRMF', RMFID, STATUS )

*  Write version id
      CALL ADI_CPUT0C( RMFID, 'Version', VERSION, STATUS )
      CALL ADI_CPUT0I( RMFID, 'NCHAN', 1, STATUS )
      CALL ADI_CPUT0I( RMFID, 'NENERGY', NEN, STATUS )

*  Energy axis
      CALL ADI_CPUT1I( RMFID, 'EnergyIndices', NEN, ELIST, STATUS )
      CALL ADI_CPUT1R( RMFID, 'Energy', NEN+1, EBNDS, STATUS )

*  Channel axis
      CALL ADI_CPUT1I( RMFID, 'ChannelIndices', NEN, CLIST, STATUS )
      CALL ADI_CPUT1R( RMFID, 'Channels', 2, CBNDS, STATUS )
      CALL ADI_CPUT1R( RMFID, 'ChannelSpec', 1, CSPEC, STATUS )

*  Response elements
      CALL ADI_CPUT1R( RMFID, 'RMF', NEN, MATRIX, STATUS )

*  Write the response
      CALL ERI_PUTIDS( FID, 1, 1, RMFID, ADI__NULLID, STATUS )

*  Tidy up
 99   IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'XRTRESP_WRIENERGY_HRI', STATUS )
      END IF

      END


*+ XRTRESP_WRIENERGY_SETBND - Sets energy bounds for output PH chans.
      SUBROUTINE XRTRESP_WRIENERGY_SETBND(NPHA, EPHA_CENTS, NPHA_OUT,
     &                   PHA_BOUNDS, EPHA_BOUNDS, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER NPHA                   ! Number of PHA chans in detector matrix
      REAL EPHA_CENTS(NPHA)          ! Centre energy of each PH bin
      INTEGER NPHA_OUT               ! Number of PH bins in file
      REAL PHA_BOUNDS(NPHA+1)        ! PH chans.
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      REAL EPHA_BOUNDS(NPHA_OUT+1)   ! PH energy boundaries
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER			LP
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the correct channel bound energies for the NPHA_OUT + 1 boundaries
      DO LP = 1, NPHA_OUT+1

*    Test if channel boundary is less than one
        IF (PHA_BOUNDS(LP) .LT. 0.99) THEN
*
            EPHA_BOUNDS(LP) = EPHA_CENTS(1) -
     &      ( EPHA_CENTS(2) - EPHA_CENTS(1) ) * (1.0 - PHA_BOUNDS(LP))
*
*      Test if channel boundary is greater than NPHA
         ELSEIF (PHA_BOUNDS(LP) .GT. NPHA) THEN
*
            EPHA_BOUNDS(LP) = EPHA_CENTS(NPHA) + ( EPHA_CENTS(NPHA) -
     &               EPHA_CENTS(NPHA-1) ) * (PHA_BOUNDS(LP) - NPHA)
*
*      Test if channel boundary is an integer
         ELSEIF (MOD (PHA_BOUNDS(LP), 1.0) .LT. 0.0001) THEN
*
            EPHA_BOUNDS(LP) = EPHA_CENTS(NINT(PHA_BOUNDS(LP)))
*
         ELSE
*
            EPHA_BOUNDS(LP) = EPHA_CENTS(INT(PHA_BOUNDS(LP))) +
     &         ( EPHA_CENTS(INT(PHA_BOUNDS(LP)+1)) -
     &                      EPHA_CENTS(INT(PHA_BOUNDS(LP))))
     &         * MOD(PHA_BOUNDS(LP),1.0)
*
         ENDIF
*
      ENDDO

      END
