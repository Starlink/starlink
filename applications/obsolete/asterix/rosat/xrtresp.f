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
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
* Global variables :
* Structure definitions :
* Status :
      INTEGER STATUS
* Function declarations :
      INTEGER CHR_LEN
* Local variables :
      CHARACTER*(DAT__SZLOC) LOC              ! Locator to input file
      CHARACTER*(DAT__SZLOC) RLOC             ! Locator to response file
      CHARACTER*80 HTEXT(2)                   ! Text for history.
      CHARACTER*100 RFILE                     ! Name of detector response file
      CHARACTER*80 CALDIR                     ! Name of XRT cal. dir
      LOGICAL INPRIM                          ! Is the input data primitive ?
      LOGICAL OK                              ! Is data array in input file ?
      LOGICAL LCONT                           ! Continue execution ?
      INTEGER LIST_DIM                        ! Dimension of list arrays
      INTEGER ELIST_PTR                       ! Pointer to mapped energy list
      INTEGER CLIST_PTR                       ! Pointer to mapped channel list
      INTEGER RESP_PTR                        ! Pointer to mapped response list
      INTEGER CR_PTR                          ! Pointer to corrected responses
      INTEGER DMX_DIM(3)                      ! Dimensions of workspace arrays
      INTEGER CDIM(2)                         ! Dimensions of corrections array
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
      LOGICAL HRI                             ! attach HRI response
      LOGICAL PSPC                            ! attach PSPC response
* Local data :
* Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'XRTRESP Version 1.8-0')
*-
      IF (STATUS.NE.SAI__OK) RETURN
*
      CALL MSG_OUT(' ',VERSION,STATUS)
*
* Initialise the Asterix88 internal common blocks
      CALL AST_INIT
*
* Get input filename
      CALL USI_ASSOCI('INPUT','UPDATE',LOC,INPRIM,STATUS)
*
* HRI or PSPC (default)
      CALL USI_GET0L('HRI',HRI,STATUS)
      PSPC=.NOT.HRI

      IF (STATUS .NE. SAI__OK) GOTO 999
*
      IF (INPRIM) THEN
          CALL MSG_OUT(' ','Input data cannot be primitive',STATUS)
          STATUS=SAI__ERROR
          GOTO 999
      ENDIF
*
* Set the default for the detector response file
      CALL XRT_CALDEF(CALDIR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Warning: XRT cal directory not found')
         CALL ERR_ANNUL(STATUS)
      ENDIF
*
      IF (HRI) THEN
        RFILE = CALDIR(1:CHR_LEN(CALDIR)) // 'hri_drm'
      ELSE
        RFILE = CALDIR(1:CHR_LEN(CALDIR)) // 'drmpspc'
      ENDIF
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

      IF (HRI) THEN

        CALL CMP_MAPN(RLOC,'ENERG_LO','_REAL','READ',1,ELO_PTR,NEN,
     :                                                       STATUS)
        CALL CMP_MAPN(RLOC,'ENERG_HI','_REAL','READ',1,EHI_PTR,NEN,
     :                                                       STATUS)
        CALL CMP_MAPN(RLOC,'MATRIX','_REAL','READ',1,MAT_PTR,NEN,
     :                                                       STATUS)

        CALL XRTRESP_WRIENERGY_HRI(LOC,NEN,%val(ELO_PTR),%val(EHI_PTR),
     :                                            %val(MAT_PTR),STATUS)

        CALL CMP_UNMAP(RLOC,'ENERG_LO',STATUS)
        CALL CMP_UNMAP(RLOC,'ENERG_HI',STATUS)
        CALL CMP_UNMAP(RLOC,'MATRIX',STATUS)

      ELSEIF (PSPC) THEN
*
* Find the size of the response array
        CALL BDA_CHKDATA(RLOC, OK, NDIM, DMX_DIM, STATUS)
*
        IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error accessing response array')
           GOTO 999
        ELSEIF (NDIM .NE. 2) THEN
           CALL MSG_PRNT('Response array does not have 2 dimensions')
           GOTO 999
        ENDIF
*
* Set number of PHA and ENERGY bins in the response file. NB: This relies
* on the PHA axis being first.
        NPHA=DMX_DIM(1)
        NENERGY=DMX_DIM(2)

*
* Map an array to hold the energy boundaries
        CALL DYN_MAPR(1, NENERGY+1, EB_PTR, STATUS)
*
* Map an array to hold the pulse height boundaries in the input file
        CALL DYN_MAPR(1, NPHA+1, PB_PTR, STATUS)
*
* Map an array to hold the energy boundaries
        CALL DYN_MAPR(1, NPHA, EPB_PTR, STATUS)
*

* Get info from the datafile including the PHA channel boundaries.
        CALL XRTRESP_READFILE(LOC, NPHA, NPHA_OUT, %val(PB_PTR),
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
*
* Map an array to hold the full NENERGY * NR energy correction array
        CDIM(1)=NENERGY
        CDIM(2)=NR
*
        CALL DYN_MAPR(2, CDIM, EC_PTR, STATUS)
*
* Map workspace
        CALL DYN_MAPR(1, NENERGY, W_PTR, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error obtaining dynamic memory')
           GOTO 999
        ENDIF
*
        CALL XRTRESP_GETCORR(LOC, NENERGY, NR, %val(W_PTR),
     &                                    %val(EC_PTR), STATUS)
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
*
* Map temp array as workspace for XRTRESP_RDRESP
        DMX_DIM(1)=NENERGY
        DMX_DIM(2)=NPHA_OUT
        CALL DYN_MAPR(2,DMX_DIM,TOT_PTR,STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_REP('Error mapping temporary space',STATUS)
           GOTO 999
        ENDIF
*
* Read the response file
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
        CALL XRTRESP_WRIENERGY_PSPC(LOC,NPHA,NENERGY,%val(EB_PTR),
     &        NPHA_OUT,NR,%val(EC_PTR),%val(PB_PTR),%val(EPB_PTR),NLIST,
     &           %VAL(ELIST_PTR), %VAL(CLIST_PTR), %VAL(RESP_PTR),
     &                                    VERSION, %VAL(CR_PTR), STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*
* Unmap dynamic components
        CALL DYN_UNMAP(EB_PTR)
        CALL DYN_UNMAP(PB_PTR)
        CALL DYN_UNMAP(EPB_PTR)
        CALL DYN_UNMAP(EC_PTR)
        CALL DYN_UNMAP(W_PTR)
        CALL DYN_UNMAP(CR_PTR)
        CALL DYN_UNMAP(ELIST_PTR)
        CALL DYN_UNMAP(CLIST_PTR)
        CALL DYN_UNMAP(RESP_PTR)
        CALL DYN_UNMAP(DMX_PTR)
        CALL DYN_UNMAP(TOT_PTR)
*

      ENDIF		! HRI or PSPC

* Write history component
      CALL HIST_ADD(LOC,VERSION,STATUS)
*
      HTEXT(1)='Set up detector response structure in datafile'
*
      CALL HIST_PTXT(LOC,1,HTEXT,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error adding history record')
      ENDIF
*
999   CONTINUE
*
      CALL HDS_CLOSE(RLOC,STATUS)
      CALL AST_CLOSE()
*
*
      END

*+  XRTRESP_GETCORR - Gets correction arrays from the input file
      SUBROUTINE XRTRESP_GETCORR(LOC, NENERGY, NR, EWORK, ECORR, STATUS)
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
      CHARACTER*(DAT__SZLOC) LOC                !Locator to input datafile
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
      CALL BDA_LOCINSTR(LOC,ILOC,STATUS)
*
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
      SUBROUTINE XRTRESP_READFILE(LOC, NPHA, NPHA_OUT,
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
      CHARACTER*(DAT__SZLOC) LOC                !Locator to input datafile
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
      CHARACTER*(DAT__SZLOC) ILOC               !Locator to instrument structure
      CHARACTER*(DAT__SZLOC) ALOC               !Locator to asterix structure
      CHARACTER*(DAT__SZLOC) HLOC               !Locator to header structure
      CHARACTER*10 EXPT                         !Experiment eg. ME or LE
      INTEGER NDIM                              !No of dimensions in data array
      INTEGER DIMS(7)                           !Dimensions of data array
      LOGICAL THERE                             !Is object present ?
      LOGICAL ERASE                             !Erase energy_resp structure ?
      LOGICAL OK                                !Is the object there ?
      LOGICAL AXOK                              !Is the axis there ?
      LOGICAL REG                               !Are axis values regular ?
      LOGICAL WIDTHS                            !Are widths present in axis ?
      CHARACTER*5 TEST_STRING                   !To compare with axis labels
      INTEGER PHA_PTR                           !Pointer to pulse height axis
      INTEGER WPTR                              !Pointer to axis widths
      INTEGER NWID                              !Number of axis widths
      INTEGER IDUM
      INTEGER PHDIM                             !Axis number of PHA dimension
      INTEGER RADIM                             !Axis number of radial dimension
      REAL BASE_DUM                             !Dummy real value.
      LOGICAL LDUM                              !Dummy logical
*
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Get locator to instrument structure and header structure
      CALL BDA_LOCINSTR(LOC,ILOC,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error obtaining locator to instrument info.',
     &                                            STATUS)
         GOTO 999
      ENDIF
*
      CALL BDA_LOCHEAD(LOC,HLOC,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error obtaining locator to HEADER info.',
     &                                            STATUS)
         GOTO 999
      ENDIF
*
      CALL BDA_LOCAST(LOC,ALOC,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error obtaining locator to ASTERIX box',
     &                                            STATUS)
         GOTO 999
      ENDIF
*
* Get info from input file.
*
*   Detector
      CALL CMP_GET0C(HLOC,'INSTRUMENT',EXPT,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
*
         CALL ERR_ANNUL(STATUS)
         CALL MSG_OUT(' ','Assuming data is from ROSAT XRT',STATUS)
*
      ELSEIF ( INDEX( EXPT,'XRT' ).EQ.0 ) THEN
*
         CALL ERR_REP(' ','*Not ROSAT XRT data*',STATUS)
         STATUS=SAI__ERROR
         GOTO 999
*
      ENDIF
*
*   Find number of dimensions in the data array
      CALL BDA_CHKDATA(LOC, OK, NDIM, DIMS, STATUS)
*
      IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error accessing data array in input file')
         GOTO 999
      ENDIF
*
*   Find which axis contains pulse height values
      TEST_STRING='PH   '
*
      CALL AXIS_FIND(LOC, TEST_STRING, NDIM, PHDIM, STATUS)
*
      IF (PHDIM .LT. 1) THEN
*
         TEST_STRING='PULSE'
*
         CALL AXIS_GET(LOC,TEST_STRING,'PH_DIM',NDIM,PHDIM,STATUS)
*
         IF (STATUS .EQ. SAI__OK .AND. PHDIM .GT. 0) THEN
            NPHA_OUT=DIMS(PHDIM)
         ELSE
            CALL MSG_PRNT('Cant find pulse height axis information')
            STATUS=SAI__ERROR
            GOTO 999
         ENDIF
*
      ENDIF
*
*   Find the RADIAL axis if any
      TEST_STRING='RAD  '
*
      CALL AXIS_FIND(LOC, TEST_STRING, NDIM, RADIM, STATUS)
*
*   Set the number of radial bins
      IF (RADIM .GE. 1) THEN
         NR = DIMS(RADIM)
      ELSE
         NR=1
      ENDIF
*
*   Get pulse height boundaries.
*    First map the bin centres
      CALL BDA_CHKAXVAL(LOC,PHDIM,AXOK,REG,NPHA_OUT,STATUS)
*
      IF (AXOK) THEN
*
         CALL BDA_MAPAXVAL(LOC,'READ',PHDIM,PHA_PTR,STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping pulse height axis')
            STATUS=SAI__ERROR
            GOTO 999
         ENDIF
*
      ELSE
         CALL MSG_PRNT('Cant find pulse height axis array')
         STATUS=SAI__ERROR
         GOTO 999
      ENDIF
*
*    Now map the bin widths
      CALL BDA_CHKAXWID(LOC,PHDIM,WIDTHS,LDUM,NWID,STATUS)
*
      IF (WIDTHS .AND. NWID .EQ. NPHA_OUT) THEN
         CALL BDA_MAPAXWID(LOC,'READ',PHDIM,WPTR,STATUS)
*
      ELSE
         CALL DYN_MAPR(1,NPHA_OUT,WPTR,STATUS)
*
*    If there is only one pulse_height channel then look for scale
         IF (NPHA_OUT .EQ. 1) THEN
*
            IF (REG) THEN
               CALL BDA_GETAXVAL(LOC,PHDIM,BASE_DUM,%VAL(WPTR),
     &                                                IDUM,STATUS)
            ELSE
               CALL MSG_PRNT('Error: dont know the width of the PH bin')
               STATUS=SAI__ERROR
               GOTO 999
            ENDIF
*
*    If no widths present invent them from the axis values.
         ELSE
            CALL BDA_MAPAXWID_INVENT(%VAL(PHA_PTR),NPHA_OUT,
     &                                          %VAL(WPTR),STATUS)
         ENDIF
*
      ENDIF
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error mapping pulse height axis values')
         GOTO 999
      ENDIF
*
*    Find the bin boundaries
      CALL XRTRESP_READFILE_BOUNDS(NPHA, NPHA_OUT, %VAL(PHA_PTR),
     &                                 %VAL(WPTR), PHA_BOUNDS)
*
*   If an energy_response structure exists ask if it should be deleted.
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
*
      ENDIF
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRTRESP_READFILE',STATUS)
      ENDIF
*
      END

*+XRTRESP_READFILE_BOUNDS    Finds boundaries of pulse height channels
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
*
         PHA_BOUNDS(LP)=PHA_CEN(LP)-PHA_WID(LP)/2.0
*
      ENDDO
*
      PHA_BOUNDS(NPHA_OUT+1)=PHA_CEN(NPHA_OUT)+PHA_WID(NPHA_OUT)/2.0
*
      END

*+XRTRESP_WRIENERGY_PSPC   Writes PSPC energy response
      SUBROUTINE XRTRESP_WRIENERGY_PSPC(LOC,NPHA,NENERGY,ENERGY_BOUNDS,
     &             NPHA_OUT,NR,ECORR,PHA_BOUNDS,EPHA_CENTS,NLIST,
     &                ENERGY_LIST,CHAN_LIST,RESP,VERSION,CRESP,STATUS)
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
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Structure definitions :
*    Import :
      CHARACTER*(DAT__SZLOC) LOC            ! Locator to input datafile
      INTEGER NPHA,NENERGY                  ! Number of PH chans and energy
*                                           ! chans in response array
      REAL ENERGY_BOUNDS(NENERGY+1)         ! Boundaries of energy bins
      INTEGER NPHA_OUT                      ! No of output PHA channels
      INTEGER NR                            ! No. of radial bins
      REAL ECORR(NENERGY,NR)                ! Energy correction array
      REAL PHA_BOUNDS(NPHA+1)               ! Boundaries of PHA bins
      REAL EPHA_CENTS(NPHA)                 ! Centre energy of PHA bins
      INTEGER NLIST                         ! List lengths for each output chn.
      INTEGER ENERGY_LIST(NENERGY*NPHA_OUT) ! List of energy indices
      INTEGER CHAN_LIST(NENERGY*NPHA_OUT)   ! List of PH channel indices
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
      CHARACTER*(DAT__SZLOC) ALOC           ! Locator to ASTERIX box
      CHARACTER*(DAT__SZLOC) ELOC           ! Locator to energy_resp struc.
      CHARACTER*(DAT__SZLOC) CLOC           ! Locator to energy_resp CELL
      CHARACTER*(DAT__SZLOC) ENLOC          ! Locator to energy_lists
      CHARACTER*(DAT__SZLOC) CHLOC          ! Locator to channel_lists
      CHARACTER*(DAT__SZLOC) RELOC          ! Locator to response lists
*
      INTEGER LP,RLP,LLP
      REAL ENERGY_SPEC(MAXEN)               ! Centre value of energy bins
      REAL CHAN_SPEC(MAXPHA)                ! Centre value of PHA bins
      INTEGER E_PTR,C_PTR,R_PTR             ! Pointers to temp mapped arrays
      INTEGER OEPTR                         ! Pointer to PHA energy bounds

*
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Ensure work arrays are large enough
      IF (NENERGY .GT. MAXEN .OR. NPHA .GT. MAXPHA) THEN
         CALL MSG_PRNT('** Response array is larger than temp arrays '/
     &                /'- refer to author **')
         GOTO 999
      ENDIF
*
      CALL BDA_LOCAST(LOC,ALOC,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error getting locator to ASTERIX box',STATUS)
         GOTO 999
      ENDIF
*
*  Create output structure. This is a scalar if only one radial bin
      IF (NR .EQ. 1) THEN
         CALL DAT_NEW(ALOC,'ENERGY_RESP','EXTENSION',0,0,STATUS)
      ELSE
         CALL DAT_NEW(ALOC,'ENERGY_RESP','EXTENSION',1,NR,STATUS)
      ENDIF
*
      CALL DAT_FIND(ALOC,'ENERGY_RESP',ELOC,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error creating energy response  structure'/
     &                  /' in output file',STATUS)
         GOTO 999
      ENDIF
*
*  Calculate energy and channel centres
      DO LP=1,NENERGY
         ENERGY_SPEC(LP) = (ENERGY_BOUNDS(LP)+ENERGY_BOUNDS(LP+1)) / 2.
      ENDDO
*
      DO LP=1,NPHA_OUT
         CHAN_SPEC(LP) = (PHA_BOUNDS(LP)+PHA_BOUNDS(LP+1)) / 2.
      ENDDO
*
*  Get cell locator if more than one radial bin
      IF (NR .GT. 1) THEN
         CALL DAT_CELL(ELOC, 1, 1, CLOC, STATUS)
      ELSE
         CALL DAT_CLONE(ELOC, CLOC, STATUS)
      ENDIF
*
*  Write XRTRESP version number in first energy correction array
      CALL HDX_PUTC(CLOC,'VERSION',1,VERSION,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error writing version number',STATUS)
         GOTO 999
      ENDIF
*
      CALL DAT_ANNUL(CLOC, STATUS)
*
* Map output channel bounds data
      CALL DYN_MAPR(1, NPHA_OUT+1, OEPTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
      CALL XRTRESP_WRIENERGY_SETBND(NPHA, EPHA_CENTS, NPHA_OUT,
     &                                   PHA_BOUNDS, %val(OEPTR))
*
*  Loop over each radial bin
      DO RLP=1,NR
*
*    Get cell locator if more than one radial bin
         IF (NR .GT. 1) THEN
            CALL DAT_CELL(ELOC, 1, RLP, CLOC, STATUS)
         ELSE
            CALL DAT_CLONE(ELOC, CLOC, STATUS)
         ENDIF
*
*     Write Energy list
         CALL DAT_NEW(CLOC,'ENERGY','LIST',0,0,STATUS)
         CALL DAT_FIND(CLOC,'ENERGY',ENLOC,STATUS)
*
*     Write Energy index list
         IF (NLIST .NE. 0) THEN
            CALL HDX_PUTI(ENLOC,'DATA_ARRAY',NLIST,
     &                                     ENERGY_LIST,STATUS)
         ENDIF
*
*     Write Energy of centre of each bin
         CALL HDX_PUTR(ENLOC,'ENERGY_SPEC',NENERGY,
     &                                     ENERGY_SPEC,STATUS)
*
*     Write Energy boundarys of each bin
         CALL HDX_PUTR(ENLOC,'ENERGY_BOUNDS',NENERGY+1,
     &                                   ENERGY_BOUNDS,STATUS)
*
         CALL DAT_ANNUL(ENLOC, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP(' ','Error writing energy lists',STATUS)
            GOTO 999
         ENDIF
*
*     Write Channel list
         CALL DAT_NEW(CLOC,'CHANNEL','LIST',0,0,STATUS)
         CALL DAT_FIND(CLOC,'CHANNEL',CHLOC,STATUS)
*
*     Write Channel index list
         IF (NLIST .NE. 0) THEN
            CALL HDX_PUTI(CHLOC,'DATA_ARRAY',NLIST,
     &                                     CHAN_LIST,STATUS)
         ENDIF
*
*     Write Channel of centre of each bin
         CALL HDX_PUTR(CHLOC,'CHANNEL_SPEC',NPHA_OUT,
     &                                     CHAN_SPEC,STATUS)
*
*     Write Channel boundaries of each bin
         CALL HDX_PUTR(CHLOC, 'CHANNEL_BOUNDS', NPHA_OUT+1,
     &                                       %val(OEPTR), STATUS)
*
         CALL DAT_ANNUL(CHLOC, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP(' ','Error writing channel lists',STATUS)
            GOTO 999
         ENDIF
*
*    Create Response list structure
         CALL DAT_NEW(CLOC,'RESPONSE','LIST',0,0,STATUS)
         CALL DAT_FIND(CLOC,'RESPONSE',RELOC,STATUS)
*
*     Write Response array
         IF (NLIST .NE. 0) THEN
*
*      Multiply the response array by the corrections array for this radial
*      bin
            DO LLP=1,NLIST
               CRESP(LLP) = RESP(LLP) * ECORR(ENERGY_LIST(LLP),RLP)
            ENDDO
*
            CALL HDX_PUTR(RELOC,'DATA_ARRAY',NLIST,CRESP,STATUS)
         ENDIF
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP(' ','Error writing response list',STATUS)
            GOTO 999
         ENDIF
*
         CALL DAT_ANNUL(RELOC,STATUS)
         CALL DAT_ANNUL(CLOC,STATUS)
*
      ENDDO
*
999   CONTINUE
*
      CALL DYN_UNMAP(E_PTR)
      CALL DYN_UNMAP(C_PTR)
      CALL DYN_UNMAP(R_PTR)
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRTRESP_WRIENERGY_PSPC',STATUS)
      ENDIF
*
      END



*+XRTRESP_WRIENERGY_HRI   Writes HRI energy response
      SUBROUTINE XRTRESP_WRIENERGY_HRI(LOC,NEN,ENERGY_LO,ENERGY_HI,
     &                                                MATRIX,STATUS)
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
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Structure definitions :
*    Import :
      INTEGER NEN
      CHARACTER*(DAT__SZLOC) LOC            ! Locator to input datafile
*                                           ! chans in response array
      REAL ENERGY_LO(*),ENERGY_HI(*)        ! Boundaries of energy bins
      REAL MATRIX(*)
*    Import/export
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      INTEGER NMAX
        PARAMETER(NMAX=100)
*    Local variables :
      CHARACTER*(DAT__SZLOC) ALOC           ! Locator to ASTERIX box
      CHARACTER*(DAT__SZLOC) ELOC           ! Locator to energy_resp struc.
      CHARACTER*(DAT__SZLOC) ENLOC          ! Locator to energy_lists
      CHARACTER*(DAT__SZLOC) CHLOC          ! Locator to channel_lists
      CHARACTER*(DAT__SZLOC) RELOC          ! Locator to response lists
*
      REAL ENERGY_SPEC(NMAX)
      REAL ENERGY_BOUNDS(NMAX+1)
      REAL CHAN_SPEC(1)
      REAL CHAN_BOUNDS(2)
      INTEGER ENERGY_LIST(NMAX)
      INTEGER CHAN_LIST(NMAX)
      INTEGER LP

*
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*

      IF (NEN.GT.NMAX) THEN
        CALL MSG_PRNT('AST_ERR: insufficient storage for matrix')
        STATUS=SAI__ERROR
        GOTO 999
      ENDIF

      CALL BDA_LOCAST(LOC,ALOC,STATUS)
*
*
*  Create output structure.
      CALL DAT_NEW(ALOC,'ENERGY_RESP','EXTENSION',0,0,STATUS)
*
      CALL DAT_FIND(ALOC,'ENERGY_RESP',ELOC,STATUS)
*
*
*  Calculate energy and channel centres
      DO LP=1,NEN
         ENERGY_LIST(LP)=LP
         CHAN_LIST(LP)=LP
         ENERGY_SPEC(LP) = (ENERGY_LO(LP)+ENERGY_HI(LP))/2.0
         ENERGY_BOUNDS(LP)=ENERGY_LO(LP)
      ENDDO
      ENERGY_BOUNDS(NEN+1)=ENERGY_HI(NEN)
*
      CHAN_SPEC(1)=1.0
      CHAN_BOUNDS(1)=ENERGY_LO(1)
      CHAN_BOUNDS(2)=ENERGY_HI(NEN)
*
*  Write XRTRESP version number in first energy correction array
C      CALL HDX_PUTC(ELOC,'VERSION',1,VERSION,STATUS)
*
*
*     Write Energy list
      CALL DAT_NEW(ELOC,'ENERGY','LIST',0,0,STATUS)
      CALL DAT_FIND(ELOC,'ENERGY',ENLOC,STATUS)
*
*     Write Energy index list
      CALL HDX_PUTI(ENLOC,'DATA_ARRAY',NEN,
     &                                     ENERGY_LIST,STATUS)
*
*     Write Energy of centre of each bin
      CALL HDX_PUTR(ENLOC,'ENERGY_SPEC',NEN,
     &                                     ENERGY_SPEC,STATUS)
*
*     Write Energy boundarys of each bin
      CALL HDX_PUTR(ENLOC,'ENERGY_BOUNDS',NEN+1,
     &                                   ENERGY_BOUNDS,STATUS)
*
      CALL DAT_ANNUL(ENLOC, STATUS)
*
*
*     Write Channel list
      CALL DAT_NEW(ELOC,'CHANNEL','LIST',0,0,STATUS)
      CALL DAT_FIND(ELOC,'CHANNEL',CHLOC,STATUS)
*
*     Write Channel index list
      CALL HDX_PUTI(CHLOC,'DATA_ARRAY',NEN,
     &                                     CHAN_LIST,STATUS)
*
*     Write Channel of centre of each bin
      CALL HDX_PUTR(CHLOC,'CHANNEL_SPEC',1,
     &                                     CHAN_SPEC,STATUS)
*
*     Write Channel boundaries of each bin
      CALL HDX_PUTR(CHLOC, 'CHANNEL_BOUNDS', 2,
     &                                       CHAN_BOUNDS, STATUS)
*
      CALL DAT_ANNUL(CHLOC, STATUS)
*

*    Create Response list structure
      CALL DAT_NEW(ELOC,'RESPONSE','LIST',0,0,STATUS)
      CALL DAT_FIND(ELOC,'RESPONSE',RELOC,STATUS)
*
*     Write Response array
      CALL HDX_PUTR(RELOC,'DATA_ARRAY',NEN,MATRIX,STATUS)
*
*
      CALL DAT_ANNUL(RELOC,STATUS)
      CALL DAT_ANNUL(ELOC,STATUS)
*
*
 999  CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRTRESP_WRIENERGY_HRI',STATUS)
      ENDIF
*
      END



***
*+ XRTRESP_WRIENERGY_SETBND - Sets energy bounds for output PH chans.
      SUBROUTINE XRTRESP_WRIENERGY_SETBND(NPHA, EPHA_CENTS, NPHA_OUT,
     &                   PHA_BOUNDS, EPHA_BOUNDS)
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      INTEGER NPHA                   ! Number of PHA chans in detector matrix
      REAL EPHA_CENTS(NPHA)          ! Centre energy of each PH bin
      INTEGER NPHA_OUT               ! Number of PH bins in file
      REAL PHA_BOUNDS(NPHA+1)        ! PH chans.
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      REAL EPHA_BOUNDS(NPHA_OUT+1)   ! PH energy boundaries
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER LP
*    Local data :
*     <any DATA initialisations for local variables>
*-
* Calculate the correct channel bound energies for the
*  npha_out + 1 boundaries
      DO LP=1,NPHA_OUT+1
*
*      Test if channel boundary is less than one
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
*
      END
