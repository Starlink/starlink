*+  WFCSPEC - Creates a WFC spectral file complete with energy response
      SUBROUTINE WFCSPEC( STATUS )
*
*    Description :
*
*     Takes an input datafile, which must be either an NDF containing only
*     data value, or an ASTERIX source search results file (SSDS).
*     This is used to construct a 1D `spectrum', containing a single value
*     (with variance). The INSTRUMENT.FILTER component is located and the
*     appropriate energy response obtained from the WFC cal routine and
*     attached to the dataset.
*
*    Environment parameters :
*
*     INP               = UNIV(R)
*        Input dataset - either binned or SSDS
*     SRC               = INTEGER(R)
*        Number of source to use if input is SSDS
*     OUT               = UNIV(W)
*        Spectral output file
*     OVERRIDE          = LOGICAL(R)
*        Override the dataset observation date
*     OBSMJD            = REAL(R)
*        Modified Julian date for observation date
*
*    Method :
*
*     Open input file
*     IF ssds THEN
*       Display list of sources
*       Select a source
*       Copy count rate to output
*     ELSE
*       Check that there is only one data element
*       Copy all input objects to the output file
*       Delete the axes structure in the output file
*     END IF
*     Produce a pulse_height axis
*     Access WFC CAL file
*     Map the 3 standard arrays
*     Use these to produce the output energy_response structure
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*     David Allan (BHVAD::DJA)
*
*    History :
*
*     12 Dec 90 : V1.4-1 Original (adapted from EXOLEPH)
*     19 Mar 91 : V1.5-1 Response energy ranges tweaked for each filter (TJP)
*     28 Aug 91 : V1.5-2 New SSO routines & allow upper limits (DJA)
*     18 Jun 92 : V1.6-0 Provides default source number (DJA)
*      5 May 93 : V1.7-0 Enable observation date to be overriden (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     24 Apr 95 : V1.8-1 Updated data interfaces (DJA)
*     15 Jan 1996 V2.0-0 (DJA):
*        ADI port
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      INTEGER NEN                             ! No.of en. channels for response
      PARAMETER (NEN=125)
*
*    Local variables :
*
      CHARACTER*132 HTEXT(5)                   ! Text for history
      CHARACTER*40 INSTR                      ! Instrument name
      CHARACTER*70 CALINFO                    ! Information about WFC filter

      DOUBLE PRECISION DMJD                   ! Double prec. MJD of obs start

      REAL			CEBND(2)		! Channel energy bounds
      REAL FLUX, FLUXERR                      ! SSDS count rate and error
      REAL PKEN                               ! Filter transmission peak energy
      REAL			SPARR(2)		! Spaced array info

      INTEGER 			IFILT                   ! Filter number
      INTEGER 			CALFILT                 ! Cal filter number
      INTEGER			DETID			! Detector info
      INTEGER ICOMP                           ! SSDS BOOK element chosen
      INTEGER 			HTLEN                   ! # of lines of history
      INTEGER			IFID			! Input dataset id
      INTEGER CF_PTR, ID_PTR,RF_PTR, RFE_PTR  ! SSDS list data pointers
      INTEGER 			NELM			! # input data elements
      INTEGER DIN, DOUT                       ! Pointers to input and out data
      INTEGER VIN, VOUT                       ! Pointers to input and out var.
      INTEGER QIN, QOUT                       ! Pointers to input and out Qual.
      INTEGER ESPTR			      ! Pointer to energy_spec
      INTEGER EBPTR			      ! Pointer to energy_bounds
      INTEGER EPTR			      ! Pointer to energy indices
      INTEGER CPTR			      ! Pointer to channel indices
      INTEGER RPTR			      ! Pointer to response values
      INTEGER 			ISRC                    ! Source chosen in SSDS
      INTEGER 			NSRC                    ! # sources in SSDS
      INTEGER			OFID			! Output dataset id
      INTEGER			RMFID			! Output matrix
      INTEGER			TFID			! Input header origin
      INTEGER			TIMID			! Timing info
      INTEGER 			TLEN                    ! Length of a text string

      LOGICAL			EXPCOR			! Exposure corrected?
      LOGICAL			GOTMJD			! Got MJD from input?
      LOGICAL 			IS_SET                  ! Is SSDS compound?
      LOGICAL LVAR,LQUAL                      ! Is variance/quality present ?
      LOGICAL OK                              ! Is object ok ?
      LOGICAL OVERRIDE                        ! Override date in dataset?
      LOGICAL SSDS                            ! Input file is srce search d/set
*
*    Version :
*
      CHARACTER*30		VERSION
         PARAMETER 		( VERSION = 'WFCSPEC Version 2.0-0' )
*-

*  Announce version
      CALL MSG_PRNT(VERSION)

*  Initialise ASTERIX
      CALL AST_INIT

*  Start up CAL
      CALL CAL_INIT( STATUS )

*  Get input file
      CALL USI_ASSOC( 'INP', 'BinDS|SSDSset|SSDS', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Is it a SSDS?
      CALL ADI_DERVD( IFID, 'SSDS', SSDS, STATUS )

*  Case of SSDS
      IF ( SSDS ) THEN

*    Get number of sources
        CALL SSI_GETNSRC( IFID, NSRC, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          IF ( NSRC .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'No sources in this SSDS', STATUS )
          END IF
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Is SSDS a set?
        CALL SSI_CHKFLD( IFID, 'ID', IS_SET, STATUS )

*    Display contents of SSDS
        CALL WFCSPEC_SSDUMP( IFID, NSRC, STATUS )

*    Select source to use and validate
        IF ( NSRC .NE. 1 ) THEN
          CALL USI_GET0I( 'SRC', ISRC, STATUS )
        ELSE
          ISRC = 1
        END IF
        IF ( STATUS .EQ. SAI__OK ) THEN
          IF ( ( ISRC .LT. 0 ) .OR. ( ISRC .GT. NSRC ) ) THEN
            CALL MSG_SETI( 'IS', ISRC )
            CALL MSG_SETI( 'N', NSRC )
            CALL MSG_PRNT( 'Invalid source number /^IS/, must'/
     :                               /' lie between 1 and ^N' )
            STATUS = SAI__ERROR
          END IF
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Locate element in BOOK structure for source ISRC
        IF ( IS_SET ) THEN
          CALL SSI_MAPFLD( IFID, 'ID', '_INTEGER', 'READ', ID_PTR,
     :                                                    STATUS )
          CALL ARR_ELEM1I( ID_PTR, NSRC, ISRC, ICOMP, STATUS )
        ELSE
          ICOMP = 1
        END IF

*    Locate the dataset searched to find the ICOMP'th source
        CALL SSI_FINDDS( IFID, ICOMP, TFID, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_FLUSH( STATUS )
          CALL MSG_PRNT( 'Unable to locate original dataset from '/
     :                           /'source search results file...' )
          CALL USI_ASSOC( 'AUX', 'BinDS', 'READ', TFID, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

        END IF

*    No quality for SSDS
        LQUAL = .FALSE.

*  Case of NDF - copy IFID
      ELSE
        CALL ADI_CLONE( IFID, TFID, STATUS )

      END IF

*  Check that it's WFC data
      CALL DCI_GETID( TFID, DETID, STATUS )
      CALL ADI_CGET0C( DETID, 'Instrument', INSTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT('Error reading instrument name - assuming WFC' )
        CALL ERR_ANNUL( STATUS )
      END IF
      IF ( INSTR .NE. 'WFC' ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Not WFC data!', STATUS )
        GOTO 99
      END IF

*  Get filter ident from INSTRUMENT box
      CALL ADI_CGET0I( DETID, 'Filter', IFILT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ','Can''t find FILTER - no response set up',
     :  STATUS )
        GOTO 99
      END IF

*  Translate to cal filter number and tell user about it
      CALL WFC_FILT_CONV( IFILT, CALFILT, STATUS )
      IF ( STATUS.NE.SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Bad filter number - response not set up',
     :                                                       STATUS )
        GOTO 99
      END IF

*   Find count rate and error

*  Source search results file
      IF ( SSDS ) THEN

*    Map lists
        CALL SSI_MAPFLD( IFID, 'FLUX', '_REAL', 'READ', RF_PTR, STATUS )
        CALL SSI_MAPFLD( IFID, 'CFLUX', '_REAL', 'READ', CF_PTR,
     :                                                  STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        CALL SSI_CHKFLDERR( IFID, 'FLUX', LVAR, STATUS )
        IF ( LVAR ) THEN
          CALL SSI_MAPFLDERR( IFID, 'FLUX', '_REAL', 'READ', RFE_PTR,
     :                                                       STATUS )
        ELSE IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_PRNT( 'Warning - unable to get flux error' )
          CALL ERR_FLUSH( STATUS )
        END IF

*    Get raw count to find fractional count error
        IF ( LVAR ) THEN
          CALL ARR_ELEM1R( RF_PTR, NSRC, ISRC, FLUX, STATUS )
          CALL ARR_ELEM1R( RFE_PTR, NSRC, ISRC, FLUXERR, STATUS )
          FLUXERR = FLUXERR / FLUX
        END IF

*    Get corrected flux and find error
        CALL ARR_ELEM1R( CF_PTR, NSRC, ISRC, FLUX, STATUS )
        IF ( LVAR ) FLUXERR = FLUXERR * FLUX

*  Case of NDF
      ELSE

*    Data array must have only one element (but could be n-dimensional)
        CALL BDI_CHK( IFID, 'Data', OK, STATUS )
        CALL BDI_GETNEL( IFID, NELM, STATUS )
        IF ( .NOT. OK .OR. STATUS .NE. SAI__OK ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Error finding input data array', STATUS )
          GOTO 99
        END IF
        IF ( NELM .NE. 1 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Data array must have a single element',
     :    STATUS)
          GOTO 99
        END IF

*    Check that it has been normalised to count/sec
        CALL PRF_GET( IFID, 'CORRECTED.EXPOSURE', EXPCOR, STATUS )
        IF ( .NOT. EXPCOR ) THEN
          CALL MSG_PRNT( '!! WARNING : Data does not appear to be '/
     :                   /'exposure corrected' )
        END IF

*    Check for variance and quality
        CALL BDI_CHK( IFID, 'Variance', LVAR, STATUS)
        IF(.NOT. LVAR .OR. STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('No variance array in input file')
          CALL ERR_ANNUL(STATUS)
	  LVAR=.FALSE.
        END IF
        CALL BDI_CHK( IFID, 'Quality', LQUAL, STATUS)
        IF(STATUS .NE. SAI__OK) THEN
          CALL ERR_ANNUL(STATUS)
          LQUAL=.FALSE.
        END IF
      END IF

*  Create output file
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      CALL BDI_LINK( 'Spectrum', 1, 1, 'REAL', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Create standard components in output file
      CALL BDI_PUT0C( OFID, 'Title', 'WFC SPECTRUM', STATUS )
      CALL BDI_PUT0C( OFID, 'Label', 'Intensity', STATUS )
      CALL BDI_PUT0C( OFID, 'Units', 'count/s', STATUS )
      CALL BDI_AXPUT0C( OFID, 1, 'Label', 'Channel', STATUS )
      SPARR(1) = 1.0
      SPARR(2) = 1.0
      CALL BDI_AXPUT1R( OFID, 1, 'SpacedData', 2, SPARR, STATUS )
      CALL BDI_AXPUT0R( OFID, 1, 'ScalarWidth', 1.0, STATUS )

*  Copy ancillaries
      CALL UDI_COPANC( TFID, 'grf', OFID, STATUS )
      IF(STATUS .NE. SAI__OK) CALL ERR_FLUSH(STATUS)

*  Map output data
      CALL BDI_MAPR( OFID, 'Data', 'WRITE', DOUT, STATUS )
      IF ( LVAR ) THEN
        CALL BDI_MAPR( OFID, 'Variance', 'WRITE', VOUT, STATUS )
      END IF
      IF ( LQUAL ) THEN
        CALL BDI_MAPUB( OFID, 'Quality', 'WRITE', QOUT, STATUS )
        CALL BDI_PUT0UB( OFID, 'QualityMask', QUAL__MASK, STATUS )
      END IF

*  SSDS case
      IF ( SSDS ) THEN

*    Copy values
        CALL ARR_COP1R( 1, FLUX, %VAL(DOUT), STATUS )
        IF ( LVAR ) THEN
          CALL ARR_COP1R( 1, FLUXERR*FLUXERR, %VAL(VOUT), STATUS )
        END IF

*  Binned dataset case
      ELSE

*    Copy across data value
        CALL BDI_MAPR( IFID, 'Data', 'READ', DIN, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Error transferring data value', STATUS )
          GOTO 99
        END IF
        CALL ARR_COP1R( 1, %VAL(DIN), %VAL(DOUT), STATUS )

*    ..and variance
        IF ( LVAR )THEN
          CALL BDI_MAPR( IFID, 'Variance', 'READ', VIN, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_PRNT( 'Error transferring variance value' )
            CALL ERR_ANNUL( STATUS )
          END IF
          CALL ARR_COP1R( 1, %VAL(VIN), %VAL(VOUT), STATUS )
        END IF

*    ..and quality
        IF ( LQUAL ) THEN
          CALL BDI_MAPUB( IFID, 'Quality', 'READ', QIN, STATUS)
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_PRNT( 'Error transferring quality value' )
            CALL ERR_ANNUL( STATUS  )
          END IF
          CALL ARR_COP1B( 1, %VAL(QIN), %VAL(QOUT), STATUS )
        END IF

      END IF

*  Tell user about filter
      CALL CAL_FILT_INFO( CALFILT, CALINFO, PKEN, STATUS )
      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
      CALL MSG_PRNT(' ')
      CALL MSG_PRNT('Filter: '//CALINFO)
      CALL MSG_SETR('PK',PKEN)
      CALL MSG_PRNT('        Response centred at ^PK eV')

*  Get decimal MJD of observation start
      CALL TCI_GETID( OFID, TIMID, STATUS )
      CALL ADI_CGET0D( TIMID, 'MJDObs', DMJD, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL MSG_PRNT( 'Cannot get observation date from dataset' )
        GOTMJD = .FALSE.
      ELSE
        GOTMJD = .TRUE.
      END IF

*  Override MJD?
      IF ( GOTMJD ) THEN
        CALL USI_GET0L( 'OVERRIDE', OVERRIDE, STATUS )
      ELSE
        OVERRIDE = .TRUE.
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( OVERRIDE ) THEN
        IF ( GOTMJD ) THEN
          CALL USI_DEF0D( 'OBSMJD', DMJD, STATUS )
        END IF
        CALL USI_GET0D( 'OBSMJD', DMJD, STATUS )
      END IF

*  Create energy response
      CALL ERI0_INIT( STATUS )
      CALL ADI_NEW0( 'AsterixRMF', RMFID, STATUS )
      CALL ADI_CPUT0I( RMFID, 'NCHAN', 1, STATUS )
      CALL ADI_CPUT0I( RMFID, 'NENERGY', NEN, STATUS )
      CALL ADI_CNEW1R( RMFID, 'Energy', NEN+1, STATUS )
      CALL ADI_CNEW1R( RMFID, 'Channels', 2, STATUS )
      CALL ADI_CNEW1R( RMFID, 'ChannelSpec', 1, STATUS )
      CALL ADI_CNEW1B( RMFID, 'ChannelIndices', NEN, STATUS )
      CALL ADI_CNEW1B( RMFID, 'EnergyIndices', NEN, STATUS )
      CALL ADI_CNEW1R( RMFID, 'RMF', NEN, STATUS )

*  Only one channel
      CALL ADI_CPUT1R( RMFID, 'ChannelSpec', 1, 1.0, STATUS )

*  Channel numbers
      CALL ADI_CMAPR( RMFID, 'Energy', 'WRITE', EBPTR, STATUS )
      CALL ADI_CMAPB( RMFID, 'ChannelIndices', 'WRITE', CPTR, STATUS )
      CALL ADI_CMAPB( RMFID, 'EnergyIndices', 'WRITE', EPTR, STATUS )
      CALL ADI_CMAPR( RMFID, 'RMF', 'WRITE', RPTR, STATUS )
      CALL DYN_MAPR( 1, NEN, ESPTR, STATUS )

*  Insert response values
      CALL WFCSPEC_RESPVAL( DMJD, CALFILT, NEN, %VAL(EBPTR),
     :                      %VAL(ESPTR), CEBND, %VAL(EPTR),
     :                      %VAL(CPTR), %VAL(RPTR), STATUS )

*  Release mapped ADI items
      CALL ADI_CUNMAP( RMFID, 'Energy', EBPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'ChannelIndices', CPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'EnergyIndices', EPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'RMF', RPTR, STATUS )

*  Write channel energy bounds
      CALL ADI_CPUT1R( RMFID, 'Channels', 2, CEBND, STATUS )

*  Write the response
      CALL ERI_PUTIDS( OFID, 1, 1, RMFID, ADI__NULLID, STATUS )

*  Write history component
      CALL HSI_ADD( OFID, VERSION, STATUS )
      HTEXT(1) = 'Set up WFC energy response structure '
      IF ( SSDS ) THEN
        CALL MSG_SETI( 'N', ISRC )
        CALL MSG_MAKE( 'Using source ^N from SSDS file {INP}',
     :                                  HTEXT(2), TLEN, STATUS )
      ELSE
        HTEXT(2) = 'Using binned dataset {INP}'
      END IF
      HTLEN = 5
      CALL USI_TEXT( 2, HTEXT, HTLEN, STATUS )
      CALL HSI_PTXT( OFID, HTLEN, HTEXT, STATUS )

*  Release input and output
      CALL USI_ANNUL( 'INP', STATUS )
      CALL USI_ANNUL( 'OUT', STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  WFCSPEC_RESPVAL - Enters WFC energy response matrix values
      SUBROUTINE WFCSPEC_RESPVAL(DMJD,FILT,NEN,EBOUNDS,ESPEC,
     :                                      CHBND,EN,CH,RESP,STATUS)
*    Description :
*     Sets up response matrices for WFC filters.
*     Energy array is logarithmically distributed over a range appropriate
*     to each filter.
*     Effective areas are obtained from the WFC master cal file by CAL_ONAA.
*     Note that the response values have units of cm**2 (i.e. they are
*     just effective areas which map photons/(cm**2*sec) onto count/sec.
*     Energy units are expressed in keV (except for call to CAL_ONAA).
*    History :
*     12 Dec 90: Original (TJP)
*     19 Mar 91: Separate energy range for each filter (TJP)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
        DOUBLE PRECISION DMJD                   ! Double prec. MJD of obs start
        INTEGER FILT                            ! Filter number
	INTEGER NEN				! No.of energy channels
*    Import-Export :
*    Export :
	REAL EBOUNDS(NEN+1)			! Energy bin bounds
	REAL ESPEC(NEN)				! Bin centre energies
	REAL CHBND(2)				! Bound energies of passband
	BYTE EN(NEN)				! Array of energy indices
	BYTE CH(NEN)				! Array of channel indices
	REAL RESP(NEN)				! Array of response values
*    Status :
	INTEGER STATUS
*    Local constants :
*    Local variables :
	INTEGER I
	INTEGER MAXEN				! Energy index of response peak
	REAL MAXRESP				! Peak response
	REAL THRESH				! Response threshold
	REAL EBOT,ETOP				! Bottom and top energies
	REAL FAC				! Log of full energy range
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set energy range
      EBOT = 0.015
      ETOP = 0.4
      IF ( FILT .EQ. 3 ) EBOT = 0.03
      IF ( FILT.EQ.2.OR.FILT.EQ.6 ) EBOT = 0.04
      IF ( FILT.EQ.4.OR.FILT.EQ.8 ) EBOT = 0.06
      IF ( FILT.EQ.3.OR.FILT.EQ.7 ) ETOP = 0.6

*  Energy bounds and centres (NEN bins stretching from 14 to 1200 eV)
      EBOUNDS(1) = EBOT
      FAC = LOG(ETOP/EBOT)
      DO I = 1, NEN
	EBOUNDS(I+1)=EBOT*DEXP(DBLE(I*FAC/NEN))
	ESPEC(I)=1000*(EBOUNDS(I)+EBOUNDS(I+1))/2	! In eV for the moment
      END DO

*  Response matrix elements
      CALL CAL_ONAA(DMJD,FILT,NEN,ESPEC,RESP,STATUS)
      IF(STATUS.NE.SAI__OK)THEN
	CALL ERR_REP('CAL_ERR','Error from CAL_ONAA',STATUS)
        GOTO 99
      END IF
      DO I=1,NEN
	ESPEC(I)=ESPEC(I)/1000		! Convert back to keV
	EN(I)=I
	CH(I)=1
      END DO

* Search for pass band boundaries (define as energies where response
*                                  drops by 2 orders of mag. from peak)
      MAXRESP=0.0
      DO I=1,NEN
	IF(RESP(I).GT.MAXRESP)THEN
	  MAXRESP=RESP(I)
	  MAXEN=I
	END IF
      END DO

      THRESH = MAXRESP/100.0
      I = MAXEN
      DO WHILE ((RESP(I).GT.THRESH).AND.(I.GT.1))
	I = I - 1
      END DO
      CHBND(1) = ESPEC(I)
      I = MAXEN
      DO WHILE ((RESP(I).GT.THRESH).AND.(I.LT.NEN))
	I = I + 1
      END DO
      CHBND(2) = ESPEC(I)

*  Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'WFCSPEC_RESPVAL', STATUS )
      END IF

      END



*+  WFCSPEC_SSDUMP - Dump sources from SSDS to ascii
      SUBROUTINE WFCSPEC_SSDUMP( SFID, NSRC, STATUS )
*
*    Description :
*
*     Dumps RA,DEC,FLUX and significance of sources in an SSDS to terminal.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     12 Dec 90 : Original ( DJA )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER			SFID			! SSDS to dump
      INTEGER               	NSRC                    ! # sources in it
*
*    Local variables :
*
      INTEGER               	RA_PTR, DEC_PTR         ! Celestial pos ptr's
      INTEGER               	CF_PTR                  ! Corrected flux

      LOGICAL               	FLX_OK                  ! Flux there?
      LOGICAL               	DEC_OK, RA_OK           ! Fields ok?
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check lists
      CALL SSI_CHKFLD( SFID, 'RA', RA_OK, STATUS )
      CALL SSI_CHKFLD( SFID, 'DEC', DEC_OK, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :              .NOT. ( RA_OK .AND. DEC_OK ) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'No pointing data in results file', STATUS )
      END IF
      CALL SSI_CHKFLD( SFID, 'CFLUX', FLX_OK, STATUS )

      IF ( ( STATUS .EQ. SAI__OK ) .AND. .NOT. FLX_OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'No corrected counts present - '/
     :               /'exposure correction needed', STATUS )
      END IF

*  Map
      CALL SSI_MAPFLD( SFID, 'RA', '_DOUBLE', 'READ', RA_PTR, STATUS )
      CALL SSI_MAPFLD( SFID, 'DEC', '_DOUBLE', 'READ', DEC_PTR, STATUS )
      CALL SSI_MAPFLD( SFID, 'CFLUX', '_REAL', 'READ', CF_PTR, STATUS )

*  Do output
      CALL WFCSPEC_SSDUMP_INT( NSRC, %VAL(RA_PTR), %VAL(DEC_PTR),
     :                                     %VAL(CF_PTR), STATUS )

*  Inform environment of errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'WFCSPEC_SSDUMP', STATUS )
      END IF

      END



*+  WFCSPEC_SSDUMP_INT - Performs output for WFCSPEC_SSDUMP
      SUBROUTINE WFCSPEC_SSDUMP_INT( NSRC, RA, DEC, FLUX, STATUS )
*
*    Description :
*
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     12 Dec 90 : Original ( DJA )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER               NSRC                      ! # sources in SSDS
      DOUBLE PRECISION      RA(*), DEC(*)             ! Source positions
      REAL                  FLUX(*)                   ! Corrected flux
*
*    Local variables :
*
      CHARACTER*80          LINE                      ! Output buffer

      CHARACTER*1           SIGN                      ! RA and DEC formatting
      INTEGER               RAB(4), DECB(4)           !

      INTEGER               I,J                       ! Loop over sources
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Table header
      CALL MSG_PRNT( '  Src      RA        DEC        FLUX' )

*    Loop sources
      DO I = 1, NSRC

*      Clear buffer
        CALL CHR_FILL( ' ', LINE )

*      Convert coords to char strings
        CALL SLA_CR2TF( 0, REAL(RA(I)*MATH__DTOR), SIGN, RAB )
        CALL SLA_CR2AF( 0, REAL(DEC(I)*MATH__DTOR), SIGN, DECB )

*      Write fields
        WRITE( LINE, '(I5,3X,3(I2.2,1X),1X,A1,3(I2.2,1X),1X,1PG11.4)')
     :                           I,(RAB(J),J=1,3), SIGN,
     :                           (DECB(J),J=1,3), FLUX(I)

*      Write the first record
        CALL MSG_PRNT( LINE )

      END DO

      END
