*+  EVPOLAR - Bins 2 lists into a 1 or 2 dimensional object
      SUBROUTINE EVPOLAR( STATUS )
*
*    Description :
*
*     A binned polar dataset is produced from 2 lists from the input dataset.
*
*     The azimuthal axis is always regularly spaced, and is specified by
*     the number of bins. The radial axis may be regularly or irregularly
*     spaced. If regular, bin width and number of bins are specified : if
*     irregular, the bin bounds.
*
*     All bins are INCLUSIVE of their lower bound and EXCLUSIVE of their
*     upper bound.
*
*     The way QUALITY lists are handled is controlled by 2 parameters QVAL,
*     and QKEEP. Values present in the quality list > QVAL are treated as bad
*     quality values. If QKEEP is true, then bad events are written to the
*     output DATA_ARRAY, and the corresponding element of the output QUALITY
*     array is set to bad (i.e.1). If QKEEP is false, then all bad events are
*     simply ignored, and no output QUALITY array is produced.
*
*    Parameters :
*
*     INP = UNIV(R)
*        Name of input EVDS
*     LISTS = CHAR(R)
*        Index No(s) of input list(s)
*     QVAL = INTEGER(R)
*        Event quality > QVAL = bad
*     QKEEP = LOGICAL(R)
*        Produce output QUALITY array?
*     REG = LOGICAL(R)
*        Regular or irregular radial bins
*     ABINSIZE = REAL(R)
*        Azimuthal bin size
*     RBINSIZE = REAL(R)
*        Regular radial bin width
*     RRANGE = CHAR(R)
*        Irregular bin bounds
*     NAZ, NRAD = INTEGER(R)
*        Numbers of bins
*     NORMALISE = LOGICAL(R)
*        Normalise output array
*     OUT = UNIV(W)
*        Name of output dataset
*
*    Method :
*
*    Deficiencies :
*
*     Irregular bins must not overlap
*
*    Bugs :
*    Authors :
*
*     David J. Allan ( BHVAD::DJA )
*
*    History :
*
*     21 Feb 91 : V1.4-0  Original (DJA)
*     11 Sep 93 : V1.7-0  EVPOLAR_CAREA renamed GEO_CAREA (DJA)
*     25 Feb 94 : V1.7-1  Use BIT_ routines to do bit manipulations (DJA)
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
      INCLUDE 'LIST_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Function calls :
*
      INTEGER                   CHR_LEN
*
*    Local Constants :
*
      INTEGER                   MXRANGE          ! 2 x max No of irregular bins
         PARAMETER             (MXRANGE = 1000)
      INTEGER                   MXTEXT
         PARAMETER             (MXTEXT = 7)
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)    BLOC(2)          ! Lists to use in polar binning
      CHARACTER*(DAT__SZNAM)    BNAME(2)         ! List names
      CHARACTER*40              BUNIT(2)         ! List units
      CHARACTER*80              HTEXT(MXTEXT)    ! History text
      CHARACTER*(DAT__SZLOC)    ILOC             ! Input event dataset
      CHARACTER*(DAT__SZLOC)    LLOC(LIST__MXNL) ! Locator to lists in the evds
      CHARACTER*(DAT__SZNAM)    LNAM(LIST__MXNL) ! Names of lists in the evds
      CHARACTER*(DAT__SZLOC)    OLOC             ! Locator to output dataset
      CHARACTER*40              ORUNIT           ! Output radial unit
      CHARACTER*(DAT__SZLOC)    QLOC             ! Locator to input quality
      CHARACTER*80              TEXT             ! Temporary text string

      REAL                      ABINSIZE         ! Azimuthal bin size
      REAL                      LOX,HIX,LOY,HIY  ! Field min and max values
      REAL                      MAXR             ! Maximum radial distance
      REAL                      RANGE(MXRANGE)   ! Irregular bin ranges.
      REAL                      RBINSIZE         ! Radial bin size
      REAL                      X0, Y0           ! Centre of polar distribution

      INTEGER                   BADQUAL          ! Exclude events with quality
                                                 ! > this value.
      INTEGER                   BPTR(2)          ! Binning list pointers
      INTEGER                   I                ! Loop counters
      INTEGER                   INDEX(LIST__MXNL)! Index number of selected lists
      INTEGER                   INLIST           ! Number of lists in input EVDS
      INTEGER                   IQPTR            ! Input quality data
      INTEGER                   LEN              ! Length of TEMP
      INTEGER                   NAZ, NRAD        ! # azimuthal/radial bins
      INTEGER                   NDIM             ! Output dimensionality
      INTEGER                   NEV              ! Number of events in input
      INTEGER                   NINDEX           ! Number of lists selected.
      INTEGER                   NLINE            ! # history lines used
      INTEGER                   NREJ             ! # events not binned
      INTEGER                   OAPTR, OWPTR     ! Output irregular axis data
      INTEGER                   ODIMS(2)         ! Output dataset dimensions
      INTEGER                   ODPTR            ! Output data pointer
      INTEGER                   ONELM            ! # output bins
      INTEGER                   OQPTR            ! Output quality pointer
      INTEGER                   WPTR             ! Pointer to workspace array

      LOGICAL                   INPRIM           ! Input primitive?
      LOGICAL                   NORMALISE        ! Normalise output bins?
      LOGICAL                   OK               ! General validity test
      LOGICAL                   QKEEP            ! If true produce output
                                                 ! quality array.
      LOGICAL                   QUALITY          ! Is QUALITY list present?
      LOGICAL                   REG              ! Allow irregular binning?
      LOGICAL                   SAME             ! Two objects the same
*
*    Version id :
*
      CHARACTER*24              VERSION
         PARAMETER              ( VERSION = 'EVPOLAR Version 1.7-1' )
*-

*    Version anouncement
      CALL MSG_PRNT( VERSION )

*    Initialize BDA routines
      CALL AST_INIT( STATUS )

*    Get event dataset
      CALL USI_ASSOCI( 'INP', 'READ', ILOC, INPRIM, STATUS )
      IF ( INPRIM ) THEN
        CALL MSG_PRNT( 'FATAL ERROR: Input is not an event dataset' )
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Find all valid lists in INP. Display them to user.
      CALL MSG_BLNK()
      CALL MSG_PRNT( 'The available lists are :' )
      CALL MSG_BLNK()
      CALL LIST_FINDALLOK( ILOC, .TRUE., LLOC, LNAM, INLIST, NEV,
     :                                                   STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Select the lists
      CALL MSG_BLNK()
      CALL MSG_PRNT( 'Select two lists to bin using index'/
     :             /' numbers above, eg. 1 2. The direction' )
      CALL MSG_PRNT( 'of zero azimuth is that of increasing list'/
     :                                               /' 1 value.')
      CALL MSG_BLNK()
      CALL PRS_GETLIST( 'LISTS', INLIST, INDEX, NINDEX, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check that at leaset one list has been selected
      IF ( NINDEX .LT. 2 ) THEN
        CALL MSG_PRNT( 'FATAL ERROR: EVPOLAR requires two lists' )
        STATUS = SAI__ERROR
      ELSE IF ( NINDEX .GT. 2 ) THEN
        CALL MSG_PRNT( 'FATAL ERROR: Too many selections made' )
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Is there a quality list?
      CALL LIST_OK( ILOC, 'QUALITY', QUALITY, STATUS )
      IF ( QUALITY ) THEN

*      Map it
        CALL DAT_FIND( ILOC, 'QUALITY', QLOC, STATUS )
        CALL BDA_MAPTDATA( QLOC, '_INTEGER', 'READ', IQPTR, STATUS )

*      Get quality processing mode
        CALL PAR_GET0I( 'QVAL',  BADQUAL, STATUS )
        CALL PAR_GET0L( 'QKEEP', QKEEP, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get info on selected lists
      DO I = 1, 2

*      Make sure user didn't select quality
        IF ( QUALITY ) THEN
          CALL HDX_SAME( LLOC(INDEX(I)), QLOC, SAME, STATUS )
          IF ( SAME ) THEN
            CALL MSG_PRNT( 'ERROR : Cannot bin using QUALITY list' )
            STATUS = SAI__ERROR
            GOTO 99
          END IF
        END IF

*      Clone the list
        CALL DAT_CLONE( LLOC(INDEX(I)), BLOC(I), STATUS )

*      Map and get attributes
        BNAME(I) = LNAM(INDEX(I))
        CALL BDA_MAPDATA( BLOC(I), 'READ', BPTR(I), STATUS )
        CALL BDA_GETUNITS( BLOC(I), BUNIT(I), STATUS )

      END DO

*    Free original list locators
      DO I = 1, INLIST
        CALL DAT_ANNUL( LLOC(I), STATUS )
      END DO
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get field minimum and maxiumum
      CALL LIST_GFLDR( BLOC(1), LOX, HIX, STATUS )
      CALL LIST_GFLDR( BLOC(2), LOY, HIY, STATUS )

*    Decide on radial units
      ORUNIT = BUNIT(1)
      IF ( NDIM .EQ. 2 ) THEN
        IF ( BUNIT(1)(:CHR_LEN(BUNIT(1))) .NE. BUNIT(2)
     :                                 (:CHR_LEN(BUNIT(2))) ) THEN
          CALL MSG_PRNT( 'WARNING : List units are different, '/
     :                                /'output may be rubbish' )
        END IF
      END IF

*    Get polar centre
      CALL PAR_GET0R( 'X0', X0, STATUS )
      CALL PAR_GET0R( 'Y0', Y0, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map workspace
      CALL DYN_MAPR( 1, 2*NEV, WPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Find azimuth and radius for each bin
      CALL EVPOLAR_RECPOL( NEV, X0, Y0, %VAL(BPTR(1)), %VAL(BPTR(2)),
     :                                     %VAL(WPTR), MAXR, STATUS )

*    Get azimuthal bin size
      CALL PAR_GET0R( 'ABINSIZE', ABINSIZE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      NAZ = 360.0 / ABINSIZE
      CALL MSG_SETI( 'NAZ', NAZ )
      CALL MSG_PRNT( 'There will be ^NAZ azimuthal bins in the output' )

*    Inform user of maximum radius, and get radial bins
      CALL PAR_GET0L( 'REG', REG, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL MSG_SETR( 'RMAX', MAXR )
      CALL MSG_SETC( 'RUNIT', ORUNIT )
      CALL MSG_PRNT( 'Radial distance varies from 0 to ^RMAX ^RUNIT' )
      IF ( .NOT. REG ) THEN

        CALL MSG_PRNT( 'You must specify INCREASING ranges e.g.'/
     :                                              /' 0:10:20' )
        CALL PRS_GETRANGES( 'RRANGE', MXRANGE, 1, 0.0, MAXR,
     :                                 RANGE, NRAD, STATUS )

      ELSE

*      Get binsize
        CALL PAR_GET0R( 'RBINSIZE', RBINSIZE, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        IF ( RBINSIZE .LE. 0.0 ) THEN
          CALL MSG_PRNT( 'ERROR : Zero or negative radial bin width' )
          STATUS = SAI__ERROR
        END IF

*      Get number of radial bins
        CALL PAR_DEF0I( 'NRAD', MAX(1,NINT(MAXR/RBINSIZE)), STATUS )
        CALL PAR_GET0I( 'NRAD', NRAD, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Decide output dimensionality
      IF ( ( NAZ .EQ. 1 ) .OR. ( NRAD .EQ. 1 ) ) THEN
        NDIM = 1
        ODIMS(1) = NRAD
      ELSE
        NDIM = 2
        ODIMS(1) = NRAD
        ODIMS(2) = NAZ
      END IF

*    Create output dataset
      CALL USI_ASSOCO( 'OUT', 'POLAR', OLOC, STATUS )
      CALL BDA_CREDATA( OLOC, NDIM, ODIMS, STATUS )
      CALL BDA_MAPDATA( OLOC, 'WRITE', ODPTR, STATUS )

*    Create AXIS structure
      CALL BDA_CREAXES( OLOC, NDIM, STATUS )
      CALL BDA_CREAXVAL( OLOC, 1, REG, NRAD, STATUS )
      CALL BDA_CREAXWID( OLOC, 1, REG, NRAD, STATUS )
      IF ( REG ) THEN
        CALL BDA_PUTAXVAL( OLOC, 1, 0.0, RBINSIZE, NRAD, STATUS )
        CALL BDA_PUTAXWID( OLOC, 1, RBINSIZE, STATUS )
      ELSE
        CALL BDA_MAPAXVAL( OLOC, 'WRITE', 1, OAPTR, STATUS )
        CALL BDA_MAPAXVAL( OLOC, 'WRITE', 1, OWPTR, STATUS )
        CALL AXIS_RNG2VALW( NRAD, RANGE, %VAL(OAPTR), %VAL(OWPTR),
     :                                                    STATUS )
      END IF
      CALL BDA_PUTAXLABEL( OLOC, 1, 'Radius', STATUS )
      IF ( ORUNIT .GT. ' ' ) THEN
        CALL BDA_PUTAXUNITS( OLOC, 1, ORUNIT, STATUS )
      END IF
      IF ( NDIM .EQ. 2 ) THEN
        CALL BDA_CREAXVAL( OLOC, 2, .TRUE., NAZ, STATUS )
        CALL BDA_PUTAXVAL( OLOC, 2,0.5*ABINSIZE, ABINSIZE, NAZ, STATUS )
        CALL BDA_CREAXWID( OLOC, 2, .TRUE., NAZ, STATUS )
        CALL BDA_PUTAXWID( OLOC, 2, ABINSIZE, STATUS )
        CALL BDA_PUTAXLABEL( OLOC, 2, 'Azimuth', STATUS )
        CALL BDA_PUTAXUNITS( OLOC, 2, 'degree', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Initialise quality to data missing
      CALL ARR_SUMDIM( NDIM, ODIMS, ONELM )
      IF ( QUALITY .AND. QKEEP ) THEN
        CALL BDA_CREQUAL( OLOC, NDIM, ODIMS, STATUS )
        CALL BDA_MAPQUAL( OLOC, 'WRITE', OQPTR, STATUS )
        CALL BDA_PUTMASK( OLOC, QUAL__MASK, STATUS )
        CALL ARR_INIT1B( QUAL__MISSING, ONELM, %VAL(OQPTR), STATUS )
      END IF

*    Zero bin counts
      CALL ARR_INIT1R( 0.0, ONELM, %VAL(ODPTR), STATUS )

*    Bin the events
      IF ( REG ) THEN
        CALL EVPOLAR_REGBIN( NAZ, NRAD, RBINSIZE, NEV, %VAL(WPTR),
     :                       QUALITY, QKEEP, %VAL(IQPTR),
     :                     %VAL(ODPTR), %VAL(OQPTR), NREJ, STATUS )
      ELSE
        CALL EVPOLAR_IRREGBIN( NAZ, NRAD, RANGE, NEV,
     :                     %VAL(WPTR), QUALITY, QKEEP, %VAL(IQPTR),
     :                     %VAL(ODPTR), %VAL(OQPTR), NREJ, STATUS )
      END IF

*    Inform of events binned
      CALL MSG_SETI( 'BIN', NEV - NREJ )
      CALL MSG_SETI( 'NIN', NEV )
      CALL MSG_PRNT( '^BIN events were binned out of ^NIN input.' )

*    Normalise
      NORMALISE = .FALSE.
      IF ( NINDEX .EQ. 2 ) THEN
        CALL PAR_GET0L( 'NORM', NORMALISE, STATUS )
      END IF
      IF ( NORMALISE .AND. ( STATUS .EQ. SAI__OK ) ) THEN
        IF ( REG ) THEN
          CALL EVPOLAR_REGNORM( NAZ, NRAD, RBINSIZE, X0, Y0, LOX, HIX,
     :                                 LOY, HIY, %VAL(ODPTR), STATUS )
        ELSE
          CALL EVPOLAR_IRREGNORM( NAZ, NRAD, RANGE, X0, Y0, LOX, HIX,
     :                                LOY, HIY, %VAL(ODPTR), STATUS )
        END IF
      END IF
      CALL BDA_PUTAXNORM( OLOC, 1, NORMALISE, STATUS )
      IF ( NDIM .EQ. 2 ) THEN
        CALL BDA_PUTAXNORM( OLOC, 2, NORMALISE, STATUS )
      END IF

*    Copy ancillary stuff
      CALL BDA_COPTEXT( ILOC, OLOC, STATUS )
      CALL BDA_COPMORE( ILOC, OLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Re-write data units
      IF ( NORMALISE ) THEN
        CALL MSG_SETC( 'UN', ORUNIT )
        CALL MSG_MAKE( 'count/^UN**2', TEXT, LEN )
        CALL BDA_PUTUNITS( OLOC, TEXT(:LEN), STATUS )
      ELSE
        CALL BDA_PUTUNITS( OLOC, 'count', STATUS )
      END IF

*    Update history
      CALL HIST_COPY( ILOC, OLOC, STATUS )
      CALL HIST_ADD( OLOC, VERSION, STATUS )

      NLINE = MXTEXT
      HTEXT(1) = ' Input {INP}'
      HTEXT(2) = ' Binned dataset created from the LISTs : '//BNAME(1)
      HTEXT(3) = '                                         '//BNAME(2)
      WRITE (HTEXT(4), '(12X,A,I,A)') 'Containing data on ',
     :                                             NEV, ' events.'
      IF ( QUALITY ) THEN
        IF ( QKEEP ) THEN
          HTEXT(5) = 'QUALITY array created'
        ELSE
          HTEXT(5) = 'Bad quality events excluded.'
        END IF
        CALL USI_TEXT( 5, HTEXT, NLINE, STATUS )
      ELSE
        CALL USI_TEXT( 4, HTEXT, NLINE, STATUS )
      END IF

*    Write expanded text
      CALL HIST_PTXT( OLOC, NLINE, HTEXT, STATUS )

*    Tidy up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  EVPOLAR_RECPOL - Perform rectangular to polar conversion
      SUBROUTINE EVPOLAR_RECPOL( NEV, X0, Y0, XV, YV, POL,MAXR, STATUS )
     :
*    Description :
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 Feb 91 : Original
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER                          NEV            ! Number of events
      REAL                             X0, Y0         ! Position of pole
      REAL                             XV(*), YV(*)   ! Rectangular coords
*
*    Export :
*
      REAL                             POL(2,NEV)     ! Polar coordinates
      REAL                             MAXR           ! Maximum radius
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                             DX, DY         ! Offsets from pole

      INTEGER                          I              ! Loop over events
*-

      IF ( STATUS .EQ. SAI__OK ) THEN
        MAXR = -1.0
        DO I = 1, NEV
          DX = XV(I) - X0
          DY = YV(I) - Y0
          POL(1,I) = SQRT(DX*DX+DY*DY)
          POL(2,I) = ATAN2D(DX,DY)
          IF ( POL(2,I) .LT. 0 ) POL(2,I) = POL(2,I) + 360.0
          IF ( POL(1,I) .GT. MAXR ) MAXR = POL(1,I)
        END DO
      END IF

      END



*+  EVPOLAR_REGBIN - Regular bin polar binning
      SUBROUTINE EVPOLAR_REGBIN( NAZ, NRAD, RBINSIZE, NEV, POL,
     :              GOTQUAL, QKEEP, IQUAL, DATA, QUAL, NREJ, STATUS )
*
*    Description :
*
*    Method :
*
*     FOR each event
*       Get radial bin
*       IF valid
*         if 2D get azimuthal bin
*         IF input_quality present and good
*           data() = data() + 1
*           if qual() not already bad, set good
*         ELSE if create output quality
*           qual() = bad
*       ELSE
*         nreject = nreject + 1
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 Feb 91 : Original
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      INTEGER                          NAZ, NRAD      ! # azimuthal,radial bins
      REAL                             RBINSIZE       ! Radial bin size
      INTEGER                          NEV            ! Number of events
      REAL                             POL(2,NEV)     ! Polar coordinates
      LOGICAL                          GOTQUAL        ! Got input quality list?
      LOGICAL                          QKEEP          ! Create output quality?
      INTEGER                          IQUAL(NEV)     ! Input quality values
      INTEGER                          BADQUAL        ! Quality threshold
*
*    Export :
*
      REAL                             DATA(NRAD,NAZ) ! Output data
      BYTE                             QUAL(NRAD,NAZ) ! Output data
      INTEGER                          NREJ           ! Events not binned
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                             ABINSIZE       ! Azimuthal binsize

      INTEGER                          ABIN           ! Azimuthal bin
      INTEGER                          I              ! Loop over events
      INTEGER                          RBIN           ! Radial bin
*-

      IF ( STATUS .EQ. SAI__OK ) THEN
        NREJ = 0
        IF ( NAZ .EQ. 1 ) THEN
          IF ( GOTQUAL ) THEN
            DO I = 1, NEV
              RBIN = INT( POL(1,I) / RBINSIZE ) + 1
              IF ( RBIN .GT. NRAD ) THEN
                NREJ = NREJ + 1
              ELSE
                IF ( IQUAL(I) .LE. BADQUAL ) THEN
                  DATA(RBIN,1) = DATA(RBIN,1) + 1.0
                  IF ( QUAL(RBIN,1) .NE. QUAL__BAD )
     :                           QUAL(RBIN,1) = QUAL__GOOD
                ELSE IF ( QKEEP ) THEN
                  DATA(RBIN,1) = DATA(RBIN,1) + 1.0
                  QUAL(RBIN,1) = QUAL__BAD
                END IF
              END IF
            END DO
          ELSE
            DO I = 1, NEV
              RBIN = INT( POL(1,I) / RBINSIZE ) + 1
              IF ( RBIN .GT. NRAD ) THEN
                NREJ = NREJ + 1
              ELSE
                DATA(RBIN,1) = DATA(RBIN,1) + 1.0
              END IF
            END DO
          END IF

        ELSE

*        Need radial and azimuthal bins. Azimuth bin value is always valid
          ABINSIZE = 360.0 / REAL(NAZ)
          IF ( GOTQUAL ) THEN
            DO I = 1, NEV
              RBIN = INT( POL(1,I) / RBINSIZE ) + 1
              IF ( RBIN .GT. NRAD ) THEN
                NREJ = NREJ + 1
              ELSE
                ABIN = INT( POL(2,I)/ ABINSIZE ) + 1
                IF ( IQUAL(I) .LE. BADQUAL ) THEN
                  DATA(RBIN,ABIN) = DATA(RBIN,ABIN) + 1.0
                  IF ( QUAL(RBIN,ABIN) .NE. QUAL__BAD )
     :                           QUAL(RBIN,ABIN) = QUAL__GOOD
                ELSE IF ( QKEEP ) THEN
                  DATA(RBIN,ABIN) = DATA(RBIN,ABIN) + 1.0
                  QUAL(RBIN,ABIN) = QUAL__BAD
                END IF
              END IF
            END DO
          ELSE
            DO I = 1, NEV
              RBIN = INT( POL(1,I) / RBINSIZE ) + 1
              IF ( RBIN .GT. NRAD ) THEN
                NREJ = NREJ + 1
              ELSE
                ABIN = INT( POL(2,I)/ ABINSIZE ) + 1
                DATA(RBIN,ABIN) = DATA(RBIN,ABIN) + 1.0
              END IF
            END DO
          END IF

        END IF
      END IF

      END



*+  EVPOLAR_IRREGBIN - Irregular polar binning
      SUBROUTINE EVPOLAR_IRREGBIN( NAZ, NRAD, RANGE, NEV, POL,
     :              GOTQUAL, QKEEP, IQUAL, DATA, QUAL, NREJ, STATUS )
*
*    Description :
*
*    Method :
*
*     FOR each event
*       Get radial bin
*       IF valid
*         if 2D get azimuthal bin
*         IF input_quality present and good
*           data() = data() + 1
*           if qual() not already bad, set good
*         ELSE if create output quality
*           qual() = bad
*       ELSE
*         nreject = nreject + 1
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 Feb 91 : Original
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      INTEGER                          NAZ, NRAD      ! # azimuthal,radial bins
      REAL                             RANGE(*)       ! Bin boundaries
      INTEGER                          NEV            ! Number of events
      REAL                             POL(2,NEV)     ! Polar coordinates
      LOGICAL                          GOTQUAL        ! Got input quality list?
      LOGICAL                          QKEEP          ! Create output quality?
      INTEGER                          IQUAL(NEV)     ! Input quality values
      INTEGER                          BADQUAL        ! Quality threshold
*
*    Export :
*
      REAL                             DATA(NRAD,NAZ) ! Output data
      BYTE                             QUAL(NRAD,NAZ) ! Output data
      INTEGER                          NREJ           ! Events not binned
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                             ABINSIZE       ! Azimuthal binsize

      INTEGER                          ABIN           ! Azimuthal bin
      INTEGER                          I              ! Loop over events
      INTEGER                          RBIN           ! Radial bin

      LOGICAL                          OK             ! Valid radial bin?
*-

      IF ( STATUS .EQ. SAI__OK ) THEN
        NREJ = 0
        IF ( NAZ .EQ. 1 ) THEN
          IF ( GOTQUAL ) THEN
            DO I = 1, NEV
              CALL AXIS_RNGIDX( NRAD, RANGE,.FALSE.,POL(1,I), RBIN, OK )
              IF ( OK ) THEN
                IF ( IQUAL(I) .LE. BADQUAL ) THEN
                  DATA(RBIN,1) = DATA(RBIN,1) + 1.0
                  IF ( QUAL(RBIN,1) .NE. QUAL__BAD )
     :                           QUAL(RBIN,1) = QUAL__GOOD
                ELSE IF ( QKEEP ) THEN
                  DATA(RBIN,1) = DATA(RBIN,1) + 1.0
                  QUAL(RBIN,1) = QUAL__BAD
                END IF
              ELSE
                NREJ = NREJ + 1
              END IF
            END DO
          ELSE
            DO I = 1, NEV
              CALL AXIS_RNGIDX( NRAD, RANGE,.FALSE.,POL(1,I), RBIN, OK )
              IF ( OK ) THEN
                DATA(RBIN,1) = DATA(RBIN,1) + 1.0
              ELSE
                NREJ = NREJ + 1
              END IF
            END DO
          END IF

        ELSE

*        Need radial and azimuthal bins. Azimuth bin value is always valid
          ABINSIZE = 360.0 / REAL(NAZ)
          IF ( GOTQUAL ) THEN
            DO I = 1, NEV
              CALL AXIS_RNGIDX( NRAD, RANGE,.FALSE.,POL(1,I), RBIN, OK )
              IF ( OK ) THEN
                ABIN = INT( POL(2,I)/ ABINSIZE ) + 1
                IF ( IQUAL(I) .LE. BADQUAL ) THEN
                  DATA(RBIN,ABIN) = DATA(RBIN,ABIN) + 1.0
                  IF ( QUAL(RBIN,ABIN) .NE. QUAL__BAD )
     :                           QUAL(RBIN,ABIN) = QUAL__GOOD
                ELSE IF ( QKEEP ) THEN
                  DATA(RBIN,ABIN) = DATA(RBIN,ABIN) + 1.0
                  QUAL(RBIN,ABIN) = QUAL__BAD
                END IF
              ELSE
                NREJ = NREJ + 1
              END IF
            END DO
          ELSE
            DO I = 1, NEV
              CALL AXIS_RNGIDX( NRAD, RANGE,.FALSE.,POL(1,I), RBIN, OK )
              IF ( OK ) THEN
                ABIN = INT( POL(2,I)/ ABINSIZE ) + 1
                DATA(RBIN,ABIN) = DATA(RBIN,ABIN) + 1.0
              ELSE
                NREJ = NREJ + 1
              END IF
            END DO
          END IF

        END IF
      END IF

      END



*+  EVPOLAR_REGNORM - Normalise data array
      SUBROUTINE EVPOLAR_REGNORM( NAZ, NRAD, RBINSIZE, CX, CY, LOX,
     :                                HIX, LOY, HIY, DATA, STATUS )
*
*    Description :
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 Feb 91 : Original
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER                          NAZ, NRAD      ! # azimuthal,radial bins
      REAL                             RBINSIZE       ! Radial bin size
      REAL                             CX,CY          ! Centre of circle
      REAL                             LOX, HIX       ! Range of rectangle in X
      REAL                             LOY, HIY       ! Range of rectangle in Y
*
*    Import / export :
*
      REAL                             DATA(NRAD,NAZ) ! Output data
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      REAL                             GEO_CAREA
*
*    Local variables :
*
      REAL                             FACTOR         ! Normalisation factor
      REAL                             LAST_AREA      ! Last circle area
      REAL                             RAD            ! Current radius
      REAL                             THIS_AREA      ! Current circle area

      INTEGER                          I, J           ! Loop over bins
*-

      IF ( STATUS .EQ. SAI__OK ) THEN
        RAD = RBINSIZE
        LAST_AREA = 0.0
        DO I = 1, NRAD
          THIS_AREA = GEO_CAREA(RAD,CX,CY,LOX,HIX,LOY,HIY)
          FACTOR = ( THIS_AREA - LAST_AREA ) / REAL(NAZ)
          DO J = 1, NAZ
            DATA(I,J) = DATA(I,J) / FACTOR
          END DO
          RAD = RAD + RBINSIZE
          LAST_AREA = THIS_AREA
        END DO
      END IF

      END



*+  EVPOLAR_IRREGNORM - Normalise irregularly data array
      SUBROUTINE EVPOLAR_IRREGNORM( NAZ, NRAD, RANGE, CX, CY, LOX,
     :                               HIX, LOY, HIY, DATA, STATUS )
*
*    Description :
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 Feb 91 : Original
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER                          NAZ, NRAD      ! # azimuthal,radial bins
      REAL                             RANGE(2*NRAD)  ! Radial bin size
      REAL                             CX,CY          ! Centre of circle
      REAL                             LOX, HIX       ! Range of rectangle in X
      REAL                             LOY, HIY       ! Range of rectangle in Y
*
*    Import / export :
*
      REAL                             DATA(NRAD,NAZ) ! Output data
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      REAL                             GEO_CAREA
*
*    Local variables :
*
      REAL                             FACTOR         ! Normalisation factor

      INTEGER                          I, J           ! Loop over bins
      INTEGER                          IR             ! Range index counter
*-

      IF ( STATUS .EQ. SAI__OK ) THEN
        DO I = 1, NRAD
          IR = (I-1)*2+1
          FACTOR = ( GEO_CAREA(RANGE(IR+1),CX,CY,LOX,HIX,LOY,HIY)
     :               - GEO_CAREA(RANGE(IR),CX,CY,LOX,HIX,LOY,HIY) )
     :                                                    / REAL(NAZ)
          DO J = 1, NAZ
            DATA(I,J) = DATA(I,J) / FACTOR
          END DO
        END DO
      END IF

      END
