      SUBROUTINE EVPOLAR( STATUS )
*+
*  Name:
*     EVPOLAR

*  Purpose:
*     Bins 2 lists into a 1 or 2 dimensional object

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL EVPOLAR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
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

*  Usage:
*     evpolar {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Name of input EVDS
*     LISTS = CHAR (read)
*        Index No(s) of input list(s)
*     QVAL = INTEGER (read)
*        Event quality > QVAL = bad
*     QKEEP = LOGICAL (read)
*        Produce output QUALITY array?
*     REG = LOGICAL (read)
*        Regular or irregular radial bins
*     ABINSIZE = REAL (read)
*        Azimuthal bin size
*     RBINSIZE = REAL (read)
*        Regular radial bin width
*     RRANGE = CHAR (read)
*        Irregular bin bounds
*     NAZ, NRAD = INTEGER (read)
*        Numbers of bins
*     NORMALISE = LOGICAL (read)
*        Normalise output array
*     OUT = CHAR (read)
*        Name of output dataset

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     evpolar, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     21 Feb 1991 V1.4-0 (DJA):
*        Original version.
*     11 Sep 1993 V1.7-0 (DJA):
*        EVPOLAR_CAREA renamed GEO_CAREA
*     25 Feb 1994 V1.7-1 (DJA):
*        Use BIT_ routines to do bit manipulations
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     21 Apr 1995 V1.8-1 (DJA):
*        Updated data interface
*     18 Aug 1995 V2.0-0 (DJA):
*        ADI port of event handling
*     21 Feb 1996 V2.0-1 (DJA):
*        Removed ATAN2D for Linux port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
      EXTERNAL                  CHR_SIMLR
        LOGICAL			CHR_SIMLR

*  Local Constants:
      INTEGER                   MXRANGE          ! 2 x max No of irregular bins
         PARAMETER             (MXRANGE = 1000)
      INTEGER                   MXTEXT
         PARAMETER             (MXTEXT = 7)
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'EVPOLAR Version 2.2-0' )

*  Local Variables:
      CHARACTER*20              BNAME(2)         ! List names
      CHARACTER*40              BUNIT(2)         ! List units
      CHARACTER*80              HTEXT(MXTEXT)    ! History text
      CHARACTER*40              ORUNIT           ! Output radial unit
      CHARACTER*80              TEXT             ! Temporary text string

      REAL                      ABINSZ         ! Azimuthal bin size
      REAL                      LOX,HIX,LOY,HIY  ! Field min and max values
      REAL                      MAXR             ! Maximum radial distance
      REAL                      RANGE(MXRANGE)   ! Irregular bin ranges.
      REAL                      RBINSZ         		! Radial bin size
      REAL			SPARR(2)		! Spaced array data
      REAL                      X0, Y0           ! Centre of polar distribution

      INTEGER                   BADQUAL          ! Exclude events with quality
                                                 ! > this value.
      INTEGER			BLID(2)			! List identifiers
      INTEGER                   BPTR(2)          	! Binning list pointers
      INTEGER                   I                	! Loop counter
      INTEGER			IFID			! Input dataset id
      INTEGER                   INDEX(2)		! Selected lists
      INTEGER                   INLIST           ! Number of lists in input EVDS
      INTEGER                   IQPTR            	! Input quality data
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
      INTEGER			OFID			! Output dataset id
      INTEGER                   ONELM            ! # output bins
      INTEGER                   OQPTR            ! Output quality pointer
      INTEGER                   WPTR             ! Pointer to workspace array

      LOGICAL                   NORMALISE        ! Normalise output bins?
      LOGICAL                   QKEEP            ! If true produce output
                                                 ! quality array.
      LOGICAL                   QUALITY          ! Is QUALITY list present?
      LOGICAL                   REG              ! Allow irregular binning?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get event dataset
      CALL USI_ASSOC( 'INP', 'EventDS', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Find all valid lists in INP. Display them to user.
      CALL MSG_BLNK()
      CALL MSG_PRNT( 'The available lists are :' )
      CALL MSG_BLNK()
      CALL EDI_DISP( IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get number of events and lists
      CALL EDI_GETNS( IFID, NEV, INLIST, STATUS )

*  Select the lists
      CALL MSG_BLNK()
      CALL MSG_PRNT( 'Select two lists to bin using index'/
     :             /' numbers above, eg. 1 2. The direction' )
      CALL MSG_PRNT( 'of zero azimuth is that of increasing list'/
     :                                               /' 1 value.')
      CALL MSG_BLNK()
      CALL EDI_SELCT( 'LISTS', INLIST, 2, 2, INDEX, NINDEX, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Is there a quality list?
      CALL EDI_CHK( IFID, 'QUALITY', QUALITY, STATUS )
      IF ( QUALITY ) THEN

*    Map it
        CALL EDI_MAPI( IFID, 'QUALITY', 'READ', 0, 0, IQPTR, STATUS )

*    Get quality processing mode
        CALL USI_GET0I( 'QVAL',  BADQUAL, STATUS )
        CALL USI_GET0L( 'QKEEP', QKEEP, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get info on selected lists
      DO I = 1, 2

*      Make sure user didn't select quality
C        IF ( QUALITY ) THEN
C          CALL HDX_SAME( LLOC(INDEX(I)), QLOC, SAME, STATUS )
C          IF ( SAME ) THEN
C            STATUS = SAI__ERROR
C            CALL ERR_REP( ' ', 'ERROR : Cannot bin using QUALITY list',
C     :                    STATUS )
C            GOTO 99
C          END IF
C        END IF

*    Locate the list
        CALL EDI_IDX( IFID, INDEX(I), BLID(I), STATUS )

*    Get its name
        CALL ADI_CGET0C( BLID(I), 'Name', BNAME(I), STATUS )

*    Map it
        CALL EDI_MAPR( IFID, BNAME(I), 'READ', 0, 0, BPTR(I), STATUS )

*    Get units
        CALL ADI_CGET0C( BLID(I), 'Units', BUNIT(I), STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          BUNIT(I) = ' '
        END IF

      END DO

*  Get field extrema
      CALL ADI_CGET0R( BLID(1), 'Min', LOX, STATUS )
      CALL ADI_CGET0R( BLID(1), 'Max', HIX, STATUS )
      CALL ADI_CGET0R( BLID(2), 'Min', LOY, STATUS )
      CALL ADI_CGET0R( BLID(2), 'Max', HIY, STATUS )

*  Decide on radial units
      ORUNIT = BUNIT(1)
      IF ( .NOT. CHR_SIMLR( BUNIT(1), BUNIT(2) ) ) THEN
        CALL MSG_PRNT( 'WARNING : List units are different, '/
     :                              /'output may be rubbish' )
      END IF

*  Announce axis ranges
      CALL MSG_SETC( 'AX', BNAME(1) )
      CALL MSG_SETR( 'LO', LOX )
      CALL MSG_SETR( 'HI', HIX )
      CALL MSG_PRNT( 'Axis ^AX ranges from ^LO to ^HI' )
      CALL MSG_SETC( 'AX', BNAME(2) )
      CALL MSG_SETR( 'LO', LOY )
      CALL MSG_SETR( 'HI', HIY )
      CALL MSG_PRNT( 'Axis ^AX ranges from ^LO to ^HI' )

*  Get polar centre
      CALL USI_GET0R( 'X0', X0, STATUS )
      CALL USI_GET0R( 'Y0', Y0, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Map workspace
      CALL DYN_MAPR( 1, 2*NEV, WPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Find azimuth and radius for each bin
      CALL EVPOLAR_RECPOL( NEV, X0, Y0, %VAL(BPTR(1)), %VAL(BPTR(2)),
     :                                     %VAL(WPTR), MAXR, STATUS )

*  Get azimuthal bin size
      CALL USI_GET0R( 'ABINSIZE', ABINSZ, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      NAZ = 360.0 / ABINSZ
      CALL MSG_SETI( 'NAZ', NAZ )
      CALL MSG_PRNT( 'There will be ^NAZ azimuthal bins in the output' )

*  Inform user of maximum radius, and get radial bins
      CALL USI_GET0L( 'REG', REG, STATUS )
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
        CALL USI_GET0R( 'RBINSIZE', RBINSZ, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        IF ( RBINSZ .LE. 0.0 ) THEN
          CALL MSG_PRNT( 'ERROR : Zero or negative radial bin width' )
          STATUS = SAI__ERROR
        END IF

*      Get number of radial bins
        CALL USI_DEF0I( 'NRAD', MAX(1,NINT(MAXR/RBINSZ)), STATUS )
        CALL USI_GET0I( 'NRAD', NRAD, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Decide output dimensionality
      IF ( ( NAZ .EQ. 1 ) .OR. ( NRAD .EQ. 1 ) ) THEN
        NDIM = 1
        ODIMS(1) = NRAD
      ELSE
        NDIM = 2
        ODIMS(1) = NRAD
        ODIMS(2) = NAZ
      END IF

*  Create output dataset
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      CALL BDI_LINK( 'BinDS', NDIM, ODIMS, 'REAL', OFID, STATUS )
      CALL BDI_MAPR( OFID, 'Data', 'WRITE/ZERO', ODPTR, STATUS )

*  Create AXIS structure
      IF ( REG ) THEN
        SPARR(1) = 0.0
        SPARR(2) = RBINSZ
        CALL BDI_AXPUT1R( OFID, 1, 'SpacedData', 2, SPARR, STATUS )
        CALL BDI_AXPUT0R( OFID, 1, 'ScalarWidth', RBINSZ, STATUS )
      ELSE
        CALL BDI_AXMAPR( OFID, 1, 'Data', 'WRITE', OAPTR, STATUS )
        CALL BDI_AXMAPR( OFID, 1, 'Width', 'WRITE', OWPTR, STATUS )
        CALL ARR_BND2CWR( NRAD, RANGE, %VAL(OAPTR), %VAL(OWPTR),
     :                                                  STATUS )
      END IF
      CALL BDI_AXPUT0C( OFID, 1, 'Label', 'Radius', STATUS )
      IF ( ORUNIT .GT. ' ' ) THEN
        CALL BDI_AXPUT0C( OFID, 1, 'Units', ORUNIT, STATUS )
      END IF
      IF ( NDIM .EQ. 2 ) THEN
        SPARR(1) = 0.5*ABINSZ
        SPARR(2) = ABINSZ
        CALL BDI_AXPUT1R( OFID, 2, 'SpacedData', 2, SPARR, STATUS )
        CALL BDI_AXPUT0R( OFID, 2, 'ScalarWidth', ABINSZ, STATUS )
        CALL BDI_AXPUT0C( OFID, 2, 'Label', 'Azimuth', STATUS )
        CALL BDI_AXPUT0C( OFID, 2, 'Units', 'degree', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Initialise quality to data missing
      CALL ARR_SUMDIM( NDIM, ODIMS, ONELM )
      IF ( QUALITY .AND. QKEEP ) THEN
        CALL BDI_MAPUB( OFID, 'Quality', 'WRITE/QMISSING', OQPTR,
     :                 STATUS )
        CALL BDI_PUT0UB( OFID, 'QualityMask', QUAL__MASK, STATUS )
      END IF

*  Bin the events
      IF ( REG ) THEN
        CALL EVPOLAR_REGBIN( NAZ, NRAD, RBINSZ, NEV, %VAL(WPTR),
     :                       QUALITY, QKEEP, %VAL(IQPTR), BADQUAL,
     :                     %VAL(ODPTR), %VAL(OQPTR), NREJ, STATUS )
      ELSE
        CALL EVPOLAR_IRREGBIN( NAZ, NRAD, RANGE, NEV,
     :                     %VAL(WPTR), QUALITY, QKEEP, %VAL(IQPTR),
     :                     BADQUAL, %VAL(ODPTR), %VAL(OQPTR), NREJ,
     :                      STATUS )
      END IF

*  Inform of events binned
      CALL MSG_SETI( 'BIN', NEV - NREJ )
      CALL MSG_SETI( 'NIN', NEV )
      CALL MSG_PRNT( '^BIN events were binned out of ^NIN input.' )

*  Normalise
      NORMALISE = .FALSE.
      IF ( NINDEX .EQ. 2 ) THEN
        CALL USI_GET0L( 'NORM', NORMALISE, STATUS )
      END IF
      IF ( NORMALISE .AND. ( STATUS .EQ. SAI__OK ) ) THEN
        IF ( REG ) THEN
          CALL EVPOLAR_REGNORM( NAZ, NRAD, RBINSZ, X0, Y0, LOX, HIX,
     :                                 LOY, HIY, %VAL(ODPTR), STATUS )
        ELSE
          CALL EVPOLAR_IRREGNORM( NAZ, NRAD, RANGE, X0, Y0, LOX, HIX,
     :                                LOY, HIY, %VAL(ODPTR), STATUS )
        END IF
      END IF
      CALL BDI_AXPUT0L( OFID, 1, 'Normalised', NORMALISE, STATUS )
      IF ( NDIM .EQ. 2 ) THEN
        CALL BDI_AXPUT0L( OFID, 2, 'Normalised', NORMALISE, STATUS )
      END IF

*  Copy ancillary stuff
      CALL BDI_COPY( IFID, 'Title,Label,Units', OFID, ' ', STATUS )
      CALL UDI_COPANC( IFID, 'grf', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Re-write data units
      IF ( NORMALISE ) THEN
        CALL MSG_SETC( 'UN', ORUNIT )
        CALL MSG_MAKE( 'count/^UN**2', TEXT, LEN )
        CALL BDI_PUT0C( OFID, 'Units', TEXT(:LEN), STATUS )
      ELSE
        CALL BDI_PUT0C( OFID, 'Units', 'count', STATUS )
      END IF

*  Update history
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )

      NLINE = MXTEXT
      HTEXT(1) = 'Input {INP}'
      HTEXT(2) = 'Binned dataset created from the LISTs : '//BNAME(1)
      HTEXT(3) = '                                        '//BNAME(2)
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

*  Write expanded text
      CALL HSI_PTXT( OFID, NLINE, HTEXT, STATUS )

*  Free the selected lists
      DO I = 1, 2
        CALL ADI_ERASE( BLID(I), STATUS )
        CALL EDI_UNMAP( IFID, BNAME(I), STATUS )
      END DO

*  Tidy up
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
      INCLUDE 'MATH_PAR'
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
          POL(2,I) = ATAN2(DX,DY)*MATH__RTOD
          IF ( POL(2,I) .LT. 0 ) POL(2,I) = POL(2,I) + 360.0
          IF ( POL(1,I) .GT. MAXR ) MAXR = POL(1,I)
        END DO
      END IF

      END



*+  EVPOLAR_REGBIN - Regular bin polar binning
      SUBROUTINE EVPOLAR_REGBIN( NAZ, NRAD, RBINSZ, NEV, POL,
     :              GOTQUAL, QKEEP, IQUAL, BADQUAL, DATA, QUAL,
     :              NREJ, STATUS )
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
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      INTEGER                          NAZ, NRAD      ! # azimuthal,radial bins
      REAL                             RBINSZ       ! Radial bin size
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
      REAL                             ABINSZ       ! Azimuthal binsize

      INTEGER                          ABIN           ! Azimuthal bin
      INTEGER                          I              ! Loop over events
      INTEGER                          RBIN           ! Radial bin
*-

      IF ( STATUS .EQ. SAI__OK ) THEN
        NREJ = 0
        IF ( NAZ .EQ. 1 ) THEN
          IF ( GOTQUAL ) THEN
            DO I = 1, NEV
              RBIN = INT( POL(1,I) / RBINSZ ) + 1
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
              RBIN = INT( POL(1,I) / RBINSZ ) + 1
              IF ( RBIN .GT. NRAD ) THEN
                NREJ = NREJ + 1
              ELSE
                DATA(RBIN,1) = DATA(RBIN,1) + 1.0
              END IF
            END DO
          END IF

        ELSE

*        Need radial and azimuthal bins. Azimuth bin value is always valid
          ABINSZ = 360.0 / REAL(NAZ)
          IF ( GOTQUAL ) THEN
            DO I = 1, NEV
              RBIN = INT( POL(1,I) / RBINSZ ) + 1
              IF ( RBIN .GT. NRAD ) THEN
                NREJ = NREJ + 1
              ELSE
                ABIN = INT( POL(2,I)/ ABINSZ ) + 1
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
              RBIN = INT( POL(1,I) / RBINSZ ) + 1
              IF ( RBIN .GT. NRAD ) THEN
                NREJ = NREJ + 1
              ELSE
                ABIN = INT( POL(2,I)/ ABINSZ ) + 1
                DATA(RBIN,ABIN) = DATA(RBIN,ABIN) + 1.0
              END IF
            END DO
          END IF

        END IF
      END IF

      END



*+  EVPOLAR_IRREGBIN - Irregular polar binning
      SUBROUTINE EVPOLAR_IRREGBIN( NAZ, NRAD, RANGE, NEV, POL,
     :              GOTQUAL, QKEEP, IQUAL, BADQUAL, DATA, QUAL,
     :              NREJ, STATUS )
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
      REAL                             ABINSZ       ! Azimuthal binsize

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
          ABINSZ = 360.0 / REAL(NAZ)
          IF ( GOTQUAL ) THEN
            DO I = 1, NEV
              CALL AXIS_RNGIDX( NRAD, RANGE,.FALSE.,POL(1,I), RBIN, OK )
              IF ( OK ) THEN
                ABIN = INT( POL(2,I)/ ABINSZ ) + 1
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
                ABIN = INT( POL(2,I)/ ABINSZ ) + 1
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
      SUBROUTINE EVPOLAR_REGNORM( NAZ, NRAD, RBINSZ, CX, CY, LOX,
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
*
*    Import :
*
      INTEGER                          NAZ, NRAD      ! # azimuthal,radial bins
      REAL                             RBINSZ       ! Radial bin size
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
        RAD = RBINSZ
        LAST_AREA = 0.0
        DO I = 1, NRAD
          THIS_AREA = GEO_CAREA(RAD,CX,CY,LOX,HIX,LOY,HIY)
          FACTOR = ( THIS_AREA - LAST_AREA ) / REAL(NAZ)
          DO J = 1, NAZ
            DATA(I,J) = DATA(I,J) / FACTOR
          END DO
          RAD = RAD + RBINSZ
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
