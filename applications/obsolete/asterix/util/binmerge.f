*+  BINMERGE - Merge 2 or more binned datasets
      SUBROUTINE BINMERGE( STATUS )
*    Description :
*      Merges up to 10 binned datasets. All must have the same number
*     of dimensions, and the axes must be in the same order.
*      BASE_TAI is included when combinig RAW_TIMETAG axes, but dataset
*     is assumed to have time units of seconds.
*      The merged dataset should be rebinned to correctly order the axes.
*    Parameters :
*     INP1,INP2,INP3,INP4,...INP10=UNIV(R)
*           Input dataset(s)
*     OUT=UNIV(W)
*           Output dataset
*    Method :
*     Each of the user-supplied list of datasets is checked against the first
*     to see that its structure is similar, i.e. that the dimensionality is
*     the same, and that the AXIS LABELS are the same.
*     The output dataset is formed using the total lengths of the arrays
*     found in the input datasets.
*     Data are copied from each input dataset into the output.
*     The 'header' data items are taken from the first dataset, though the
*    Deficiencies :
*     i) Program requires that the axes are in the same order in all files.
*    ii) Program only aligns RAW_TIMETAG axis (using BASE_TAI). Assumes time
*        axis units are seconds, but does not check. Should be able to
*        align X_RAW & Y_RAW axes using FIELD_RA & FIELD_DEC values.
*   iii) Program does not allow for rotations - eg if it tried to merge one
*        image with another of same dimensions & with same central RA & DEC,
*        but with a different North angle it would produce meaningless rubbish!
*                   THESE ARE NOT VALID ASSUMPTIONS
*    iv) FIELD_MAX & FIELD_MIN components should be updated.
*     v) Does nothing with live time or exposure time. These are
*        all taken from the first input dataset.
*    Bugs :
*    Authors :
*     Alan McFadzean (BHVAD::ADM)
*     Phil Andrews   (BHVAD::PLA)
*    History :
*
*      2 Sep 88 : V1.0-1 Original adapted from ASTERIX routine (ADM)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*      7 Feb 95 : V1.8-1 Various bugs fixed (DJA)
*      5 Sep 95 : V1.8-2 Partial ADI port (DJA)
*     16 Jan 1996 V2.0-0 (DJA):
*        Full ADI port
*
*    Type Definitions :
*
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN
      LOGICAL CHR_SIMLR
*    Global variables :
      INTEGER                MXINP             ! maximum number of input datasets
         PARAMETER          (MXINP = 10)
*
*    Local variables :
*
      CHARACTER              		C               ! character value
      CHARACTER*(PAR__SZNAM)	PARNAM            	! Parameter name
      CHARACTER*1            LAST              ! number of axis to be merged
      CHARACTER*80           LABEL             ! An axis label
      CHARACTER*80           LABEL1(ADI__MXDIM)! Axis labels for first dataset
      CHARACTER*80           UNITS(ADI__MXDIM,MXINP)    ! Axis units
      CHARACTER*80           TESTUN1,TESTUN    ! Dummy tests for unit values

      DOUBLE PRECISION       AXIS_DEC(MXINP)   ! AXIS_DEC of datasets
      DOUBLE PRECISION       AXIS_RA(MXINP)    ! AXIS_RA of datasets
      DOUBLE PRECISION       C1,C2,C3,C4,C5,C6 ! Used in transforming between axes
      DOUBLE PRECISION       F_DEC             ! Field_Dec of current dataset
      DOUBLE PRECISION       F_DEC1            ! Field_Dec of first dataset
      DOUBLE PRECISION       F_RA              ! Field_RA of current dataset
      DOUBLE PRECISION       F_RA1             ! Field_RA of first dataset
      DOUBLE PRECISION       	PA(MXINP) 		! PA of datasets
      DOUBLE PRECISION		SPOINT(2)		! Pointing direction
      DOUBLE PRECISION		TOFFS(MXINP)		! Timing offsets

      REAL                   ADD               ! Add to timetag of current dataset
      REAL                   YMIN              ! Minimium Y axis value
      REAL                   XMIN              ! Minimium X axis value

      INTEGER                AXOFF(ADI__MXDIM) ! axis(N) offset
      INTEGER                DSETS             ! number of input datasets
      INTEGER                EQUINOX1          ! EQUINOX of 1st dataset
      INTEGER                EQUINOX           ! EQUINOX of dataset
      INTEGER                IAXPTR            ! pointer to input AXISn_DATA
      INTEGER                IDPTR             ! pointer to input DATA_ARRAY
      INTEGER			IFID(MXINP)		! Input dataset ids
      INTEGER			IFILES			! Input file info
      INTEGER                INAXW             ! pointer to input AXIS(n) WIDTH
      INTEGER                INP, AX ,I        ! loop counters
      INTEGER                IQPTR             ! pointer to input QUALITY
      INTEGER                LDIMS(ADI__MXDIM, MXINP)   ! dimensions of datasets' DATA_ARRAYs
      INTEGER                LENGTH            ! Of character strings
      INTEGER                NDIMS             ! # of dimensions of a dataset DATA_ARRAY
      INTEGER                NDIMS1            ! # of dimensions of dataset 1 DATA_ARRAY
      INTEGER                NPTS              ! Number of points in output dataset
      INTEGER			PRJID(MXINP)		! WCS projection data
      INTEGER			PIXID(MXINP)		! WCS pixel data
      INTEGER			SYSID(MXINP)		! WCS coord sys data
      INTEGER                	OAXPTR(ADI__MXDIM)	! Output axis data
      INTEGER                	OAXW(ADI__MXDIM)  	! Output axis widths
      INTEGER                	ODPTR             	! Output primary data
      INTEGER                OFFSET(ADI__MXDIM)! output data array offset
      INTEGER        		OFID			! Output dataset id
      INTEGER                OLDIMS(ADI__MXDIM)! dimensions of output dataset.
      INTEGER                OQPTR             ! pointer to output QUALITY
      INTEGER                SIZ               ! Dummy variable
      INTEGER                IVPTR             ! pointer to input VARIANCE
      INTEGER                OVPTR            ! pointer to output VARIANCE
      INTEGER			OTIMID			! O/p timing data
      INTEGER			TBASE			! Time reference
      INTEGER			TIMID(MXINP)		! Timing data
      INTEGER			X_AX, Y_AX, T_AX	! Axis numbers
      INTEGER                	XAXPTR            	! X axis data
      INTEGER                	YAXPTR            	! Y axis data

      LOGICAL                	AXISOK            	! Axis data present
      LOGICAL                	AXWOK             	! Axis width present
      LOGICAL                	CHKLABEL(ADI__MXDIM) 	! Check axis label?
      LOGICAL                	CHKUNITS(ADI__MXDIM) 	! Check axis units?
      LOGICAL                	CONTINUE          ! controls copying of axis info
      LOGICAL                	DONE              ! Copied the X &/or Y axes
      LOGICAL                	INPUT             	! Loops over input
      LOGICAL                	OK                	! Data item acceptable
      LOGICAL                	QUAL              	! Data quality present
      LOGICAL                	VAROK             	! Data errors present?
*
*    Version id :
*
      CHARACTER*24           VERSION
         PARAMETER          (VERSION = 'BINMERGE version 2.0-0' )
*-

*    Version announcement
      CALL MSG_PRNT( VERSION )

*  Initialise
      CALL AST_INIT()

      CALL ARR_INIT1I( 1, ADI__MXDIM*MXINP, LDIMS, STATUS )
      DO I = 1, ADI__MXDIM
        AXOFF(I)  = 0
        OFFSET(I) = 0
        OLDIMS(I) = 1
      END DO
      AXISOK = .TRUE.

*  Get first input file
      CALL USI_ASSOC( 'INP1', 'BinDS|Array', 'READ', IFID(1), STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check its data
      CALL BDI_CHK( IFID(1), 'Data', OK, STATUS)
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR: Invalid dataset - '/
     :                /'Bad data', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get its dimensions
      CALL BDI_GETSHP( IFID(1), ADI__MXDIM, LDIMS(1,1), NDIMS1, STATUS)
      CALL CHR_ITOC (NDIMS1, LAST, LENGTH)

*  Check out the axes
      DO AX = 1, NDIMS1

*    Data ok?
        CALL BDI_AXCHK( IFID(1), AX, 'Data', OK, STATUS )
        AXISOK = (AXISOK.AND.OK)

*    Get label
        CALL BDI_AXGET0C( IFID(1), AX, 'Label', LABEL1(AX), STATUS )
        IF ( LABEL1(AX) .LE. ' ' ) THEN
          CHKLABEL(AX) = .FALSE.
          CALL MSG_SETI ('AX', AX)
          CALL MSG_PRNT ('WARNING: Axis ^AX has no LABEL - '//
     :                             'Unable to check for consistency')
        ELSE
          CHKLABEL(AX) = .TRUE.
        END IF

*    Get units
        CALL BDI_AXGET0C( IFID(1), AX, 'Units', UNITS(AX,1), STATUS )
        CALL CHR_UCASE( UNITS(AX,1) )
        IF ( UNITS(AX,1) .LE. ' ' ) THEN
          CHKUNITS(AX) = .FALSE.
          CALL MSG_SETI ('AX', AX)
          CALL MSG_PRNT ('WARNING: Axis ^AX has no UNITS')
        ELSE
          CHKUNITS(AX) = .TRUE.
        END IF
      END DO

*  If all axes ok, look for important ones
      IF ( AXISOK ) THEN

*    Look for X, Y and time axes
        CALL BDI0_FNDAXC( IFID(1), 'X', X_AX, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          X_AX = 0
        END IF
        CALL BDI0_FNDAXC( IFID(1), 'Y', Y_AX, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          Y_AX = 0
        END IF
        CALL BDI0_FNDAXC( IFID(1), 'T', T_AX, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          Y_AX = 0
        END IF

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR: Invalid dataset - '/
     :                  /'Bad axis data', STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get world coordinates & timing
      CALL WCI_GETIDS( IFID(1), PIXID(1), PRJID(1), SYSID(1), STATUS )
      CALL TCI_GETID( IFID(1), TIMID(1), STATUS )

*  Extract pointing
      IF ( PRJID(1) .NE. ADI__NULLID ) THEN
        CALL ADI_CGET1D( PRJID(1), 'SPOINT', 2, SPOINT, SIZ, STATUS )
        AXIS_RA(1) = SPOINT(1)
        AXIS_DEC(1) = SPOINT(2)
      ELSE
        AXIS_RA(1) = 0D0
        AXIS_DEC(1) = 0D0
      END IF
      IF ( PIXID(1) .NE. ADI__NULLID ) THEN
        CALL ADI_CGET0D( PIXID(1), 'ROTATION', PA(1), STATUS )
      ELSE
        PA(1) = 0D0
      END IF

*  Get equinox of dataset
      CALL ADI_CGET0I( SYSID(1), EQUINOX1, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Obtain other inputs
      DSETS  = 2
      INPUT  = .TRUE.
      DO WHILE (INPUT)

*      Deduce parameter name and associate with dataset.
        CALL CHR_ITOC (DSETS, C, LENGTH)
        PARNAM = 'INP'//C(1:LENGTH)
        CALL USI_ASSOC( PARNAM, 'BinDS|Array', 'READ',
     :                   IFID(DSETS), STATUS )

*    Null denotes end of input
        IF ( STATUS .EQ. PAR__NULL ) THEN
          CALL ERR_ANNUL (STATUS)
          INPUT  = .FALSE.

*    Abort for other bad status
        ELSE IF ( STATUS .NE. SAI__OK ) THEN
          GOTO 99

*    Otherwise process the file
        ELSE

*      Check it out
          CALL BDI_CHK( IFID(DSETS), 'Data', OK, STATUS )
          CALL BDI_GETSHP( IFID(DSETS), ADI__MXDIM, LDIMS(1,DSETS),
     :                     NDIMS, STATUS )

          IF (OK .AND. NDIMS .EQ. NDIMS1) THEN
            AX = 0

            DO WHILE (OK .AND. AX .LT. NDIMS1)
              AX = AX + 1

*          Still checking axis AX units?
              IF (CHKUNITS(AX)) THEN
                CALL BDI_AXGET0C( IFID(DSETS), AX, 'Units',
     :                            UNITS(AX,DSETS), STATUS )
                UNITS(AX,DSETS)=TESTUN
                UNITS(AX,1)=TESTUN1
                IF ( .NOT. CHR_SIMLR( TESTUN, TESTUN1 ) ) THEN
                  CALL MSG_PRNT ('WARNING: Axis units mismatch - '//
     :                                         'OUTPUT MAY BE GARBAGE!')

                  CALL USI_DEF0L( 'CONT', .TRUE., STATUS )
                  CALL USI_GET0L( 'CONT', OK, STATUS )
                  CALL USI_CANCL( 'CONT', STATUS )

                END IF

              END IF

*          Still checking axis AX labels?
              IF (CHKLABEL(AX)) THEN

*            Get this datasets axis label
                CALL BDI_AXGET0C( IFID(DSETS), AX, 'Label', LABEL,
     :                            STATUS )

                IF ( .NOT. CHR_SIMLR( LABEL, LABEL1(AX) ) ) THEN
                  CALL MSG_PRNT ('WARNING: Axis label mismatch - '//
     :                                         'OUTPUT MAY BE GARBAGE!')

                  CALL USI_DEF0L( 'CONT', .TRUE., STATUS )
                  CALL USI_GET0L( 'CONT', OK, STATUS )
                  CALL USI_CANCL( 'CONT', STATUS )

                END IF
              END IF
            END DO

*        Get world coordinates & timing
            CALL WCI_GETIDS( IFID(DSETS), PIXID(DSETS), PRJID(DSETS),
     :                                         SYSID(DSETS), STATUS )
            CALL TCI_GETID( IFID(DSETS), TIMID(DSETS), STATUS )

*        Extract pointing
            IF ( PRJID(DSETS) .NE. ADI__NULLID ) THEN
              CALL ADI_CGET1D( PRJID(DSETS), 'SPOINT', 2, SPOINT,
     :                         SIZ, STATUS )
              AXIS_RA(DSETS) = SPOINT(1)
              AXIS_DEC(DSETS) = SPOINT(2)
            ELSE
              AXIS_RA(DSETS) = 0D0
              AXIS_DEC(DSETS) = 0D0
            END IF
            IF ( PIXID(DSETS) .NE. ADI__NULLID ) THEN
              CALL ADI_CGET0D( PIXID(DSETS), 'ROTATION',
     :                       PA(DSETS), STATUS )
            ELSE
              PA(DSETS) = 0D0
            END IF

*        Get equinox of dataset
            CALL ADI_CGET0I( SYSID(DSETS), EQUINOX, STATUS )

            IF ( EQUINOX .NE. EQUINOX1 ) THEN
              CALL MSG_PRNT
     :                 ('ERROR: Datasets have different EQUINOX values')
              OK = .FALSE.

            END IF

            IF (OK) THEN
*            Update number of valid inputs
              DSETS = DSETS + 1

              IF (DSETS .GT. MXINP) THEN
                CALL MSG_PRNT ('WARNING: Maximum number if datsets'/
     :                                /' - no more input files allowed')
                INPUT = .FALSE.

              END IF
            END IF

          ELSE IF (OK .AND. NDIMS .NE. NDIMS1) THEN
            OK = .FALSE.
            CALL MSG_PRNT ('ERROR: Datasets have different '//
     :                                                 'dimensionality')

          ELSE
            OK = .FALSE.

          END IF

          IF (INPUT .AND. ((STATUS .NE. SAI__OK) .OR. .NOT. OK)) THEN
            IF (STATUS.NE.SAI__OK) CALL ERR_FLUSH( STATUS )
            CALL MSG_PRNT ('Invalid input - try again.')
            CALL USI_CANCL (PARNAM, STATUS)

          END IF
        END IF
      END DO
      IF (STATUS .NE. SAI__OK) GOTO 99

*  End of input
      DSETS = DSETS - 1

*  Obtain output array size
      DO AX = 1, NDIMS1
        OLDIMS(AX) = LDIMS(AX,1)
        DO INP = 2, DSETS
          OLDIMS(AX) = OLDIMS(AX) + LDIMS(AX,INP)
        END DO
      END DO

*  Create output dataset
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      CALL BDI_LINK( 'BinDS', NDIMS1, OLDIMS, 'REAL', OFID, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Find length of output dataset
      CALL ARR_SUMDIM( NDIMS1, OLDIMS, NPTS )

*  Copy across subsidiary data from first input dataset
      CALL BDI_COPY( IFID(1), 'QualityMask,Title,Label,Units',
     :               OFID, ' ', STATUS )
      CALL UDI_COPANC( IFID(1), 'grf', OFID, STATUS )
      CALL HSI_COPY( IFID(1), OFID, STATUS )

*  Merge timing data and write it
      CALL TCI_MERGE( DSETS, TIMID, OTIMID, TBASE, TOFFS, STATUS )
      CALL TCI_PUTID( OFID, OTIMID, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Create output data
      CALL BDI_MAPR( OFID, 'Data', 'WRITE', ODPTR, STATUS )
      CALL ARR_INIT1R( 0.0, NPTS, %VAL(ODPTR), STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get first lot of axis data
      DO AX = 1, NDIMS1

*    Map data
        CALL BDI_AXMAPR( OFID, AX, 'Data', 'WRITE', OAXPTR(AX), STATUS )

*    Copy stuff to output
        CALL BDI_AXCOPY( IFID(1), AX, 'Label,Units,Normalised',
     :                   OFID, AX, STATUS )

*    Map widths if present
        CALL BDI_AXCHK( IFID(1), AX, 'Width', AXWOK, STATUS )
        IF ( AXWOK ) THEN
          CALL BDI_AXMAPR( OFID, AX, 'Width', 'WRITE', OAXW(AX),
     :                     STATUS )
        END IF

      END DO

*  Check quality
      CALL BDI_CHK( IFID(1), 'Quality', QUAL, STATUS )
      IF ( QUAL ) THEN
        CALL BDI_MAPUB( OFID, 'Quality', 'WRITE', OQPTR, STATUS )
        CALL ARR_INIT1B( QUAL__GOOD, NPTS, %VAL(OQPTR), STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check variance
      CALL BDI_CHK( IFID(1), 'Variance', VAROK, STATUS )
      IF ( VAROK ) THEN
        CALL BDI_MAPR( OFID, 'Variance', 'WRITE', OVPTR, STATUS )
        CALL ARR_INIT1R( 0.0, NPTS, %VAL(OVPTR), STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Loop through input objects merging arrays
      DO INP = 1, DSETS
        CALL MSG_SETI( 'INP', INP )
        CALL MSG_PRNT( ' Writing input dataset ^INP to output file' )

        AX = 0

        IF (X_AX .GT. -1 .OR. Y_AX .GT. -1) THEN
          IF (PA(1) .EQ. PA(INP) .AND.
     :        AXIS_DEC(1)       .EQ. AXIS_DEC(INP)       .AND.
     :        AXIS_RA (1)       .EQ. AXIS_RA (INP)) THEN
            DONE = .FALSE.

          ELSE
            DONE = .TRUE.

            IF (X_AX .GT. -1) THEN
              CALL BINMERGE_GET_MIN( IFID(INP), UNITS(X_AX,1),
     :             UNITS(X_AX,INP), LDIMS(X_AX,INP), X_AX, XMIN,
     :                                                   XAXPTR, STATUS)

            ELSE
              XMIN = AXIS_RA(INP)

            END IF

            IF (Y_AX .GT. -1) THEN
              CALL BINMERGE_GET_MIN( IFID(INP), UNITS(Y_AX,1),
     :             UNITS(Y_AX,INP), LDIMS(Y_AX,INP), Y_AX, YMIN,
     :                                                   YAXPTR, STATUS)

            ELSE
              YMIN = AXIS_DEC(INP)

            END IF

            IF (X_AX .GT. -1) THEN
              C1 = COSD(PA(1))
              C2 = COSD(PA(INP))
              C3 = (AXIS_RA(INP) - AXIS_RA(1))
     :                      - (YMIN * SIND(PA(INP)))
              C4 = SIND(PA(1))
              C5 = SIND(PA(INP))
              C6 = (AXIS_DEC(INP) - AXIS_DEC(1))
     :                      + (YMIN * COSD(PA(INP)))

              CALL BINMERGE_ALTER (C1, C2, C3, C4, C5, C6,
     :            LDIMS(X_AX,INP), %VAL(XAXPTR), %VAL(OAXPTR(X_AX)))

            END IF

            IF (Y_AX .GT. -1) THEN
              C1 = SIND(PA(1))
              C2 = - SIND(PA(INP))
              C3 = (AXIS_RA(INP) - AXIS_RA(1))
     :                      + (XMIN * SIND(PA(INP)))
              C4 = - COSD(PA(1))
              C5 = COSD(PA(INP))
              C6 = (AXIS_DEC(INP) - AXIS_DEC(1))
     :                      + (XMIN * COSD(PA(INP)))

              CALL BINMERGE_ALTER (C1, C2, C3, C4, C5, C6,
     :            LDIMS(Y_AX,INP), %VAL(YAXPTR), %VAL(OAXPTR(Y_AX)))

            END IF
          END IF

        ELSE IF (AXIS_DEC(1) .NE. AXIS_DEC(INP) .OR.
     :           AXIS_RA (1) .NE. AXIS_RA (INP)) THEN

          CALL ADI_CGET1D( PRJID(1), 'NPOINT', 2, SPOINT, SIZ, STATUS )
          F_RA1 = SPOINT(1)
          F_DEC1 = SPOINT(2)

          IF (F_DEC1 .NE. F_DEC .OR. F_RA1 .NE. F_RA) THEN
            CALL MSG_SETI('INP', INP)
            CALL MSG_SETD('F_RA1',  F_RA1)
            CALL MSG_SETD('F_RA',   F_RA)
            CALL MSG_SETD('F_DEC1', F_DEC1)
            CALL MSG_SETD('F_DEC',  F_DEC)
            CALL MSG_BLNK()
            CALL MSG_PRNT
     :             ('WARNING: Dataset 1 has RA = ^F_RA1, DEC = ^F_DEC1')
            CALL MSG_PRNT
     :            ('     but dataset ^INP has RA = ^F_RA, DEC = ^F_DEC')

          END IF
        END IF

        DO WHILE (AX .LT. NDIMS1)
          AX       = AX + 1
          CONTINUE = .TRUE.

          IF (AX .EQ. X_AX .OR. AX .EQ. Y_AX) THEN
            IF (DONE) CONTINUE = .FALSE.

          END IF

          IF (CONTINUE) THEN
            CALL BDI_AXMAPR( IFID(INP), AX, 'Data', 'READ',
     :                       IAXPTR, STATUS )

*          Check status
            IF (STATUS .NE. SAI__OK) GOTO 99

            IF (CHKUNITS(AX)) THEN
              IF (CHR_LEN(UNITS(AX,INP)) .GT. 0) THEN
                IF (UNITS(AX,1)(1:CHR_LEN(UNITS(AX,1))) .NE.
     :              UNITS(AX,INP)(1:CHR_LEN(UNITS(AX,INP)))) THEN
                  CALL MSG_SETC ('LAB', LABEL1(AX))
                  CALL MSG_SETI ('AX', AX)
                  CALL MSG_PRNT ('WARNING: Possible units mismatch in'
     :                                             //' axis ^AX (^LAB)')

                END IF
              ELSE
                CALL MSG_SETC ('LAB', LABEL1(AX))
                CALL MSG_SETI ('AX', AX)
                CALL MSG_PRNT ('WARNING: Possible units mismatch in'
     :                                             //' axis ^AX (^LAB)')

              END IF
            END IF

*        Adjust timing axis
            IF ( (AX .EQ. T_AX) .AND. (TOFFS(INP).NE.0D0) ) THEN

*          Assumes T_AX is in seconds.
              ADD = REAL(TOFFS(INP))
              CALL BINMERGE_AXCOPYA (%VAL(IAXPTR), ADD, AXOFF(AX),
     :                                 LDIMS(AX, INP), %VAL(OAXPTR(AX)))
            ELSE
              CALL BINMERGE_AXCOPYA(%VAL(IAXPTR), 0.0, AXOFF(AX),
     :                                 LDIMS(AX, INP), %VAL(OAXPTR(AX)))

            END IF
            CALL BDI_AXUNMAP( IFID(INP), AX, 'Data', IAXPTR, STATUS )

          END IF

*        Merge AXIS(n) WIDTH - delete if not present in all datasets
          IF (AXWOK) THEN
            CALL BDI_AXCHK( IFID(INP), AX, 'Width', OK, STATUS )
            IF (OK) THEN
              CALL BDI_AXMAPR( IFID(INP), AX, 'Width', 'READ',
     :                         INAXW, STATUS )

              IF (AX .EQ. X_AX) THEN
                CALL BINMERGE_SCALEXY (UNITS(AX,1), UNITS(AX,INP),
     :                      LDIMS(AX,INP), INAXW, .TRUE., INAXW, STATUS)

              ELSE IF (AX .EQ. Y_AX) THEN
                CALL BINMERGE_SCALEXY (UNITS(AX,1), UNITS(AX,INP),
     :                     LDIMS(AX,INP), INAXW, .FALSE., INAXW, STATUS)

              END IF
              CALL BINMERGE_AXCOPYA(%VAL(INAXW), 0.0, AXOFF(AX),
     :                       LDIMS(AX,INP), %VAL(OAXW(AX)))
              CALL BDI_AXUNMAP( IFID(INP), AX, 'Width', INAXW, STATUS )

            ELSE
              CALL MSG_SETI ('NSET', INP)
              CALL MSG_PRNT ('WARNING: AXIS WIDTH missing from '//
     :                   'dataset ^NSET  - erasing from output dataset')
              CALL BDI_DELETE( OFID, 'Axis('//LAST//'_Width', STATUS )
              AXWOK = .FALSE.

            END IF
          END IF
          AXOFF(AX) = AXOFF(AX) + LDIMS(AX, INP)

*        Check status
          IF (STATUS .NE. SAI__OK) GOTO 99

        END DO

*    Merge primary data
        CALL BDI_MAPR( IFID(INP), 'Data', 'READ', IDPTR, STATUS )
        CALL BINMERGE_COPY (%VAL(IDPTR), OFFSET, LDIMS(1,INP),
     :                   LDIMS(2,INP), LDIMS(3,INP), LDIMS(4,INP),
     :                   LDIMS(5,INP), LDIMS(6,INP), LDIMS(7,INP),
     :                   OLDIMS(1),OLDIMS(2),OLDIMS(3),OLDIMS(4),
     :                   OLDIMS(5),OLDIMS(6),OLDIMS(7), %VAL(ODPTR))
        CALL BDI_UNMAP( IFID(INP), 'Data', IDPTR, STATUS )

*    Merge QUALITY - if missing from odd data sets then assume it is good
        IF ( QUAL ) THEN
          CALL BDI_CHK( IFID(INP), 'Quality', OK, STATUS )
          IF ( OK ) THEN
            CALL BDI_MAPUB( IFID(INP), 'Quality', 'READ', IQPTR,
     :                      STATUS )

            CALL BINMERGE_QCOPY( %VAL(IQPTR), OFFSET, LDIMS(1,INP),
     :                   LDIMS(2,INP), LDIMS(3,INP), LDIMS(4,INP),
     :                   LDIMS(5,INP), LDIMS(6,INP), LDIMS(7,INP),
     :                   OLDIMS(1),OLDIMS(2),OLDIMS(3),OLDIMS(4),
     :                   OLDIMS(5),OLDIMS(6),OLDIMS(7), %VAL(OQPTR))

          ELSE
            CALL MSG_SETI ('NSET', INP)
            CALL MSG_PRNT ('WARNING: Quality data missing '//
     :                    'in dataset ^NSET - Assuming QUALITY is good')

          END IF
        ENDIF
        IF (STATUS .NE. SAI__OK) GOTO 99

*        Merge VARIANCE - delete if not present in all datasets
          IF (VAROK) THEN
            CALL BDI_CHK( IFID(INP), 'Variance', OK, STATUS )

            IF (OK) THEN
              CALL BDI_MAPR( IFID(INP), 'Variance', 'READ', IVPTR,
     :                         STATUS )
        CALL BINMERGE_COPY (%VAL(IVPTR), OFFSET, LDIMS(1,INP),
     :                   LDIMS(2,INP), LDIMS(3,INP), LDIMS(4,INP),
     :                   LDIMS(5,INP), LDIMS(6,INP), LDIMS(7,INP),
     :                   OLDIMS(1),OLDIMS(2),OLDIMS(3),OLDIMS(4),
     :                   OLDIMS(5),OLDIMS(6),OLDIMS(7), %VAL(OVPTR))

              CALL BDI_UNMAP( IFID(INP), 'Variance', IVPTR, STATUS )

            ELSE
              CALL MSG_SETI ('NSET', INP)
              CALL MSG_PRNT ('WARNING: Data errors missing from'
     :                //' dataset ^NSET  - erasing from output dataset')
              CALL BDI_UNMAP( IFID(INP), 'Variance', IVPTR, STATUS )
              CALL BDI_DELETE( OFID, 'Variance', STATUS )
              VAROK = .FALSE.

            END IF
          END IF

*        Check status
          IF (STATUS .NE. SAI__OK) GOTO 99

          DO AX = 1, NDIMS1
            OFFSET(AX) = OFFSET(AX) + LDIMS(AX, INP)
          END DO
C          CALL BDI_UNMAP( IFID(INP), STATUS )

C        ELSE
C          CALL MSG_SETI ('NSET', INP)
C          CALL MSG_PRNT ('WARNING: AXIS DATA missing from '//
C     :                   'dataset ^NSET  - Unable to use this dataset')

C        END IF
      END DO

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Set up history - add in names of objects merged
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Write input filenames to TEXT
      CALL USI_NAMES( 'I', IFILES, STATUS )
      CALL HSI_PTXTI( OFID, IFILES, .TRUE., STATUS )

*  Clean up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  BINMERGE_AXCOPYA- Copies Axis values with offset & slip
      SUBROUTINE BINMERGE_AXCOPYA (AXISIN, SLIP, START, NVALS, AXISOUT)
*    Description :
*     Copies NVALS values from AXISIN to AXISOUT, placing them at
*     START in AXISOUT. SLIP is added to the values, to alow for differences in datum values.
*    History :
*     26/9/88:  original (PLA_AST88%UK.AC.BHAM.SR.STAR)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      REAL                   AXISIN(*)                ! Axis values to be copied
      REAL                   SLIP                     ! Difference in reference values of the axes

      INTEGER                START                    ! Start position for the copy
      INTEGER                NVALS                    ! Number of values to copy
*    Import-Export :
      REAL                   AXISOUT(*)               ! Output axis values
*    Local variables :
      INTEGER                I                        ! Loop counter
*-
      DO I = 1, NVALS
        AXISOUT (I + START) = AXISIN (I) + SLIP

      END DO
      END




*+  BINMERGE_COPY - Copy a 7D input array into output array with offset
      SUBROUTINE BINMERGE_COPY (IN, START, IL1, IL2, IL3, IL4,
     :  IL5, IL6, IL7, OL1, OL2, OL3, OL4, OL5, OL6, OL7, OUT)
*    Description :
*     Copies REAL data from IN to OUT starting at START
*    History :
*     27/9/88:  original (PLA_AST88@UK.AC.BHAM.SR.STAR)
*    Type Definitions :
      IMPLICIT NONE
*
*    Import :
*
      INTEGER                START(*)                 ! Start posn in OUT
      INTEGER                IL1,IL2,IL3,IL4,IL5,IL6,IL7
      INTEGER                OL1,OL2,OL3,OL4,OL5,OL6,OL7
      REAL                   IN(IL1,IL2,IL3,IL4,IL5,IL6,IL7)! Input array
*
*    Import-Export :
*
      REAL                   OUT(OL1,OL2,OL3,OL4,OL5,OL6,OL7)! Output array
*
*    Local variables :
*
      INTEGER                I, J, K, L, M, N, O      ! Loop counters
*-

      DO O = 1, IL7
        DO N = 1, IL6
          DO M = 1, IL5
            DO L = 1, IL4
              DO K = 1, IL3
                DO J = 1, IL2
                  DO I = 1, IL1
                    OUT (I+START(1), J+START(2), K+START(3), L+START(4),
     :                   M+START(5), N+START(6), O+START(7))
     :            = IN(I,J,K,L,M,N,O)

                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO
      END




*-  BINMERGE_QCOPY - Copy a 7D input array into output array with offset
      SUBROUTINE BINMERGE_QCOPY (IN, START, IL1, IL2,IL3,IL4,IL5,
     :         IL6,IL7, OL1,OL2,OL3,OL4,OL5,OL6,OL7, OUT)
*    Description :
*     Copies BYTE data from IN to OUT starting at START
*    History :
*     27/9/88:  original (PLA_AST88@UK.AC.BHAM.SR.STAR)
*    Type Definitions :
      IMPLICIT NONE
*
*    Import :
*
      INTEGER                START(*)                 ! Start posn in OUT
      INTEGER                IL1,IL2,IL3,IL4,IL5,IL6,IL7
      INTEGER                OL1,OL2,OL3,OL4,OL5,OL6,OL7
      BYTE                   IN(IL1,IL2,IL3,IL4,IL5,IL6,IL7)! Input array
*
*    Import-Export :
*
      BYTE                   OUT(OL1,OL2,OL3,OL4,OL5,OL6,OL7)! Output array
*    Local variables :
      INTEGER                I, J, K, L, M, N, O      ! Loop counters
*-

      DO O = 1, IL7
        DO N = 1, IL6
          DO M = 1, IL5
            DO L = 1, IL4
              DO K = 1, IL3
                DO J = 1, IL2
                  DO I = 1, IL1
                    OUT (I+START(1), J+START(2), K+START(3), L+START(4),
     :                   M+START(5), N+START(6), O+START(7))
     :            = IN(I,J,K,L,M,N,O)

                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO
      END



*+  BINMERGE_GET_MIN - Find min of axis AX, also adjust for units if necessary.
      SUBROUTINE BINMERGE_GET_MIN( FID, OUTUNITS, UNITS, LEN, AX, XAX,
     :                                                 MIN, PTR, STATUS)
*    Description :
*     Find the minimum value of the axis AX. Also, if neccesary correct
*     for different units.
*    Method :
*
*    Deficiencies :
*
*    Bugs :
*
*    Authors :
*     Phil Andrews
*    History :
*     16-DEC-1988 :  Original  (BHVAD::PLA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER			FID			! Dataset identifier
      CHARACTER*(*)          OUTUNITS          ! Output units
      CHARACTER*(*)          UNITS             ! Units of axis

      INTEGER                LEN               ! Length of axis
      INTEGER                AX                ! Axis identifier

      LOGICAL                XAX               ! Is this the X axis?

*    Export :
      REAL                   MIN               ! Min value of axis

      INTEGER                PTR               ! Pointer to mapped axis
                                               ! values

*    Status :
      INTEGER                STATUS
*    Function declarations :
      INTEGER                CHR_LEN

*    Local variables :
      REAL			MAX			! Not used

      INTEGER                	TPTR             ! Temporary pointer
      INTEGER                	OUTLEN           ! Legth of output units string
      INTEGER                	INLEN            ! Length of input units string

      LOGICAL                CONTINUE
*-

*  Check status
      IF (STATUS .NE. SAI__OK) RETURN

*  Map the axis data
      CALL BDI_AXMAPR( FID, AX, 'Data', 'READ', TPTR, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

      CONTINUE = .TRUE.
      OUTLEN   = CHR_LEN(OUTUNITS)
      INLEN    = CHR_LEN(UNITS)

      IF (OUTLEN .GT. 0 .AND. INLEN .GT. 0) THEN
        IF (OUTUNITS(1:OUTLEN) .EQ. UNITS(1:INLEN)) THEN
          PTR = TPTR

        ELSE
          CALL BINMERGE_SCALEXY( OUTUNITS, UNITS, LEN, TPTR, XAX, PTR,
     :                                                          STATUS)

        END IF
      ELSE IF (XAX) THEN
        CALL MSG_PRNT ('WARNING: possible X axis units mismatch')

      ELSE
        CALL MSG_PRNT ('WARNING: possible Y axis units mismatch')

      END IF
      CALL ARR_RANG1R( LEN, %VAL(PTR), MIN, MAX, STATUS )

*    Exit
 99   IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'BINMERGE_GET_MIN', STATUS )
      END IF

      END


*+  BINMERGE_SCALEXY - Rescales x or y axis accordig to units.
      SUBROUTINE BINMERGE_SCALEXY (OUTUNITS, UNITS, LEN, INPTR, XAX,
     :                                                   OUTPTR, STATUS)
*    Description :
*     Produces a temporary array, pointed to by PTR, containing
*     the rescaled axis values.
*    Method :
*
*    Deficiencies :
*
*    Bugs :
*
*    Authors :
*     Phil Andrews
*    History :
*     16-DEC-1988 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*)		OUTUNITS          	! Output units
      CHARACTER*(*)         	UNITS             	! Units of axis

      INTEGER                LEN               ! Length of axis
      INTEGER                INPTR             ! pointer to mapped values

      LOGICAL                XAX               ! Is this the X axis?

*    Export :
      INTEGER                OUTPTR            ! Pointer to scaled
                                               ! values

*    Status :
      INTEGER                STATUS

*    Local variables :
      REAL                   OUTVAL           ! output units value
      REAL                   INVAL            ! input units value

      LOGICAL                CONTINUE

*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

      IF (OUTUNITS(1:6) .EQ. 'ARCSEC') THEN
        OUTVAL = 1.0

      ELSE IF (OUTUNITS(1:6) .EQ. 'ARCMIN') THEN
        OUTVAL = 60.0

      ELSE IF (OUTUNITS(1:6) .EQ. 'DEGREE') THEN
        OUTVAL = 3600.0

      ELSE IF (XAX) THEN
        CALL MSG_PRNT ('WARNING: Possible X axis units mismatch')
        CONTINUE = .FALSE.

      ELSE
        CALL MSG_PRNT ('WARNING: Possible Y axis units mismatch')
        CONTINUE = .FALSE.

      END IF

      IF (CONTINUE) THEN
        IF (UNITS(1:6) .EQ. 'ARCSEC') THEN
          INVAL = 1.0

        ELSE IF (UNITS(1:6) .EQ. 'ARCMIN') THEN
          INVAL = 60.0

        ELSE IF (UNITS(1:6) .EQ. 'DEGREE') THEN
          INVAL = 3600.0

        ELSE IF (XAX) THEN
          CALL MSG_PRNT ('WARNING: Possible X axis units mismatch')
          CONTINUE = .FALSE.

        ELSE
          CALL MSG_PRNT ('WARNING: Possible Y axis units mismatch')
          CONTINUE = .FALSE.

        END IF

        IF (CONTINUE) THEN
          CALL DYN_MAPR (1, LEN, OUTPTR, STATUS)

          CALL ARR_MULT1R( LEN, %VAL(INPTR), INVAL/OUTVAL,
     :                     %VAL(OUTPTR), STATUS )

        END IF
      END IF

*    Exit
 999  IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'BINMERGE_SCALEXY', STATUS )
      END IF

      END


*+  BINMERGE_ALTER - Transform input axis values to output axis values
      SUBROUTINE BINMERGE_ALTER (C1, C2, C3, C4, C5, C6, LEN, IN, OUT)
*    Description :
*     A transformation is performed of the IN axis AXIS_RA, AXIS_DEC, &
*     POSITION_ANGLE to those of the output dataset.
*    Authors :
*     Phil Andrews
*    History :
*     16-DEC-1988 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      DOUBLE PRECISION     C1,C2,C3,C4,C5,C6  ! constants of the
                                              ! transformation

      INTEGER              LEN                ! Length of axis

      REAL                 IN(*)              ! Input array

*    Export :
      REAL                 OUT(*)             ! Output array

*    Local variables :
      INTEGER              I                  ! Loop counter

*-

      DO I = 1, LEN
        OUT(I) = (C1 * ((C2 * DBLE(IN(I))) + C3)) +
     :           (C4 * ((C5 * DBLE(IN(I))) + C6))

      END DO
      END
