*+  BINMERGE - Merge 2 or more binned datasets
      SUBROUTINE BINMERGE(STATUS)
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
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'
*    Structure definitions:
      STRUCTURE/axesdef/
         INTEGER             T                 ! Index to Time axis
         INTEGER             X                 ! Index to X axis
         INTEGER             Y                 ! Index to Y axis
      END STRUCTURE

      RECORD/axesdef/        AXES              ! Indicies to X, Y, & T axes
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN
*    Global variables :
      INTEGER                MXINP             ! maximum number of input datasets
         PARAMETER          (MXINP = 10)
*
*    Local variables :
*
      CHARACTER              C                 ! character value
      CHARACTER*(DAT__SZLOC) HEADER            ! ASTERIX HEADER locator
      CHARACTER*(DAT__SZLOC) ILOC(MXINP)       ! input dataset locators
      CHARACTER*(DAT__SZLOC) MORE              ! MORE box locator
      CHARACTER*(DAT__SZLOC) OLOC              ! output dataset locator
      CHARACTER*(DAT__SZNAM) PARNAM            ! parameter name
      CHARACTER*(DAT__SZTYP) TYPE1             ! data type, 1st input dataset
      CHARACTER*(DAT__SZTYP) TYPEN             ! data type
      CHARACTER*1            LAST              ! number of axis to be merged
      CHARACTER*80           TEXT(4*MXINP+2)   ! History text
      CHARACTER*80           LABEL             ! An axis label
      CHARACTER*80           LABEL1(7)         ! Axis labels for first dataset
      CHARACTER*80           UNITS(7,MXINP)    ! Axis units
      CHARACTER*80           TESTUN1,TESTUN    ! Dummy tests for unit values

      DOUBLE PRECISION       AXIS_DEC(MXINP)   ! AXIS_DEC of datasets
      DOUBLE PRECISION       AXIS_RA(MXINP)    ! AXIS_RA of datasets
      DOUBLE PRECISION       BASE_TAI(MXINP)   ! BASE_TAI of datasets
      DOUBLE PRECISION       C1,C2,C3,C4,C5,C6 ! Used in transforming between axes
      DOUBLE PRECISION       F_DEC             ! Field_Dec of current dataset
      DOUBLE PRECISION       F_DEC1            ! Field_Dec of first dataset
      DOUBLE PRECISION       F_RA              ! Field_RA of current dataset
      DOUBLE PRECISION       F_RA1             ! Field_RA of first dataset
      DOUBLE PRECISION       POSITION_ANGLE(MXINP) ! POSITION_ANGLE of datasets

      INTEGER                AXOFF(7)          ! axis(N) offset
      INTEGER                DSETS             ! number of input datasets
      INTEGER                EQUINOX1          ! EQUINOX of 1st dataset
      INTEGER                EQUINOX           ! EQUINOX of dataset
      INTEGER                IAXPTR            ! pointer to input AXISn_DATA
      INTEGER                IDPTR             ! pointer to input DATA_ARRAY
      INTEGER                ILDIMS(DAT__MXDIM)! dimensions of input  dataset component.
      INTEGER                INAXW             ! pointer to input AXIS(n) WIDTH
      INTEGER                INP, AX ,I        ! loop counters
      INTEGER                IQPTR             ! pointer to input QUALITY
      INTEGER                LDIMS(7, MXINP)   ! dimensions of datasets' DATA_ARRAYs
      INTEGER                LENGTH            ! Of character strings
      INTEGER                NDIMS             ! # of dimensions of a dataset DATA_ARRAY
      INTEGER                NDIMS1            ! # of dimensions of dataset 1 DATA_ARRAY
      INTEGER                NLINES            ! Number of history TEXT lines
      INTEGER                NPTS              ! Number of points in output dataset
      INTEGER                OAXPTR(7)         ! pointer to output AXISn_DATA
      INTEGER                OAXW(7)           ! pointer to output AXIS(n) WIDTH
      INTEGER                ODPTR             ! pointer to output DATA_ARRAY
      INTEGER                OFFSET(7)         ! output data array offset
      INTEGER                OLDIMS(DAT__MXDIM)! dimensions of output dataset.
      INTEGER                OQPTR             ! pointer to output QUALITY
      INTEGER                SIZ               ! Dummy variable
      INTEGER                IVPTR             ! pointer to input VARIANCE
      INTEGER                OVPTR            ! pointer to output VARIANCE
      INTEGER                YAXPTR            ! pointer to mapped y axis
      INTEGER                XAXPTR            ! pointer to mapped x axis

      LOGICAL                AXISOK            ! axis data present
      LOGICAL                AXWOK             ! axis width present
      LOGICAL                CHKLABEL(DAT__MXDIM) ! Check axis label?
      LOGICAL                CHKUNITS(DAT__MXDIM) ! Check axis units?
      LOGICAL                CONTINUE          ! controls copying of axis info
      LOGICAL                DONE              ! Coppied the X &/or Y axes
      LOGICAL                INPRIM            ! Is input primitive?
      LOGICAL                INPUT             ! Loops over input
      LOGICAL                OK                ! data item acceptable
      LOGICAL                QUAL              ! data quality present
      LOGICAL                REG               ! Regular axis data
      LOGICAL                VAROK             ! data errors present

      REAL                   ADD               ! Add to timetag of current dataset
      REAL                   YMIN              ! Minimium Y axis value
      REAL                   XMIN              ! Minimium X axis value
*
*    Version id :
*
      CHARACTER*24           VERSION
         PARAMETER          (VERSION = ' BINMERGE version 1.8-0')
*-

*    Version announcement
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT
      CALL ARR_INIT1I( 1, DAT__MXDIM*MXINP, LDIMS, STATUS )

      DO I = 1, DAT__MXDIM
        AXOFF(I)  = 0
        OFFSET(I) = 0
        OLDIMS(I) = 1
      END DO

*    Get first input file
      CALL USI_ASSOCI ('INP1', 'READ', ILOC(1), INPRIM, STATUS)
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      CALL BDA_CHKDATA (ILOC(1), OK, NDIMS1, LDIMS(1,1), STATUS)

      IF (OK) THEN
        CALL CHR_ITOC (NDIMS1, LAST, LENGTH)
        CALL DAT_TYPE (ILOC(1), TYPE1, STATUS)

        DO AX = 1, NDIMS1
          CALL BDA_CHKAXVAL   (ILOC(1), AX, AXISOK, REG, SIZ, STATUS)
          CALL BDA_GETAXLABEL (ILOC(1), AX, LABEL1(AX),       STATUS)
          CALL CHR_UCASE( LABEL1(AX) )

          IF (CHR_LEN(LABEL1(AX)) .EQ. 0) THEN
            CHKLABEL(AX) = .FALSE.
            CALL MSG_SETI ('AX', AX)
            CALL MSG_PRNT ('WARNING: Axis ^AX has no LABEL - '//
     :                             'Unable to check for consistency')

          ELSE
            CHKLABEL(AX) = .TRUE.

          END IF
          CALL BDA_GETAXUNITS (ILOC(1), AX, UNITS(AX,1),      STATUS)
          CALL CHR_UCASE( UNITS(AX,1) )

          IF (CHR_LEN(UNITS(AX,1)) .EQ. 0) THEN
            CHKUNITS(AX) = .FALSE.
            CALL MSG_SETI ('AX', AX)
            CALL MSG_PRNT ('WARNING: Axis ^AX has no UNITS')

          ELSE
            CHKUNITS(AX) = .TRUE.

          END IF
        END DO

        IF (AXISOK) THEN
          CALL AXIS_FINDXYT (ILOC(1), NDIMS1, AXES.X, AXES.Y, AXES.T,
     :                                                           STATUS)

        ELSE
          CALL MSG_PRNT ('FATAL ERROR: Invalid dataset - Bad axis data')
          STATUS = SAI__ERROR

        END IF

        CALL BDA_LOCHEAD (ILOC(1), HEADER, STATUS)
        CALL CMP_GET0I (HEADER, 'EQUINOX',        EQUINOX1,      STATUS)
        CALL CMP_GET0D (HEADER, 'POSITION_ANGLE', POSITION_ANGLE(1),
     :                                                           STATUS)
        POSITION_ANGLE(1) = MOD (POSITION_ANGLE(1), 360.0D0)
        CALL CMP_GET0D (HEADER, 'AXIS_RA',        AXIS_RA(1),    STATUS)
        CALL CMP_GET0D (HEADER, 'AXIS_DEC',       AXIS_DEC(1),   STATUS)
        CALL CMP_GET0D (HEADER, 'BASE_TAI',       BASE_TAI(1),   STATUS)
        CALL DAT_ANNUL (HEADER,                                  STATUS)

      ELSE
        CALL MSG_PRNT ('FATAL ERROR: Invalid dataset - Bad data')
        STATUS = SAI__ERROR

      END IF

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Obtain list of input datasets
      DSETS  = 2
      INPUT  = .TRUE.

      DO WHILE (INPUT)
*      Deduce parameter name and associate with dataset.
        CALL CHR_ITOC (DSETS, C, LENGTH)
        PARNAM = 'INP'//C(1:LENGTH)
        CALL USI_ASSOCI (PARNAM, 'READ', ILOC(DSETS), INPRIM, STATUS)

        IF (STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT) THEN
*	 End of input
          CALL ERR_ANNUL (STATUS)
          INPUT  = .FALSE.

        ELSE
          CALL BDA_CHKDATA (ILOC(DSETS), OK, NDIMS, LDIMS(1,DSETS),
     :                                                           STATUS)

          IF (OK .AND. NDIMS .EQ. NDIMS1) THEN
            CALL DAT_TYPE (ILOC(DSETS), TYPEN, STATUS)

            IF (TYPEN .NE. TYPE1) THEN
              CALL MSG_PRNT ('WARNING: Dataset type mismatch')

            END IF
            AX = 0

            DO WHILE (OK .AND. AX .LT. NDIMS1)
              AX = AX + 1

              IF (CHKUNITS(AX)) THEN
                CALL BDA_GETAXUNITS (ILOC(DSETS), AX, UNITS(AX,DSETS),
     :                                                           STATUS)
                CALL CHR_UCASE( UNITS(AX,DSETS) )
                UNITS(AX,DSETS)=TESTUN
                UNITS(AX,1)=TESTUN1
                IF (TESTUN(1:CHR_LEN(TESTUN)) .NE.
     :                           TESTUN1(1:CHR_LEN(TESTUN1))) THEN
                  CALL MSG_PRNT ('WARNING: Axis units mismatch - '//
     :                                         'OUTPUT MAY BE GARBAGE!')

                  CALL USI_DEF0L ('CONT', .TRUE.)
                  CALL USI_GET0L ('CONT', OK, STATUS)
                  CALL USI_CANCL ('CONT',     STATUS)

                END IF

              END IF

              IF (CHKLABEL(AX)) THEN
                CALL BDA_GETAXLABEL (ILOC(DSETS), AX, LABEL, STATUS)
                CALL CHR_UCASE( LABEL )

                IF (LABEL(1:CHR_LEN(LABEL)) .NE.
     :                           LABEL1(AX)(1:CHR_LEN(LABEL1(AX)))) THEN
                  CALL MSG_PRNT ('WARNING: Axis label mismatch - '//
     :                                         'OUTPUT MAY BE GARBAGE!')

                  CALL USI_DEF0L ('CONT', .TRUE.)
                  CALL USI_GET0L ('CONT', OK, STATUS)
                  CALL USI_CANCL ('CONT',     STATUS)

                END IF
              END IF
            END DO

            CALL BDA_LOCHEAD (ILOC(DSETS), HEADER, STATUS)
            CALL CMP_GET0I (HEADER, 'EQUINOX', EQUINOX, STATUS)

            IF (EQUINOX .EQ. EQUINOX1) THEN
              CALL CMP_GET0D (HEADER, 'POSITION_ANGLE',
     :                                    POSITION_ANGLE(DSETS), STATUS)
              POSITION_ANGLE(DSETS) =
     :                              MOD (POSITION_ANGLE(DSETS), 360.0D0)

              CALL CMP_GET0D (HEADER, 'AXIS_RA',
     :                                    AXIS_RA(DSETS),        STATUS)
              CALL CMP_GET0D (HEADER, 'AXIS_DEC',
     :                                    AXIS_DEC(DSETS),       STATUS)
              CALL CMP_GET0D (HEADER, 'BASE_TAI',
     :                                    BASE_TAI(DSETS),       STATUS)
              CALL DAT_ANNUL (HEADER,                            STATUS)

            ELSE
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

          IF (INPUT .AND. (STATUS .NE. SAI__OK .OR. .NOT. OK)) THEN
            STATUS = SAI__OK
            CALL MSG_PRNT ('Invalid input - try again.')
            CALL USI_CANCL (PARNAM, STATUS)

          END IF
        END IF
      END DO

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    End of input
      DSETS = DSETS - 1


      IF (DSETS .LT. 1) THEN
        CALL MSG_PRNT ('FATAL ERROR: No valid inputs!')
        STATUS = SAI__ERROR
        GOTO 99

      END IF

*    Create output dataset
      CALL USI_ASSOCO ('OUT', TYPE1, OLOC, STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Obtain output array size
      DO INP = 1, DSETS
        DO AX = 1, NDIMS1
          IF (INP .NE. 1) THEN
            OLDIMS(AX) = OLDIMS(AX) + LDIMS(AX,INP)

          ELSE
            OLDIMS(AX) = LDIMS(AX,INP)

          END IF
        END DO
      END DO

*    Find length of output dataset
      NPTS = 1

      DO AX = 1, NDIMS1
        NPTS = NPTS * OLDIMS(AX)

      END DO

*    Copy across subsidiary data from first input dataset
      CALL BDA_GETTITLE (ILOC(1), LABEL, STATUS)

      IF (CHR_LEN(LABEL) .GT. 0) THEN
        CALL BDA_PUTTITLE (OLOC, LABEL, STATUS)

      END IF

      CALL BDA_GETLABEL (ILOC(1), LABEL, STATUS)

      IF (CHR_LEN(LABEL) .GT. 0) THEN
        CALL BDA_PUTLABEL (OLOC, LABEL, STATUS)

      END IF

      CALL BDA_GETUNITS (ILOC(1), LABEL, STATUS)

      IF (CHR_LEN(LABEL) .GT. 0) THEN
        CALL BDA_PUTUNITS (OLOC, LABEL, STATUS)

      END IF

      CALL DAT_FIND (ILOC(1), 'MORE', MORE, STATUS)
      CALL DAT_COPY (MORE, OLOC, 'MORE',    STATUS)

      CALL HIST_COPY (ILOC(1), OLOC, STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Create output data
      CALL BDA_CREDATA (OLOC, NDIMS1, OLDIMS, STATUS)
      CALL BDA_MAPDATA (OLOC, 'WRITE', ODPTR, STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

      CALL ARR_INIT1R( 0.0, NPTS, %VAL(ODPTR), STATUS )

*    Create output axes
      CALL BDA_CREAXES (OLOC, NDIMS1, STATUS)

*    Check AXIS WIDTH
      CALL BDA_CHKAXWID (ILOC(1), NDIMS1, AXWOK, REG, SIZ, STATUS)

      DO AX = 1, NDIMS1
        CALL BDA_CREAXVAL (OLOC, AX, .FALSE., OLDIMS(AX), STATUS)
        CALL BDA_MAPAXVAL (OLOC, 'WRITE', AX, OAXPTR(AX), STATUS)

        CALL BDA_PUTAXUNITS (OLOC, AX, UNITS(AX,1),       STATUS)

        IF (CHKLABEL(AX)) THEN
          CALL BDA_PUTAXLABEL (OLOC, AX, LABEL1(AX), STATUS)

        END IF

        IF (AXWOK) THEN
          CALL BDA_CREAXWID (OLOC, AX, .FALSE., OLDIMS(AX),  STATUS)
          CALL BDA_MAPAXWID (OLOC, 'WRITE', AX, OAXW(AX),    STATUS)

        END IF
      END DO

*    Check QUALITY
      CALL BDA_CHKQUAL (ILOC(1), QUAL, NDIMS, ILDIMS, STATUS)

      IF ( QUAL ) THEN
        CALL BDA_CREQUAL( OLOC, NDIMS1,  OLDIMS, STATUS )
        CALL BDA_MAPQUAL( OLOC, 'WRITE', OQPTR, STATUS )

*      Check status
        IF (STATUS .NE. SAI__OK) GOTO 99

        CALL ARR_INIT1B( QUAL__GOOD, NPTS, %VAL(OQPTR), STATUS )

      END IF

*    Check VARIANCE
      CALL BDA_CHKVAR (ILOC(1), VAROK, NDIMS, ILDIMS, STATUS)

      IF (VAROK) THEN
        CALL BDA_CREVAR (OLOC, NDIMS1,  OLDIMS, STATUS)
        CALL BDA_MAPVAR (OLOC, 'WRITE', OVPTR, STATUS)

*      Check status
        IF (STATUS .NE. SAI__OK) GOTO 99

        CALL ARR_INIT1R( 0.0, NPTS, %VAL(OVPTR), STATUS )

      END IF


*    Loop through input objects merging arrays
      DO INP = 1, DSETS
        CALL MSG_SETI ('INP', INP)
        CALL MSG_PRNT (' Writing input dataset ^INP to output file')

        AX = 0

        IF (AXES.X .GT. -1 .OR. AXES.Y .GT. -1) THEN
          IF (POSITION_ANGLE(1) .EQ. POSITION_ANGLE(INP) .AND.
     :        AXIS_DEC(1)       .EQ. AXIS_DEC(INP)       .AND.
     :        AXIS_RA (1)       .EQ. AXIS_RA (INP)) THEN
            DONE = .FALSE.

          ELSE
            DONE = .TRUE.

            IF (AXES.X .GT. -1) THEN
              CALL BINMERGE_GET_MIN (ILOC(INP), UNITS(AXES.X,1),
     :             UNITS(AXES.X,INP), LDIMS(AXES.X,INP), AXES.X, XMIN,
     :                                                   XAXPTR, STATUS)

            ELSE
              XMIN = AXIS_RA(INP)

            END IF

            IF (AXES.Y .GT. -1) THEN
              CALL BINMERGE_GET_MIN (ILOC(INP), UNITS(AXES.Y,1),
     :             UNITS(AXES.Y,INP), LDIMS(AXES.Y,INP), AXES.Y, YMIN,
     :                                                   YAXPTR, STATUS)

            ELSE
              YMIN = AXIS_DEC(INP)

            END IF

            IF (AXES.X .GT. -1) THEN
              C1 = COSD(POSITION_ANGLE(1))
              C2 = COSD(POSITION_ANGLE(INP))
              C3 = (AXIS_RA(INP) - AXIS_RA(1))
     :                      - (YMIN * SIND(POSITION_ANGLE(INP)))
              C4 = SIND(POSITION_ANGLE(1))
              C5 = SIND(POSITION_ANGLE(INP))
              C6 = (AXIS_DEC(INP) - AXIS_DEC(1))
     :                      + (YMIN * COSD(POSITION_ANGLE(INP)))

              CALL BINMERGE_ALTER (C1, C2, C3, C4, C5, C6,
     :            LDIMS(AXES.X,INP), %VAL(XAXPTR), %VAL(OAXPTR(AXES.X)))

            END IF

            IF (AXES.Y .GT. -1) THEN
              C1 = SIND(POSITION_ANGLE(1))
              C2 = - SIND(POSITION_ANGLE(INP))
              C3 = (AXIS_RA(INP) - AXIS_RA(1))
     :                      + (XMIN * SIND(POSITION_ANGLE(INP)))
              C4 = - COSD(POSITION_ANGLE(1))
              C5 = COSD(POSITION_ANGLE(INP))
              C6 = (AXIS_DEC(INP) - AXIS_DEC(1))
     :                      + (XMIN * COSD(POSITION_ANGLE(INP)))

              CALL BINMERGE_ALTER (C1, C2, C3, C4, C5, C6,
     :            LDIMS(AXES.Y,INP), %VAL(YAXPTR), %VAL(OAXPTR(AXES.Y)))

            END IF
          END IF
        ELSE IF (AXIS_DEC(1) .NE. AXIS_DEC(INP) .OR.
     :           AXIS_RA (1) .NE. AXIS_RA (INP)) THEN
          CALL BDA_LOCHEAD (ILOC(1), HEADER, STATUS)
          CALL CMP_GET0D (HEADER, 'FIELD_DEC', F_DEC1, STATUS)
          CALL CMP_GET0D (HEADER, 'FIELD_RA',  F_RA1,  STATUS)

          CALL BDA_LOCHEAD (ILOC(AXES.X), HEADER, STATUS)
          CALL CMP_GET0D (HEADER, 'FIELD_DEC', F_DEC1, STATUS)
          CALL CMP_GET0D (HEADER, 'FIELD_RA',  F_RA1,  STATUS)

          IF (F_DEC1 .NE. F_DEC .OR. F_RA1 .NE. F_RA) THEN
            CALL MSG_SETI ('INP', INP)
            CALL MSG_SETD ('F_RA1',  F_RA1)
            CALL MSG_SETD ('F_RA',   F_RA)
            CALL MSG_SETD ('F_DEC1', F_DEC1)
            CALL MSG_SETD ('F_DEC',  F_DEC)
            CALL MSG_PRNT (' ')
            CALL MSG_PRNT
     :             ('WARNING: Dataset 1 has RA = ^F_RA1, DEC = ^F_DEC1')
            CALL MSG_PRNT
     :            ('     but dataset ^INP has RA = ^F_RA, DEC = ^F_DEC')

          END IF
        END IF

        DO WHILE (AX .LT. NDIMS1)
          AX       = AX + 1
          CONTINUE = .TRUE.

          IF (AX .EQ. AXES.X .OR. AX .EQ. AXES.Y) THEN
            IF (DONE) CONTINUE = .FALSE.

          END IF

          IF (CONTINUE) THEN
            CALL BDA_MAPAXVAL (ILOC(INP), 'READ', AX, IAXPTR, STATUS)

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

*          Allow for possible diffence in BASE_TAI.
            IF (AX .EQ. AXES.T .AND. BASE_TAI(1)
     :                                          .NE. BASE_TAI(INP)) THEN
              ADD = REAL((BASE_TAI(1) - BASE_TAI(INP)) * 86400.0D0) !Assumes AXES.T is in seconds.
              CALL MSG_PRNT('Assuming BASE_TAI in seconds')
              CALL BINMERGE_AXCOPYA (%VAL(IAXPTR), ADD, AXOFF(AX),
     :                                 LDIMS(AX, INP), %VAL(OAXPTR(AX)))
            ELSE
              CALL BINMERGE_AXCOPY (%VAL(IAXPTR), AXOFF(AX),
     :                                 LDIMS(AX, INP), %VAL(OAXPTR(AX)))

            END IF
            CALL BDA_UNMAPAXVAL (ILOC(INP), AX, STATUS)

          END IF

*        Merge AXIS(n) WIDTH - delete if not present in all datasets
          IF (AXWOK) THEN
            CALL BDA_CHKAXWID (ILOC(INP), AX, OK, REG, LDIMS(AX,INP),
     :                                                           STATUS)

            IF (OK) THEN
              CALL BDA_MAPAXWID (ILOC(INP), 'READ', AX, INAXW, STATUS)

              IF (AX .EQ. AXES.X) THEN
                CALL BINMERGE_SCALEXY (UNITS(AX,1), UNITS(AX,INP),
     :                      LDIMS(AX,INP), INAXW, .TRUE., INAXW, STATUS)

              ELSE IF (AX .EQ. AXES.Y) THEN
                CALL BINMERGE_SCALEXY (UNITS(AX,1), UNITS(AX,INP),
     :                     LDIMS(AX,INP), INAXW, .FALSE., INAXW, STATUS)

              END IF
              CALL BINMERGE_COPYWD (%VAL(INAXW), AXOFF(AX),
     :                                    LDIMS(AX,INP), %VAL(OAXW(AX)))
              CALL BDA_UNMAPAXWID (ILOC(INP), AX, STATUS)

            ELSE
              CALL MSG_SETI ('NSET', INP)
              CALL MSG_PRNT ('WARNING: AXIS WIDTH missing from '//
     :                   'dataset ^NSET  - erasing from output dataset')
              CALL DAT_ERASE (OLOC, 'AXIS('//LAST//').WIDTH', STATUS)
              AXWOK = .FALSE.

            END IF
          END IF
          AXOFF(AX) = AXOFF(AX) + LDIMS(AX, INP)

*        Check status
          IF (STATUS .NE. SAI__OK) GOTO 99

        END DO

*      Merge DATA_ARRAY
        CALL BDA_MAPDATA   (ILOC(INP), 'READ', IDPTR, STATUS)
        CALL BINMERGE_COPY (%VAL(IDPTR), OFFSET, LDIMS(1,INP),
     :                   LDIMS(2,INP), LDIMS(3,INP), LDIMS(4,INP),
     :                   LDIMS(5,INP), LDIMS(6,INP), LDIMS(7,INP),
     :                   OLDIMS(1),OLDIMS(2),OLDIMS(3),OLDIMS(4),
     :                   OLDIMS(5),OLDIMS(6),OLDIMS(7), %VAL(ODPTR))

        CALL BDA_UNMAPDATA (ILOC(INP), STATUS)

*      Merge QUALITY - if missing from odd data sets then assume it is good
        IF (QUAL) THEN
          CALL BDA_CHKQUAL (ILOC(INP), OK, NDIMS, ILDIMS, STATUS)

          IF (OK) THEN
            CALL BDA_MAPQUAL (ILOC(INP), 'READ', IQPTR, STATUS)
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
*        Check status
          IF (STATUS .NE. SAI__OK) GOTO 99

*        Merge VARIANCE - delete if not present in all datasets
          IF (VAROK) THEN
            CALL BDA_CHKVAR (ILOC(INP), OK, NDIMS, ILDIMS, STATUS)

            IF (OK) THEN
              CALL BDA_MAPVAR (ILOC(INP), 'READ', IVPTR, STATUS)
        CALL BINMERGE_COPY (%VAL(IVPTR), OFFSET, LDIMS(1,INP),
     :                   LDIMS(2,INP), LDIMS(3,INP), LDIMS(4,INP),
     :                   LDIMS(5,INP), LDIMS(6,INP), LDIMS(7,INP),
     :                   OLDIMS(1),OLDIMS(2),OLDIMS(3),OLDIMS(4),
     :                   OLDIMS(5),OLDIMS(6),OLDIMS(7), %VAL(OVPTR))

              CALL BDA_UNMAPVAR (ILOC(INP), STATUS)

            ELSE
              CALL MSG_SETI ('NSET', INP)
              CALL MSG_PRNT ('WARNING: Data errors missing from'
     :                //' dataset ^NSET  - erasing from output dataset')
              CALL BDA_UNMAPVAR( OLOC, STATUS )
              CALL DAT_ERASE( OLOC, 'VARIANCE', STATUS )
              VAROK = .FALSE.

            END IF
          END IF

*        Check status
          IF (STATUS .NE. SAI__OK) GOTO 99

          DO AX = 1, NDIMS1
            OFFSET(AX) = OFFSET(AX) + LDIMS(AX, INP)

          END DO
          CALL BDA_UNMAP (ILOC(INP), STATUS)

C        ELSE
C          CALL MSG_SETI ('NSET', INP)
C          CALL MSG_PRNT ('WARNING: AXIS DATA missing from '//
C     :                   'dataset ^NSET  - Unable to use this dataset')

C        END IF
      END DO

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Set up history - add in names of objects merged
      CALL HIST_ADD( OLOC, VERSION, STATUS )

*    Write input filenames to TEXT
      TEXT (1) = '  Previous HISTORY copied from input file 1'
      TEXT (2) = ' '
      CALL USI_NAMEI (NLINES, TEXT(3),      STATUS)
      CALL HIST_PTXT (OLOC, NLINES+2, TEXT, STATUS)

*    Clean up
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



*+  BINMERGE_AXCOPY - Copies Axis values with offset
      SUBROUTINE BINMERGE_AXCOPY (AXISIN, START, NVALS, AXISOUT)
*    Description :
*     Copies NVALS values from AXISIN to AXISOUT, placing them at
*     START in AXISOUT.
*    History :
*     26/9/88:  original (PLA_AST88%UK.AC.BHAM.SR.STAR)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER                START           ! Start position for the copy
      INTEGER                NVALS           ! Number of values to copy

      REAL                   AXISIN(NVALS)   ! Axis values to be copied

*    Import-Export :
      REAL                   AXISOUT(*)      ! Output axis values

*    Local variables :
      INTEGER                I               ! Loop counter
*-
      DO I = 1, NVALS
        AXISOUT (I + START) = AXISIN (I)
      END DO

      END



*-  BINMERGE_COPYWD - Copy input width into output dataset
      SUBROUTINE BINMERGE_COPYWD (IN, START, IL, OUT)
*    Description :
*     Copies REAL data from IN to OUT starting at START
*    History :
*     27/9/88:  original (PLA_AST88@UK.AC.BHAM.SR.STAR)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      REAL                   IN(*)                    ! Input array

      INTEGER                START                    ! Start posn in OUT
      INTEGER                IL                       ! Dimensions of IN
*    Import-Export :
      REAL                   OUT(*)                   ! Output array
*    Local variables :
      INTEGER                I                        ! Loop counter
*-

      DO I = 1, IL
        OUT (I + START) = IN (I)

      END DO
      END



*-  BINMERGE_COPY - Copy a 7D input array into output array with offset
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
      SUBROUTINE BINMERGE_GET_MIN (LOC, OUTUNITS, UNITS, LEN, AX, XAX,
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
*    Import :
      CHARACTER*(DAT__SZLOC) LOC               ! Locator to NDF
      CHARACTER*(80)         OUTUNITS          ! Output units
      CHARACTER*(80)         UNITS             ! Units of axis

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
      INTEGER                TPTR             ! Temporary pointer
      INTEGER                OUTLEN           ! Legth of output units string
      INTEGER                INLEN            ! Length of input units string

      LOGICAL                CONTINUE
*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

      CALL BDA_MAPAXVAL (LOC, 'READ', AX, TPTR, STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 999

      CONTINUE = .TRUE.
      OUTLEN   = CHR_LEN(OUTUNITS)
      INLEN    = CHR_LEN(UNITS)

      IF (OUTLEN .GT. 0 .AND. INLEN .GT. 0) THEN
        IF (OUTUNITS(1:OUTLEN) .EQ. UNITS(1:INLEN)) THEN
          PTR = TPTR

        ELSE
          CALL BINMERGE_SCALEXY (OUTUNITS, UNITS, LEN, TPTR, XAX, PTR,
     :                                                          STATUS)
          CALL BDA_UNMAP (LOC, TPTR, STATUS)

        END IF
      ELSE IF (XAX) THEN
        CALL MSG_PRNT ('WARNING: possible X axis units mismatch')

      ELSE
        CALL MSG_PRNT ('WARNING: possible Y axis units mismatch')

      END IF
      CALL ARR_MINR( LEN, %VAL(PTR), MIN)

*    Exit
 999  IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_REP ('E', '...from BINMERGE_GET_MIN', STATUS)
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      CHARACTER*(80)         OUTUNITS          ! Output units
      CHARACTER*(80)         UNITS             ! Units of axis

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

*        Check status
          IF (STATUS .NE. SAI__OK) GOTO 999

          CALL BINMERGE_SCALE (LEN, OUTVAL, INVAL, %VAL(INPTR),
     :                                                     %VAL(OUTPTR))

        END IF
      END IF

*    Exit
 999  IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_REP ('E', '...from BINMERGE_SCALEXY', STATUS)

      END IF
      END







*+  BINMERGE_SCALE - Alter axis data to correct units.
      SUBROUTINE BINMERGE_SCALE (LEN, OUTVAL, INVAL, IN, OUT)
*    Description :
*     Alters the array IN according to the OUTVAL & INVAL values
*     to produce OUT.
*    Authors :
*     Phil Andrews
*    History :
*     16-DEC-1988 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER                LEN               ! Length of axis

      REAL                   OUTVAL           ! output units value
      REAL                   INVAL            ! input units value
      REAL                   IN(*)            ! input array

*    Export :
      REAL                   OUT(*)           ! output array

*    Local variables :
      INTEGER                I                ! loop counter

*-
      DO I = 1, LEN
        OUT(I) = IN(I) * INVAL / OUTVAL

      END DO
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
*    Structure definitions :
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
