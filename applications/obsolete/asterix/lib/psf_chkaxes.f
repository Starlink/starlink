*+  PSF_CHKAXES - Locate axis data in dataset and produce radian measures
      SUBROUTINE PSF_CHKAXES( SLOT, STATUS )
*
*    Description :
*
*     Gets axis information for dataset pointed to by SLOT. This involves
*     reading the AXIS data for binned datasets, the X_<> and Y_<> lists
*     for event datasets, and a cunning fabrication for primitive arrays.
*
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     03 Nov 89 : Original (DJA)
*     19 Jul 90 : Improvements for primitive data (DJA)
*     26 Jul 90 : Bug fixed when irregular axes and no units (DJA)
*     16 Nov 91 : Uses BDA_x_INT routines (DJA)
*     26 Apr 93 : Added axis identification and EVDS switch (DJA)
*     16 Dec 93 : Use PSF1_ calls for axis stuff (DJA)
*     17 Feb 94 : Allocate psf instance if not done externally (DJA)
*     30 Jun 94 : Improved default behaviour to trap case where no
*                 axis labels present (DJA)
*     10 Apr 95 : Added PI to list of allowable energy axis names (DJA)
*     12 Apr 95 : Made test for EVDS more robust (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      INTEGER                 SLOT              ! PSF handle
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      LOGICAL                 CHR_INSET
*
*    Local variables :
*
      CHARACTER*5             LSUF              ! List suffix for evds
      CHARACTER*(DAT__SZTYP)  TYPE              ! Dataset type
      CHARACTER*40            LABEL, UNITS	! Axis attributes
      CHARACTER*(DAT__SZLOC)  LLOC(2)           ! X & Y list locators

      REAL                    AXV1, AXV2        ! Axis values
      REAL                    BASE, SCALE       ! Axis data attributes
      REAL                    TOR               ! Radian conversion factor

      INTEGER                 BDA               ! BDA identifier
      INTEGER                 DIM               ! Size of an axis
      INTEGER                 DIMS(DAT__MXDIM)  ! Dataset dimensions
      INTEGER                 IAX               ! Loop over axes
      INTEGER                 NAXES             ! Number of axes
      INTEGER                 NDIM              ! Dimensionality
      INTEGER                 PTR               ! Pointer to axis data
      INTEGER                 WE                ! Word delimiter
      INTEGER                 X_AX,Y_AX,E_AX,T_AX

      LOGICAL                 OK                ! General validity check
      LOGICAL                 PRIM              ! Dataset primitive?
      LOGICAL                 REG               ! Axis is regular?
      LOGICAL                 XOK, YOK          ! X/Y lists present?
      LOGICAL			XTHERE			! X_CORR present?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Allocate internal storage if not already done
      IF ( P_INST(SLOT) .EQ. 0 ) THEN
        CALL PSF1_ALLOC( P_INST(SLOT), STATUS )
      END IF

*    Already done this?
      IF ( P_GOTAX(SLOT) ) RETURN

*    Initialise
      P_EVDS(SLOT) = .FALSE.
      X_AX = 0
      Y_AX = 0
      E_AX = 0
      T_AX = 0

*    Primitive?
      CALL DAT_PRIM( P_LOC(SLOT), PRIM, STATUS )
      IF ( PRIM ) THEN

*      Can't be event dataset so check data
        CALL BDA_FIND( P_LOC(SLOT), BDA, STATUS )
        CALL BDA_CHKDATA_INT( BDA, OK, NDIM, DIMS, STATUS )

*      If data isn't ok then we can't do much anyway!
        IF ( OK ) THEN
          CALL PSF_CHKAXES_DUMMY( P_INST(SLOT), NDIM, DIMS, STATUS )
        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Invalid primitive data', STATUS )
        END IF

      ELSE

*      Decide whether event or image dataset
        CALL DAT_TYPE( P_LOC(SLOT), TYPE, STATUS )

        CALL DAT_THERE( P_LOC(SLOT), 'X_CORR', XTHERE, STATUS )

*      If binned
        IF ( .NOT. ((TYPE(:4).EQ.'EVDS') .OR.
     :              (TYPE(:5).EQ.'EVENT')) .AND. .NOT. XTHERE ) THEN

*        Check data
          CALL BDA_FIND( P_LOC(SLOT), BDA, STATUS )
          CALL BDA_CHKDATA_INT( BDA, OK, NDIM, DIMS, STATUS )

*        Get number of axes
          CALL BDA_CHKAXES_INT( BDA, NAXES, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            NAXES = 0
          END IF

*        Any axes present?
          IF ( NAXES .GT. 0 ) THEN

            NDIM = NAXES

*          Check axes
            DO IAX = 1, NAXES

*            Some defaults
              TOR = 1.0

*            Get axis description
              CALL BDA_CHKAXVAL_INT( BDA, IAX, OK, REG, DIM, STATUS )
              IF ( .NOT. OK ) THEN
                OK = .TRUE.
                BASE = 1.0
                SCALE = 1.0
                TOR = 1.0
                DIM = DIMS(IAX)

              ELSE IF ( REG ) THEN
                CALL BDA_GETAXVAL_INT( BDA, IAX, BASE, SCALE, DIM,
     :                                 STATUS )

              ELSE
                CALL BDA_MAPAXVAL_INT( BDA, 'READ', IAX, PTR, STATUS )
                CALL ARR_ELEM1R( PTR, DIM, 1, AXV1, STATUS )
                BASE = AXV1
                IF ( DIM .EQ. 1 ) THEN
                  SCALE = 1.0
                ELSE
                  CALL ARR_ELEM1R( PTR, DIM, 2, AXV2, STATUS )
                  SCALE = AXV2 - AXV1
                END IF
                TOR = 1.0
              END IF

*            Get units and label
              CALL BDA_GETAXLABEL_INT( BDA, IAX, LABEL, STATUS )
              CALL BDA_GETAXUNITS_INT( BDA, IAX, UNITS, STATUS )

*            Identify axis
              WE = 1
              CALL CHR_FIWE( LABEL, WE, STATUS )
              IF ( (X_AX.EQ.0) .AND.
     :             CHR_INSET( 'X,X_CORR,X_RAW',LABEL(:WE) ) ) THEN
                X_AX = IAX
              ELSE IF ( (Y_AX.EQ.0) .AND.
     :             CHR_INSET( 'Y,Y_CORR,Y_RAW',LABEL(:WE) ) ) THEN
                Y_AX = IAX
              ELSE IF ( (X_AX.EQ.0) .AND. (IAX.EQ.1) .AND.
     :                 (NDIM.EQ.2) ) THEN
                X_AX = IAX
              ELSE IF ( (Y_AX.EQ.0) .AND. (IAX.EQ.2) .AND.
     :                 (NDIM.EQ.2) ) THEN
                Y_AX = IAX
              ELSE IF ( (E_AX.EQ.0) .AND.
     :             CHR_INSET( 'CORR_PH_CH,PULSE_HEIGHT,PI,'/
     :                        /'PULSE_HEIGHT_CH,ENERGY',
     :                        LABEL(:WE) ) .OR.
     :             (INDEX(LABEL,'PHA').NE.0) ) THEN
                E_AX = IAX
              ELSE IF ( (T_AX.EQ.0) .AND.
     :             CHR_INSET( 'RAW_TIMETAG,TIME',LABEL(:WE) ) ) THEN
                T_AX = IAX
              END IF

*            Was this axis spatial?
              IF ( (IAX.EQ.X_AX) .OR. (IAX.EQ.Y_AX) ) THEN
                IF ( UNITS .GT. ' ' ) THEN
                  CALL CONV_UNIT2R( UNITS, TOR, STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                    CALL ERR_ANNUL( STATUS )
                    UNITS = 'pixels'
                  END IF
                END IF
              END IF

*            Write axis data
              CALL PSF1_PUTAX( P_INST(SLOT), IAX, OK, DIM, REG,
     :                         .FALSE., PTR, BASE, SCALE,
     :                         TOR, LABEL, UNITS, STATUS )

            END DO

*        no...same as for primitive
          ELSE

*          Issue warning
            CALL MSG_PRNT( 'Binned dataset has no valid axis data' )

*          Set up dummies
            CALL PSF_CHKAXES_DUMMY( P_INST(SLOT), NDIM, DIMS, STATUS )

          END IF

*      else if event...
        ELSE IF ( XTHERE ) THEN

*        Set event dataset flag
          P_EVDS(SLOT) = .TRUE.

*        Look for X_CORR list
          XOK = XTHERE
          IF ( XOK ) THEN
            CALL DAT_FIND( P_LOC(SLOT), 'X_CORR', LLOC(1), STATUS )
            LSUF = '_CORR'
          ELSE

*          Look for X_RAW list
            CALL DAT_THERE( P_LOC(SLOT), 'X_RAW', XOK, STATUS )
            IF ( XOK ) THEN
              CALL DAT_FIND( P_LOC(SLOT), 'X_RAW', LLOC(1), STATUS )
              LSUF = '_RAW'

*          Give up
            ELSE
              CALL MSG_PRNT( '! No X axis list data present' )
              STATUS = SAI__ERROR
              GOTO 99

            END IF

          END IF

*        Look for matching Y list
          CALL DAT_THERE( P_LOC(SLOT), 'Y'//LSUF, YOK, STATUS )
          IF ( YOK ) THEN
            CALL DAT_FIND( P_LOC(SLOT), 'Y'//LSUF, LLOC(2), STATUS )
          ELSE
            CALL MSG_PRNT( '! No Y axis list data present' )
            STATUS = SAI__ERROR
            GOTO 99
          END IF

*        Get quanta and units for each list
          DO IAX = 1, 2

*          Get quantum and units
            CALL CMP_GET0R( LLOC(IAX), 'QUANTUM', SCALE, STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
            CALL CMP_GET0C( LLOC(IAX), 'UNITS', UNITS, STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*          Free locator
            CALL DAT_ANNUL( LLOC(IAX), STATUS )

*          Was this axis spatial?
            TOR = 1.0
            IF ( UNITS .GT. ' ' ) THEN
              CALL CONV_UNIT2R( UNITS, TOR, STATUS )
              IF ( STATUS .NE. SAI__OK ) THEN
                CALL ERR_ANNUL( STATUS )
                UNITS = 'pixels'
              END IF
            END IF

*          Write axis data
            CALL PSF1_PUTAX( P_INST(SLOT), IAX, .TRUE., -1, .TRUE.,
     :                       .FALSE., 0, BASE, SCALE,
     :                       TOR, LABEL, UNITS, STATUS )

          END DO

*        Set axis numbers
          X_AX = 1
          Y_AX = 2

        END IF

      END IF

*    Write axis identifiers
      CALL PSF1_PUTAXID( P_INST(SLOT), X_AX, Y_AX, E_AX, T_AX, STATUS )

*    Mark as done
      P_GOTAX(SLOT) = .TRUE.

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_CHKAXES', STATUS )
      END IF

      END



*+  PSF_CHKAXES_DUMMY - Constructs dummy axis data
      SUBROUTINE PSF_CHKAXES_DUMMY( P_INST, NDIM, DIMS, STATUS )
*
*    Description :
*
*     Construct default transformations for binned data.
*
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     29 Jun 90 : Original (DJA)
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
      INTEGER                 P_INST            ! PSF instance data
      INTEGER                 NDIM              ! Dimensionality
      INTEGER                 DIMS(*)           ! Dataset dimensions
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*6             LABEL
      INTEGER                 IAX
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop over dimensions
      DO IAX = 1, NDIM

*      Construct a label
        WRITE( LABEL, '(A,I1)' ) 'Axis ', IAX

*      Write dummy axis data
        CALL PSF1_PUTAX( P_INST, IAX, .TRUE., DIMS(IAX), .TRUE.,
     :                   .FALSE., 0, 1.0, 1.0,
     :                   1.0, LABEL, 'pixels', STATUS )

      END DO

      END
