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
*     16 Nov 91 : Uses internal BDA routines (DJA)
*     26 Apr 93 : Added axis identification and EVDS switch (DJA)
*     16 Dec 93 : Use PSF1_ calls for axis stuff (DJA)
*     17 Feb 94 : Allocate psf instance if not done externally (DJA)
*     30 Jun 94 : Improved default behaviour to trap case where no
*                 axis labels present (DJA)
*     10 Apr 95 : Added PI to list of allowable energy axis names (DJA)
*     12 Apr 95 : Made test for EVDS more robust (DJA)
*     25 Apr 95 : Switched to use BDI_ (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
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
*    Local variables :
*
      CHARACTER*40            	LABEL, UNITS		! Axis attributes

      REAL                    BASE, SCALE       ! Axis data attributes
      REAL                    	TOR               	! Radian conversion factor

      INTEGER			AXID(4)			! Axis numbers
      INTEGER                 	DIMS(ADI__MXDIM)  	! Dataset dimensions
      INTEGER                 	IAX               	! Loop over axes
      INTEGER			LID			! List identifier
      INTEGER                 	NDIM              	! Dimensionality
      INTEGER                 	PTR               	! Pointer to axis data

      LOGICAL                 	OK                	! General validity check
      LOGICAL                 	REG               	! Axis is regular?
*
*  Local Data:
*
      CHARACTER*1		QCODE(4)
        DATA			QCODE/'X','Y','E','T'/
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Allocate internal storage if not already done
      IF ( P_INST(SLOT) .EQ. 0 ) THEN
        CALL PSF1_ALLOC( P_INST(SLOT), STATUS )
        CALL ADI_CPUT0I( P_PSID(SLOT), 'Instance', P_INST(SLOT),
     :                   STATUS )
      END IF

*  Already done this?
      IF ( P_GOTAX(SLOT) ) RETURN

*  Initialise
      DO IAX = 1, 4
        AXID(IAX) = 0
      END DO

*  Is input an event dataset?
      CALL ADI_DERVD( P_FID(SLOT), 'EventDS', P_EVDS(SLOT), STATUS )
      IF ( P_EVDS(SLOT) ) THEN

*    Locate 'axes' by quantity code. EDI returns the list number
        DO IAX = 1, 4
          CALL EDI_QFND( P_FID(SLOT), QCODE(IAX), LABEL, AXID(IAX),
     :                   STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            AXID(IAX) = 0
          END IF
        END DO

*    X and Y axis lists supplied?
        DO IAX = 1, 2

          IF ( AXID(1) .GT. 0 ) THEN

*        Index list
            CALL EDI_IDX( P_FID(SLOT), AXID(1), LID, STATUS )

*        Get name as label
            CALL ADI_CGET0C( LID, 'Name', LABEL, STATUS )

*        Get units if present
            TOR = 1.0
            CALL ADI_CGET0C( LID, 'Units', UNITS, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
              CALL CONV_UNIT2R( UNITS, TOR, STATUS )
              IF ( STATUS .NE. SAI__OK ) THEN
                CALL ERR_ANNUL( STATUS )
                UNITS = 'pixels'
              END IF
            END IF

*        Write axis data
            CALL PSF1_PUTAX( P_INST(SLOT), IAX, .TRUE., -1, .TRUE.,
     :                       .FALSE., 0, BASE, SCALE,
     :                       TOR, LABEL, UNITS, STATUS )

*        Release the list
            CALL ADI_ERASE( LID, STATUS )

          END IF

        END DO

*  Otherwise binned
      ELSE

*    Check data
        CALL BDI_GETSHP( P_FID(SLOT), ADI__MXDIM, DIMS, NDIM, STATUS )

*    Locate axes by quantity code
        DO IAX = 1, 4
          CALL BDI0_FNDAXC( P_FID(SLOT), QCODE(IAX), AXID(IAX), STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            AXID(IAX) = 0
          END IF
        END DO

*    Check axes
        DO IAX = 1, NDIM

*      Some defaults
          TOR = 1.0

*      Get axis description
          CALL BDI_AXCHK( P_FID(SLOT), IAX, 'Data', OK, STATUS )
          IF ( .NOT. OK ) THEN
            OK = .TRUE.
            BASE = 1.0
            SCALE = 1.0

          ELSE

*        Map axis values
            CALL BDI_AXMAPR( P_FID(SLOT), IAX, 'Data', 'READ', PTR,
     :                         STATUS )

*        Regular?
            CALL ARR_CHKREG( %VAL(PTR), DIMS(IAX), REG, BASE,
     :                       SCALE, STATUS )

*        If regular unmap
            IF ( REG ) THEN
              CALL BDI_AXUNMAP( P_FID(SLOT), IAX, 'Data', PTR, STATUS )
            END IF

          END IF

*      Get units and label
          CALL BDI_AXGET0C( P_FID(SLOT), IAX, 'Label', LABEL, STATUS )
          CALL BDI_AXGET0C( P_FID(SLOT), IAX, 'Units', UNITS, STATUS )

*      Was this axis spatial?
          IF ( (IAX.EQ.AXID(1)) .OR. (IAX.EQ.AXID(2)) ) THEN
            IF ( UNITS .GT. ' ' ) THEN
              CALL CONV_UNIT2R( UNITS, TOR, STATUS )
              IF ( STATUS .NE. SAI__OK ) THEN
                CALL ERR_ANNUL( STATUS )
                UNITS = 'pixels'
              END IF
            END IF
          END IF

*      Write axis data
          CALL PSF1_PUTAX( P_INST(SLOT), IAX, OK, DIMS(IAX), REG,
     :                     .FALSE., PTR, BASE, SCALE,
     :                     TOR, LABEL, UNITS, STATUS )

        END DO

      END IF

*  Write axis identifiers
      CALL PSF1_PUTAXID( P_INST(SLOT), AXID(1), AXID(2), AXID(3),
     :                   AXID(4), STATUS )

*  Mark as done
      P_GOTAX(SLOT) = .TRUE.

*  Tidy up
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
