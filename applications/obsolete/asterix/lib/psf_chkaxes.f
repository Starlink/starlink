*+  PSF_CHKAXES - Locate axis data in dataset and produce radian measures
      SUBROUTINE PSF_CHKAXES( PSID, STATUS )
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
*      8 May 1996 (DJA):
*        Updated to ADI
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Import :
*
      INTEGER                 	PSID              	! PSF handle
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
      INTEGER			FID			! File identifier
      INTEGER                 	IAX               	! Loop over axes
      INTEGER			LID			! List identifier
      INTEGER                 	NDIM              	! Dimensionality
      INTEGER                 	PTR               	! Pointer to axis data

      LOGICAL			EVDS			! File is EventDS
      LOGICAL			GOTAX			! Got axis info?
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

*  Alread done the axes?
      CALL ADI_CGET0L( PSID, 'GotAx', GOTAX, STATUS )
      IF ( GOTAX ) RETURN

*  Extract file identifier
      CALL ADI_CGET0I( PSID, 'FileID', FID, STATUS )

*  Initialise
      DO IAX = 1, 4
        AXID(IAX) = 0
      END DO

*  Is input an event dataset?
      CALL ADI_DERVD( FID, 'EventDS', EVDS, STATUS )
      CALL ADI_CPUT0L( PSID, 'IsEventDS', EVDS, STATUS )
      IF ( EVDS ) THEN

*    Locate 'axes' by quantity code. EDI returns the list number
        DO IAX = 1, 4
          CALL EDI_QFND( FID, QCODE(IAX), LABEL, AXID(IAX),
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
            CALL EDI_IDX( FID, AXID(1), LID, STATUS )

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
            CALL PSF0_PUTAX( PSID, IAX, .TRUE., -1, .TRUE., 0, BASE,
     :                       SCALE, TOR, LABEL, UNITS, STATUS )

*        Release the list
            CALL ADI_ERASE( LID, STATUS )

          END IF

        END DO

*  Otherwise binned
      ELSE

*    Check data
        CALL BDI_GETSHP( FID, ADI__MXDIM, DIMS, NDIM, STATUS )

*    Locate axes by quantity code
        DO IAX = 1, 4
          CALL BDI0_FNDAXC( FID, QCODE(IAX), AXID(IAX), STATUS )
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
          CALL BDI_AXCHK( FID, IAX, 'Data', OK, STATUS )
          IF ( .NOT. OK ) THEN
            OK = .TRUE.
            BASE = 1.0
            SCALE = 1.0

          ELSE

*        Map axis values
            CALL BDI_AXMAPR( FID, IAX, 'Data', 'READ', PTR, STATUS )

*        Regular?
            CALL ARR_CHKREG( %VAL(PTR), DIMS(IAX), REG, BASE,
     :                       SCALE, STATUS )

*        If regular unmap
            IF ( REG ) THEN
              CALL BDI_AXUNMAP( FID, IAX, 'Data', PTR, STATUS )
            END IF

          END IF

*      Get units and label
          CALL BDI_AXGET0C( FID, IAX, 'Label', LABEL, STATUS )
          CALL BDI_AXGET0C( FID, IAX, 'Units', UNITS, STATUS )

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
          CALL PSF0_PUTAX( PSID, IAX, OK, DIMS(IAX), REG, PTR, BASE,
     :                            SCALE, TOR, LABEL, UNITS, STATUS )

        END DO

      END IF

*  Write axis identifiers
      CALL PSF0_PUTAXID( PSID, AXID(1), AXID(2), AXID(3),
     :                   AXID(4), STATUS )

*  Mark as done
      CALL ADI_CPUT0L( PSID, 'GotAx', .TRUE., STATUS )

*  Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_CHKAXES', STATUS )
      END IF

      END
