*+  FIT_GETINS - Gets instrument energy response and checks against axis values
      SUBROUTINE FIT_GETINS( ID, INDEX, FOUND, INSTR, STATUS )
*
*    Description :
*
*     Looks for an ENERGY_RESP component within the structure located by LOC.
*     If this is found then the three main response arrays (model and data
*     space indices, and the response values) which represent the sparse matrix
*     are mapped in. If data axis values are available then these are checked
*     for agreement with the CHANNEL_SPEC values in the response.
*     If INDEX>0 then a SPECTRAL_SET is being accessed, and ENERGY_RESP is a
*     structure array whose INDEXth component is required.
*
*    Parameters :
*    Method :
*    Deficiencies :
*     Specific to energy response structure.
*    Bugs :
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*    History :
*      3 Mar 88: Original (code stripped from FIT_DATGET)
*     20 Oct 88: Response locator included in INSTR structure (TJP)
*     15 Feb 89: ASTERIX88 version - SPECTRAL_SET handling (TJP)
*     20 Mar 91: Fix for case where no axis data are present (TJP)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*    Global variables :
*    Structure definitions :
      INCLUDE 'FIT_STRUC'
*    Import :
      INTEGER			ID			! Dataset identifier
      INTEGER 			INDEX			! Position in spectral set (0 if not)
*
*    Export :
*
	LOGICAL FOUND			! Instrument energy response found?
	RECORD /INSTR_RESP/ INSTR	! Instrument response
*
*    Status :
*
	INTEGER STATUS
*    Local variables :
      INTEGER 			AXPTR			! Pointer to axis data
      INTEGER 			NCB			! # channel bounds
      INTEGER 			NAX			! # axis values
      INTEGER			RCPTR			! Channel bounds

      LOGICAL 			OK			! Present & correct?
      LOGICAL 			REG			! Regularly spaced?
*-

*  Check inherited status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FOUND = .FALSE.

*  Handle slice!

*  Locate the response data
      CALL ERI_GETIDS( ID, INSTR.R_ID, INSTR.A_ID, STATUS )

*  Fill in dummy field
      INSTR.LOC = DAT__NOLOC

*  Went ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Axis values exist?
	CALL BDI_CHKAXVAL( ID, 1, OK, REG, NAX, STATUS )
	IF ( OK ) THEN

*    Response dimensions must
          CALL ADI_CSIZE( INSTR.R_ID, 'Channels', NCB, STATUS )
          IF ( NCB .NE. (NAX+1) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Number of dataset energy axis bins '/
     :                /'does not match response dimensions', STATUS )

*    ChannelSpec exists?
          ELSE
            CALL ADI_CGET0C( INSTR.R_ID, 'CompressMethod', MTH, STATUS )
            IF ( MTH(1:7) .EQ. 'ASTERIX' ) THEN

*      Map channel centres field and check against axis values
              CALL ADI_CMAP( INSTR.R_ID, 'ChannelSpec', 'READ',
     :                       RCPTR, STATUS )

*      Map in axis values
              CALL BDI_MAPAXVAL( ID, 'READ', 1, AXPTR, STATUS )

*      And check against dataset channels...
	      CALL FIT_INSGET_CCHECK( NAX, %VAL(AXPTR), NCB-1,
     :                                %VAL(RCPTR), STATUS )

*      Release the axis
	      CALL BDI_UNMAPAXVAL( ID, 1, STATUS )

            END IF

          END IF

          IF ( STATUS .EQ. SAI__OK ) FOUND = .TRUE.

        END IF

      END IF

* Exit
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_GETINS', STATUS )
      END IF

      END
