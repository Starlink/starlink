*+  FIT_GETINS - Gets instrument energy response and checks against axis values
      SUBROUTINE FIT_GETINS( ID, INDEX, E_AX, FOUND, INSTR, STATUS )
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
*      3 Mar 1988 : Original (code stripped from FIT_DATGET)
*     20 Oct 1988 : Response locator included in INSTR structure (TJP)
*     15 Feb 1989 : ASTERIX88 version - SPECTRAL_SET handling (TJP)
*     20 Mar 1991 : Fix for case where no axis data are present (TJP)
*      2 Aug 1995 : Specify energy axis by argument (DJA)
*     30 Nov 1995 : New data interface routines (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'FIT_PAR'
*    Global variables :
*    Structure definitions :
      INCLUDE 'FIT_STRUC'
*    Import :
      INTEGER			ID			! Dataset identifier
      INTEGER 			INDEX			! Position in spectral set (0 if not)
      INTEGER			E_AX			! Energy axis number
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
      CHARACTER*10		MTH			! Storage method

      INTEGER 			AXPTR			! Pointer to axis data
      INTEGER			DIMS(ADI__MXDIM)	! Input dimensions
      INTEGER 			NCC			! # channel centres
      INTEGER			NDIM			! Input dimensionality
      INTEGER			RCPTR			! Channel bounds

      LOGICAL 			OK			! Present & correct?
*-

*  Check inherited status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FOUND = .FALSE.

*  Locate the response data
      CALL ERI_GETIDS( ID, INDEX, INSTR.R_ID, INSTR.A_ID, STATUS )

*  Went ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Axis values exist?
        CALL BDI_AXCHK( ID, E_AX, 'Data', OK, STATUS )
	IF ( OK ) THEN

*    Get dataset dimensions
          CALL BDI_GETSHP( ID, ADI__MXDIM, DIMS, NDIM, STATUS )

*    Response dimensions must
          CALL ADI_CSIZE( INSTR.R_ID, 'ChannelSpec', NCC, STATUS )
          IF ( NCC .NE. DIMS(E_AX) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Number of dataset energy axis bins '/
     :                /'does not match response dimensions', STATUS )

*    ChannelSpec exists?
          ELSE
            CALL ADI_CGET0C( INSTR.R_ID, 'CompressMethod', MTH, STATUS )
            IF ( MTH(1:7) .EQ. 'ASTERIX' ) THEN

*      Map channel centres field and check against axis values
              CALL ADI_CMAPR( INSTR.R_ID, 'ChannelSpec', 'READ',
     :                        RCPTR, STATUS )

*      Map in axis values
              CALL BDI_AXMAPR( ID, E_AX, 'Data', 'READ', AXPTR, STATUS )

*      And check against dataset channels...
	      CALL FIT_INSGET_CCHECK( DIMS(E_AX), %VAL(AXPTR), NCC,
     :                                %VAL(RCPTR), STATUS )

*      Release the axis and spec array
              CALL ADI_CUNMAP( INSTR.R_ID, 'ChannelSpec',
     :                          RCPTR, STATUS )
              CALL BDI_AXUNMAP( ID, E_AX, 'Data', AXPTR, STATUS )

            END IF

          END IF

          IF ( STATUS .EQ. SAI__OK ) FOUND = .TRUE.

        END IF

      END IF

*  Exit
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_GETINS', STATUS )
      END IF

      END
