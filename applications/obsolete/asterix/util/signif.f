*+  SIGNIF - Change the input binned dataset to its significance
      SUBROUTINE SIGNIF( STATUS )
*
*    Description :
*
*     For a binned dataset, weight the data according to a specified algorithm.
*
*    Parameters :
*
*     INP=UNIV(R)
*           input data object (binned dataset)
*     OUT=UNIV(W)
*           output data object (can be same as input)
*     WEIGHT_DATA(N)=REAL(R)
*           array of SIGNIF data
*     OVER=LOGICAL(R)
*           Overwriting to take place ( default is N )
*
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden   (BHVAD::JCMP)
*     David Allan (BHVAD::DJA)
*
*    History :
*
*     27 Jan 85 : V0.6   Original (BHVAD::JCMP)
*     20 Jul 88 : V1.0-0 Converted to new STARLINK HDS standards. (DJA)
*      7 Oct 88 : V1.0-1 Now handles overwriting using OVER hidden
*                        parameter. (DJA)
*     23 Aug 89 : V1.0-2 Quality handling now included. Output variances
*                        are set to unity (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     12 Jan 95 : V1.8-1 Use new data interfaces (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local Constants :
*
      INTEGER 			INVAR  			! Algorithm number
        PARAMETER 		( INVAR = 1 )

      INTEGER                	MAXLINES
        PARAMETER           	( MAXLINES = 10 )
*
*    Local variables :
*
      CHARACTER*80           	ACTION(MAXLINES) 	! History records

      INTEGER                	DIMS(ADI__MXDIM) 	! Input data dimensions

      INTEGER			IFID			! I/p file identifier
      INTEGER                	NDIM             	! Dimensionality of object
      INTEGER	             	NELM	      		! Size of input data
      INTEGER                	NREC             ! Number of history records written
      INTEGER			OFID			! O/p file identifier
      INTEGER	             	OPTR	      ! pointer to output data
      INTEGER	             	QPTR	      ! pointer to quality data
      INTEGER	             	WEIGHT_NELM      	! Size of SIGNIF data
      INTEGER			WFID			! Weights data file
      INTEGER	             	WPTR	      		! pointer to SIGNIF data

      LOGICAL                	ANYBAD           	! Are there any bad quality data?
      LOGICAL	             	OK		      	! Data present and valid?
      LOGICAL                	OVERWRITE        	! Overwriting?
      LOGICAL                	PRIM             	! Input is primitive data?
      LOGICAL		     	Q_OK	      		! Quality present and valid?
      LOGICAL		     	V_OK	      		! Variance present and valid?

*  Version id:
      CHARACTER*22           VERSION
         PARAMETER ( VERSION = 'SIGNIF Version 1.8-1' )
*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialise the ASTERIX common blocks
      CALL AST_INIT()

*    See whether we're going to overwrite
      CALL USI_GET0L( 'OVER', OVERWRITE, STATUS )

*    Obtain data object, access and check it
      IF ( OVERWRITE ) THEN
        CALL USI_TASSOCI( 'INP', '*', 'UPDATE', OFID, STATUS )
        CALL BDI_PRIM( OFID, PRIM, STATUS )
      ELSE
        CALL USI_TASSOC2( 'INP', 'OUT', 'READ', IFID, OFID, STATUS )
        CALL BDI_PRIM( IFID, PRIM, STATUS )
        IF ( .NOT. PRIM ) THEN
          CALL USI_SHOW( 'Output data {OUT}', STATUS )
          CALL ADI_FCOPY( IFID, OFID, STATUS )
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    The input must not be primitive
      IF ( PRIM ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Input must not be primitive', STATUS )
        GOTO 99
      END IF

*    Get size of data array
      CALL BDI_CHKDATA( OFID, OK, NDIM, DIMS, STATUS )
      IF ( OK ) THEN
	CALL ARR_SUMDIM( NDIM, DIMS, NELM )
      ELSE
	CALL MSG_PRNT( 'Data object invalid' )
	STATUS = SAI__ERROR
	GOTO 99
      END IF

*    See if we have quality data in input object
      CALL BDI_CHKQUAL( OFID, Q_OK, NDIM, DIMS, STATUS )
      IF ( Q_OK ) THEN

*      Map it - are there any bad quality points
        CALL BDI_MAPLQUAL( OFID, 'READ', ANYBAD, QPTR, STATUS )
        IF ( .NOT. ANYBAD ) THEN
          CALL BDI_UNMAPLQUAL( OFID, STATUS )
          Q_OK = .FALSE.
        END IF

      END IF

*    See if we have error data in input object
      CALL BDI_CHKVAR( OFID, V_OK, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    If variance is present, map it as error. If not then get a weights array
*    from the user
      IF ( V_OK ) THEN
	CALL BDI_MAPERR( OFID, 'UPDATE', WPTR, STATUS )
      ELSE

*      We don't so try for a weights data array
	CALL MSG_PRNT( 'No error data in object' )
        CALL MSG_SETI( 'N', NELM )
	CALL MSG_PRNT( 'Data has ^N values' )

        CALL USI_ASSOCI( 'WEIGHT_DATA', '*', 'READ', WFID, STATUS )
	IF ( STATUS .NE. SAI__OK ) GOTO 99
        CALL BDI_PRIM( WFID, PRIM, STATUS )
        IF ( .NOT. PRIM ) THEN
	  STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'A numeric data object is required.',
     :                  STATUS )
          GOTO 99
	END IF

        CALL BDI_CHKDATA( WFID, OK, NDIM, DIMS, STATUS )
        IF ( .NOT. OK ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Weights data invalid', STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

        CALL ARR_SUMDIM( NDIM, DIMS, WEIGHT_NELM )

	IF ( NELM .NE. WEIGHT_NELM ) THEN
	  STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Sizes of input data and weights arrays '/
     :                                        /'don''t match', STATUS )
	END IF

        CALL BDI_MAPDATA( WFID, 'READ', WPTR, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map output data. Since we've copied the data over to the output if
*    we're not overwriting, we just overwrite the output data.
      CALL BDI_MAPDATA( OFID, 'UPDATE', OPTR, STATUS )
      IF ( Q_OK ) THEN
        CALL UTIL_WEIGHTQ( INVAR, NELM, %VAL(OPTR), %VAL(QPTR),
     :                         %VAL(WPTR), %VAL(OPTR), STATUS )
      ELSE
        CALL UTIL_WEIGHT( INVAR, NELM, %VAL(OPTR), %VAL(WPTR),
     :                                    %VAL(OPTR), STATUS )
      END IF

*    Set output VARIANCE array to unity if present
      IF ( V_OK ) THEN
        CALL ARR_INIT1R( 1.0, NELM, %VAL(WPTR), STATUS )
        CALL BDI_UNMAPERR( OFID, STATUS )
      END IF

*    History file entry
      CALL HSI_ADD( OFID, VERSION, STATUS)
      ACTION(1)='Input data {INP}'
      IF ( V_OK ) THEN
        ACTION(2)='Weight data was variance array'
      ELSE
        ACTION(2)='Weights data {WEIGHT_DATA}'
      END IF
      IF ( Q_OK ) THEN
        ACTION(3)='Data quality taken into account'
      ELSE
        ACTION(3)='No bad quality points in dataset'
      END IF
      NREC = MAXLINES
      CALL USI_TEXT( 3, ACTION, NREC, STATUS )
      CALL HSI_PTXT( OFID, NREC, ACTION, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
