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
*     27 Jan 85 : V0.6    Original (BHVAD::JCMP)
*     20 Jul 88 : V1.0-0  Converted to new STARLINK HDS standards. (BHVAD::DJA)
*      7 Oct 88 : V1.0-1  Now handles overwriting using OVER hidden
*                         parameter. ( BHVAD::DJA )
*     23 Aug 89 : V1.0-2  Quality handling now included. Output variances
*                         are set to unity ( BHVAD::DJA )
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local Constants :
*
      INTEGER INVAR  			      ! algorithm number
         PARAMETER (INVAR=1)

      CHARACTER*22           VERSION
         PARAMETER ( VERSION = 'SIGNIF Version 1.0-2' )

      INTEGER                MAXLINES
         PARAMETER           (MAXLINES = 10 )
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ILOC	      ! input object locator
      CHARACTER*(DAT__SZLOC) OLOC	      ! output object locator
      CHARACTER*(DAT__SZLOC) WLOC   	      ! SIGNIF data locator

      CHARACTER*80           ACTION(MAXLINES) ! History records

      INTEGER                DIMS(DAT__MXDIM) ! Sizes of each dimension

      INTEGER	             WEIGHT_NELM      ! size of SIGNIF data
      INTEGER                NDIM             ! Dimensionality of object
      INTEGER	             NELM	      ! size of input data
      INTEGER                NREC             ! Number of history records written
      INTEGER	             OPTR	      ! pointer to output data
      INTEGER	             QPTR	      ! pointer to quality data
      INTEGER	             WPTR	      ! pointer to SIGNIF data

      LOGICAL                ANYBAD           ! Are there any bad quality data?
      LOGICAL	             OK		      ! data present and valid?
      LOGICAL                OVERWRITE        ! Overwriting?
      LOGICAL                PRIM             ! input is primitive data?
      LOGICAL		     Q_OK	      ! quality present and valid?
      LOGICAL		     V_OK	      ! variance present and valid?
*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialise the ASTERIX common blocks
      CALL AST_INIT( STATUS )

*    See whether we're going to overwrite
      CALL PAR_GET0L( 'OVER', OVERWRITE, STATUS )

*    Obtain data object, access and check it
      IF ( OVERWRITE ) THEN
        CALL USI_ASSOCI( 'INP', 'UPDATE', OLOC, PRIM, STATUS )
      ELSE
        CALL USI_ASSOC2( 'INP', 'OUT', 'READ', ILOC, OLOC,PRIM, STATUS )
        IF ( .NOT. PRIM ) THEN
          CALL USI_SHOW( 'Output data {OUT}', STATUS )
          CALL HDX_COPY( ILOC, OLOC, STATUS )
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    The input must not be primitive
      IF ( PRIM ) THEN
        CALL MSG_PRNT( 'ERROR : Input must not be primitive.' )
        STATUS = SAI__ERROR
        GOTO 99
      END IF

*    Get size of data array
      CALL BDA_CHKDATA( OLOC, OK, NDIM, DIMS, STATUS )
      IF ( OK ) THEN
	CALL ARR_SUMDIM( NDIM, DIMS, NELM )
      ELSE
	CALL MSG_PRNT( 'Data object invalid' )
	STATUS = SAI__ERROR
	GOTO 99
      END IF

*    See if we have quality data in input object
      CALL BDA_CHKQUAL( OLOC, Q_OK, NDIM, DIMS, STATUS )
      IF ( Q_OK ) THEN

*      Map it - are there any bad quality points
        CALL BDA_MAPLQUAL( OLOC, 'READ', ANYBAD, QPTR, STATUS )
        IF ( .NOT. ANYBAD ) THEN
          CALL BDA_UNMAPLQUAL( OLOC, STATUS )
          Q_OK = .FALSE.
        END IF

      END IF

*    See if we have error data in input object
      CALL BDA_CHKVAR( OLOC, V_OK, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    If variance is present, map it as error. If not then get a weights array
*    from the user
      IF ( V_OK ) THEN
	CALL BDA_MAPERR( OLOC, 'UPDATE', WPTR, STATUS )
      ELSE

*      We don't so try for a weights data array
	CALL MSG_PRNT( 'No error data in object' )
        CALL MSG_SETI( 'N', NELM )
	CALL MSG_PRNT( 'Data has ^N values' )

        CALL USI_ASSOCI( 'WEIGHT_DATA', WLOC, PRIM, STATUS )
	IF ( STATUS .NE. SAI__OK ) GOTO 99

        IF ( .NOT. PRIM ) THEN
          CALL MSG_PRNT( 'A numeric data object is required.' )
	  CALL DAT_CANCL( 'WEIGHT_DATA', STATUS )
	  STATUS = SAI__ERROR
          GOTO 99
	END IF

        CALL BDA_CHKDATA( WLOC, OK, NDIM, DIMS, STATUS )
        IF ( .NOT. OK ) THEN
          CALL MSG_PRNT( 'ERROR : Weights data invalid' )
          STATUS = SAI__ERROR
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

        CALL ARR_SUMDIM( NDIM, DIMS, WEIGHT_NELM )

	IF ( NELM .NE. WEIGHT_NELM ) THEN
          CALL MSG_PRNT( 'Sizes of input data and weights arrays '/
     :                                            /'don''t match' )
	  STATUS = SAI__ERROR
	END IF

        CALL BDA_MAPDATA( WLOC, 'READ', WPTR, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map output data. Since we've copied the data over to the output if
*    we're not overwriting, we just overwrite the output data.
      CALL BDA_MAPDATA( OLOC, 'UPDATE', OPTR, STATUS )
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
        CALL BDA_UNMAPERR( OLOC, STATUS )
      END IF

*    History file entry
      CALL HIST_ADD( OLOC, VERSION, STATUS)
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
      CALL HIST_PTXT( OLOC, NREC, ACTION, STATUS )

*    Tidy up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END
