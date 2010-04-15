	SUBROUTINE COLOUR_BLOCK( STATUS)

* Description : Routine to plot a colour table block on workstation

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA (26Jan85)

* History :
* 26-MAY-86 : REVA::CAA : added box around colour block and annotate option
* 02-Mar-94   SKL@JACH    changed DAT calls to NDF
* 20-Jul-1994 SKL@JACH    changed STR$ to CHR_, IFIX to INT
* 25-Jul-1994 SKL@JACH    changed error reporting to ERR calls, removed VALUE
* 02-Sep-1994 SKL@JACH   removed unused variables flagged by UNIX compiler
* 29-Sep-1994 SKL@JACH   changed calls to BLOCKSUB for dimensions
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

	INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'

* Status :

	INTEGER STATUS

* Import :

* Import-Export :

* Export :

* External references :

* Global variables

	INCLUDE 'PLT2DCOM'

* Local Constants :

* Local variables :

	INTEGER CSCALE
	INTEGER NAXIS( 2)
        INTEGER LOCSR
	INTEGER SCRATCH_POINTER
        INTEGER NELEMENTS
	INTEGER XCENTRE
	INTEGER YCENTRE
        INTEGER LBND(2)

        DATA LBND / 1, 1 /

	REAL XREAL
	REAL YREAL

	CHARACTER*20 BLOCK_ANNOT
	CHARACTER*20 CORIENT
	CHARACTER*3 USE_CURSOR

* Internal References :

* ================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK) THEN
	  RETURN
	END IF

* get the pointer to use cursor or get centre coordinates

	CALL PAR_GET0C( 'USE_CURSOR', USE_CURSOR, STATUS)
	CALL CHR_UCASE(USE_CURSOR)

* test if user wants to use cursor
	IF( USE_CURSOR .EQ. 'YES') THEN

* call subroutine to display cursor and get the users input position for the
* colour block

	  CALL CURSOR_POSITION( STATUS)

	  IF( STATUS. NE. SAI__OK)THEN
	    RETURN
	  END IF

* get the real cursor position from the parameter system

	  CALL PAR_GET0R( 'X_CUR_REAL', XREAL, STATUS)
	  CALL PAR_GET0R( 'Y_CUR_REAL', YREAL, STATUS)

	  XCENTRE = INT( XREAL + 0.5)
	  YCENTRE = INT( YREAL + 0.5)

	  IF( STATUS. NE. SAI__OK)THEN
	    RETURN
	  END IF

	ELSE

* get centre in X and Y for colour table position

	  CALL PAR_GET0I( 'CT_XCEN', XCENTRE, STATUS)
	  CALL PAR_GET0I( 'CT_YCEN', YCENTRE, STATUS)

	END IF

* get scale factor for colour table size

	CALL PAR_GET0I( 'CT_SCALE', CSCALE, STATUS)

* get orientation pointer for colour table

	CALL PAR_GET0C( 'CT_ORIENTATION', CORIENT, STATUS)
	CALL CHR_UCASE( CORIENT )

* get the annoation option flag from the parameter system

	CALL PAR_GET0C( 'CT_ANNOTATION', BLOCK_ANNOT, STATUS)
	CALL CHR_UCASE( BLOCK_ANNOT )

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'COLOUR_BLOCK : after PAR_GETs',
     :                 STATUS )
	  RETURN
	END IF

* associate scratch area and create colour block image

	IF( CORIENT. EQ. 'HORIZONTAL' .OR. CORIENT .EQ. 'H') THEN
	  NAXIS( 1) = MAXIMCOL - MINIMCOL
	  NAXIS( 2) = 10
	ELSE
	  NAXIS( 1) = 10
	  NAXIS( 2) = MAXIMCOL - MINIMCOL
	END IF

	CALL NDF_ASSOC( 'SCRATCH_NAME', 'WRITE', LOCSR, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'Error, creating working array ...',
     :                 STATUS )
	  CALL NDF_ANNUL( LOCSR, STATUS )
	  RETURN
	END IF

*      reset pixel boundaries of scratch file
        CALL NDF_SBND( 2, LBND, NAXIS, LOCSR, STATUS)

	CALL NDF_MAP( LOCSR, 'Data', '_INTEGER', 'WRITE',
     :	              SCRATCH_POINTER, NELEMENTS, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :               'Error : During NDF_MAP scratch image...WHOOPS',
     :                 STATUS )
	  CALL NDF_ANNUL( LOCSR, STATUS )
	  RETURN
	END IF

* call subroutine to do work

	CALL COLOUR_BLOCKSUB( NAXIS(1), NAXIS(2), %VAL( SCRATCH_POINTER),
     :	        CORIENT, XCENTRE, YCENTRE, CSCALE, BLOCK_ANNOT)

	CALL NDF_ANNUL( LOCSR, STATUS )

	END
