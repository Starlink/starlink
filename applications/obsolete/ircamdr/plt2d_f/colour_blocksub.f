	SUBROUTINE COLOUR_BLOCKSUB( NAXIS1, NAXIS2, COLOUR_ARRAY, CORIENT,
     :	                            XCENTRE, YCENTRE, CSCALE,
     :	                            BLOCK_ANNOT)

* Description : Routine to plot a colour table block on workstation

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA (26Jan85)

* History :
* 26-MAY-86 : REVA::CAA : added box around colour block and annotate option
* 29-Sept-94  SKL@JACH Changed GCA calls for UNIX GKS, changed arguments
*             to list NAXIS(1) and (2) separately for compiler
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'
	INCLUDE 'SAE_PAR'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

* Status :

	INTEGER STATUS

* Import :

	INTEGER NAXIS1
      INTEGER NAXIS2
	INTEGER COLOUR_ARRAY( NAXIS1, NAXIS2 )
	INTEGER XCENTRE
	INTEGER YCENTRE
	INTEGER CSCALE

	CHARACTER*20 BLOCK_ANNOT

* Import-Export :

* Export :

* External references :

* Global variables

	INCLUDE 'PLT2DCOM'

* Local Constants :

* Local variables :

	INTEGER J
	INTEGER K
	INTEGER PEN_NUMBER

	REAL XENC
	REAL XSTC
	REAL XMAXIMUM
	REAL XMINIMUM
	REAL YENC
	REAL YSTC

	CHARACTER*20 CORIENT
	CHARACTER*1 COLOUR_CODE

* Internal References :

* ================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK) THEN
	  RETURN
	END IF

* zero colour block arrays

	DO J = 1, NAXIS2
	  DO K = 1, NAXIS1
	    COLOUR_ARRAY( K, J) = 0.0
	  END DO
	END DO

* loops to create image for plotting

	DO K = 1, NAXIS2

	  DO J = 1, NAXIS1

* put value into plotting array ARRAY

	    IF( CORIENT. EQ. 'HORIZONTAL' .OR. CORIENT .EQ. 'H') THEN
	      COLOUR_ARRAY( J, K) = J - 1
	    ELSE
	      COLOUR_ARRAY( J, K) = NAXIS2 - K - 1
	    END IF

	  END DO

	END DO

* calculate end pixels in X and Y for colour table plotting

	XSTC = IFIX( XCENTRE - CSCALE*( NAXIS1/2.0) + 0.5)
	YSTC = IFIX( YCENTRE - CSCALE*( NAXIS2/2.0) + 0.5)
	XENC = XSTC + CSCALE*NAXIS1
	YENC = YSTC + CSCALE*NAXIS2

* plot data array ARRAY of size NX by NY on PLT2D

	CALL GCA( XSTC, YENC, XENC, YSTC, NAXIS1, NAXIS2, 1, 1,
     :        NAXIS1, NAXIS2, COLOUR_ARRAY)

* get the colour block annotation pen and colour and set it

	CALL PAR_GET0I( 'COLBLOCK_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'COLBLOCK_COLOUR', COLOUR_CODE, STATUS)

	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* plot box around colour table

	CALL SGS_BOX( XSTC, XENC, YSTC, YENC)
	CALL SGS_BOX( XSTC+1, XENC-1, YSTC+1, YENC-1)

* test block annotation option to see if user wants to draw annotation around
* the colour block

	IF( BLOCK_ANNOT .EQ. 'ANNOTATE') THEN

* get the maximum and minimum plot range for the numbers

	  CALL PAR_GET0R( 'CALCULATED_MAX', XMAXIMUM, STATUS)
	  CALL PAR_GET0R( 'CALCULATED_MIN', XMINIMUM, STATUS)

* call subroutine to plot ticks on the long axis of the block

	  CALL BLOCK_TICKS( XSTC, YSTC, XENC, YENC, CSCALE, CORIENT,
     :	                    NAXIS1, NAXIS2, STATUS)

* call subroutine to plot number on long axis of block

	  CALL BLOCK_NUMBERS( XSTC, YSTC, XENC, YENC, CSCALE, CORIENT,
     :	                      XMAXIMUM, XMINIMUM, NAXIS1, NAXIS2,
     :	                      STATUS)

	END IF

* flush buffer of residual output

	CALL SGS_FLUSH

	END
