	SUBROUTINE BLOCK_NUMBERS( XSTC, YSTC, XENC, YENC, BLOCK_MAGNIF,
     :	                          CORIENT, XMAXIMUM, XMINIMUM, N1,
     :	                          N2, STATUS)

* Description : Routine to write numbers on colour block

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*   20-Jul-1994 Changed LIB$ call to CHR_, and non-standard IFIX to INT
*                                                    (SKL@JACH)
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

        INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'

* Status :

	INTEGER STATUS

* Import :

	INTEGER BLOCK_MAGNIF
	INTEGER N1
	INTEGER N2

	REAL XSTC
	REAL XENC
	REAL YSTC
	REAL YENC
	REAL XMAXIMUM
	REAL XMINIMUM

	CHARACTER*( *) CORIENT

* Import-Export :

* Export :

* External references :

* Global variables

	INCLUDE 'PLT2DCOM'

* Local Constants :

* Local variables :

	INTEGER J
	INTEGER NF
	INTEGER NPR
	INTEGER NUMB_CHAR
	INTEGER NUMTICKS /5/

	REAL AR
	REAL HM
	REAL HT
	REAL SP
	REAL TEXT_HT
	REAL X1
	REAL X_NUMBER
	REAL XU
	REAL Y1
	REAL Y_NUMBER
	REAL YU

	CHARACTER*20 NUMBERS_WHERE
	CHARACTER STRING*10
	CHARACTER TXJ*2

* Internal References :

* =====================================================================

* test status on entry

	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF

* get text attributes from SGS

	CALL SGS_IMTX( HM, NF, NPR, HT, AR, XU, YU, SP, TXJ)

* get the positioning of the numbers L, R, T, B

	CALL PAR_GET0C( 'BLOCKNUM_WHERE', NUMBERS_WHERE, STATUS)

* set text height

	IF( BLOCK_MAGNIF .EQ. 1) THEN
	  TEXT_HT = 7.5
	ELSE
	  TEXT_HT = 10
	END IF

	CALL SGS_SHTX( TEXT_HT)

* test which orientation the block is plotted in

	IF( CORIENT .EQ. 'H' .OR. CORIENT .EQ. 'HORIZONTAL') THEN

* centre text at position specified

	  CALL SGS_STXJ( 'CC')

* loop for X axis numbers

	  DO J = 1, NUMTICKS

* calculate position of X number

	    X1 = XSTC + N1*0.25*BLOCK_MAGNIF*( J - 1)

	    IF( NUMBERS_WHERE .EQ. 'T' .OR. NUMBERS_WHERE .EQ. 'TOP') THEN
	      Y1 = YENC + 1.3*TEXT_HT
	    ELSE
	      Y1 = YSTC - 1.3*TEXT_HT
	    END IF

* calculate X number

	    X_NUMBER = XMINIMUM + ( J - 1)*( XMAXIMUM - XMINIMUM)/4.0

* convert number to string for length test

            CALL CHR_ITOC( INT(X_NUMBER), STRING, NUMB_CHAR )

* plot bottom X axis number

	    CALL SGS_BTEXT( X1, Y1)
	    CALL SGS_ATXI( INT( X_NUMBER), -NUMB_CHAR)

	  END DO

	ELSE IF( CORIENT .EQ. 'V' .OR. CORIENT .EQ. 'VERTICAL') THEN

* centre text at position specified

	  IF( NUMBERS_WHERE .EQ. 'R' .OR. NUMBERS_WHERE .EQ. 'RIGHT') THEN
	    CALL SGS_STXJ( 'CL')
	  ELSE
	    CALL SGS_STXJ( 'CR')
	  END IF

* loop for Y axis numbers

	  DO J = 1, NUMTICKS

* calculate Y number

	    Y_NUMBER = XMINIMUM + ( J-1)*( XMAXIMUM - XMINIMUM)/4

* calculate position of Y number

	    IF( NUMBERS_WHERE .EQ. 'R' .OR. NUMBERS_WHERE .EQ. 'RIGHT') THEN

	      X1 = XENC + TEXT_HT
	      Y1 = YSTC + N2*0.25*BLOCK_MAGNIF*( J - 1)

* plot left Y axis number

	      CALL SGS_BTEXT( X1, Y1)
	      CALL SGS_ATXI( INT( Y_NUMBER), 0)

	    ELSE

	      X1 = XSTC - TEXT_HT
	      Y1 = YSTC + N2*0.25*BLOCK_MAGNIF*( J - 1)

* plot left Y axis number

	      CALL SGS_BTEXT( X1, Y1)
	      CALL SGS_ATXI( INT( Y_NUMBER), -15)

	    END IF

	  END DO

	END IF

* reset centre text at position specified

	  CALL SGS_STXJ( 'CC')

* empty plot buffer of remaining graphics

	CALL SGS_FLUSH

	END
