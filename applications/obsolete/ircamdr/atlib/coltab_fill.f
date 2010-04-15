	SUBROUTINE COLTAB_FILL( COLOURS, ARRAY)

* Description : Subroutine to fill the HDS container file with the colour table

	IMPLICIT NONE

	INTEGER J
	INTEGER K

	REAL ARRAY( 3, 256)
	REAL COLOURS( 3, 256)

* loops to fill the HDS data_array component

	DO J = 1, 256
	  DO K = 1, 3
	    ARRAY( K, J) = COLOURS( K, J)
	  END DO
	END DO

	END
