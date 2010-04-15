

	SUBROUTINE HDSCREATE_SETPRIM( HEIGHT,
     *	                              LATITUDE,
     *	                              LONGITUDE,
     *	                              NUMBER_COLUMNS,
     *	                              NUMBER_ROWS,
     *	                              SOFTWARE_PACKAGE,
     *	                              TELESCOPE_NAME,
     *	                              HEIGHT_PRIM,
     *	                              LATITUDE_PRIM,
     *	                              LONGITUDE_PRIM,
     *	                              NUMBCOLS_PRIM,
     *	                              NUMBROWS_PRIM,
     *	                              SOFTWARE_PRIM,
     *	                              TELESCOPE_PRIM)

C
C Description :	Routine to set the primitive values defaulted in HDSCREATE
C
C ===========================================================================
C
C Parameters :
C
C Method :
C
C Deficiencies :
C
C Bugs :
C
C Authors : C.Aspin, (ROE), REVA::CAA : 21-APR-86
C
C History :
C endhistory
C
C Type Definitions :
C
	IMPLICIT NONE
C
C Global constants :
C
	INCLUDE 'SAE_PAR'
C
C Import :
C
C Import-Export :
C
C Export :
C
C Status :
C
	INTEGER STATUS
C
C External references :
C
C Global variables :
C
C Local Constants :
C
C Local variables :
C
	INTEGER NUMBCOLS_PRIM
	INTEGER NUMBER_COLUMNS
	INTEGER NUMBER_ROWS
	INTEGER NUMBROWS_PRIM

	REAL HEIGHT
	REAL HEIGHT_PRIM
	REAL LATITUDE( 3)
	REAL LATITUDE_PRIM( 3)
	REAL LONGITUDE( 3)
	REAL LONGITUDE_PRIM( 3)

	CHARACTER*( *) SOFTWARE_PACKAGE
	CHARACTER*50   SOFTWARE_PRIM
	CHARACTER*( *) TELESCOPE_NAME
	CHARACTER*50   TELESCOPE_PRIM
C
C Internal References :
C
C Local data :
C
C ===========================================================================
C
C check status on entry
C
	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF
C
C set ALL primitives
C
	HEIGHT_PRIM = HEIGHT
	LATITUDE_PRIM( 1) = LATITUDE( 1)
	LATITUDE_PRIM( 2) = LATITUDE( 2)
	LATITUDE_PRIM( 3) = LATITUDE( 3)
	LONGITUDE_PRIM( 1) = LONGITUDE( 1)
	LONGITUDE_PRIM( 2) = LONGITUDE( 2)
	LONGITUDE_PRIM( 3) = LONGITUDE( 3)
	NUMBCOLS_PRIM = NUMBER_COLUMNS
	NUMBROWS_PRIM = NUMBER_ROWS
	SOFTWARE_PRIM = SOFTWARE_PACKAGE
	TELESCOPE_PRIM = TELESCOPE_NAME

	END
