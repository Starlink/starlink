	SUBROUTINE GLITCH_VALUES( XRPOS, YRPOS, X_PIXEL, Y_PIXEL,
     :	                          CURSOR_VALUE, BADPOS, STATUS)

* Description : gets cursor position, value from an input image

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  04-MAR-1994 SKL@JACH Changed DAT_ routines to NDF_
*  27-Jul-1994 SKL@JACH Changed error reporting to use ERR_, removed VALUE,
*                       changed IFIX to INT
*  26-Oct-1994 SKL@JACH Changed MAGNIF from INT to REAL
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

	INCLUDE 'SAE_PAR'
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

* Import :

	INTEGER STATUS

	INTEGER SUBIM_EN( 2)
	INTEGER SUBIM_ST( 2)
	INTEGER X_PIXEL
	INTEGER Y_PIXEL

	REAL CURSOR_VALUE
	REAL XRPOS
	REAL YRPOS

	LOGICAL BADPOS

* Import-Export :

* Export :

* External references :

* Local Constants :

	INCLUDE 'PLT2DCOM'

* Local variables :

	REAL MAGNIF
	INTEGER NAXIS( 2)
	INTEGER POINTER_CURSOR_IMAGE
	INTEGER LOC_CURSOR_IMAGE
	INTEGER LOC_SUBIM
        INTEGER NELEMENTS
        INTEGER NDIM

	CHARACTER*30 IMAGE_ORI
	CHARACTER*1 SUBIM_OPTION

* Internal References :

* Local data :

* ============================================================================

* test status on entry

	IF( STATUS. NE. SAI__OK) THEN

	  RETURN

	END IF

* get sub-image option

	CALL PAR_GET0C( 'SUBIM_OPTION', SUBIM_OPTION, STATUS)

	IF( STATUS. NE. SAI__OK)THEN

          CALL ERR_REP('ERR', 'after PAR_GET SUBIM_OPTION', STATUS )
	  RETURN

	END IF

* associate image in CURSOR_IMAGE

	CALL NDF_ASSOC( 'CURSOR_IMAGE', 'READ', LOC_CURSOR_IMAGE,
     :                   STATUS)

	IF( STATUS. NE. SAI__OK)THEN

          CALL ERR_REP('ERR', 'after NDF_ASSOC', STATUS )

	  RETURN

	END IF

* test sub-image option for whole or slice of image

	IF( SUBIM_OPTION .NE. 'S') THEN

* map data in 'DATA_ARRAY' to obtain memory pointer POINTER_CURSOR_IMAGE
* and size of 2D image in NAXIS

	  CALL NDF_MAP( LOC_CURSOR_IMAGE, 'DATA', '_REAL', 'READ',
     :	                 POINTER_CURSOR_IMAGE, NELEMENTS, STATUS)

          CALL NDF_DIM(LOC_CURSOR_IMAGE, 2, NAXIS, NDIM, STATUS)

	  IF( STATUS. NE. SAI__OK)THEN

            CALL ERR_REP('ERR', 'During NDF_MAP image', STATUS )
	    RETURN

	  END IF

	ELSE

* here if want to plot slice of current image : GET slice area

	  CALL PAR_GET0I( 'SUBIM_XST', SUBIM_ST( 1), STATUS)
	  CALL PAR_GET0I( 'SUBIM_XEN', SUBIM_EN( 1), STATUS)
	  CALL PAR_GET0I( 'SUBIM_YST', SUBIM_ST( 2), STATUS)
	  CALL PAR_GET0I( 'SUBIM_YEN', SUBIM_EN( 2), STATUS)

	  IF( STATUS. NE. SAI__OK)THEN

            CALL ERR_REP('ERR', 'after PAR_GETs SUBIM_ST/EN', STATUS )
	    RETURN

	  END IF

* GET slice locator

	  CALL NDF_SECT( LOC_CURSOR_IMAGE, 2, SUBIM_ST,
     :                   SUBIM_EN, LOC_SUBIM, STATUS)

	  IF( STATUS. NE. SAI__OK)THEN

            CALL ERR_REP('ERR', 'after NDF_SECT', STATUS )
	    RETURN

	  END IF

* map data in 'DATA_ARRAY' to obtain memory pointer POINTER_CURSOR_IMAGE
* and size of 2D image in NAXIS

	  NAXIS( 1) = ( SUBIM_EN( 1) - SUBIM_ST( 1) + 1)
	  NAXIS( 2) = ( SUBIM_EN( 2) - SUBIM_ST( 2) + 1)

	  CALL NDF_MAP( LOC_SUBIM, 'DATA', '_REAL', 'READ',
     :                  POINTER_CURSOR_IMAGE, NELEMENTS, STATUS)

	  IF( STATUS. NE. SAI__OK)THEN

            CALL ERR_REP('ERR', 'During NDF_MAP sub-image', STATUS )
	    RETURN

	  END IF
	END IF

* get the option that plots images flipped EW

	CALL PAR_GET0C( 'IMAGE_ORIENT', IMAGE_ORI, STATUS)

* initialize cursor value to rubbish

	CURSOR_VALUE = -9999.0

* test if cursor image same size as displayed image

	IF( NAXIS( 1) .EQ. NX .AND. NAXIS( 2) .EQ. NY) THEN

* test if cursor position on image and on plotting area

	  IF(   ( XRPOS .GE. IM_XST) .AND. ( XRPOS .LE. IM_XEN) .AND.
     :	        ( YRPOS. GE. IM_YST) .AND. ( YRPOS. LE. IM_YEN)) THEN

* calculate magnification from start,end values and image size

	    MAGNIF = (IM_XEN - IM_XST) / REAL(NAXIS( 1))

* test if image orientation flipped for plotting

	    IF( IMAGE_ORI .EQ. 'IRCAM') THEN

* calculate real position of cursor in image when image flipped EW

	      X_PIXEL = NAXIS( 1) - INT(( XRPOS - IM_XST)/ MAGNIF) + 1
	      Y_PIXEL = NAXIS( 2) - INT(( YRPOS - IM_YST)/ MAGNIF) + 1

	    ELSE

* calculate real position of cursor in image

	      X_PIXEL = INT(( XRPOS - IM_XST)/ MAGNIF) + 1
	      Y_PIXEL = INT(( YRPOS - IM_YST)/ MAGNIF) + 1

	    END IF

* test if inside array boundaries

	    IF(   ( X_PIXEL .GE. 1) .AND. ( X_PIXEL .LE. NAXIS( 1)) .AND.
     :	          ( Y_PIXEL .GE. 1) .AND. ( Y_PIXEL .LE. NAXIS( 2))) THEN

* find pixel value corresponding to pixel X_PIXEL,Y_PIXEL

	      CALL ACCESS_PIXEL( %VAL( POINTER_CURSOR_IMAGE),
     :	                         X_PIXEL, Y_PIXEL, CURSOR_VALUE)

	      IF( X_PIXEL .EQ. 1 .AND. Y_PIXEL .EQ. 1) THEN

* pixel idicates end so flag it

	        BADPOS = .TRUE.

	      ELSE

* pixel OK so flag it

	        BADPOS = .FALSE.

	      END IF

	    ELSE

* bad pixel entered so flag it

	      BADPOS = .TRUE.

	    END IF
	  ELSE

* put cursor pixel to null value if calculated pixel position outside
* current image limits

	    CURSOR_VALUE = -9999.0
	    BADPOS = .TRUE.

	  END IF

	ELSE

* here if cursor image not equal in size to display image

          CALL MSG_OUT('ERR', 'Cursor image size incorrect', STATUS )

	END IF

* release data depending on whether full and sub-image plotted

	IF( SUBIM_OPTION .EQ. 'S') THEN

* release locators from image

	  CALL NDF_ANNUL( LOC_SUBIM, STATUS)

	  IF( STATUS .NE. SAI__OK) THEN

            CALL ERR_REP('ERR', 'after NDF_ANNUL sub-image', STATUS )
	    RETURN

	  END IF

	END IF

        CALL NDF_ANNUL( LOC_CURSOR_IMAGE, STATUS)

        IF( STATUS .NE. SAI__OK) THEN

          CALL ERR_REP('ERR', 'after NDF_ANNULs', STATUS )
	  RETURN

	END IF

	RETURN
	END
