*+  CSGEN - generates a centro-symmetric polarization pattern

      SUBROUTINE CSGEN ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL CSGEN ( STATUS )
*
*    Parameters :
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get size of output image
*     If no error so far then
*        Create output image data structure
*        If no error so far then
*           Map an output DATA_ARRAY component
*           If no errors then
*              Get input x,y centre of cs pattern
*              do work
*           Endif
*           Tidy up output structure
*        Endif
*     Endif
*     End
*
*    Authors :
*
*     Colin Aspin (JACH::CAA)
*
*    History :
*
*     14/08/1989 : Original version  (JACH::CAA)
*     18-May-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE           ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'       ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS          ! global status parameter

*    Local constants :

      INTEGER NDIMS           ! dimensionality of images
      PARAMETER ( NDIMS = 2 ) ! 2-d only

*    Local variables :

      INTEGER
     :  LOCO,                 ! locator for output IMAGE structure
     :  ODIMS( NDIMS ),       ! dimensions of output DATA_ARRAYs
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRO,                ! pointer to output DATA_ARRAY
     :  XCENTRE,
     :  YCENTRE



*-
*      check for error on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	  RETURN
	END IF

*      get output image size
	CALL PAR_GET0I( 'XSIZE', ODIMS( 1), STATUS)
	CALL PAR_GET0I( 'YSIZE', ODIMS( 2), STATUS)

*      create the output image and get a title for it
	CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

*      check for error
	IF( STATUS .EQ. SAI__OK ) THEN

*        find and map output DATA_ARRAY component

          CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO, NELEMENTS, STATUS )

*        check for error before getting centre of cs pattern
	  IF( STATUS .EQ. SAI__OK ) THEN

*          ask user for centre pixel of cs pattern
	    CALL PAR_GET0I( 'XCENTRE', XCENTRE, STATUS)
	    CALL PAR_GET0I( 'YCENTRE', YCENTRE, STATUS)

*          pass everything to the work routine
	    CALL CSGENSUB( ODIMS( 1), ODIMS( 2), %VAL( PNTRO),
     :	                   XCENTRE, YCENTRE, STATUS)

	  END IF

*        release the ouput image
	  CALL NDF_ANNUL( LOCO, STATUS )

*      end of if-no-error-after-getting-output check
	END IF

*      end
	END
