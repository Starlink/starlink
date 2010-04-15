	SUBROUTINE RONEXT( STATUS)

* Description :
*
* Invocation :
*     CALL RONEXT( STATUS)
*
* Parameters :
*
* Method :
*
* Bugs :
*     None known.
*
* Authors :
*     Sandy Leggett JACH ( JACH::SKL )
*
* History :
*     02-Mar-94   Gets number of extensions in an NDF file and names
*     25-Jul-94  Changed error reporting to ERR calls, removed VALUE (SKL@JACH)
*
* Type definitions :
	IMPLICIT  NONE			! no default typing allowed

* Global constants :
	INCLUDE 'SAE_PAR'		! SSE global definitions
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'
* Status :
	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

        INTEGER NEXT                    ! Number of extensions
	INTEGER LOC_IMAGE   		! locator for image
        INTEGER NUMBER                  ! Extension count

	CHARACTER*80 XNAME              ! Name of extension


*-
*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
           CALL ERR_REP('ERR','Bad status on RONEXT entry', STATUS )
	   RETURN
	END IF

*      associate container file
	CALL NDF_ASSOC( 'IMAGE_NAME', 'READ', LOC_IMAGE, STATUS )
	IF( STATUS .NE. SAI__OK) THEN
          CALL NDF_ANNUL( LOC_IMAGE, STATUS)
          CALL ERR_REP('ERR','Image not found', STATUS )
	  RETURN
	END IF


*      Get number of extensions
        CALL NDF_XNUMB( LOC_IMAGE, NEXT, STATUS)

        CALL MSG_SETI( 'NEXT', NEXT )
        CALL MSG_OUT( 'TOTAL',
     :             '   Number of extensions:   ^NEXT', STATUS)


        DO NUMBER = 1, NEXT

             CALL NDF_XNAME( LOC_IMAGE, NUMBER, XNAME, STATUS)

             CALL MSG_SETI( 'NUMBER', NUMBER )
             CALL MSG_OUT( 'NAME',
     :      '   Name of extension ^NUMBER :  '// XNAME, STATUS)


        END DO


*      tidy up the output container file

	CALL NDF_ANNUL( LOC_IMAGE, STATUS)

	END
