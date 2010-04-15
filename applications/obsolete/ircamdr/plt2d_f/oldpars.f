*+  OLDPARS - gets container file parameters and stuffs them into
*   paramter system

	SUBROUTINE OLDPARS( STATUS)

* Description :
*
* Invocation :
*     CALL OLDPARS( STATUS)
*
* Parameters :
*
* Method :
*
* Bugs :
*     None known.
*
* Authors :
*     Colin Aspin ROE ( JACH::CAA )
*
* History :
*     16-08-1988 : First implementation (JACH::CAA)
*     07-09-1993 : THIS VERSION FROM LINCONT (JACH::CAA)
*     12-MAY-1994  Changed routine and par names to match new formats (SKL@JACH)
*     25-Jul-1994  Changed LIB$SIGNAL to ERR_REP (SKL@JACH)
*     27-JUL-1994  Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
*
* Type definitions :
	IMPLICIT  NONE			! no default typing allowed

* Global constants :
	INCLUDE 'SAE_PAR'		! SSE global definitions
        INCLUDE 'DAT_PAR'               ! Necessary for non-VMS
	INCLUDE 'DAT_ERR'		! DAT subroutines error messages

* Status :
	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :
	REAL EXPOSURE_TIME              ! exposure time
	REAL AIRMASS                    ! for airmass start/end parameters

	INTEGER DIMS( 2)		! dimensions array
	INTEGER NDIMS			! number of dimensions variable
	INTEGER IMAGE_NO		! number of element in OBS structure
	INTEGER NUMBER_ELEMENTS		! number of elements in OBS structure
	INTEGER NUMBER_COADDS 		! for number_exp parameter
	INTEGER SUBS( 2)                ! subscripts for playing with OBS
       	INTEGER STAT			! status for annuls

	CHARACTER*80 INNAME		! name of input container file
	CHARACTER*80 FILTER
	CHARACTER*80 OBJECT_NAME

	LOGICAL FILLED			! defines is an observation is filled

	CHARACTER*( DAT__SZLOC)		! locators for :
     :    LOCTOP_O,			! input container file structure
     :    LOCGEN_O,			! general structure
     :	  LOCINST_O,			! instrument structure
     :    LOCID_O,			! id structure
     :    LOCOBS_O,			! obs structure
     :	  LOCTELE_O,			! telescope structure
     :    LOCCELL_O,			! cell locator
     :	  LOCPRIM_O			! locator for primitive element
*-
*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	   CALL ERR_REP( 'ERR', 'Error on OLDPARS entry', STATUS )
	   RETURN
	END IF

*      get the input image name
	CALL PAR_GET0C( 'CONTNAME', INNAME, STATUS)
	IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'ERR', 'Error getting input', STATUS )
	  RETURN
	END IF

*      get number of element for parameters getting
	CALL PAR_GET0I( 'IMAGE_NO', IMAGE_NO, STATUS)
	IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'ERR', 'Error getting image number', STATUS )
	  RETURN
	END IF

*      associate container file
	CALL DAT_ASSOC( 'CONTAINER', 'READ', LOCTOP_O, STATUS )
	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'NO CONTAINER FILE BY THE NAME FOUND',
     :                  STATUS )
	  RETURN
	END IF

*      find the top level locators for the GENERAL, ID and OBS structures
	CALL DAT_FIND( LOCTOP_O, 'GENERAL', LOCGEN_O, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :             'NO GENERAL STRUCTURE IN SPECIFIED CONTAINER FILE ',
     :                  STATUS )
	  RETURN
	END IF
	CALL DAT_FIND( LOCGEN_O, 'INSTRUMENT', LOCINST_O, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :         'NO INSTRUMENT STRUCTURE IN SPECIFIED CONTAINER FILE ',
     :                  STATUS )
	  RETURN
	END IF
	CALL DAT_FIND( LOCGEN_O, 'ID', LOCID_O, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :            'NO ID STRUCTURE IN SPECIFIED CONTAINER FILE ',
     :                  STATUS )
	  RETURN
	END IF
	CALL DAT_FIND( LOCGEN_O, 'TELESCOPE', LOCTELE_O, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :         'NO TELESCOPE STRUCTURE IN SPECIFIED CONTAINER FILE ',
     :                  STATUS )
	  RETURN
	END IF
	CALL DAT_FIND( LOCTOP_O, 'OBS', LOCOBS_O, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :            'NO OBS STRUCTURE IN SPECIFIED CONTAINER FILE ',
     :                  STATUS )
	  RETURN
	END IF

*      get the size of the OBS structure to be scanned
	CALL DAT_SHAPE( LOCOBS_O, 2, DIMS, NDIMS, STATUS)
	NUMBER_ELEMENTS = DIMS( 1)

*      check that image is defined
	CALL REDCONT_CHECKDATA( IMAGE_NO, LOCOBS_O, FILLED, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :                  'DATA IN SPECIFIED OBSERVATION NOT DEFINED',
     :                  STATUS )
	  RETURN
	END IF

*      test whether image is filled
	IF( FILLED) THEN

*        get parameters
	  SUBS( 1) = IMAGE_NO
	  SUBS( 2) =0
	  CALL DAT_CELL( LOCOBS_O, 1, SUBS, LOCCELL_O, STATUS)
	  CALL DAT_FIND( LOCCELL_O, 'NUMBER_COADDS', LOCPRIM_O,
     :                   STATUS)
	  CALL DAT_GETI( LOCPRIM_O, 0, DIMS, NUMBER_COADDS, STATUS)
	  CALL DAT_ANNUL( LOCPRIM_O, STATUS)
	  CALL DAT_FIND( LOCCELL_O, 'EXPOSURE_TIME', LOCPRIM_O, STATUS)
	  CALL DAT_GETR( LOCPRIM_O, 0, DIMS, EXPOSURE_TIME, STATUS)
	  CALL DAT_ANNUL( LOCPRIM_O, STATUS)
	  CALL DAT_FIND( LOCCELL_O, 'FILTER', LOCPRIM_O, STATUS)
	  CALL DAT_GETC( LOCPRIM_O, 0, DIMS, FILTER, STATUS)
	  CALL DAT_ANNUL( LOCPRIM_O, STATUS)
	  CALL DAT_FIND( LOCCELL_O, 'AIRMASS', LOCPRIM_O, STATUS)
	  CALL DAT_GETR( LOCPRIM_O, 0, DIMS, AIRMASS, STATUS)
	  CALL DAT_ANNUL( LOCPRIM_O, STATUS)
	  CALL DAT_FIND( LOCCELL_O, 'OBJECT_NAME', LOCPRIM_O, STATUS)
	  CALL DAT_GETC( LOCPRIM_O, 0, DIMS, OBJECT_NAME, STATUS)
	  CALL DAT_ANNUL( LOCPRIM_O, STATUS)

*      set values in parameter file
	  CALL PAR_PUT0R( 'AIRMASS', AIRMASS, STATUS)
	  CALL PAR_PUT0R( 'EXPOSURE_TIME', EXPOSURE_TIME, STATUS)
	  CALL PAR_PUT0I( 'NUMBER_COADDS', NUMBER_COADDS, STATUS)
	  CALL PAR_PUT0C( 'FILTER', FILTER, STATUS)
	  CALL PAR_PUT0C( 'OBJECT_NAME', OBJECT_NAME, STATUS)
	END IF

*      tidy up the output container file

	STAT = SAI__OK
	CALL DAT_ANNUL( LOCCELL_O, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCOBS_O, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCID_O, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCGEN_O, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCINST_O, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTELE_O, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTOP_O, STAT)

	END
