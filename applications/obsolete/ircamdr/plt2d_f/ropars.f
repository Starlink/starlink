	SUBROUTINE ROPARS( STATUS)

* Description :
*
* Invocation :
*     CALL ROPARS( STATUS)
*
* Parameters :
*     Note this and subroutine FITS_VAL assume FITS data stored as CHAR*80
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
*     17-Feb-94    (most) DAT routines changed to NDF (SKL@JACH)
*                  and slightly different parameters obtained
*                  from new data format
*     25-Jul-94  Changed error reporting to ERR clls, removed VALUE (SKL@JACH)
*     30-Dec=94  Added UTstart and UTend to fits items got (CAA@JACH)
*     11-Apr-94  Changed to use AB's new search routine (CAA@JACH)
*     14-Apr-95  Added MODE to fits items soughtand got (CAA@JACH)
*
* Type definitions :
	IMPLICIT  NONE			! no default typing allowed

* Global constants :
	INCLUDE 'SAE_PAR'		! SSE global definitions
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'
        INCLUDE 'DAT_PAR'               ! Necessary for non-VMS
        INCLUDE 'DAT_ERR'
        INCLUDE 'CHR_ERR'
* Status :
	INTEGER  STATUS			! global status parameter

* Local Constants :
!        INTEGER PARNUM                  ! number of obs parameters
!        PARAMETER ( PARNUM = 12 )
!        INTEGER NELEMENTS               ! Number of FITS array elements
!        PARAMETER ( NELEMENTS = 75)
!        INTEGER MINEL                   ! minimum acceptable size array
!        PARAMETER ( MINEL = 57)

* Local variables :
!        CHARACTER*(DAT__SZLOC) LOC_FITS ! HDS locator for FITS array
!	CHARACTER*80 ARRAY(NELEMENTS)   ! array of FITS info

!        INTEGER NUMBER                  ! For DO loop
	INTEGER          		!
     :    LOC_IMAGE,			! NDF locator for input file structure
!     :    PNTR,                         ! DAT pointer to mapped array
!     :	  EL,     	                ! number of elements mapped
!     :    PARLIS(PARNUM),               ! element of FITS array required
     :    INT_PAR                       !
!     :    VAL_LENGTH                    ! Length of trimmed FITS string

        REAL REAL_PAR                   ! Real value of parameter

!	CHARACTER*80 PARVAL(PARNUM)     ! Values of FITS array elements
        CHARACTER*80 CHAR_PAR           ! Character value of parameter

!	LOGICAL FILLED			! defines is an extension exits


*-
*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
           CALL ERR_REP('ERR', 'ERROR ON ROPARS ENTRY', STATUS )
	   RETURN
	END IF

*      associate with image file
	CALL NDF_ASSOC( 'IMAGE_NAME', 'READ', LOC_IMAGE, STATUS )
	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'IMAGE FILE NOT FOUND', STATUS )
          CALL NDF_ANNUL( LOC_IMAGE, STATUS )
	  RETURN
	END IF

*      check if FITS extension exists
!        CALL NDF_XSTAT( LOC_IMAGE, 'FITS', FILLED, STATUS)
!
!	IF( FILLED .NE. .TRUE. ) THEN
!          CALL MSG_OUT('ERR', 'EXTENSION TO FILE DOES NOT EXIST',
!     :                 STATUS )
!          CALL NDF_ANNUL( LOC_IMAGE, STATUS )
!	  RETURN
!	END IF

*      get access to FITS info array

!        CALL NDF_XLOC( LOC_IMAGE, 'FITS', 'READ', LOC_FITS, STATUS)
!	IF( STATUS .NE. SAI__OK) THEN
!          CALL ERR_REP('ERR',
!     :                 'Error getting access to FITS array - NDF_XLOC',
!     :                 STATUS )
!          CALL DAT_ANNUL( LOC_FITS, STATUS )
!          CALL NDF_ANNUL( LOC_IMAGE, STATUS )
!	  RETURN
!        END IF


*      use DAT_ routines to read primitive array

!        CALL DAT_MAPV( LOC_FITS, '_CHAR*80', 'READ', PNTR,
!     :                 EL, STATUS)


!	IF( (STATUS .NE. SAI__OK) .OR. (EL .LT. MINEL) ) THEN
!          CALL MSG_OUT('ERR',
!     :          'Error mapping FITS array or too few FITS elements',
!     :                 STATUS )
!          CALL DAT_ANNUL( LOC_FITS, STATUS )
!          CALL NDF_ANNUL( LOC_IMAGE, STATUS )
!	  RETURN
!        END IF

*      get location of required parameters in FITS array


!        CALL PAR_GET0I( 'FITS_AMST', PARLIS(1), STATUS)
!        CALL PAR_GET0I( 'FITS_AMEND', PARLIS(2), STATUS)
!        CALL PAR_GET0I( 'FITS_EXP', PARLIS(3), STATUS)
!        CALL PAR_GET0I( 'FITS_NEXP', PARLIS(4), STATUS)
!        CALL PAR_GET0I( 'FITS_FILTER', PARLIS(5), STATUS)
!        CALL PAR_GET0I( 'FITS_OBJECT', PARLIS(6), STATUS)
!        CALL PAR_GET0I( 'FITS_RAOFF', PARLIS(7), STATUS)
!        CALL PAR_GET0I( 'FITS_DECOFF', PARLIS(8), STATUS)
!        CALL PAR_GET0I( 'FITS_PIXSIZE', PARLIS(9), STATUS)
!        CALL PAR_GET0I( 'FITS_MAGNIFY', PARLIS(10), STATUS)
!        CALL PAR_GET0I( 'FITS_RUTSTART', PARLIS(11), STATUS)
!        CALL PAR_GET0I( 'FITS_RUTEND', PARLIS(12), STATUS)

*      get values of observational parameters

!        DO NUMBER = 1, PARNUM
!          IF ( (PARLIS(NUMBER) .GT. NELEMENTS) .OR.
!     :          (PARLIS(NUMBER) .LT. 1 ) ) THEN
!             PARVAL(NUMBER) = ' '
!          ELSE
!
!             CALL FITS_VAL( EL, %VAL(PNTR), PARLIS(NUMBER),
!     :                      PARVAL(NUMBER), VAL_LENGTH, STATUS )
!
D            WRITE (6,*) ( 'VALUE   ', PARVAL(NUMBER))
!
!          END IF
!        END DO

*      get parameters from FITS list using AB's routine and set it in
*      interface file
        CALL NDFX_GET_FITSR (LOC_IMAGE, 'AMSTART', REAL_PAR, STATUS)
!        CALL CHR_CTOR( PARVAL(1), REAL_PAR, STATUS )
	CALL PAR_PUT0R( 'AIRMASS_START', REAL_PAR, STATUS)

        CALL NDFX_GET_FITSR (LOC_IMAGE, 'AMEND', REAL_PAR, STATUS)
!        CALL CHR_CTOR( PARVAL(2), REAL_PAR, STATUS )
	CALL PAR_PUT0R( 'AIRMASS_END', REAL_PAR, STATUS)

        CALL NDFX_GET_FITSR (LOC_IMAGE, 'DEXPTIME', REAL_PAR, STATUS)
!        CALL CHR_CTOR( PARVAL(3), REAL_PAR, STATUS )
	CALL PAR_PUT0R( 'EXPOSURE_TIME', REAL_PAR, STATUS)

        CALL NDFX_GET_FITSI (LOC_IMAGE, 'NEXP', INT_PAR, STATUS)
!        CALL CHR_CTOI( PARVAL(4), INT_PAR, STATUS )
	CALL PAR_PUT0I( 'NUMBER_EXP', INT_PAR, STATUS)

        CALL NDFX_GET_FITSC (LOC_IMAGE, 'FILTER', CHAR_PAR, STATUS)
	CALL PAR_PUT0C( 'FILTER', CHAR_PAR, STATUS)

        CALL NDFX_GET_FITSC (LOC_IMAGE, 'OBJECT', CHAR_PAR, STATUS)
	CALL PAR_PUT0C( 'OBJECT_NAME', CHAR_PAR, STATUS)

        CALL NDFX_GET_FITSR (LOC_IMAGE, 'RAOFF', REAL_PAR, STATUS)
!        CALL CHR_CTOR( PARVAL(7), REAL_PAR, STATUS )
	CALL PAR_PUT0R( 'RA_OFF', REAL_PAR, STATUS)

        CALL NDFX_GET_FITSR (LOC_IMAGE, 'DECOFF', REAL_PAR, STATUS)
!        CALL CHR_CTOR( PARVAL(8), REAL_PAR, STATUS )
	CALL PAR_PUT0R( 'DEC_OFF', REAL_PAR, STATUS)

        CALL NDFX_GET_FITSR (LOC_IMAGE, 'PIXELSIZ', REAL_PAR, STATUS)
!        CALL CHR_CTOR( PARVAL(9), REAL_PAR, STATUS )
	CALL PAR_PUT0R( 'PIX_SIZE', REAL_PAR, STATUS)

        CALL NDFX_GET_FITSC (LOC_IMAGE, 'MAGNIFIE', CHAR_PAR, STATUS)
	CALL PAR_PUT0C( 'MAGNIFIER', CHAR_PAR, STATUS)

        CALL NDFX_GET_FITSR (LOC_IMAGE, 'RUTSTART', REAL_PAR, STATUS)
!        CALL CHR_CTOR( PARVAL(11), REAL_PAR, STATUS )
	CALL PAR_PUT0R( 'UTSTART', REAL_PAR, STATUS)

        CALL NDFX_GET_FITSR (LOC_IMAGE, 'RUTEND', REAL_PAR, STATUS)
!        CALL CHR_CTOR( PARVAL(12), REAL_PAR, STATUS )
	CALL PAR_PUT0R( 'UTEND', REAL_PAR, STATUS)

        CALL NDFX_GET_FITSC (LOC_IMAGE, 'MODE', CHAR_PAR, STATUS)
	CALL PAR_PUT0C( 'MODE', CHAR_PAR, STATUS)

	IF( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP('ERR',
     :                 'WARNING: ERROR AFTER PUTTING PARAMETER VALUES',
     :                 STATUS )
        END IF

*      unmap and release locators

!	CALL DAT_UNMAP( LOC_FITS, STATUS)
!	CALL DAT_ANNUL( LOC_FITS, STATUS)
	CALL NDF_ANNUL( LOC_IMAGE, STATUS)


	END

