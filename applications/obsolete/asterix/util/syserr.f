*+  SYSERR - Adds constant fractional term to variance component.
      SUBROUTINE SYSERR( STATUS )
*
*   Description :
*
*     A constant fractional systematic error is added to the variance component
*     of a binned dataset.
*
*   Parameters :
*
*     INP=UNIV(U)
*           Dataset to be updated
*     ERR=REAL(R)
*           percentage error to be added
*     OVER=LOGICAL(R)
*           Overwriting to take place ( default is N )
*
*   Method :
*   Deficiencies :
*   Bugs :
*   Authors :
*     Trevor Ponman  (TJP::BHVAD)
*     David Allan    (DJA::BHVAD)
*
*   History :
*
*     2  Apr 85 : Original
*     25 Sep 86 : V0.5-1 ERRSUB renamed (BHVAD::JCMP)
*     21 Jul 88 : V1.0-0 Rewritten for new STARLINK definitions and ASTERIX88.
*                        Replacement of DATA_ERROR with VARIANCE and appropriate
*                        changes in algorithm (see SYSERR_ERRSUB). (dja)
*      5 Oct 88 : V1.0-1 OVER parameter added to enable user not to overwrite.
*                        ( dja )
*     12 Dec 88 : V1.0-1 USIs now used for user interface. (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     15 Feb 95 : V1.8-1 Use new data interface routines (DJA)
*
*   Type Definitions :
*
      IMPLICIT NONE
*
*   Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*   Status :
*
      INTEGER 			STATUS		! Run-time status
*
*   Local constants :
*
      INTEGER                   MAXLINES
         PARAMETER              (MAXLINES=10)
*
*   Local variables :
*
      CHARACTER*(ADI__SZTYP)	OBTYPE		! Type of data object
      CHARACTER*80 		ACTION(MAXLINES)! History file entries

      INTEGER                   ACTLEN          ! Length of ACTION sting
      INTEGER 			DIMS(ADI__MXDIM)! Dimensions of data
      INTEGER 			DIMSV(ADI__MXDIM)!Dimensions of variance
      INTEGER 			DPTR		! Pointer to data array
      INTEGER		  	IFID			! Input dataset
      INTEGER		 	NDIM	        ! Dimensionality of data
      INTEGER		 	NDIMV	        ! Dimensionality of variance
      INTEGER			NELM		! No of data/variance values
      INTEGER                   NREC            ! No. of history records
      INTEGER		  	OFID			! Output dataset
      INTEGER                   USE             ! History records no.
      INTEGER 			VPTR		! Pointer to variance array

      REAL 			PERCERR		! Percentage systematic error
      REAL 			SERR		! Fractional systematic error

      LOGICAL			OK		! General test
      LOGICAL 			NEWVARA		! New VARIANCE created?
      LOGICAL			PRIM            ! Is primitive input?
      LOGICAL                   OVERWRITE       ! Overwriting?
*
*    Version id :
*
      CHARACTER*(22)		VERSION
	 PARAMETER              ( VERSION='SYSERR Version 1.8-1' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT()

*    See whether we're going to overwrite
      CALL USI_GET0L( 'OVER', OVERWRITE, STATUS )

*    Obtain data object, access and check it
      IF ( OVERWRITE ) THEN
         CALL USI_TASSOCI( 'INP', '*', 'UPDATE', OFID, STATUS )
         CALL BDI_PRIM( OFID, PRIM, STATUS )
      ELSE
         CALL USI_TASSOC2(  'INP', 'OUT', 'READ', IFID, OFID, STATUS )
         CALL BDI_PRIM( IFID, PRIM, STATUS )
         CALL USI_SHOW( 'Output dataset {OUT}', STATUS )
         CALL ADI_FCOPY( IFID, OFID, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL BDI_TYPE( OFID, OBTYPE, STATUS )
      CALL MSG_SETC( 'OTYPE', OBTYPE )
      CALL MSG_PRNT( 'Data object is of type ^OTYPE' )

      IF ( PRIM ) THEN
	 STATUS = SAI__ERROR
	 CALL ERR_REP( ' ', 'ERROR : Cannot use primitive object.',
     :                 STATUS )

      ELSE
	 CALL BDI_CHKDATA( OFID, OK, NDIM, DIMS, STATUS )
	 IF ( OK ) THEN
	    CALL BDI_MAPDATA( OFID, 'READ', DPTR, STATUS )
	    CALL ARR_SUMDIM( NDIM, DIMS, NELM )

	 ELSE
	    STATUS = SAI__ERROR
	    CALL ERR_REP( ' ', 'ERROR: No data object found.', STATUS )

	 END IF

      END IF

*    Obtain size of systematic error
      CALL USI_GET0R( 'ERR', PERCERR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      SERR = 0.01 * PERCERR

*    See if VARIANCE is present. If it is check it and map it ; otherwise
*    create a new VARIANCE structure.
      CALL BDI_CHKVAR( OFID, OK, NDIMV, DIMSV, STATUS )

      IF ( OK ) THEN
         CALL BDI_MAPVAR( OFID, 'UPDATE', VPTR, STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
	 CALL BDI_CREVAR( OFID, NDIM, DIMS, STATUS )
         CALL MSG_PRNT( 'No variance present - new array created' )
         CALL BDI_MAPVAR( OFID, 'WRITE', VPTR, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      NEWVARA = ( .NOT. OK )

*    Pass to subroutine to add in systematic errors
      CALL SYSERR_ERRSUB( SERR, NELM, NEWVARA, %VAL(DPTR), %VAL(VPTR))

*    History file entry
      CALL HSI_ADD( OFID, VERSION, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      CALL MSG_SETR( 'PCERR', PERCERR )
      IF ( .NOT. OVERWRITE ) THEN
         ACTION(1) = 'Input dataset {INP}'
         USE = 2
      ELSE
         USE = 1
      END IF
      CALL MSG_MAKE( '^PCERR percent systematic error added',
     :                                  ACTION(USE), ACTLEN )

      NREC=MAXLINES
      CALL USI_TEXT( USE, ACTION, NREC, STATUS )
      CALL HSI_PTXT( OFID, NREC, ACTION, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  SYSERR_ERRSUB - Adds errors to previous errors
      SUBROUTINE SYSERR_ERRSUB( SERR, NDAT, NEWVAR, DAT, VAR)
*
*   Description :
*     If the variances are new then they are set to the square of the error.
*     Otherwise we add the square of the error (variances add algebraically).
*
*   Authors :
*     Trevor Ponman (BHVAD::TJP)
*
*   History :
*     -- --- -- : Original undocumented!
*     21 Jul 88 : Used to add systematic errors in quadrature. Now operates on
*                 VARIANCE (ie error squared) simplifying code a little (dja).
*
*   Type Definitions :
      IMPLICIT NONE
*   Import :
      INTEGER			NDAT		! No of variances

      LOGICAL			NEWVAR		! True if new variances

      REAL			SERR		! Fractional error to add
      REAL			DAT(NDAT)	! Data values
*
*   Export :
      REAL			VAR(NDAT)	! Result variances
*
*   Local variables :
      INTEGER			I		! Loop counter
*-

      IF ( NEWVAR ) THEN
	DO I=1,NDAT
	  VAR(I) = (SERR*DAT(I))**2
	END DO

      ELSE
	DO I=1,NDAT
	  VAR(I) = VAR(I) + ( SERR*DAT(I) )**2
	END DO

      END IF

      END
