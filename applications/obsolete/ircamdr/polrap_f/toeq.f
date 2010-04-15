*+  TOEQ - rotates position angle to equatorial system

	SUBROUTINE TOEQ ( STATUS )

*    Description :

*    Parameters :

*    Method :

*    Authors :

*    History :
*       22-Aug-1994  Changed CMP, DAT calls to NDF (SKL@JACH)
*    Type Definitions :

	IMPLICIT NONE

*    Global constants :

	INCLUDE 'SAE_PAR'
        INCLUDE 'NDF_PAR'

*    Status :

	INTEGER STATUS

*    Local constants :

	INTEGER NDIM
	PARAMETER ( NDIM = 2 ) ! dimensionality of input/output images

*    Local variables :

	INTEGER
     :    LOCIT,               ! locator for input data structure
     :	  LOCIP,               !
     :	  LOCOT,               !
     :    DIMST( NDIM ),! dimensions of the input/output DATA_ARRAYs
     :    DIMSP( NDIM ),! dimensions of the input/output DATA_ARRAYs
     :    PNTRIT,       ! pointer to : input DATA_ARRAY
     :    PNTRIP,       ! pointer to : input DATA_ARRAY
     :    PNTROT,       ! pointer to : output DATA_ARRAY
     :    EL,           ! Number of elements mapped by NDF_MAP
     :    NDIMS         ! Total number of dimensions from NDF_DIM

	REAL
     :	  THETACOR      !

*-
*      get locator to input IMAGE type data structure
	CALL GETINP( 'INPICP', LOCIP, STATUS )
	CALL GETINP( 'INPICT', LOCIT, STATUS )

*      check for error
	IF( STATUS .EQ. SAI__OK ) THEN

*        map input DATA_ARRAY component and get dimensions
          CALL NDF_MAP( LOCIP, 'DATA', '_REAL', 'READ', PNTRIP, EL,
     :                  STATUS )
          CALL NDF_DIM( LOCIP, NDIM, DIMSP, NDIMS, STATUS )
          CALL NDF_MAP( LOCIT, 'DATA', '_REAL', 'READ', PNTRIT, EL,
     :                  STATUS )
          CALL NDF_DIM( LOCIT, NDIM, DIMST, NDIMS, STATUS )


*        check for error
	  IF( STATUS .EQ. SAI__OK) THEN

*          see if dimensions of input images are same
	    IF( DIMSP( 1) .EQ. DIMST( 1) .AND.
     :	        DIMSP( 2) .EQ. DIMST( 2)) THEN

*            tell user image size
	      CALL MSG_SETI( 'NX', DIMSP( 1))
	      CALL MSG_SETI( 'NY', DIMST( 2))
	      CALL MSG_OUT( 'MESS',
     :         'Images are ^NX by ^NY pixels in size', STATUS )

*            get the position angle correction for rotation to EQ
              CALL AIF_GET0R( 'THETACOR', 0.0, 0.0, 180.0, THETACOR,
     :	                      STATUS )

*            see if error
	      IF( STATUS .EQ. SAI__OK) THEN

*              create output image
	        CALL CREOUT( 'OUTPIC', 'OTITLE', NDIM, DIMSP, LOCOT,
     :	                     STATUS )

*              map output image
                CALL NDF_MAP( LOCOT, 'DATA', '_REAL', 'WRITE', PNTROT,
     :                  EL, STATUS )

*              see if error
	        IF( STATUS .EQ. SAI__OK) THEN

*                call subroutine to do work
	          CALL TOEQSUB( DIMSP( 1), DIMSP( 2), %VAL( PNTRIP),
     :	                        %VAL( PNTRIT), %VAL( PNTROT), THETACOR,
     :	                        STATUS)

	        END IF

*              release output image
	        CALL NDF_ANNUL(  LOCOT, STATUS )

	      END IF

	    ELSE

*            tell user image diemsions are wrong
	      CALL MSG_OUT( 'ERR',
     :	        'Error, input images different sizes', STATUS)
	      CALL MSG_SETI( 'XP', DIMSP( 1))
	      CALL MSG_SETI( 'YP', DIMSP( 2))
	      CALL MSG_OUT( 'ERR',
     :	        'P-IMAGE is ^XP by ^YP pixels', STATUS)
	      CALL MSG_SETI( 'XT', DIMST( 1))
	      CALL MSG_SETI( 'YT', DIMST( 2))
	      CALL MSG_OUT( 'ERR',
     :	        'T-IMAGE is ^XT by ^YT pixels', STATUS)

	    END IF
	  END IF

*        tidy up structure
	  CALL NDF_ANNUL(  LOCIP, STATUS )
	  CALL NDF_ANNUL(  LOCIT, STATUS )

	END IF

	END
