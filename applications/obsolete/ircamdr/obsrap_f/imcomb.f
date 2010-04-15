*+  IMCOMB - combines two images into one by replacement

      SUBROUTINE IMCOMB ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL IMCOMB ( STATUS )
*
*    Parameters :
*
*    Method :
*
*    Authors :
*
*     Colin Aspin (JACH::CAA)
*
*    History :
*
*     21/03/1991 : Original version  (JACH::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
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
     :  LOCI1,                 ! input IMAGE structure
     :  LOCI2,                 ! input IMAGE structure
     :  LOCO,                  ! output IMAGE structure
     :  DIMS1( NDIMS ),        ! dimensions of input DATA_ARRAYs
     :  DIMS2( NDIMS ),        ! dimensions of input DATA_ARRAYs
     :  ODIMS( NDIMS ),        ! dimensions of output DATA_ARRAYs
     :  ACTDIM,                ! actual dimensions from NDF_DIM
     :  NELEMENTS,             ! number of elements mapped by NDF_MAP
     :  PNTRI1,                !    "     " input      "
     :  PNTRI2,                !    "     " input      "
     :  PNTRO,                 ! pointer to output DATA_ARRAY
     :	J,
     :	NUMAREA,
     :	XST( 100),
     :	YST( 100),
     :	XSZ( 100),
     :	YSZ( 100)



*-
*      check for error on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	  RETURN
	END IF

*      get a locator to input IMAGE type data structure
	CALL GETINP( 'INPIC1', LOCI1, STATUS )

*      check for error
	IF( STATUS .EQ. SAI__OK ) THEN

*        map input DATA_ARRAY component and get dimensions
          CALL NDF_MAP( LOCI1, 'DATA', '_REAL', 'READ',
     :                  PNTRI1, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI1, NDIMS, DIMS1, ACTDIM, STATUS )

*        get a locator to input IMAGE type data structure
	  CALL GETINP( 'INPIC2', LOCI2, STATUS )

*        check for error
	  IF( STATUS .EQ. SAI__OK ) THEN

*          map input DATA_ARRAY component and get dimensions
            CALL NDF_MAP( LOCI2, 'DATA', '_REAL', 'READ',
     :                  PNTRI2, NELEMENTS, STATUS )
            CALL NDF_DIM( LOCI2, NDIMS, DIMS2, ACTDIM, STATUS )

*          test if input images are same size
	    IF( DIMS1( 1) .EQ. DIMS2( 1) .AND.
     :	        DIMS1( 2) .EQ. DIMS2( 2)) THEN

*            set the output image dimensions
	      ODIMS( 1) = DIMS1( 1)
	      ODIMS( 2) = DIMS1( 2)

*            create the output image and get a title for it
	      CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

*	     check for error
	      IF( STATUS .EQ. SAI__OK ) THEN

*              find and map output DATA_ARRAY component
                CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                        PNTRO, NELEMENTS, STATUS )

*              check for error before getting other values region and
*	       accessing pointers
	        IF( STATUS .EQ. SAI__OK ) THEN

*                get the number of areas to be taken from image 2 and
*                start and size
	          CALL PAR_GET0I( 'NUMAREA', NUMAREA, STATUS)
	          DO J = 1, NUMAREA
	            CALL PAR_GET0I( 'XST', XST( J), STATUS)
	            CALL PAR_GET0I( 'YST', YST( J), STATUS)
	            CALL PAR_GET0I( 'XSZ', XSZ( J), STATUS)
	            CALL PAR_GET0I( 'YSZ', YSZ( J), STATUS)
	            CALL PAR_CANCL( 'XST', STATUS)
	            CALL PAR_CANCL( 'YST', STATUS)
	            CALL PAR_CANCL( 'XSZ', STATUS)
	            CALL PAR_CANCL( 'YSZ', STATUS)
	            CALL MSG_OUT( 'BLANK', ' ', STATUS)
	          END DO

*		pass everything to the airmass correction routine
		  CALL IMCOMBSUB( DIMS1( 1), DIMS1( 2), %VAL( PNTRI1),
     :	                          %VAL( PNTRI2), %VAL( PNTRO), NUMAREA,
     :	                          XST, YST, XSZ, YSZ, STATUS)

		END IF

	      ELSE

	        CALL MSG_OUT( 'ERR',
     :	          'ERROR, input images are different sizes!!', STATUS)

	      END IF

	      CALL NDF_ANNUL( LOCO, STATUS )
	      CALL NDF_ANNUL(  LOCI1, STATUS )
	      CALL NDF_ANNUL(  LOCI2, STATUS )

	    END IF

	  END IF

	END IF

	END
