*+  CALCOL - calculates colour from input magnitude images

      SUBROUTINE CALCOL ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL CALCOL ( STATUS )
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
*     14/08/1989 : Original version  (JACH::CAA)
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
     :  PNTRO                  ! pointer to output DATA_ARRAY

      REAL
     :	LSB1,                  ! limiting surface brightness/sq "
     :  LSB2,                  !    "        "         "
     :	PLATESCALE             !

*-
*    check for error on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
        RETURN
      END IF

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC1', LOCI1, STATUS )
      CALL GETINP( 'INPIC2', LOCI2, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*      map input DATA_ARRAY component and get dimensions

         CALL NDF_MAP( LOCI1, 'DATA', '_REAL', 'READ',
     :                  PNTRI1, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI1, NDIMS, DIMS1, ACTDIM, STATUS )

         CALL NDF_MAP( LOCI2, 'DATA', '_REAL', 'READ',
     :                  PNTRI2, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI2, NDIMS, DIMS2, ACTDIM, STATUS )

*      test if input images same size
	IF( DIMS1( 1) .EQ. DIMS2( 1) .AND.
     :	    DIMS1( 2) .EQ. DIMS2( 2)) THEN

*        set the output image dimensions
	  ODIMS( 1) = DIMS1( 1)
	  ODIMS( 2) = DIMS1( 2)

*        create the output image and get a title for it
          CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

*        check for error
          IF( STATUS .EQ. SAI__OK ) THEN

*          find and map output DATA_ARRAY component

            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                    PNTRO, NELEMENTS, STATUS )

*          check for error accessing pointers
            IF( STATUS .EQ. SAI__OK ) THEN

*            ask user for limiting surface brightness/sq "
	      CALL PAR_GET0R( 'LSB1', LSB1, STATUS)
	      CALL PAR_GET0R( 'LSB2', LSB2, STATUS)

*            ask user for plate scale arcsec/pixel
	      CALL PAR_GET0R( 'PLATESCALE', PLATESCALE, STATUS)

*            pass everything to the calculation routine
              CALL CALCOLSUB( DIMS1( 1), DIMS1( 2), %VAL( PNTRI1),
     :	                      %VAL( PNTRI2), %VAL( PNTRO), LSB1, LSB2,
     :	                      PLATESCALE, STATUS)

            END IF

	  END IF

*        release the ouput image
          CALL NDF_ANNUL( LOCO, STATUS )

	ELSE

	  CALL MSG_OUT( 'ERR',
     :	    'Error, input images different sizes',
     :	    STATUS)
	  CALL MSG_SETI( 'X1', DIMS1( 1))
	  CALL MSG_SETI( 'Y1', DIMS1( 2))
	  CALL MSG_OUT( 'ERR',
     :	    'Images 1 is ^X1,^Y1',
     :	    STATUS)
	  CALL MSG_SETI( 'X2', DIMS2( 1))
	  CALL MSG_SETI( 'Y2', DIMS2( 2))
	  CALL MSG_OUT( 'ERR',
     :	    'Images 2 is ^X2,^Y2',
     :	    STATUS)

        END IF

*      tidy up the input structure
        CALL NDF_ANNUL(  LOCI1, STATUS )
        CALL NDF_ANNUL(  LOCI2, STATUS )

      END IF

      END
