*+  WRAPCOR - corrects for bit wrap around by adding number to values
*             below specified input level.

      SUBROUTINE WRAPCOR ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL WRAPCOR ( STATUS )
*
*    Parameters :
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image data structure
*     If no error so far then
*        Map the input DATA_ARRAY component
*        Create output image data structure
*        If no error so far then
*           Map an output DATA_ARRAY component
*           If no errors then
*              get value below which number added
*              get number added to values below input level
*              do work
*           Endif
*           Tidy up output structure
*        Endif
*        Tidy up input structure
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
     :  LOCI,                 ! locator for input IMAGE structure
     :  LOCO,                 ! locator for output IMAGE structure
     :  DIMS( NDIMS ),        ! dimensions of input DATA_ARRAYs
     :  ODIMS( NDIMS ),       ! dimensions of output DATA_ARRAYs
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRO,                ! pointer to output DATA_ARRAY
     :  PNTRI                 !    "     " input      "

      REAL
     :  DATAVAL,              ! value below which number is added
     :  VALADD                ! value added to data below input level

*-
*    check for error on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       map input DATA_ARRAY component and get dimensions
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

*       set the output image dimensions
         ODIMS( 1) = DIMS( 1)
	 ODIMS( 2) = DIMS( 2)

*       create the output image and get a title for it
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*          find and map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                    PNTRO, NELEMENTS, STATUS )

*          check for error before getting exclusion region and
*          accessing pointers
            IF( STATUS .EQ. SAI__OK ) THEN

*            ask user for level for addtion and value added
	      CALL PAR_GET0R( 'DATAVAL', DATAVAL, STATUS)
	      CALL PAR_GET0R( 'VALADD', VALADD, STATUS)

*            pass everything to the work routine
              CALL WRAPCORSUB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :	                       ODIMS( 1), ODIMS( 2), %VAL( PNTRO),
     :                         DATAVAL, VALADD,
     :	                       STATUS)

            END IF

*         release the ouput image
            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-after-getting-output check
         END IF

*       tidy up the input structure
         CALL NDF_ANNUL(  LOCI, STATUS )

*    end of if-no-error-after-getting-input check
      END IF

*    end
      END
