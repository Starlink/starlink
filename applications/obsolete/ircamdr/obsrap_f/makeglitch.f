*+  MAKEGLITCH - makes bad pixel list from bad pixel mask

      SUBROUTINE MAKEGLITCH ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL MAKEGLITCH ( STATUS )
*
*    Parameters :
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image data structure
*     If no error so far then
*        Map the input DATA_ARRAY component
*        Open output bad pixel list file
*        Get bad pixels from image bad=1, good=0
*        If no errors then
*           Scan through input image
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
*     28-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
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
     :  LOCI,                 ! input IMAGE structure
     :  DIMS( NDIMS ),        ! dimensions of input DATA_ARRAYs
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRI                 !    "     " input      "


      CHARACTER*80
     :	OUTFILE

*-
*    check for error on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       map input DATA_ARRAY component

         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*         ask user for output bad pixel filename
	   CALL PAR_GET0C( 'OUTFILE', OUTFILE, STATUS)

*         pass everything to the work routine
           CALL MAKEGLITCHSUB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :	                       OUTFILE, STATUS)

         END IF

*       tidy up the input structure
         CALL NDF_ANNUL(  LOCI, STATUS )

*    end of if-no-error-after-getting-input check
      END IF

*    end
      END
