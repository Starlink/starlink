*+  MAKEBAD - makes bad pixel list from sigma cut

      SUBROUTINE MAKEBAD ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL MAKEBAD ( STATUS )
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
*        Get sigma of cut for bad pixels
*        If no errors then
*           Scan through input image setting pixels outside sigma to bad
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
     :  DIMS( NDIMS ),        ! dimensions of input DATA_ARRAYs
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRI                 !    "     " input      "

      REAL
     :  SIGMA                 ! sigma of cut for bad pixels

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

*       map input DATA_ARRAY component and get dimensions
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*         ask user for sigma level for cut for bad pixels
	   CALL PAR_GET0R( 'SIGMA', SIGMA, STATUS)

*         ask user for output bad pixel filename
	   CALL PAR_GET0C( 'OUTFILE', OUTFILE, STATUS)

*         pass everything to the work routine
           CALL MAKEBADSUB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :	                    SIGMA, OUTFILE, STATUS)

         END IF

*       tidy up the input structure
         CALL NDF_ANNUL(  LOCI, STATUS )

*    end of if-no-error-after-getting-input check
      END IF

*    end
      END
