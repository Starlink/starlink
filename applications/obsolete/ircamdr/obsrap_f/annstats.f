
*+  ANNSTATS - gives stats in annuli from specified point

      SUBROUTINE ANNSTATS ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL ANNSTATS ( STATUS )
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*           IMAGE structure containing 2-D array
*     XCENTRE = INTEGER( READ )
*           X-Centre of annuli
*     YCENTRE = INTEGER( READ )
*           Y-Centre of annuli
*     ECCENTRICITY = REAL( READ )
*           Eccenricity of annuli
*     POSITION ANGLE = REAL( READ )
*           Position angle wrt N of major axis
*     WIDTH = REAL( READ )
*           Width of annuli in arcseconds
*     PLATSCAL = REAL( READ )
*           Plate scale arcsec/pixel
*     OUTPIC = IMAGE( WRITE )
*           Output map array image
*
*    Method :
*
*    Authors :
*
*      Colin Aspin (UKTH::CAA)
*
*    History :
*
*     26-03-1990 : Original version
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     24-Jun-1994  Changed STR$ to CHR_ (SKL@JACH)
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
     :	LOCO,                 ! locator for output IMAGE structure
     :  DIMS( NDIMS ),        ! dimensions of input DATA_ARRAYs
     :  ODIMS( NDIMS ),       ! dimensions of output DATA_ARRAYs
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRI,                !    "     " input      "
     :  PNTRO,                !    "     " output     "
     :	XCENTRE,              ! x centre of annuli
     :	YCENTRE,              ! y centre of annuli
     :	LEN2                  ! length of string variable

      REAL
     :  WIDTH,                ! width of annuli in arcseconds
     :	PLATSCAL,             ! plate scale in arcsec/pixel
     :	BADVAL,               ! bad pixel value in image
     :	ECCENTRICITY,         ! eccentricity of annuli
     :	POSITION_ANGLE        ! position angle of major axis wrt E

      CHARACTER
     :	OUTFILE*80            ! ouput text file with results

      LOGICAL
     :	USEBAD                ! option to look for bad value in image

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

*       set dimensions of output array
	 ODIMS( 1) = DIMS( 1)
	 ODIMS( 2) = DIMS( 2)

*       create output array
	 CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

*       map output image
	 CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                         PNTRO, NELEMENTS, STATUS )

*       get the x,y centre of radial study and width in arcsec
         CALL AIF_GET0I( 'XCENTRE', 1, -10000, 10000, XCENTRE,
     :	                 STATUS )
         CALL AIF_GET0I( 'YCENTRE', 1, -10000, 10000, YCENTRE,
     :	                 STATUS )
         CALL AIF_GET0R( 'ECCENTRICITY', 0.0, 0.0, 0.999, ECCENTRICITY,
     :	                 STATUS )
         CALL AIF_GET0R( 'POSANG', 0.0, 0.0, 180.0, POSITION_ANGLE,
     :	                 STATUS )
         CALL AIF_GET0R( 'WIDTH', 5.0, 0.01, 100.0, WIDTH,
     :	                 STATUS )
         CALL AIF_GET0R( 'PLATSCAL', 1.0, 0.01, 20.0, PLATSCAL,
     :	                 STATUS )

*       get option to use bad value in compiling pixel list
         CALL PAR_GET0L( 'USEBAD', USEBAD, STATUS )
	 IF( USEBAD) THEN
           CALL PAR_GET0R( 'BADVAL', BADVAL, STATUS )
	 END IF

*       get the output filename
	 CALL PAR_GET0C( 'OUTFILE', OUTFILE, STATUS)
         CALL CHR_CLEAN( OUTFILE )
         LEN2 = 0
	 CALL CHR_APPND( OUTFILE, OUTFILE, LEN2)

*       check for error before accessing pointers
         IF( STATUS .EQ. SAI__OK ) THEN

*          call subroutine to create radial cut image
	    CALL ANNSTATSSUB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :	                      ODIMS( 1), ODIMS( 2), %VAL( PNTRO),
     :	                      XCENTRE, YCENTRE, ECCENTRICITY,
     :	                      POSITION_ANGLE, WIDTH, PLATSCAL,
     :	                      USEBAD, BADVAL, OUTFILE( 1:LEN2))

         END IF

*       tidy up the input structure
         CALL NDF_ANNUL(  LOCI, STATUS )

*       tidy up the output structure
         CALL NDF_ANNUL(  LOCO, STATUS )

*    end of if-no-error-after-getting-input check
      END IF

      END
