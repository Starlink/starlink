*+  GAUSSTH - smooth a 2-D image using a 2-D Gaussian filter below threshold

      SUBROUTINE GAUSSTH ( STATUS )

*    Description :
*
*     The input image, %INPIC, is smoothed using a 2-D gaussian of standard
*     deviation %SIGMA which must be in the range 0.1 to 5.0 pixels. Pixel
*     replication is used when smoothing is being performed at the edges of
*     the input image. The smoothed image is written to the output image
*     %OUTPIC, along with the title %OTITLE.
*
*     Only pixels with intrnsity below specified level are smoothed
*
*    Invocation :
*
*     CALL GAUSSTH ( STATUS )
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*           IMAGE structure containing 2-D array to be smoothed.
*     SIGMA  = REAL( READ )
*           Sigma of Gaussian to be used for the smoothing.
*     BOXSIZE = INTEGER( READ )
*           Size of box over which smooth is to operate
*     THRESH  = REAL ( READ )
*           Threshold below which pixels are smoothed
*     OUTPIC = IMAGE( WRITE )
*           IMAGE structure to contain 2-D array after smoothing.
*     OTITLE = CHAR( READ )
*           Will be used as the TITLE component of the output IMAGE structure.
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image data structure
*     If no error so far then
*        Map the input DATA_ARRAY component
*        Get value for SIGMA in range 0.1 to 5.0
*        Get value for the boxsize, suggesting 3-sigma
*        Get value for the threshold, suggesting 0
*        Create output image data structure
*        If no error so far then
*           Map an output DATA_ARRAY component
*           Allocate the workspace required by RAPGAU
*           If no errors then
*              Copy the input DATA_ARRAY into the output DATA_ARRAY
*              Call RAPGAU to perform smoothing
*           Endif
*           Tidy up workspace
*           Tidy up output structure
*        Endif
*        Tidy up input structure
*     Endif
*     End
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Mark McCaughrean (REVA::MJM)
*
*    History :
*
*     27/07/1983 : Original version                (ROE::ASOC5)
*     17/02/1984 : Modified to use TITLE component (ROE::ASOC5)
*     11-04-1986 : Modified to include choice of box size (REVA::MJM)
*     03-09-1989 : Modified to include threshold (JACH::CAA)
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
     :  DIMS( NDIMS ),        ! dimensions of input/output DATA_ARRAYs
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  ROLL( 2 ),            ! dimensions of 2-D workspace
     :  BOXSIZE,              ! side-length in pixels of smoothing box
     :  PNTRO,                ! pointer to output DATA_ARRAY
     :  PNTRI,                !    "     " input      "
     :  PLACE1,               ! place holder for temporary NDF
     :  PLACE2,               ! place holder for temporary NDF
     :  PLACE3,               ! place holder for temporary NDF
     :  PLACE4,               ! place holder for temporary NDF
     :  WPNTR1,               !    "     " workspace for RAPGAU
     :  WPNTR2,               !    "     "     "      "    "
     :  WPNTR3,               !    "     "     "      "    "
     :  WPNTR4,               !    "     "     "      "    "
     :  LBND( 2 )             ! lower bounds for temporary array

      DATA LBND / 1, 1 /

      INTEGER                 ! locators for :
     :  LOCI,                 ! input IMAGE structure
     :  LOCO,                 ! output IMAGE structure
     :  WLOC1,                ! workspace for RAPGAU routine
     :  WLOC2,                !     "      "     "      "
     :  WLOC3,                !     "      "     "      "
     :  WLOC4                 !     "      "     "      "

      REAL
     :  SIGMA,                ! sigma of the gaussian used for the smoothing
     :  THRESH                ! Smooth threshold

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

*       value of SIGMA must be between 0.1 and 5.0, suggest default of 1.0
         CALL AIF_GET0R( 'SIGMA', 1.0, 0.1, 5.0, SIGMA, STATUS )

*       get a smoothing box size - the box size must be an odd integer.
*       Suggest as a default a 3-sigma box. That is, the smallest odd
*       integer box size that contains a 3-sigma radius Gaussian profile.
         BOXSIZE  =  ( 2 * ( NINT( 3.0 * SIGMA ) ) ) + 1
         CALL AIF_GODDN( 'BOXSIZE', BOXSIZE, 1, 31, BOXSIZE, STATUS )

*       value of THRESH, suggest default of 0.0
         CALL AIF_GET0R( 'THRESH', 0.0, -1.0E20, 1.0E20, THRESH,
     :                    STATUS )

*       create the output image and get a title for it
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS, LOCO, STATUS )

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*          find and map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO, NELEMENTS, STATUS )

*          set up the workspace array dimensions
            ROLL( 1 ) = DIMS( 1 )
            ROLL( 2 ) = BOXSIZE

*          create and map temporary workspace

            CALL NDF_TEMP( PLACE1, STATUS )
            CALL NDF_NEW( '_REAL', NDIMS, LBND, ROLL, PLACE1, WLOC1,
     :                    STATUS )
            CALL NDF_MAP( WLOC1, 'DATA', '_REAL', 'WRITE', WPNTR1,
     :                    NELEMENTS, STATUS )

            CALL NDF_TEMP( PLACE2, STATUS )
            CALL NDF_NEW( '_REAL', 1, 1, BOXSIZE, PLACE2, WLOC2,
     :                    STATUS )
            CALL NDF_MAP( WLOC2, 'DATA', '_REAL', 'WRITE', WPNTR2,
     :                    NELEMENTS, STATUS )

            CALL NDF_TEMP( PLACE3, STATUS )
            CALL NDF_NEW( '_REAL', 1, 1, BOXSIZE, PLACE3, WLOC3,
     :                    STATUS )
            CALL NDF_MAP( WLOC3, 'DATA', '_REAL', 'WRITE', WPNTR3,
     :                    NELEMENTS, STATUS )

            CALL NDF_TEMP( PLACE4, STATUS )
            CALL NDF_NEW( '_REAL', 1, 1, DIMS(1), PLACE4, WLOC4,
     :                    STATUS )
            CALL NDF_MAP( WLOC4, 'DATA', '_REAL', 'WRITE', WPNTR4,
     :                    NELEMENTS, STATUS )

*          check for error before accessing pointers
            IF( STATUS .EQ. SAI__OK ) THEN

*             copy the input DATA_ARRAY into the output DATA_ARRAY
               CALL COPY2D( DIMS(1), DIMS(2), %VAL( PNTRI ),
     :                      %VAL( PNTRO ), STATUS )

*             pass everything to the rapid gaussian smoothing routine
               CALL RAPGAUTH(
     :                  SIGMA, BOXSIZE, THRESH, DIMS( 1), DIMS( 2),
     :	                %VAL( PNTRO ), %VAL( WPNTR1 ), %VAL( WPNTR2 ),
     :	                %VAL( WPNTR3 ), %VAL( WPNTR4 ), STATUS )

            ENDIF

*          tidy up the workspace and output structure
            CALL NDF_ANNUL( WLOC1, STATUS )
            CALL NDF_ANNUL( WLOC2, STATUS )
            CALL NDF_ANNUL( WLOC3, STATUS )
            CALL NDF_ANNUL( WLOC4, STATUS )
            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-after-getting-workspace-and-output check
         ENDIF

*       tidy up the input structure
         CALL NDF_ANNUL(  LOCI, STATUS )

*    end of if-no-error-after-getting-input check
      ENDIF


*    end
      END
