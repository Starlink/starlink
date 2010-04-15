*+  HISTGEN - Generates a histogram image (with DATA_ARRAY component) from
*             a 2D image

	SUBROUTINE HISTGEN ( STATUS )

* Description :
*
* This routine ...
*
* Invocation : CALL HISTGEN ( STATUS )
*
* Parameters :
*
* Method :
*
* Bugs :
*
* None known.
*
* Authors : Colin Aspin ROE ( REVA::CAA )
*
* History :
*
*  30-04-1986 :  First implementation (REVA::CAA)
*  24-05-1986 :  Added the storage of the X axis scale in DATA_ARRAY( i,2)
*                (REVA::CAA)
*  25-05-1986 :  Added the option to get terminal output of histogram
*                (REVA::CAA)
*  20-Apr-1994   Changed DAT and CMP calls to NDF (SKL@JACH)
*  12-AUG-1994   Changed input DIMS for GENHIS (SKL@JACH)
*  11-OCT-1994   Changed input DIMS for HISTOGRAM_SAVE (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE		  ! no implicit typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'	  ! SSE global definitions
        INCLUDE  'NDF_PAR'
        INCLUDE  'NDF_ERR'

* Status :

	INTEGER  STATUS		  ! global status parameter

* Local constants :

	INTEGER NDIMS		  ! input image dimensionality

	PARAMETER ( NDIMS = 2 )

* Local variables :

	INTEGER
     :    LOCI,             ! locator for input data structure
     :    LOCO,             ! locator for output data structure
     :	  HIST_BINS,        ! number of bins in calculated histogram
     :	  HISTOGRAM(100000),! array for the histogram distribution
     :    IDIMS( NDIMS ),   ! dimensions of input DATA_ARRAY
     :    ODIMS( NDIMS ),   ! dimensions of output DATA_ARRAY
     :    ACTDIM,           ! actual dimensions from NDF_DIM
     :    NELEMENTS,        ! number of elements mapped by NDF_MAP
     :    PNTRI,	    ! pointer to input DATA_ARRAY component
     :    PNTRO,	    ! pointer to output DATA_ARRAY component
     :    X_END,            ! end of the histogram sub-image in X
     :    X_START,	    ! sub-image X start pixel
     :	  X_SIZE,	    ! sub-image X size
     :	  Y_END,            ! end of the histogram sub-image in Y
     :	  Y_START,          ! sub-image Y start pixel
     :	  Y_SIZE            ! sub-image Y size

	REAL
     :	  HIST_MAX,	    ! maximum DN number for histogram bins
     :	  HIST_MIN,         ! minimum DN number for hitogram bins
     :	  SUGGESTED_MAX,    ! suggested maximum value for histogram
     :	  SUGGESTED_MIN     ! suggested minimum value for histogram

	LOGICAL
     :	  TERMINAL_OUTPUT  ! Option to type the output histogram to the terminal
*-
*    check status on entry - return if not o.k.
*
	IF ( STATUS .NE. SAI__OK ) THEN

	   RETURN

	END IF
*
* get locator to input IMAGE type data structure
*
	CALL GETINP( 'INPIC', LOCI, STATUS)

	IF ( STATUS .EQ. SAI__OK ) THEN
*
* get the number of bins to be calculated
*
	  CALL PAR_GET0I( 'HIST_BINS', HIST_BINS, STATUS)
*
* map in its DATA_ARRAY component
*
          CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI, NDIMS, IDIMS, ACTDIM, STATUS )

	  IF ( STATUS .EQ. SAI__OK ) THEN
*
* output the dimensions of the input image to the user
*
	    CALL MSG_SETI( 'XDIM', IDIMS(1))
	    CALL MSG_SETI( 'YDIM', IDIMS(2))

	    CALL MSG_OUT( 'INPUT_DIMS', 'Input image is ^XDIM by ^YDIM pixels',
     :	                  STATUS)
*
* suggest default values of the maximum imagew size for sub-image
*
	    CALL PAR_DEF0I( 'X_START', 1, STATUS)
	    CALL PAR_DEF0I( 'X_SIZE', IDIMS( 1), STATUS)
	    CALL PAR_DEF0I( 'Y_START', 1, STATUS)
	    CALL PAR_DEF0I( 'Y_SIZE', IDIMS( 2), STATUS)
*
* get the area of the image to use in histogram calculation
*
	    CALL PAR_GET0I( 'X_START', X_START, STATUS)
	    CALL PAR_GET0I( 'Y_START', Y_START, STATUS)
	    CALL PAR_GET0I( 'X_SIZE', X_SIZE, STATUS)
	    CALL PAR_GET0I( 'Y_SIZE', Y_SIZE, STATUS)

	    IF( STATUS .EQ. SAI__OK) THEN
*
* calculate the X and Y end position in the array from the X,Y start and size
*
	      X_END = X_START + X_SIZE - 1
	      Y_END = Y_START + Y_SIZE - 1
*
* calculate maximum and minimum in sub-image and suggest as defaults for
* HIST_MAX and HIST_MIN parameters. The suggested defaults are in varaibles
* SUGGESTED_MAX and SUGGESTED_MIN
*
	      CALL HIST_MAXMIN(
     :                IDIMS( 1), IDIMS( 2), %VAL( PNTRI), X_START,
     :	              Y_START, X_END, Y_END, SUGGESTED_MAX,
     :	              SUGGESTED_MIN)

	      CALL PAR_DEF0R( 'HIST_MAX', SUGGESTED_MAX, STATUS)
	      CALL PAR_DEF0R( 'HIST_MIN', SUGGESTED_MIN, STATUS)
*
* get the maximum and minimum of the histogram calculation
*
	      CALL PAR_GET0R( 'HIST_MAX', HIST_MAX, STATUS)
	      CALL PAR_GET0R( 'HIST_MIN', HIST_MIN, STATUS)

	      IF( STATUS .EQ. SAI__OK) THEN
*
* call subroutine to generate the histogram of the requested section of the
* image between the requested maximum and minimum values
*
	        CALL GENHIS(
     :                 IDIMS(1), IDIMS(2), %VAL( PNTRI ), X_START,
     :	               Y_START, X_END, Y_END, HIST_MAX, HIST_MIN,
     :	               HIST_BINS, HISTOGRAM, STATUS )

	        IF( STATUS .EQ. SAI__OK) THEN
*
* set output images dimensions
*
	          ODIMS( 1) = HIST_BINS
	          ODIMS( 2) = 2
*
* create output image structures
*
	          CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS,
     :                          LOCO, STATUS )

	          IF ( STATUS .EQ. SAI__OK ) THEN
*
* map in a DATA_ARRAY components
*
                    CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                            PNTRO, NELEMENTS, STATUS )

	            IF ( STATUS .EQ. SAI__OK ) THEN
*
* get the option to type the histogram to the terminal
*
	              CALL PAR_GET0L( 'TERMINAL_OUTPUT',
     :                                TERMINAL_OUTPUT, STATUS)
*
* call subroutine to put the histogram parameters into the structure
*
	              CALL HISTOGRAM_SAVE(
     :                           ODIMS(1), ODIMS(2), %VAL( PNTRO),
     :	                         HISTOGRAM, HIST_MAX, HIST_MIN,
     :	                         HIST_BINS, TERMINAL_OUTPUT, STATUS)

	            END IF
	          END IF
	        END IF
	      END IF
*
* unmap output image and annul locator
*
	      CALL NDF_ANNUL( LOCO, STATUS )

	    END IF
	  END IF
*
* tidy up the input/output structures
*
	  CALL NDF_ANNUL( LOCI, STATUS )

	END IF

	END
