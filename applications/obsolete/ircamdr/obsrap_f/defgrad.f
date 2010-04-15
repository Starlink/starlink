*+  DEFGRAD - defines gradient across image by median of columns

      SUBROUTINE DEFGRAD ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL DEFGRAD ( STATUS )
*
*    Parameters :
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image data structure
*     If no error so far then
*        Get option to use columns or rows
*        Get number of columns/rows at start and end for gradient definition
*        Map the input DATA_ARRAY component
*        Create output image data structure
*        If no error so far then
*           Map an output DATA_ARRAY component
*           If no errors then
*              Median filter down columns/across rows and put resutant
*              number into output then interpolate across all other
*              columns/rows
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
*     14/01/1990 : Original version  (JACH::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     23-JUN-1994  Changed STR$ to CHR_ (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE           ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'       ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
      INCLUDE 'CHR_ERR'

*    Status :

      INTEGER STATUS          ! global status parameter

*    Local constants :

      INTEGER NDIMS           ! dimensionality of images
      PARAMETER ( NDIMS = 2 ) ! 2-d only

*    Local variables :

      INTEGER
     :  LOCI,                 ! input IMAGE structure locator
     :  LOCO,                 ! output IMAGE structure locator
     :  DIMS( NDIMS ),        ! dimensions of input DATA_ARRAYs
     :  ODIMS( NDIMS ),       ! dimensions of output DATA_ARRAYs
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRO,                ! pointer to output DATA_ARRAY
     :  PNTRI,                !    "     " input      "
     :	NUMCOL                ! number of columns at start and end

      REAL
     :	MEDIAN1,
     :	MEDIAN2

      CHARACTER
     :	COLROW*80             ! columns or rows to define gradient

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

*          check for error before getting other values region and accessing
*          pointers

            IF( STATUS .EQ. SAI__OK ) THEN

*            ask user whether want to use columns or rows for gradient
*            definition

	      CALL PAR_GET0C( 'COLROW', COLROW, STATUS)
	      CALL CHR_UCASE( COLROW )

*            ask user number of columsn to use for gradient definition

	      CALL PAR_GET0I( 'NUMCOL', NUMCOL, STATUS)

*            pass everything to the median calculation routine

              CALL DEFGRADSUB(
     :              DIMS( 1), DIMS( 2), %VAL( PNTRI), ODIMS( 1),
     :	            ODIMS( 2), %VAL( PNTRO), COLROW, NUMCOL,
     :	            MEDIAN1, MEDIAN2, STATUS)

            END IF

*          tell user what the medians were

	    CALL MSG_SETC( 'COLROW', COLROW)
	    CALL MSG_OUT( 'MESS', 'Using ^COLROW for gradient definition',
     :	      STATUS)

	    CALL MSG_SETR( 'MED1', MEDIAN1)
	    CALL MSG_SETR( 'MED2', MEDIAN2)
	    CALL MSG_OUT( 'MESS',
     :	     'Median at start = ^MED1, Median at end = ^MED2', STATUS)

*           release the ouput image

            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-after-getting-output check

         END IF

*       tidy up the input structure

         CALL NDF_ANNUL(  LOCI, STATUS )

*    end of if-no-error-after-getting-input check

      END IF

*    end

      END
