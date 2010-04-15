*+  RADIM - creates a radial cut image from an image and centre

      SUBROUTINE RADIM ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL RADIM ( STATUS )
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*           IMAGE structure containing 2-D array
*     OUTPIC = IMAGE( WRITE )
*           IMAGE structure to contain output 2-D array
*     XCENTRE
*     YCENTRE
*     RADIALD
*
*    Method :
*
*    Authors :
*
*      Colin Aspin (UKTH::CAA)
*
*    History :
*
*     30-09-1987 : Original version
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
     :  DIMSO( NDIMS ),       ! dimensions of output DATA_ARRAYs
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRO,                ! pointer to output DATA_ARRAY
     :  PNTRI,                !    "     " input      "
     :  RADIALD,              !
     :	XCENTRE,              !
     :	YCENTRE               !

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

*       get the x,y centre of radial cut and radial size

         CALL AIF_GET0I( 'XCENTRE', 31, 1, DIMS( 1), XCENTRE, STATUS )

         CALL AIF_GET0I( 'YCENTRE', 29, 1, DIMS( 2), YCENTRE, STATUS )

         CALL AIF_GET0I( 'RADIALD', 20, 1, DIMS( 1)/2, RADIALD, STATUS )

*       create the output image and get a title for it

	 DIMSO( 1) = 180
	 DIMSO( 2) = RADIALD

         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMSO, LOCO, STATUS )

*       check for error

         IF( STATUS .EQ. SAI__OK ) THEN

*          find and map output DATA_ARRAY component

            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                    PNTRO, NELEMENTS, STATUS )

*          check for error before accessing pointers

            IF( STATUS .EQ. SAI__OK ) THEN

*            call subroutine to create radial cut image

	      CALL RADIMSUB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :	                     DIMSO( 1), DIMSO( 2), %VAL( PNTRO),
     :	                     XCENTRE, YCENTRE, RADIALD)

            END IF

*          tidy up the workspace and output structure

            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-after-getting-workspace-and-output check

         ENDIF

*       tidy up the input structure

         CALL NDF_ANNUL(  LOCI, STATUS )

*    end of if-no-error-after-getting-input check

      ENDIF

      END
