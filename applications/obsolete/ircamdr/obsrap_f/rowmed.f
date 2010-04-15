*+  ROWMED - median filters rows to produce row map slice

      SUBROUTINE ROWMED ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL ROWMED ( STATUS )
*
*    Parameters :
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image data structure
*     If no error so far then
*        Get the possible exclusion region and it's coordinates
*        Map the input DATA_ARRAY component
*        Create output image data structure
*        If no error so far then
*           Map an output DATA_ARRAY component
*           If no errors then
*              Median filter across each row and put resutant number into
*              output
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
     :  DIMS( NDIMS ),        ! dimensions of input DATA_ARRAYs
     :  ODIMS( NDIMS ),       ! dimensions of output DATA_ARRAYs
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRO,                ! pointer to output DATA_ARRAY
     :  PNTRI,                !    "     " input      "
     :	XST,                  ! X-start of exclusion region
     :	YST,                  ! Y-start of exclusion region
     :	XSZ,                  ! X-size of exclusion region
     :	YSZ,                  ! Y-size of exclusion region
     :	XEN,                  ! X-end of exclusion region
     :	YEN,                  ! Y-end of exclusion region
     :  LOCI,                 ! locator for input IMAGE structure
     :  LOCO                  ! locator for output IMAGE structure

      LOGICAL
     :	EXCLREG               ! whether user wants to define an exclusion
                              ! region

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

	ODIMS( 1) = 1
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

*            ask user if he/she wants to define an exclusion region

	      CALL PAR_GET0L( 'EXCLREG', EXCLREG, STATUS)

*            test if user wnats to define an exclusion region

	      IF( EXCLREG) THEN

*              get the exclusion regions start/size

	        CALL PAR_GET0I( 'XST', XST, STATUS)
	        CALL PAR_GET0I( 'YST', YST, STATUS)
	        CALL PAR_GET0I( 'XSZ', XSZ, STATUS)
	        CALL PAR_GET0I( 'YSZ', YSZ, STATUS)

*              calculate the x,y end of the exclusion region

	        XEN = XST+XSZ-1
	        YEN = YST+YSZ-1

	      ELSE

*               set the exclusion region size for comparison below

	         XST = 1
	         YST = 1
	         XEN = 1
	         YEN = 1

	      END IF

*            pass everything to the rapid median calculation routine

              CALL ROWMEDSUB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :	                      ODIMS( 1), ODIMS( 2), %VAL( PNTRO),
     :                        EXCLREG, XST, YST,
     :	                      XEN, YEN, STATUS)

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
