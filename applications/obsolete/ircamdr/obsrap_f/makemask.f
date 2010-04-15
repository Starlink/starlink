*+  MAKEMASK - makes mask from sigma cut on sky

      SUBROUTINE MAKEMASK ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL MAKEMASK ( STATUS )
*
*    Parameters :
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image data structure
*     If no error so far then
*        Map the input DATA_ARRAY component
*        Create output mask image
*        Map the output DATA_ARRAY component
*        If no error so far then
*           Get sigma of cut for bad pixels
*           If no errors then
*              Scan through input image setting pixels outside sigma to bad
*           Endif
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
     :  LOCI,                 ! input IMAGE structure
     :  LOCO,                 ! output IMAGE structure
     :  DIMS( NDIMS ),        ! dimensions of input DATA_ARRAYs
     :  ODIMS( NDIMS ),       ! dimensions of output DATA_ARRAYs
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRI,                !    "     " input      "
     :  PNTRO,                !    "     " output     "
     :	IXST,
     :	IYST,
     :	IXEN,
     :	IYEN,
     :	NUMPIX

      REAL
     :	MEAN,                 ! mean in area specified
     :	STD,                  ! n-sigma in area specified
     :  SIGMALEVEL            ! sigma of cut for bad pixels


*-
*      check for error on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	  RETURN
	END IF

*      get a locator to input IMAGE type data structure
	CALL GETINP( 'INPIC', LOCI, STATUS )

*      check for error
	IF( STATUS .EQ. SAI__OK ) THEN

*        map input DATA_ARRAY component and get dimensions
          CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

*        check for error
	  IF( STATUS .EQ. SAI__OK ) THEN

*          set output image dimensions
	    ODIMS( 1) = DIMS( 1)
	    ODIMS( 2) = DIMS( 2)

*          create output array
	    CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

*          map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                    PNTRO, NELEMENTS, STATUS )

*          check for error
	    IF( STATUS .EQ. SAI__OK ) THEN

*            ask user for area for bad pixels
	      CALL PAR_GET0I( 'XST', IXST, STATUS)
	      CALL PAR_GET0I( 'YST', IYST, STATUS)
	      CALL PAR_GET0I( 'XEN', IXEN, STATUS)
	      CALL PAR_GET0I( 'YEN', IYEN, STATUS)

*            test input and see if user wants to end of array from start
	      IF( IXEN .GT. DIMS( 1)) IXEN = DIMS( 1)
	      IF( IYEN .GT. DIMS( 2)) IYEN = DIMS( 2)

*            test if starts less than ends
	      IF( IXST .LE. IXEN .AND. IYST .LE. IYEN) THEN

*              ask user for sigma level for cut for bad pixels
	        CALL PAR_GET0R( 'SIGMALEVEL', SIGMALEVEL, STATUS)

*              pass everything to the work routine
	        CALL MAKEMASKSUB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :	                          %VAL( PNTRO), IXST, IYST, IXEN, IYEN,
     :	                          SIGMALEVEL, MEAN, STD, NUMPIX,
     :                            STATUS)

*              tell user the bad news...
	        CALL MSG_SETR( 'ME', MEAN)
	        CALL MSG_OUT( 'MESS',
     :	         'Mean in input image area = ^ME',
     :	         STATUS)
	        CALL MSG_SETR( 'NS', SIGMALEVEL)
	        CALL MSG_SETR( 'ST', STD)
	        CALL MSG_OUT( 'MESS',
     :	      '^NS-sigma standard deviation in input image area = ^ST',
     :	         STATUS)
	        CALL MSG_SETI( 'NP', NUMPIX)
	        CALL MSG_OUT( 'MESS',
     :	         'Number of masked to 0 in image = ^NP',
     :	         STATUS)

	      END IF

	    END IF

*          tidy up the input structure
	    CALL NDF_ANNUL( LOCO, STATUS )

*        end of if-no-error-after-getting-output check
	  END IF

*        tidy up the input structure
	  CALL NDF_ANNUL(  LOCI, STATUS )

*      end of if-no-error-after-getting-input check
	END IF

	END
