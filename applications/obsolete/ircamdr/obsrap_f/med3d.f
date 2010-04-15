	SUBROUTINE MED3D( STATUS)

*+ MED3D - median filters image stack pixels by pixel

*    Invocation :
*
*     CALL MED3D( STATUS)
*
*    Parameters :
*
*    Method :
*
*    Bugs :
*
*    Authors :
*
*     Colin Aspin (ROE/UKIRT) : 20-Oct-1987
*
*    History :
*
*     20-10-1987 : First implementation
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     29-JUN-1994  Changed MSG_OUT on error to ERR_REP, LIB$ and STR$
*                  to CHR_, FIO_ (SKL@JACH)
*
*    Type definitions :

	IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

	INCLUDE 'SAE_PAR'          ! SSE global definitions
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'
        INCLUDE 'FIO_PAR'
        INCLUDE 'CHR_ERR'

*    Status :

	INTEGER  STATUS             ! global status parameter

*    Local constants :

	INTEGER NDIMS               ! input image dimensionality
	PARAMETER ( NDIMS = 2)

*    Local variables :

	INTEGER
     :    LOCI,           ! locator for input data structure
     :    LOCW,           ! locator for work data structure
     :    LOCWL,          ! locator for linear work data structure
     :    LOCO,           ! locator for output data structure
     :	  COUNTER,
     :    DIMS( NDIMS ),  ! dimensions of input DATA_ARRAY
     :    DIMSO( NDIMS ), ! dimensions of output DATA_ARRAY
     :    ACTDIM,         ! actual dimensions from NDF_DIM
     :    NELEMENTS,      ! number of elements mapped by NDF_MAP
     :	  IMCODE,         ! code for image parameter
     :	  J,              ! counting variable
     :	  LUN             ! logical unit number for open
      INTEGER
     :	  MAXFILES,       ! maximum number of images in stack allowed
     :	  MAXX,           ! maximum allowed X size of images
     :	  MAXY,           ! maximum allowed Y size of image
     :	  MAXL,           ! maximum allowed  size of linear work array
     :	  NUMDIMS,        ! number of dimensions in images
     :	  PNTRW,          ! pointer to work image
     :	  PNTRWL,         ! pointer to linear work image
     :    PNTRI,          ! pointer to input DATA_ARRAY component
     :    PNTRO,          ! pointer to output DATA_ARRAY component
     :    WDIMS( 3),      ! dimensions of work DATA_ARRAY
     :    LBND( 3),       ! lower bounds of work array
     :    WLDIMS( 1),     ! dimensions of linear work DATA_ARRAY
     :	  XPIX,           ! X coordinate of pixel to be viewed
     :	  YPIX,           ! Y coordinate of pixel to be viewed
     :	  XST,            ! X start of scaling area
     :	  XSZ,            ! X size of scaling area
     :	  YST,            ! Y size of scaling area
     :	  YSZ             ! Y size of scaling area

        DATA LBND / 1, 1, 1 /

	PARAMETER ( MAXFILES = 100)    ! allow 100 images in stack
	PARAMETER ( MAXX     = 256)    ! allow 256 pixels in X max
	PARAMETER ( MAXY     = 256)    ! allow 256 pixels in Y max
	PARAMETER ( MAXL     = 65536)  ! allow 256**2 pixels in total

	REAL
     :    ACTDIMS( NDIMS),             ! dimensions of input DATA_ARRAY
     :	  SUMDIM1,                     ! sum of dimensions in X
     :	  SUMDIM2,                     ! sum of dimensions in Y
     :    TOLERANCE                    ! tolerance on real comparisons

	PARAMETER ( TOLERANCE = 0.00001)

	CHARACTER
     :	  FFILE*80,                    ! name of file containing stack
     :	  IMAGENAME( 100)*80,          ! name of images in stack
     :	  WHICH*80                     !

	LOGICAL
     :	  MORE,                        !
     :	  VIEW,                        ! view a pixel during median filtering
     :	  SCALING,                     ! scale images before filtering
     :	  NORMALIZATION                ! normalization of output image

*-

*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	  RETURN
	END IF

*      tell user what we are ...
	CALL MSG_OUT( 'MESSAGE',
     :	  'MED3D - Median filtering through image stack', STATUS)

*      get the code of the image interface file entry parameter
	CALL SUBPAR_FINDPAR( 'INPIC', IMCODE, STATUS)

*      get the name of the file containing the list of images
	CALL PAR_GET0C( 'FILENAME', FFILE, STATUS)

*      get a lun and try to open the file
	CALL FIO_GUNIT( LUN, STATUS )

	OPEN( UNIT=LUN, FILE=FFILE, STATUS='OLD', ERR=999)

*      read in all images from free-format file
	COUNTER = 0
	MORE = .TRUE.
	DO WHILE (MORE)

*        increment the counting variable
	  COUNTER = COUNTER + 1

*        test if the number of filenames read in is greater than the max
	  IF( COUNTER .GT. MAXFILES) GOTO 100

*        read the image name from the file
	  READ( LUN, '(A)', END=100, ERR=998) IMAGENAME( COUNTER)

*        put name to upper case and trim it to get length
*	  CALL CHR_UCASE( IMAGENAME( COUNTER) )
	END DO
  100	CONTINUE

*      close the input file and release the lun
	CLOSE( LUN)
	CALL FIO_PUNIT( LUN, STATUS )

*      decrement counter with number of image names
	COUNTER = COUNTER - 1

*      test the number of files for number greater than 0
	IF( COUNTER .LE. 0 .OR. COUNTER .GE. MAXFILES) THEN
	  CALL MSG_OUT( 'MESSAGE',
     :                  'Error, number of files read in illegal',
     :	                STATUS)
	  CALL MSG_SETI( 'MAXF', MAXFILES)
	  CALL MSG_OUT( 'MESSAGE',
     :	    'Number of files should be between 1 and ^MAXF ...',
     :      STATUS)
	END IF

*      tell user number of filenames read in
	CALL MSG_SETI( 'NUMF', COUNTER)
	CALL MSG_OUT( 'MESSAGE',
     :                'Number of IMAGES in median stack = ^NUMF',
     :	              STATUS)

*      create work 1D and 3D array on disk
	CALL MSG_OUT( 'MESSAGE',
     :                'Creating disk work file ... please wait',
     :	              STATUS)

*      create work 3D array on disk
	CALL MSG_OUT( 'MESSAGE', '3D work array ... ', STATUS)

	NUMDIMS = 3
	WDIMS( 1) = MAXX
	WDIMS( 2) = MAXY
	WDIMS( 3) = COUNTER
	CALL NDF_CREAT( 'WORKFILE', '_REAL', NUMDIMS, LBND, WDIMS,
     :                   LOCW, STATUS)
        CALL NDF_MAP( LOCW, 'DATA', '_REAL', 'WRITE', PNTRW,
     :                NELEMENTS, STATUS )

	IF( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCW, STATUS)
	END IF

*      create work 1D array on disk
	CALL MSG_OUT( 'MESSAGE', '1D work array ... ', STATUS)

	NUMDIMS = 1
	WLDIMS( 1) = MAXL
	CALL NDF_CREAT( 'LWORKFILE', '_DOUBLE', NUMDIMS, LBND(1), WLDIMS,
     :                   LOCWL, STATUS)

        CALL NDF_MAP( LOCWL, 'DATA', '_DOUBLE', 'WRITE', PNTRWL,
     :                NELEMENTS, STATUS )

	IF( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCW, STATUS)
	  CALL NDF_ANNUL( LOCWL, STATUS)
	END IF

*      set working array to zero
	CALL MED3D_TOZERO( WDIMS( 1), WDIMS( 2), WDIMS( 3),
     :                     %VAL( PNTRW))

*      set the sum of the dimensions variables to 0
	SUMDIM1 = 0.0
	SUMDIM2 = 0.0

*      scan through all input files and put data into work array
	DO J = 1, COUNTER

*        sets the parameter with image name to be associated
	  CALL SUBPAR_PUTNAME ( IMCODE, IMAGENAME( J), STATUS)

*        associate the image and get locator
	  CALL NDF_ASSOC( 'INPIC', 'READ', LOCI, STATUS)

*        map the data array component and get dimensions
          CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

	  IF( J .EQ. 1) THEN
	    ACTDIMS( 1) = DIMS( 1)
	    ACTDIMS( 2) = DIMS( 2)
	  END IF
	  SUMDIM1 = SUMDIM1 + DIMS( 1)
	  SUMDIM2 = SUMDIM2 + DIMS( 2)

*        tell user the number of image, image name and size of image
	  CALL MSG_SETC( 'NAME', IMAGENAME( J))
          CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
          CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
          CALL MSG_SETI( 'COUN', J)
          CALL MSG_OUT( 'INPUT_DIMS',
     :                  'Image ^COUN = ^NAME, ^XDIM by ^YDIM pixels',
     :                   STATUS)

*        call subroutine to put data into the 3d array
	  CALL MED3D_TO3D( J, DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :	                   WDIMS( 1), WDIMS( 2), WDIMS( 3),
     :	                   %VAL( PNTRW))


*        annul the input locator
	  CALL NDF_ANNUL( LOCI, STATUS)

*        cancel the input parameter association
	  CALL PAR_CANCL( 'INPIC', STATUS)
	END DO

*      test the dimensions of the input arrays to see if all the same
	ACTDIMS( 1) = ACTDIMS( 1)*COUNTER
	ACTDIMS( 2) = ACTDIMS( 2)*COUNTER
	IF( ABS( SUMDIM1-ACTDIMS( 1)) .GT. TOLERANCE .OR.
     :	    ABS( SUMDIM2-ACTDIMS( 2)) .GT. TOLERANCE) THEN
	  CALL MSG_OUT( 'MESSAGE',
     :                  'Error, input arrays of different sizes',
     :	                STATUS)
	  CALL NDF_ANNUL( LOCW, STATUS)
	  CALL NDF_ANNUL( LOCWL, STATUS)
	  RETURN
	END IF

*      create the output image
	DIMSO( 1) = DIMS( 1)
	DIMSO( 2) = DIMS( 2)
	CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMSO, LOCO, STATUS)
        CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                PNTRO, NELEMENTS, STATUS )

*      ask user if he/she wants to use median, mean or mode value
	CALL PAR_GET0C( 'WHICH', WHICH, STATUS)
	CALL CHR_UCASE( WHICH )

*      ask user if he/she wants to view data on one pixel
	CALL PAR_GET0L( 'VIEW', VIEW, STATUS)
	IF( VIEW ) THEN

*        get the x,y pixel position for viewing
          CALL AIF_GET0I( 'XPIX', DIMS( 1)/2, 1, DIMS( 1), XPIX, STATUS)
          CALL AIF_GET0I( 'YPIX', DIMS( 2)/2, 1, DIMS( 2), YPIX, STATUS)
	END IF

*      ask user if he/she wants to scale the stack images to same value in
*      sub-image
	CALL PAR_GET0L( 'SCALING', SCALING, STATUS)

*      if user wants scaling get scaling region
	IF( SCALING) THEN
          CALL AIF_GET0I( 'XST', 1, 1, DIMS( 1), XST, STATUS)
          CALL AIF_GET0I( 'YST', 1, 1, DIMS( 2), YST, STATUS)
          CALL AIF_GET0I( 'XSZ', DIMS( 1)-XST+1, 1, DIMS( 1)-XST+1, XSZ,
     :                     STATUS)
          CALL AIF_GET0I( 'YSZ', DIMS( 2)-YST+1, 1, DIMS( 2)-YST+1, YSZ,
     :                     STATUS)
	END IF

*      ask user if he/she wants to normalize the median filtered image to 1
	CALL PAR_GET0L( 'NORMALIZATION', NORMALIZATION, STATUS)

*      get the scaling box for the normalization if not scaling input images
	IF( .NOT. SCALING .AND. NORMALIZATION) THEN
          CALL AIF_GET0I( 'XST', 1, 1, DIMS( 1), XST, STATUS)
          CALL AIF_GET0I( 'YST', 1, 1, DIMS( 2), YST, STATUS)
          CALL AIF_GET0I( 'XSZ', DIMS( 1)-XST+1, 1, DIMS( 1)-XST+1, XSZ,
     :                     STATUS)
          CALL AIF_GET0I( 'YSZ', DIMS( 2)-YST+1, 1, DIMS( 2)-YST+1, YSZ,
     :                    STATUS)
	END IF

*      test status for bad input and stop if there has been
	IF( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCO, STATUS)
	  CALL NDF_ANNUL( LOCW, STATUS)
	  CALL NDF_ANNUL( LOCWL, STATUS)
	  RETURN
	END IF

*      call subroutine to do the median filtering through the images and put
*      result into the output image
	CALL MED3D_MEDFIL( COUNTER, WDIMS( 1), WDIMS( 2), WDIMS( 3),
     :	                   %VAL( PNTRW), WLDIMS( 1), %VAL( PNTRWL),
     :	                   DIMSO( 1), DIMSO( 2), %VAL( PNTRO), VIEW,
     :	                   XPIX, YPIX, WHICH, SCALING, XST, XSZ, YST,
     :	                   YSZ, NORMALIZATION)


*      annul the output locator
	CALL NDF_ANNUL( LOCO, STATUS)

*      annul the work array locators and implicitly unmap data
	CALL NDF_ANNUL( LOCW, STATUS)
	CALL NDF_ANNUL( LOCWL, STATUS)
	RETURN

*      error messages from open and read statements
  999	CONTINUE
	CALL FIO_PUNIT( LUN, STATUS )
	CALL ERR_REP( 'MESSAGE', 'Error, cannot find image list file',
     :	              STATUS)
	RETURN
  998	CONTINUE
	CLOSE( LUN)
	CALL FIO_PUNIT( LUN, STATUS )
	CALL ERR_REP( 'MESSAGE',
     :                'Error, cannot read from specified file',
     :	              STATUS)
	END
