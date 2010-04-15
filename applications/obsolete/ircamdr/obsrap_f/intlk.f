*+  INTLK - writes out a integer picture of image sub-array

	SUBROUTINE INTLK ( STATUS )

* Description :
*
* This routine forms an integer picture of a sub-image from and HDS data
* image component called DATA_ARRAY. This programs prompts for the name of
* the HDS file conaining DATA_ARRAY, the start row,column of the sub-image,
* the number of rows,columns in the sub-image, the increment of the sub-image
* row,column scan, the offset value that is subtracted from the values in the
* sub-image and the normalization factor the is divided into the values in the
* sub-image before the integer representing the pixel value is set. Thus every
* pixel in the specified sub-image is represented by an integer value calculated
* thus :
*
*         INTEGER = INTEGER_PART( (PIXEL_VALUE - OFFSET)/NORMALIZATION)
*
* If the integer is larger than 16 the symbol printed is a + sign
* If the integer is smaller than 0 then the symbol printed is a - sign
* The range of integers 0 to 16 are represented by a HEX digit 0 thru F
*
* Invocation : CALL INTLK ( STATUS )
*
* Parameters : STATUS : adam status returned to A-task fixed part
*
* Bugs :
*
* None known.
*
* Authors : Colin Aspin ROE ( REVA::CAA )
*
* History :
*
*  08-01-1986 :  First implementation from DSCL IMPOL version (REVA::CAA)
*  27-04-1986 :  Modified working to FORTRAN 77/ADAM standard (REVA::CAA)
*  20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE		  ! no implicit typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'	    ! SSE global definitions
        INCLUDE  'NDF_PAR'
        INCLUDE  'NDF_ERR'

* Status :

	INTEGER  STATUS		 ! global status parameter

* Local constants :

	INTEGER NDIMS		   ! input image dimensionality
	PARAMETER ( NDIMS = 2 )

* Local variables :

	INTEGER
     :  LOCI,           ! locator for input data structure
     :  DIMS( NDIMS ),  ! dimensions of input DATA_ARRAY
     :  ACTDIM,         ! actual dimensions from NDF_DIM
     :  NELEMENTS,      ! number of elements mapped by NDF_MAP
     :  PNTRI,	    	! pointer to input DATA_ARRAY component
     :  LINESTART,	! start line number of sub-image
     :  LINENUMB,	! number of lines in sub-image
     :  LINEINC,	! increment of line scan in sub-image
     :  ICOLSTART,	! start column in sub-image
     :  ICOLNUMB,	! number of columns in sub-image
     :  ICOLINC		! increment of column scan in sub-image

	REAL
     :  SUBT,		! offset factor, subtracted from pixel value
     :  NORM		! normalization factor divided into (pixel_value-offset)

	CHARACTER
     :	DATA*80            ! buffer for output integer values

	LOGICAL
     :  AGAIN		  ! whether or not user wants to do another
			  ! sub-array

*-
*    check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN
	   RETURN
	ENDIF
*
* initialize some variables
*
	AGAIN = .TRUE.
*
* get locator to input IMAGE type data structure
*
	CALL GETINP( 'INPIC', LOCI, STATUS)

	IF ( STATUS .NE. SAI__OK ) THEN
	   RETURN
	ENDIF
*
* map in its DATA_ARRAY component and get dimensions
*
        CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                PNTRI, NELEMENTS, STATUS )
        CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

*
* output the dimensions of the image to the user
*
	CALL MSG_SETI( 'XDIM', DIMS(1))
	CALL MSG_SETI( 'YDIM', DIMS(2))

	CALL MSG_OUT( 'INPUT_DIMS', 'Image is ^XDIM by ^YDIM pixels',
     :	STATUS)
*
* loop while the user wants another sub-array
*
	DO WHILE ( AGAIN )
*
* get other input parameters
*
	  CALL AIF_GET0I( 'LINE_ST', 1,  1, DIMS( 2), LINESTART, STATUS)
	  CALL AIF_GET0I( 'LINE_NM', 20, 1, DIMS( 2), LINENUMB,  STATUS)
	  CALL AIF_GET0I( 'LINE_IN', 1,  1, 20,       LINEINC,   STATUS)

	  CALL AIF_GET0I( 'COL_ST',  1,  1, DIMS( 1), ICOLSTART, STATUS)
	  CALL AIF_GET0I( 'COL_NM',  50, 1, DIMS( 1), ICOLNUMB,  STATUS)
	  CALL AIF_GET0I( 'COL_IN',  1,  1, 20,       ICOLINC,   STATUS)

	  CALL AIF_GET0R( 'OFFSET',  0.0, -1.0E10, 1.0E10, SUBT, STATUS)

	  CALL AIF_GET0R( 'SCALAR',  1.0, -1.0E10, 1.0E10, NORM, STATUS)
*
* write out headings for users info
*
	  CALL MSG_OUT( 'BLANK', ' ', STATUS)

	  CALL MSG_SETI( 'LINST', LINESTART)
	  CALL MSG_OUT( 'MESSAGE', 'Start line number     = ^LINST', STATUS)

	  CALL MSG_SETI( 'LINNM', LINENUMB)
	  CALL MSG_OUT( 'MESSAGE', 'Number of lines       = ^LINNM', STATUS)

	  CALL MSG_SETI( 'LININ', LINEINC)
	  CALL MSG_OUT( 'MESSAGE', 'Line increment        = ^LININ', STATUS)

	  CALL MSG_SETI( 'COLST', ICOLSTART)
	  CALL MSG_OUT( 'MESSAGE', 'Start column number   = ^COLST', STATUS)

	  CALL MSG_SETI( 'COLNM', ICOLNUMB)
	  CALL MSG_OUT( 'MESSAGE', 'Number of columns     = ^COLNM', STATUS)

	  CALL MSG_SETI( 'COLIN', ICOLINC)
	  CALL MSG_OUT( 'MESSAGE', 'Column increment      = ^COLIN', STATUS)

	  CALL MSG_SETR( 'SUBT', SUBT)
	  CALL MSG_OUT( 'MESSAGE', 'Value Subtracted      = ^SUBT', STATUS)

	  CALL MSG_SETR( 'NORM', NORM)
	  CALL MSG_OUT( 'MESSAGE', 'Normalization factor  = ^NORM', STATUS)

	  CALL MSG_OUT( 'BLANK', ' ', STATUS)
*
* write out column ID
*
	  CALL COLUMN_LABEL( ICOLSTART,
     :                       ICOLNUMB,
     :                       ICOLINC)
*
* call subroutine to process line of data and put integer representation into
* DATA array
*
	    CALL INTEGER_PICTURE( DIMS( 1),
     :                            DIMS( 2),
     :                            %VAL( PNTRI),
     :                            LINESTART,
     :                            LINENUMB,
     :                            LINEINC,
     :                            ICOLSTART,
     :                            ICOLNUMB,
     :                            ICOLINC,
     :                            SUBT,
     :                            NORM,
     :                            DATA)
*
* put column numbers at end of picture
*
	  CALL COLUMN_LABEL( ICOLSTART,
     :                       ICOLNUMB,
     :                       ICOLINC)
*
* write out a blank line
*
	  CALL MSG_OUT( 'BLANK', ' ', STATUS)
*
* cancel again parameter and get users choice of another sub-array or stop
*
	  CALL PAR_CANCL( 'AGAIN', STATUS)
	  CALL PAR_GET0L( 'AGAIN', AGAIN, STATUS)
*
* annul all active locators
*
	  CALL PAR_CANCL( 'LINE_ST', STATUS)
	  CALL PAR_CANCL( 'LINE_NM', STATUS)
	  CALL PAR_CANCL( 'LINE_IN', STATUS)
	  CALL PAR_CANCL( 'COL_ST', STATUS)
	  CALL PAR_CANCL( 'COL_NM', STATUS)
	  CALL PAR_CANCL( 'COL_IN', STATUS)
	  CALL PAR_CANCL( 'OFFSET', STATUS)
	  CALL PAR_CANCL( 'SCALAR', STATUS)
	END DO
*
* tidy up image locators by unmapping data and annulling active locators
*

	CALL NDF_ANNUL( LOCI, STATUS)

	END
