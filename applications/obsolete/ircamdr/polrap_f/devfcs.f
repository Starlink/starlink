*+  DEVFCS - calculates deviation of vector from centro-symmetry

	SUBROUTINE DEVFCS ( STATUS )

*    Description :

*    Parameters :

*    Method :

*    Authors :

*    History :
*     18-May-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     11-Aug-1994  Changed input DIMS to DEVFCSSUB (SKL@JACH)
*    Type Definitions :

	IMPLICIT NONE

*    Global constants :

	INCLUDE 'SAE_PAR'
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

*    Status :

	INTEGER STATUS

*    Local constants :

	INTEGER NDIM

	PARAMETER ( NDIM = 2 ) ! dimensionality of input/output images

*    Local variables :

	INTEGER
     :    LOCI,         ! locator for input data structure
     :    DIMS( NDIM ), ! dimensions of the input/output DATA_ARRAYs
     :    ACTDIM,       ! actual dimensions from NDF_DIM
     :    NELEMENTS,    ! number of elements mapped by NDF_MAP
     :    PNTRI,        ! pointer to : input DATA_ARRAY
     :	  XCEN,         !
     :	  YCEN,         !
     :	  XPOS,         !
     :	  YPOS          !

	REAL
     :	  ACTTHETA,     !
     :	  CALTHETA,     !
     :	  DEVTHETA      !

	LOGICAL
     :	  MORE          !

*-

*      get locator to input IMAGE type data structure

	CALL GETINP( 'INPIC', LOCI, STATUS )

*      check for error

	IF( STATUS .EQ. SAI__OK ) THEN

*        map input DATA_ARRAY component and get dimensions

          CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI, NDIM, DIMS, ACTDIM, STATUS )

*        tell user image size

	  CALL MSG_SETI( 'NX', DIMS( 1))
	  CALL MSG_SETI( 'NY', DIMS( 2))
	  CALL MSG_OUT( 'MESS',
     :     'Image is ^NX by ^NY pixels in size', STATUS )

*        get centre of centro-symmetry

	  CALL PAR_GET0I( 'XCEN', XCEN, STATUS)
	  CALL PAR_GET0I( 'YCEN', YCEN, STATUS)

*        loop to get another vector position

	  MORE = .TRUE.

	  DO WHILE ( MORE)

*          get the position of the vector

	    CALL PAR_GET0I( 'XPOS', XPOS, STATUS)
	    CALL PAR_GET0I( 'YPOS', YPOS, STATUS)

	    CALL PAR_CANCL( 'XPOS', STATUS)
	    CALL PAR_CANCL( 'YPOS', STATUS)

*          call subroutine to do work

	    CALL DEVFCSSUB( DIMS(1), DIMS(2), %VAL( PNTRI), XCEN,
     :	                    YCEN, XPOS, YPOS, ACTTHETA, CALTHETA,
     :                      DEVTHETA, STATUS)

*          tell user the bad news

	    CALL MSG_OUT( 'BLANK', ' ', STATUS)

	    CALL MSG_SETI( 'XP', XPOS)
	    CALL MSG_SETI( 'YP', YPOS)

	    CALL MSG_OUT( 'MESS',
     :	      'Vector position = ^XP,^YP', STATUS)

	    CALL MSG_SETR( 'AC', ACTTHETA)
	    CALL MSG_SETR( 'CA', CALTHETA)
	    CALL MSG_SETR( 'DE', DEVTHETA)

	    CALL MSG_OUT( 'MESS',
     :	 'Actual THETA = ^AC, Calculated THETA = ^CA, Deviation = ^DE',
     :	      STATUS)

	    CALL MSG_OUT( 'BLANK', ' ', STATUS)

*          ask user for another input position

	    CALL PAR_GET0L( 'ANOTHER', MORE, STATUS)

	    CALL PAR_CANCL( 'ANOTHER', STATUS)

	    CALL MSG_OUT( 'BLANK', ' ', STATUS)

	  END DO

*      tidy up structure

	CALL NDF_ANNUL(  LOCI, STATUS )

	END IF

	END
