*+  ASCIILIST - lists sdf image to ascii file

	SUBROUTINE ASCIILIST ( STATUS )

*    Description :

*    Parameters :

*    Method :

*    Authors :

*    History :
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     24-JUN-1994  Changed LIB$ to FIO_ (SKL@JACH)
*
*    Type Definitions :

	IMPLICIT NONE

*    Global constants :

	INCLUDE 'SAE_PAR'
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'
        INCLUDE 'FIO_PAR'

*    Status :

	INTEGER STATUS

*    Local constants :

	INTEGER NDIM, LUN

	PARAMETER ( NDIM = 2 ) ! dimensionality of input/output images

*    Local variables :

	CHARACTER*80
     :	  FILEN                ! name of output filename

	INTEGER
     :    LOCI,         ! input data structure
     :    DIMS( NDIM ), ! dimensions of the input/output DATA_ARRAYs
     :    ACTDIM,       ! actual dimensions from NDF_DIM
     :    NELEMENTS,    ! number of elements mapped by NDF_MAP
     :    PNTRI,        ! pointer to : input DATA_ARRAY
     :	  OUTFORM       ! output format specifier

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

	  CALL MSG_OUT( 'BLANK', ' ', STATUS)
	  CALL MSG_SETI( 'NX', DIMS( 1))
	  CALL MSG_SETI( 'NY', DIMS( 2))
	  CALL MSG_OUT( 'MESS',
     :     'Image is ^NX by ^NY pixels in size', STATUS )

*        get filename of output ascii file

	  CALL PAR_GET0C( 'FILEN', FILEN, STATUS)

*        get output format specifier

	  CALL MSG_OUT( 'BLANK', ' ', STATUS)
	  CALL MSG_OUT( 'MESSAGE',
     :	    'Output format =1, dimensions line then Intensities',
     :	    STATUS)
	  CALL MSG_OUT( 'MESSAGE',
     :	    'Output format =2, X, Y,Intensity',
     :	    STATUS)
	  CALL MSG_OUT( 'BLANK', ' ', STATUS)

	  CALL PAR_GET0I( 'OUTFORM', OUTFORM, STATUS)

*        create output ascii file

	  CALL FIO_GUNIT( LUN, STATUS )

	  OPEN( UNIT=LUN, FILE=FILEN, STATUS='UNKNOWN')

*        call subroutine to do work

	  CALL ASCIILISTSUB( DIMS( 1), DIMS( 2), %VAL( PNTRI), OUTFORM,
     :	                     LUN, STATUS)

*       tidy up input structure

	  CALL NDF_ANNUL(  LOCI, STATUS )

*       close ascii file

	  CLOSE( LUN)

	  CALL FIO_PUNIT( LUN, STATUS )

	END IF

	END
