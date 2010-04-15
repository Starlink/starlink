*+  POLCAL - calculates the polarization and position angle images from the
*            dark and sky subtracted images taken at the 4 waveplate positions

	SUBROUTINE POLCAL ( STATUS)

* Description :
*
* This routine ...
*
* Invocation : CALL POLCAL ( STATUS)
*
* Parameters :
*
* Method :
*
* Bugs :
*
* None known.
*
* Authors : Colin Aspin ROE ( REVA::CAA)
*
* History :
*
*  24-01-1987 :  First implementation (REVA::CAA)
*  11-10-1988 :  Added unpolarized intensity image (JACH::CAA)
*  18-May-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*  11-Aug-1994  Changed input DIM arguments in POL_CALPT (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE		  ! no implicit typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'	    ! SSE global definitions
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

* Status :

	INTEGER  STATUS		 ! global status parameter

* Local constants :

	INTEGER NDIMS		   ! input image dimensionality
	PARAMETER ( NDIMS = 2)

* Local variables :

	INTEGER
     :    LOCI_1,  ! locator for 1st input data structure
     :    LOCI_2,  ! locator for 2nd input data structure
     :    LOCI_3,  ! locator for 3rd input data structure
     :    LOCI_4,  ! locator for 4th input data structure
     :    LOCOI,   ! locator for output intensity data structure
     :    LOCOPE,  ! locator for output p error data structure
     :    LOCOP,   ! locator for output p data structure
     :    LOCOPI,  ! locator for output pi data structure
     :    LOCOQ,   ! locator for output q data structure
     :    LOCOTE,  ! locator for output theta error data structure
     :    LOCOT,   ! locator for output theta data structure
     :    LOCOU,   ! locator for output u data structure
     :    LOCOUPI  ! locator for output upi data structure
      INTEGER
     :    IDIMS_1( NDIMS),  ! dimensions of 1st input DATA_ARRAY
     :    IDIMS_2( NDIMS),  ! dimensions of 2nd input DATA_ARRAY
     :    IDIMS_3( NDIMS),  ! dimensions of 3rd input DATA_ARRAY
     :    IDIMS_4( NDIMS),  ! dimensions of 4th input DATA_ARRAY
     :    ODIMS( NDIMS),    ! dimensions of output DATA_ARRAY
     :    ACTDIM,           ! actual dimensions from NDF_DIM
     :    NELEMENTS,        ! number of elements mapped by NDF_MAP
     :    PNTRI_1,	   ! pointer to 1st input DATA_ARRAY component
     :    PNTRI_2,	   ! pointer to 2nd input DATA_ARRAY component
     :    PNTRI_3,	   ! pointer to 3rd input DATA_ARRAY component
     :    PNTRI_4 	   ! pointer to 4th input DATA_ARRAY component
      INTEGER
     :    PNTROI,	   ! pointer to output intensity DATA_ARRAY
     :    PNTROPE,	   ! pointer to output polarization error DATA_ARRAY
     :    PNTROP,	   ! pointer to output polarization DATA_ARRAY
     :    PNTROPI,	   ! pointer to output polarized intensity DATA_ARRAY
     :    PNTROQ,	   ! pointer to output Q Stokes parameter DATA_ARRAY
     :    PNTROTE,	   ! pointer to output position angle error DATA_ARRAY
     :    PNTROT,	   ! pointer to output position angle DATA_ARRAY
     :    PNTROU,	   ! pointer to output U Stokes parameter DATA_ARRAY
     :    PNTROUPI	   ! pointer to output unpolarized intensity DATA_ARRAY

	REAL
     :	  ELDN			! electron/dn in data set



*-
*     check status on entry - return if not o.k.
*
	IF ( STATUS .NE. SAI__OK) THEN

	   RETURN

	END IF
*
*      get locator to all 4 input IMAGES data structure
*
	CALL GETINP( 'INPIC1', LOCI_1, STATUS)
	CALL GETINP( 'INPIC3', LOCI_3, STATUS)
	CALL GETINP( 'INPIC2', LOCI_2, STATUS)
	CALL GETINP( 'INPIC4', LOCI_4, STATUS)

	IF ( STATUS .NE. SAI__OK) THEN
*
*      tidy up the input/output structures
*
	  CALL NDF_ANNUL( LOCI_1, STATUS)
	  CALL NDF_ANNUL( LOCI_2, STATUS)
	  CALL NDF_ANNUL( LOCI_3, STATUS)
	  CALL NDF_ANNUL( LOCI_4, STATUS)

	  RETURN

	END IF
*
*      get the electrons/dn in data
*
	CALL PAR_GET0R( 'ELDN', ELDN, STATUS)

	IF ( STATUS .NE. SAI__OK) THEN
*
*      tidy up the input/output structures
*
	  CALL NDF_ANNUL( LOCI_1, STATUS)
	  CALL NDF_ANNUL( LOCI_2, STATUS)
	  CALL NDF_ANNUL( LOCI_3, STATUS)
	  CALL NDF_ANNUL( LOCI_4, STATUS)

	  RETURN

	END IF
*
*      map in all DATA_ARRAY components and get dimensions
*
        CALL NDF_MAP( LOCI_1, 'DATA', '_REAL', 'READ',
     :                PNTRI_1, NELEMENTS, STATUS )
        CALL NDF_DIM( LOCI_1, NDIMS, IDIMS_1, ACTDIM, STATUS )

        CALL NDF_MAP( LOCI_2, 'DATA', '_REAL', 'READ',
     :                PNTRI_2, NELEMENTS, STATUS )
        CALL NDF_DIM( LOCI_2, NDIMS, IDIMS_2, ACTDIM, STATUS )

        CALL NDF_MAP( LOCI_3, 'DATA', '_REAL', 'READ',
     :                PNTRI_3, NELEMENTS, STATUS )
        CALL NDF_DIM( LOCI_3, NDIMS, IDIMS_3, ACTDIM, STATUS )

        CALL NDF_MAP( LOCI_4, 'DATA', '_REAL', 'READ',
     :                PNTRI_4, NELEMENTS, STATUS )
        CALL NDF_DIM( LOCI_4, NDIMS, IDIMS_4, ACTDIM, STATUS )


	IF ( STATUS .NE. SAI__OK) THEN
*
*      tidy up the input/output structures
*
	  CALL NDF_ANNUL( LOCI_1, STATUS)
	  CALL NDF_ANNUL( LOCI_2, STATUS)
	  CALL NDF_ANNUL( LOCI_3, STATUS)
	  CALL NDF_ANNUL( LOCI_4, STATUS)

	  RETURN

	END IF
*
*      check the input images dimensions to see if all the same
*
	IF( IDIMS_1( 1) .NE. IDIMS_2( 1) .OR.
     :	    IDIMS_1( 1) .NE. IDIMS_3( 1) .OR.
     :	    IDIMS_1( 1) .NE. IDIMS_4( 1) .OR.
     :	    IDIMS_1( 2) .NE. IDIMS_2( 2) .OR.
     :	    IDIMS_1( 2) .NE. IDIMS_3( 2) .OR.
     :	    IDIMS_1( 2) .NE. IDIMS_4( 2)) THEN

	  CALL MSG_OUT( 'MESSAGE',
     :                  'Error, input images are of different sizes',
     :	                STATUS)

	  CALL NDF_ANNUL( LOCI_1, STATUS)
	  CALL NDF_ANNUL( LOCI_2, STATUS)
	  CALL NDF_ANNUL( LOCI_3, STATUS)
	  CALL NDF_ANNUL( LOCI_4, STATUS)


	  RETURN

	END IF
*
*      output the dimensions of the input image to the user
*
	CALL MSG_SETI( 'XDIM', IDIMS_1( 1))
	CALL MSG_SETI( 'YDIM', IDIMS_1( 2))

	CALL MSG_OUT( 'INPUT_DIMS',
     :                'Images are of size ^XDIM by ^YDIM pixels',
     :	              STATUS)
*
*      set output images dimensions
*
	ODIMS( 1) = IDIMS_1( 1)
	ODIMS( 2) = IDIMS_1( 2)
*
*      create output image structures
*
	CALL CREOUT( 'OUTPICQ', 'OTITLE', NDIMS, ODIMS, LOCOQ, STATUS)
	CALL CREOUT( 'OUTPICU', 'OTITLE', NDIMS, ODIMS, LOCOU, STATUS)
	CALL CREOUT( 'OUTPICP', 'OTITLE', NDIMS, ODIMS, LOCOP, STATUS)
	CALL CREOUT( 'OUTPICT', 'OTITLE', NDIMS, ODIMS, LOCOT, STATUS)
	CALL CREOUT( 'OUTPICI', 'OTITLE', NDIMS, ODIMS, LOCOI, STATUS)
	CALL CREOUT( 'OUTPICPI', 'OTITLE', NDIMS, ODIMS, LOCOPI,
     :                STATUS)
	CALL CREOUT( 'OUTPICUPI', 'OTITLE', NDIMS, ODIMS, LOCOUPI,
     :                STATUS)
	CALL CREOUT( 'OUTPICPE', 'OTITLE', NDIMS, ODIMS, LOCOPE,
     :                STATUS)
	CALL CREOUT( 'OUTPICTE', 'OTITLE', NDIMS, ODIMS, LOCOTE,
     :                STATUS)

	IF ( STATUS .NE. SAI__OK) THEN
*
*      tidy up the input/output structures
*
	  CALL NDF_ANNUL( LOCI_1, STATUS)
	  CALL NDF_ANNUL( LOCI_2, STATUS)
	  CALL NDF_ANNUL( LOCI_3, STATUS)
	  CALL NDF_ANNUL( LOCI_4, STATUS)

	  CALL NDF_ANNUL( LOCOI, STATUS)
	  CALL NDF_ANNUL( LOCOPE, STATUS)
	  CALL NDF_ANNUL( LOCOP, STATUS)
	  CALL NDF_ANNUL( LOCOPI, STATUS)
	  CALL NDF_ANNUL( LOCOUPI, STATUS)
	  CALL NDF_ANNUL( LOCOQ, STATUS)
	  CALL NDF_ANNUL( LOCOTE, STATUS)
	  CALL NDF_ANNUL( LOCOT, STATUS)
	  CALL NDF_ANNUL( LOCOU, STATUS)

	  RETURN

	END IF
*
*      map in a DATA_ARRAY components
*
        CALL NDF_MAP( LOCOQ, 'DATA', '_REAL', 'WRITE',
     :                PNTROQ, NELEMENTS, STATUS )
        CALL NDF_MAP( LOCOU, 'DATA', '_REAL', 'WRITE',
     :                PNTROU, NELEMENTS, STATUS )
        CALL NDF_MAP( LOCOP, 'DATA', '_REAL', 'WRITE',
     :                PNTROP, NELEMENTS, STATUS )
        CALL NDF_MAP( LOCOT, 'DATA', '_REAL', 'WRITE',
     :                PNTROT, NELEMENTS, STATUS )
        CALL NDF_MAP( LOCOI, 'DATA', '_REAL', 'WRITE',
     :                PNTROI, NELEMENTS, STATUS )
        CALL NDF_MAP( LOCOPI, 'DATA', '_REAL', 'WRITE',
     :                PNTROPI, NELEMENTS, STATUS )
        CALL NDF_MAP( LOCOUPI, 'DATA', '_REAL', 'WRITE',
     :                PNTROUPI, NELEMENTS, STATUS )
        CALL NDF_MAP( LOCOPE, 'DATA', '_REAL', 'WRITE',
     :                PNTROPE, NELEMENTS, STATUS )
        CALL NDF_MAP( LOCOTE, 'DATA', '_REAL', 'WRITE',
     :                PNTROTE, NELEMENTS, STATUS )

	IF ( STATUS .NE. SAI__OK) THEN
*
*      tidy up the input/output structures
*
	  CALL NDF_ANNUL( LOCI_1, STATUS)
	  CALL NDF_ANNUL( LOCI_2, STATUS)
	  CALL NDF_ANNUL( LOCI_3, STATUS)
	  CALL NDF_ANNUL( LOCI_4, STATUS)

	  CALL NDF_ANNUL( LOCOI, STATUS)
	  CALL NDF_ANNUL( LOCOPE, STATUS)
	  CALL NDF_ANNUL( LOCOP, STATUS)
	  CALL NDF_ANNUL( LOCOPI, STATUS)
	  CALL NDF_ANNUL( LOCOUPI, STATUS)
	  CALL NDF_ANNUL( LOCOQ, STATUS)
	  CALL NDF_ANNUL( LOCOTE, STATUS)
	  CALL NDF_ANNUL( LOCOT, STATUS)
	  CALL NDF_ANNUL( LOCOU, STATUS)

	  RETURN

	END IF
*
*      call subroutine to take the input images and calculate the polarization
*      and position angle
*
	CALL POL_CALPT( IDIMS_1(1), IDIMS_1(2), %VAL( PNTRI_1),
     :	                %VAL( PNTRI_2), %VAL( PNTRI_3), %VAL( PNTRI_4),
     :	                ELDN, ODIMS(1), ODIMS(2), %VAL( PNTROQ),
     :	                %VAL( PNTROU), %VAL( PNTROP), %VAL( PNTROT),
     :	                %VAL( PNTROI), %VAL( PNTROPI), %VAL( PNTROUPI),
     :	                %VAL( PNTROPE), %VAL( PNTROTE), STATUS)
*
*      tidy up the input/output structures
*
	CALL NDF_ANNUL( LOCI_1, STATUS)
	CALL NDF_ANNUL( LOCI_2, STATUS)
	CALL NDF_ANNUL( LOCI_3, STATUS)
	CALL NDF_ANNUL( LOCI_4, STATUS)

	CALL NDF_ANNUL( LOCOI, STATUS)
	CALL NDF_ANNUL( LOCOPE, STATUS)
	CALL NDF_ANNUL( LOCOP, STATUS)
	CALL NDF_ANNUL( LOCOPI, STATUS)
	CALL NDF_ANNUL( LOCOUPI, STATUS)
	CALL NDF_ANNUL( LOCOQ, STATUS)
	CALL NDF_ANNUL( LOCOTE, STATUS)
	CALL NDF_ANNUL( LOCOT, STATUS)
	CALL NDF_ANNUL( LOCOU, STATUS)

	END
