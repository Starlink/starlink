*+  OEFIX - rescales even channel wrt odd channel by median values

	SUBROUTINE OEFIX ( STATUS )

* Description :
*
* This routine ...
*
* Invocation : CALL OEFIX ( STATUS )
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
*  23-04-1986 :  First implementation (REVA::CAA)
*  14-Jul-1994   Changed OEFIXSUB call for altered arguments (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE		  ! no implicit typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'	    ! SSE global definitions
        INCLUDE 'DAT_PAR'          ! Necessary for non-VMS

* Status :

	INTEGER  STATUS		 ! global status parameter

* Local constants :

	INTEGER NDIMS		   ! input image dimensionality
	PARAMETER ( NDIMS = 2 )

* Local variables :

	INTEGER
     :  IDIMS_1( NDIMS ),  ! dimensions of input DATA_ARRAY
     :  ODIMS( NDIMS ),    ! dimensions of output DATA_ARRAY
     :  PNTRI,	     	   ! pointer to input DATA_ARRAY component
     :  PNTRO	    	   ! pointer to output DATA_ARRAY component

	CHARACTER
     :  LOCI*(DAT__SZLOC),   ! locator for input data structure
     :  LOCO*(DAT__SZLOC)    ! locator for output channel B data structure

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
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL DAT_ANNUL( LOCI, STATUS )
	  RETURN
	END IF
*
* map in its DATA_ARRAY component
*
	CALL CMP_MAPN( LOCI, 'DATA_ARRAY', '_REAL', 'READ',
     :		       NDIMS, PNTRI, IDIMS_1, STATUS)
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL CMP_UNMAP( LOCI, 'DATA_ARRAY', STATUS )
	  CALL DAT_ANNUL( LOCI, STATUS )
	  RETURN
	END IF
*
* output the dimensions of the input image to the user
*
	CALL MSG_SETI( 'XDIM', IDIMS_1( 1))
	CALL MSG_SETI( 'YDIM', IDIMS_1( 2))

	CALL MSG_OUT( 'INPUT_DIMS', 'Input image is ^XDIM by ^YDIM pixels',
     :	              STATUS)
*
* set output images dimensions
*
	ODIMS( 1) = IDIMS_1( 1)
	ODIMS( 2) = IDIMS_1( 2)
*
* create output image structures
*
	CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL CMP_UNMAP( LOCI, 'DATA_ARRAY', STATUS )
	  CALL DAT_ANNUL( LOCI, STATUS )
	  CALL DAT_ANNUL( LOCO, STATUS )
	  RETURN
	END IF
*
* map in a DATA_ARRAY components
*
	CALL CMP_MAPN( LOCO, 'DATA_ARRAY', '_REAL', 'WRITE',
     :                 NDIMS, PNTRO, ODIMS, STATUS )
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL CMP_UNMAP( LOCI, 'DATA_ARRAY', STATUS )
	  CALL DAT_ANNUL( LOCI, STATUS )
	  CALL CMP_UNMAP( LOCO, 'DATA_ARRAY', STATUS )
	  CALL DAT_ANNUL( LOCO, STATUS )
	  RETURN
	END IF
*
* call subroutine to do work ...
*
	CALL OEFIXSUB( IDIMS_1(1), IDIMS_1(2), %VAL( PNTRI),
     :                 %VAL( PNTRO), STATUS)
*
* tidy up the input/output structures
*
	CALL CMP_UNMAP( LOCI, 'DATA_ARRAY', STATUS )
	CALL DAT_ANNUL( LOCI, STATUS )
	CALL CMP_UNMAP( LOCO, 'DATA_ARRAY', STATUS )
	CALL DAT_ANNUL( LOCO, STATUS )

	END
