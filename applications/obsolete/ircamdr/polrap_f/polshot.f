*+ POLSHOT - corrects polarization image for shot-noise error

        SUBROUTINE POLSHOT ( STATUS)

* Description :
*
* This routine takes in a polarization image and a polarization error
* image and removes the error due to shot-noise biasing from the
* polarization image. This is done in quadrature i.e. the square of the
* error image (output from POLCAL) is subtrac ted from the square of
* the polarization image. The results is square rooted and put into
* the output image.
*
* Invocation :
*
*     CALL POLSHOT ( STATUS )
*
* Parameters :
*
*
* Method :
*
* Bugs :
*
*     None known.
*
* Authors :
*
*     Colin Aspin ROE ( REVA::CAA )
*
* History :
*
*     03-02-1987 : First implementation (REVA::CAA)
*     18-May-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     24-Jun-1994  Changed type, msg_out statements to ERR_REP (SKL@JACH)
*     11-Aug-1994  Changed input DIM arguments to polshot_remove (SKL@JACH)
* Type definitions :

        IMPLICIT  NONE                  ! no default typing allowed

* Global constants :

        INCLUDE  'SAE_PAR'              ! SSE global definitions
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

* Status :

        INTEGER  STATUS                 ! global status parameter

* Local Constants :

* Local variables :

        INTEGER
     :      ACTDIM,               ! actual dimensions from NDF_DIM
     :      NELEMENTS,            ! number of elements mapped by NDF_MAP
     :	    IDIMS_1( 2),
     :	    IDIMS_2( 2),
     :	    ODIMS( 2),
     :      PNTRIP,
     :      PNTRIPE,
     :      PNTRO,
     :      LOCIP,                        ! locator for input image
     :      LOCIPE,                       ! locator for input image
     :      LOCO                          ! locator for output image

* ======================================================================
*-
*      check status on entry - return if not o.k.

        IF ( STATUS .NE. SAI__OK ) THEN

           RETURN

        END IF

*      get the input polarization and polarization errorimage locator

        CALL GETINP( 'INPICP', LOCIP, STATUS)
        CALL GETINP( 'INPICPE', LOCIPE, STATUS)

	if ( status .ne. sai__ok ) then
	  PRINT *,'after getinp ...'
	  CALL NDF_ANNUL( LOCIP, STATUS)
	  CALL NDF_ANNUL( LOCIPE, STATUS)
	  return
	end if

*      map in the input images

         CALL NDF_MAP( LOCIP, 'DATA', '_REAL', 'READ',
     :                  PNTRIP, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCIP, 2, IDIMS_1, ACTDIM, STATUS )

         CALL NDF_MAP( LOCIPE, 'DATA', '_REAL', 'READ',
     :                  PNTRIPE, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCIPE, 2, IDIMS_2, ACTDIM, STATUS )


	IF (STATUS .NE. SAI__OK ) THEN

	  CALL ERR_REP( 'MESSAGE', 'Error, after ndf_map IN POLSHOT',
     :	    STATUS)
	  CALL NDF_ANNUL( LOCIP, STATUS)
	  CALL NDF_ANNUL( LOCIPE, STATUS)

          RETURN

        END IF

*      check sizes of input images are same

	IF( IDIMS_1( 1) .NE. IDIMS_2( 1) .OR.
     :	    IDIMS_1( 2) .NE. IDIMS_2( 2)) THEN

	  CALL MSG_OUT( 'MESSAGE',
     :	    'Error, input images are of different sizes ...',
     :	    STATUS)

	  CALL NDF_ANNUL( LOCIP, STATUS)
	  CALL NDF_ANNUL( LOCIPE, STATUS)

	  RETURN

	END IF

*      tell user size of images

	CALL MSG_SETI( 'NX', IDIMS_1( 1))
	CALL MSG_SETI( 'NY', IDIMS_1( 2))
	CALL MSG_OUT( 'MESSAGE',
     :	  'Images are of size ^NX by ^NY ',
     :	  STATUS)

*      set the size of the output images

	ODIMS( 1) = IDIMS_1( 1)
	ODIMS( 2) = IDIMS_1( 2)

*      create the output image and map it in

	CALL CREOUT( 'OUTPICA', 'OTITLE', 2, ODIMS, LOCO, STATUS )

	IF( STATUS .NE. SAI__OK) THEN

	  CALL NDF_ANNUL( LOCIP, STATUS)
	  CALL NDF_ANNUL( LOCIPE, STATUS)

	  RETURN

	END IF

*      yeah yeah yeah, map the damn thing

        CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO, NELEMENTS, STATUS )

	IF( STATUS .NE. SAI__OK) THEN

	  CALL NDF_ANNUL( LOCIP, STATUS)
	  CALL NDF_ANNUL( LOCIPE, STATUS)
	  CALL NDF_ANNUL( LOCO, STATUS)

	  RETURN

	END IF

*      pass the pointer to the subroutine to remove the shot-noise error

	CALL POLSHOT_REMOVE( IDIMS_1(1), IDIMS_1(2), %VAL( PNTRIP),
     :	                     %VAL( PNTRIPE), %VAL( PNTRO))

*      annul association with the input and output images

        CALL NDF_ANNUL( LOCIP, STATUS)
	CALL NDF_ANNUL( LOCIPE, STATUS)
	CALL NDF_ANNUL( LOCO, STATUS)


        END
