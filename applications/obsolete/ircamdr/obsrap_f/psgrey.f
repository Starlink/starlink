* PSGREY - plots grey scale to postscript device

	SUBROUTINE PSGREY( STATUS)

* Description :
*
* Invocation :
*
*     CALL PSGREY( STATUS)
*
* Parameters :
*
*     INPIC = ( STRUCTURE)        ! name of sdf file
*
* Method :
*
* Bugs :
*
*     None known.  Note lib$spawn command is vax and site-specific.
*
* Authors :
*
*     Colin Aspin ROE ( REVA::CAA )
*
* History :
*
*     26-03-1988 : First implementation (UKTH::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     23-JUN-1994  Changed MSG_OUT on error to ERR_REP (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE			! no default typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'		! SSE global definitions
        INCLUDE  'NDF_PAR'
        INCLUDE  'NDF_ERR'

* Status :

	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

	INTEGER
     :    LOCI,				! locator for input structure
     :	  DIMS( 2),                     ! axes sizes
     :    ACTDIM,                       ! actual dimensions from NDF_DIM
     :    NELEMENTS,                    ! number of elements mapped by NDF_MAP
     :	  IPNTR,
     :	  NDIMS,
     :	  ISTAT,
     :	  IXST,                         ! The first x-pixel to be used.
     :	  IXEN,                         ! The last x-pixel to be used.
     :	  IYST,                         ! The first y-pixel to be used.
     :	  IYEN                          ! The last y-pixel to be used.

	PARAMETER ( NDIMS = 2)

	REAL
     :    HIGH,                          ! Max data value to be used (the
*                                        ! white level)
     :	  LOW                            ! Min data value to be used (the
*                                        ! black level)
	CHARACTER
     :    DEVICE*80,                    ! The device/type to be used for the
*                                       ! plot, in the form required by PGBEGIN.
     :	  LABEL*80,                     ! A label for the plot.
     :	  XLABEL*80,                    ! A label for the plot.
     :	  YLABEL*80                     ! A label for the plot.

	LOGICAL
     :	  ERASE,                         ! Erase the screen before plotting,
*                                        ! if true.
     :	  AXES,                          ! Plot and label axes, if true.
     :	  ADJUST                         ! Adjust scales so as to fil display.
*-
*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	   RETURN
	END IF

*      initialzie local status variable for pgplot
	ISTAT = 0

*     get a locator to input IMAGE type data structure
	CALL GETINP( 'INPIC', LOCI, STATUS )

*     check for error
	IF( STATUS .EQ. SAI__OK ) THEN

*        map input DATA_ARRAY component and get dimensions
          CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  IPNTR, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

*       check for error
	  IF( STATUS .EQ. SAI__OK ) THEN

*          tell user size of input image
	    CALL MSG_SETI( 'NX', DIMS( 1))
	    CALL MSG_SETI( 'NY', DIMS( 2))
	    CALL MSG_OUT( 'MESS', 'Input image is ^NX by ^NY pixels',
     :	                  STATUS)

*          get the start pixel in x,y and end in x,y
	    CALL AIF_GET0I( 'IXST', 1, 1, DIMS( 1), IXST, STATUS )
	    CALL AIF_GET0I( 'IYST', 1, 1, DIMS( 2), IYST, STATUS )
	    CALL AIF_GET0I( 'IXEN', DIMS( 1), 1, 1000, IXEN,
     :                       STATUS )
	    CALL AIF_GET0I( 'IYEN', DIMS( 2), 1, 1000, IYEN,
     :                       STATUS )

*          setup maximum end values if user input 1000
	    IF( IXEN .EQ. 1000) IXEN = DIMS( 1)
	    IF( IYEN .EQ. 1000) IYEN = DIMS( 2)

*          get device from interface file
	    CALL PAR_GET0C( 'DEVICE', DEVICE, STATUS)

*          setup erasure of screen before plotting
	    ERASE = .FALSE.

*          setup plotting of axes
	    AXES = .TRUE.

*          setup adjustment of size to fill screen parameter
	    ADJUST = .FALSE.

*          get the maximum/minimum value to be white/black
	    CALL AIF_GET0R( 'HIGH', 255.0, -1.0E20, 1.0E20, HIGH,
     :	                    STATUS )
	    CALL AIF_GET0R( 'LOW', 0.0, -1.0E20, 1.0E20, LOW,
     :	                    STATUS )

*          get the label TOP/X/Y for top of plot
	    CALL PAR_GET0C( 'LABEL', LABEL, STATUS)
	    CALL PAR_GET0C( 'XLABEL', XLABEL, STATUS)
	    CALL PAR_GET0C( 'YLABEL', YLABEL, STATUS)

*          call fig routine to plot greyscale
            CALL LFIG_GPLOT( %VAL( IPNTR), DIMS( 1), DIMS( 2), IXST,
     :	                     IXEN, IYST, IYEN, DEVICE, LABEL, XLABEL,
     :	                     YLABEL, ERASE, AXES, ADJUST, HIGH, LOW,
     :                       ISTAT)

	  ELSE

	    CALL ERR_REP( 'ERR',
     :	       'STATUS not OK after NDF_MAP ', STATUS)

	  END IF


	ELSE

	    CALL ERR_REP( 'ERR',
     :	      'STATUS not OK after GETINP ', STATUS)

	END IF

	CALL NDF_ANNUL(  LOCI, STATUS )


	END
