*+  CSFIT - calculates least-squares best fit of centro-symmetry

	SUBROUTINE CSFIT ( STATUS )

*    Description :

*    Parameters :

*    Method :

*    Authors :

*    History :
*     18-May-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
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
     :    LOCIT,               ! locator for input data structure
     :	  LOCIP,               !
     :	  LOCOT,               !
     :    DIMST( NDIM ),! dimensions of the input/output DATA_ARRAYs
     :    DIMSP( NDIM ),! dimensions of the input/output DATA_ARRAYs
     :    ACTDIM,       ! actual dimensions from NDF_DIM
     :    NELEMENTS,    ! number of elements mapped by NDF_MAP
     :    PNTRIT,       ! pointer to : input DATA_ARRAY
     :    PNTRIP,       ! pointer to : input DATA_ARRAY
     :    PNTROT        ! pointer to : output DATA_ARRAY
      INTEGER
     :	  XST,          !
     :	  XEN,          !
     :	  YST,          !
     :	  YEN,          !
     :	  AXST,         !
     :	  AXEN,         !
     :	  AYST,         !
     :	  AYEN,         !
     :	  XRES,         !
     :	  YRES          !


	INTEGER
     :	  MAXPIX

	PARAMETER ( MAXPIX = 100000)

	REAL
     :	  RESIDUAL,     !
     :	  POLMIN        !

	CHARACTER*80
     :	  OUTFILE       !

*-
*      get locator to input IMAGE type data structure
	CALL GETINP( 'INPICP', LOCIP, STATUS )
	CALL GETINP( 'INPICT', LOCIT, STATUS )

*      check for error
	IF( STATUS .EQ. SAI__OK ) THEN

*        map input DATA_ARRAY component and get dimensions

          CALL NDF_MAP( LOCIP, 'DATA', '_REAL', 'READ',
     :                  PNTRIP, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCIP, NDIM, DIMSP, ACTDIM, STATUS )

          CALL NDF_MAP( LOCIT, 'DATA', '_REAL', 'READ',
     :                  PNTRIT, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCIT, NDIM, DIMST, ACTDIM, STATUS )

*        check for error
	  IF( STATUS .EQ. SAI__OK) THEN

*          see if dimensions of input images are same
	    IF( DIMSP( 1) .EQ. DIMST( 1) .AND.
     :	        DIMSP( 2) .EQ. DIMST( 2)) THEN

*            tell user image size
	      CALL MSG_SETI( 'NX', DIMSP( 1))
	      CALL MSG_SETI( 'NY', DIMST( 2))
	      CALL MSG_OUT( 'MESS',
     :         'Images are ^NX by ^NY pixels in size', STATUS )

*            get the start and end of the area of image cs pattern to be
*            considered
              CALL AIF_GET0I( 'XSTART', 1, 1, DIMSP( 1), XST, STATUS )
              CALL AIF_GET0I( 'YSTART', 1, 1, DIMSP( 2), YST, STATUS )
              CALL AIF_GET0I( 'XEND', MAXPIX, 1, MAXPIX, XEN, STATUS )
              CALL AIF_GET0I( 'YEND', MAXPIX, 1, MAXPIX, YEN, STATUS )
	      IF( XEN .GT. DIMSP( 1)) XEN = DIMSP( 1)
	      IF( YEN .GT. DIMSP( 2)) YEN = DIMSP( 2)

*            get the start and end of the area centre to be considered
              CALL AIF_GET0I( 'AXSTART', 1, 1, DIMSP( 1), AXST,
     :                         STATUS )
              CALL AIF_GET0I( 'AYSTART', 1, 1, DIMSP( 2), AYST,
     :                         STATUS )
              CALL AIF_GET0I( 'AXEND', MAXPIX, 1, MAXPIX, AXEN,
     :                         STATUS )
              CALL AIF_GET0I( 'AYEND', MAXPIX, 1, MAXPIX, AYEN,
     :                         STATUS )
	      IF( AXEN .GT. DIMSP( 1)) AXEN = DIMSP( 1)
	      IF( AYEN .GT. DIMSP( 2)) AYEN = DIMSP( 2)

*            get minimum percentage polarization in calculation
	      CALL PAR_GET0R( 'POLMIN', POLMIN, STATUS)

*            get name of output text file
	      CALL PAR_GET0C( 'OUTFILE', OUTFILE, STATUS)

*            see if error
	      IF( STATUS .EQ. SAI__OK) THEN

*              create output image
	        CALL CREOUT( 'OUTPIC', 'OTITLE', NDIM, DIMSP, LOCOT,
     :	                     STATUS )

*              map output image
                CALL NDF_MAP( LOCOT, 'DATA', '_REAL', 'WRITE',
     :                        PNTROT, NELEMENTS, STATUS )

*              see if error
	        IF( STATUS .EQ. SAI__OK) THEN

*                call subroutine to do work
	          CALL CSFITSUB( DIMSP( 1), DIMSP( 2), %VAL( PNTRIP),
     :	                         %VAL( PNTRIT), XST, YST, XEN, YEN,
     :	                         AXST, AYST, AXEN, AYEN, POLMIN,
     :	                         OUTFILE,%VAL( PNTROT), RESIDUAL,
     :                           XRES, YRES, STATUS)

*                tell user the bad news
	          CALL MSG_OUT( 'BLANK', ' ', STATUS)
	          CALL MSG_SETI( 'XS', XST)
	          CALL MSG_SETI( 'YS', YST)
	          CALL MSG_SETI( 'XE', XEN)
	          CALL MSG_SETI( 'YE', YEN)
	          CALL MSG_OUT( 'MESS',
     :	            'Area of vector map fitted = ^XS,^YS to ^XE,^YE',
     :	            STATUS)
	          CALL MSG_SETI( 'AXS', AXST)
	          CALL MSG_SETI( 'AYS', AYST)
	          CALL MSG_SETI( 'AXE', AXEN)
	          CALL MSG_SETI( 'AYE', AYEN)
	          CALL MSG_OUT( 'MESS',
     :	          'Area considered as center = ^AXS,^AYS to ^AXE,^AYE',
     :	            STATUS)
	          CALL MSG_SETI( 'XP', XRES)
	          CALL MSG_SETI( 'YP', YRES)
	          CALL MSG_OUT( 'MESS',
     :	          'Least-squares fit suggests centre pixel = ^XP,^YP' ,
     :	            STATUS)
	          CALL MSG_SETR( 'RE', RESIDUAL)
	          CALL MSG_OUT( 'MESS',
     :	            'Standard deviation of fit               = ^RE',
     :	            STATUS)
	          CALL MSG_OUT( 'BLANK', ' ', STATUS)

	        END IF

*              release output image
	        CALL NDF_ANNUL(  LOCOT, STATUS )

	      END IF

	    ELSE

*            tell user image diemsions are wrong
	      CALL MSG_OUT( 'ERR',
     :	        'Error, input images different sizes', STATUS)
	      CALL MSG_SETI( 'XP', DIMSP( 1))
	      CALL MSG_SETI( 'YP', DIMSP( 2))
	      CALL MSG_OUT( 'ERR',
     :	        'P-IMAGE is ^XP by ^YP pixels', STATUS)
	      CALL MSG_SETI( 'XT', DIMST( 1))
	      CALL MSG_SETI( 'YT', DIMST( 2))
	      CALL MSG_OUT( 'ERR',
     :	        'T-IMAGE is ^XT by ^YT pixels', STATUS)

	    END IF
	  END IF

*        tidy up structure
	  CALL NDF_ANNUL(  LOCIP, STATUS )
	  CALL NDF_ANNUL(  LOCIT, STATUS )

	END IF

	END
