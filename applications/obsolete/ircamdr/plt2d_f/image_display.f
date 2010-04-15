	SUBROUTINE IMAGE_DISPLAY( STATUS)

* Description : Main routine to plot an image on a workstation using max/min
*               scaling and man/cur positioning.

* ==============================================================================

* Invocation : CALL IMAGE_DISPLAY( VALUE, STATUS)

* Parameters : VALUE 	- char 	- import/export error message string
*              STATUS 	- int 	- import/export status variable

* Authors : Colin Aspin (UOE/ROE/UKIRT)

* Date : original version 1stApr85 ...

* History :
*   1) CAA, 03rdJul85 : rationalized structure to conform to SSE/ADAM I/O on
*                  image data
*   2) CAA, 31stOct85 : changed to CMP HDS commands and rationalized operation
*   3) CAA, 11thNov85 : added option to get slice/subarray of image and plot
*   4) CAA, 09thMar87 : changed 0 magnif calculation and added error actions,
*                       added cursor positioning option
*   5) CAA, 09thMar87 : changed back to DAT_ routines from CMP_
*   6) CAA, 10thMar87 : added NSIGMA and RANPLOT options
*   7) CAA, 07thDec87 : added VARGREY option
*   8) CAA, 01stJan90 : added setting of image start,end points to IF
*      SKL  17thFeb94   changed DAT and CMP routines to NDF
*      SKL  27thJul94   changed error reporting to use ERR_, removed VALUE
*                       changed IFIX to INT
*      SKL  28thJul94   corrected logic for annulling sub-image locator
*      CAA  20thAug94   modified image st,end calculation
*      SKL  2ndSept94   removed unused variables flagged by UNIX compiler
*      SKL  29thSep94   changed GCA calls for UNIX version of GKS
*      SKL  25thOct94   changed INT magnification variables to REAL
*                       changed start and end logic to add 1 to start
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

	INCLUDE 'SAE_PAR'

* Status :

	INTEGER STATUS

* Import :

* Import-Export :

* Export :

* External references :

* Global variables

	INCLUDE 'PLT2DCOM'

* Local Constants :

* Local variables :

	REAL MAGNIF				! magification factor
	REAL MAGNIF_X			! calculated mag in X
	REAL MAGNIF_Y			! calculated mag in Y
	REAL MAX_MAGNIF			! maximum mag allowed

	INTEGER NAXIS( 2)			! array for image size
	INTEGER NDIMS				! number of dimensions in image
	INTEGER POINTER_IMAGE			! pointer for input image
	INTEGER POINTER_SCRATCH			! pointer for scratch array
	INTEGER SUBIM_EN( 2)			! array for sub-image end
	INTEGER SUBIM_ST( 2)			! array for sub-image start
	INTEGER XIMCEN				! X image centre
	INTEGER YIMCEN				! Y image centre
 	INTEGER LOC_IMAGE	                ! locator for input image
	INTEGER LOCSR		                ! locator for scratch image
	INTEGER LOC_SUBIM	                ! locator for input sub-image
        INTEGER NELEMENTS                       ! number of pointer elements
        INTEGER LBND(2)                         ! lower pixel bounds

	PARAMETER ( MAX_MAGNIF = 500.0)		! Max mag allowed
        DATA LBND / 1, 1 /


	REAL RANGE				! range for ranplot option
	REAL SIGMA_DOWN				! lower sigma level
	REAL SIGMA_LEVEL			! sigma level
	REAL SIGMA_UP				! upper sigma level
	REAL TEMP_X				! temporary X cursor centre
	REAL TEMP_Y				! temporary Y cursor centre
	REAL VARGREY_XPC                        ! percentage cut for X
	REAL VARGREY_YPC                        ! percentage cut for Y
	REAL XMINIMUM				! maximum for scaling
	REAL XMAXIMUM				! minimum for scaling

	CHARACTER*20 CURSOR_WHERE		! cursor position refers to
	CHARACTER*10 NSIGMA_SORT		! defines sigma level use
	CHARACTER*20 PLOT_WHICH			! defines which plot to execute
	CHARACTER*10 SUBIM_OPTION		! sub-image option defined
	CHARACTER*20 USE_CURSOR			! use cursor for positioning

* Internal References :

* ==============================================================================

*      check status on entry

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'Error : On entry ...', STATUS )
	  RETURN
	END IF


*      get the parameter defining the type of plot to be made

	CALL PAR_GET0C( 'PLOT_WHICH', PLOT_WHICH, STATUS)

*      test the plot to be made from list PLOT, NSIGMA, RANPLOT

*        PLOT plot ...

	IF( PLOT_WHICH .EQ. 'PLOT') THEN

*        get min,max for display

	  CALL PAR_GET0R( 'MINIMUM', XMINIMUM, STATUS)
	  CALL PAR_GET0R( 'MAXIMUM', XMAXIMUM, STATUS)

*        NSIGMA plot ...

	ELSE IF( PLOT_WHICH .EQ. 'NSIGMA') THEN

*        get the nsigma level sort

	  CALL PAR_GET0C( 'NSIGMA_SORT', NSIGMA_SORT, STATUS)

*        test the value of nsigma sort and get the level(s)

	  IF( NSIGMA_SORT .EQ. 'SAME') THEN

*          get sigma level for display

	    CALL PAR_GET0R( 'SIGMA_LEVEL', SIGMA_LEVEL, STATUS)

	  ELSE

*          get sigma up and down level for display

	    CALL PAR_GET0R( 'SIGMA_UP', SIGMA_UP, STATUS)
	    CALL PAR_GET0R( 'SIGMA_DOWN', SIGMA_DOWN, STATUS)

	  END IF


*        RANPLOT plot ...

	ELSE IF( PLOT_WHICH .EQ. 'RANPLOT') THEN

*        get plot range for display

	  CALL PAR_GET0R( 'PLOT_RANGE', RANGE, STATUS)

*        VARGREY plot ...

	ELSE IF( PLOT_WHICH .EQ. 'VARGREY') THEN

*        get percentage DN and OUTPUT levels for scaling

	  CALL PAR_GET0R( 'VARGREY_XPC', VARGREY_XPC, STATUS)
	  CALL PAR_GET0R( 'VARGREY_YPC', VARGREY_YPC, STATUS)

	ELSE IF( PLOT_WHICH .NE. 'FLASH' .AND.
     :	         PLOT_WHICH .NE. 'CFLASH') THEN

*        ILLEGAL plot ...

          CALL MSG_OUT('ERR', 'Error, illegal PLOT TYPE', STATUS )
	  RETURN

	END IF

*      get sub-image option

	CALL PAR_GET0C( 'SUBIM_OPTION', SUBIM_OPTION, STATUS)

*        get data image locator from parameter name 'IMAGE'

	CALL NDF_ASSOC( 'IMAGE_NAME', 'READ', LOC_IMAGE, STATUS)

	IF( STATUS. NE. SAI__OK) THEN

          CALL ERR_REP('ERR', 'Error : IMAGE NOT FOUND ...', STATUS )
	  CALL NDF_ANNUL( LOC_IMAGE, STATUS)
	  RETURN

	END IF

*      test sub-image option for whole or slice of image

	IF( SUBIM_OPTION .NE. 'S' .AND. SUBIM_OPTION .NE. 'SUBIM' .AND.
     :	    SUBIM_OPTION .NE. 'SUB_IMAGE') THEN

*        map data in 'DATA_ARRAY' to obtain memory pointer POINTER_IMAGE
*        and size of 2D image in NAXIS

	  CALL NDF_DIM( LOC_IMAGE, 2, NAXIS, NDIMS, STATUS)


	  IF( PLOT_WHICH .EQ. 'FLASH' .OR.
     :	      PLOT_WHICH .EQ. 'CFLASH') THEN

	    CALL NDF_MAP( LOC_IMAGE, 'DATA', '_INTEGER', 'READ',
     :	                  POINTER_IMAGE, NELEMENTS, STATUS)

	  ELSE

	    CALL NDF_MAP( LOC_IMAGE, 'Data', '_REAL', 'READ',
     :	                  POINTER_IMAGE, NELEMENTS, STATUS)

	  END IF

	  IF( STATUS. NE. SAI__OK) THEN

            CALL ERR_REP('ERR', 'Error : During NDF_MAP image', STATUS )
	    CALL NDF_ANNUL( LOC_IMAGE, STATUS)
	    RETURN

	  END IF

	ELSE

*        here if want to plot slice of current image : GET slice area

	  CALL PAR_GET0I( 'SUBIM_XST', SUBIM_ST( 1), STATUS)
	  CALL PAR_GET0I( 'SUBIM_XEN', SUBIM_EN( 1), STATUS)

	  CALL PAR_GET0I( 'SUBIM_YST', SUBIM_ST( 2), STATUS)
	  CALL PAR_GET0I( 'SUBIM_YEN', SUBIM_EN( 2), STATUS)

*        calculate the size of the sub image area

	  NAXIS( 1) = ( SUBIM_EN( 1) - SUBIM_ST( 1) + 1)
	  NAXIS( 2) = ( SUBIM_EN( 2) - SUBIM_ST( 2) + 1)

*        map data in 'DATA_ARRAY' to obtain memory pointer POINTER_IMAGE
*        and size of 2D image in NAXIS

	  CALL NDF_SECT( LOC_IMAGE, 2, SUBIM_ST, SUBIM_EN, LOC_SUBIM,
     :	                  STATUS)

	  IF( PLOT_WHICH .EQ. 'FLASH' .OR.
     :	      PLOT_WHICH .EQ. 'CFLASH') THEN

	    CALL NDF_MAP( LOC_SUBIM, 'Data', '_INTEGER', 'READ',
     :	                  POINTER_IMAGE, NELEMENTS, STATUS)

	  ELSE

	    CALL NDF_MAP( LOC_SUBIM, 'Data', '_REAL', 'READ',
     :	                  POINTER_IMAGE, NELEMENTS, STATUS)

	  END IF

	  IF( STATUS. NE. SAI__OK) THEN

            CALL ERR_REP('ERR', 'Error : During NDF_MAP sub-image',
     :                   STATUS )
	    CALL NDF_ANNUL( LOC_SUBIM, STATUS)
	    CALL NDF_ANNUL( LOC_IMAGE, STATUS)
	    RETURN

	  END IF

	END IF

*      put size of data image into simple variables

	NX = NAXIS( 1)
	NY = NAXIS( 2)

*      put size of current image to parameter system

	CALL PAR_PUT0I( 'IM_XSIZE', NX, STATUS)
	CALL PAR_PUT0I( 'IM_YSIZE', NY, STATUS)

*      create scratch area for scaled image and map it to obtain
*      scratch area memory pointer

	IF( PLOT_WHICH .NE. 'FLASH' .AND.
     :	    PLOT_WHICH .NE. 'CFLASH') THEN

	  CALL NDF_ASSOC( 'SCRATCH_NAME', 'WRITE', LOCSR, STATUS)

*        reset scratch file pixel boundaries
          CALL NDF_SBND( 2, LBND, NAXIS, LOCSR, STATUS )

	  CALL NDF_MAP( LOCSR, 'Data', '_INTEGER', 'WRITE',
     :	                POINTER_SCRATCH, NELEMENTS, STATUS)

	  IF( STATUS. NE. SAI__OK) THEN

            CALL ERR_REP('ERR', 'Error : During NDF_ scratch area',
     :                   STATUS )

	    IF( SUBIM_OPTION .EQ. 'S' .OR. SUBIM_OPTION .EQ. 'SUBIM' .OR.
     :	        SUBIM_OPTION .EQ. 'SUB_IMAGE') THEN

	      CALL NDF_ANNUL( LOC_SUBIM, STATUS)

	    END IF

	    CALL NDF_ANNUL( LOCSR, STATUS)
	    CALL NDF_ANNUL( LOC_IMAGE, STATUS)
	    RETURN

	  END IF

	END IF

*      test which plot is specified and call correct scaling routine

	IF( PLOT_WHICH .EQ. 'PLOT') THEN

*        call subroutine to scale PLOT image creating a scratch image

	  CALL IMAGE_SCALE( %VAL( POINTER_IMAGE),	! Input image
     :	                    %VAL( POINTER_SCRATCH), 	! Scratch area
     :	                    XMAXIMUM, 			! Maximum for scaling
     :	                    XMINIMUM,			! Minimum for scaling
     :	                    STATUS)			! Return status

	ELSE IF( PLOT_WHICH .EQ. 'NSIGMA') THEN

*        call subroutine to scale NSIGMA image creating a scratch image

	  CALL NSIGMA_SCALE( %VAL( POINTER_IMAGE),	! Input image
     :	                   %VAL( POINTER_SCRATCH), 	! Scratch image
     :	                   NSIGMA_SORT,			! Sigma type
     :	                   SIGMA_LEVEL,			! Sigma level
     :	                   SIGMA_UP,			! Upper sigma level
     :	                   SIGMA_DOWN,			! Lower sigma level
     :	                   STATUS)			! Return status



	ELSE IF( PLOT_WHICH .EQ. 'RANPLOT') THEN

*        call subroutine to scale RANPLOT image creating a scratch image

	  CALL RANGE_SCALE(  %VAL( POINTER_IMAGE),	! Input image
     :	                     %VAL( POINTER_SCRATCH), 	! Scratch image
     :	                     RANGE,			! Range for plot
     :	                     STATUS)			! Return status

	ELSE IF( PLOT_WHICH .EQ. 'VARGREY') THEN

*        call subroutine to scale VARGREY image creating a scratch image

	  CALL VARGREY_SCALE(  %VAL( POINTER_IMAGE),	! Input image
     :	                       %VAL( POINTER_SCRATCH), 	! Scratch image
     :	                       VARGREY_XPC,		! X% for cut
     :	                       VARGREY_YPC,		! Y% for cut
     :	                       STATUS)			! Return status

	ELSE IF( PLOT_WHICH .EQ. 'FLASH' .OR.
     :	         PLOT_WHICH .EQ. 'CFLASH') THEN

	END IF

*      get the magnification factor wanted

	CALL PAR_GET0R( 'MAGNIFICATION', MAGNIF, STATUS)

*      if magnification is 0 then need to auto scale to fill screen

	IF( MAGNIF .EQ. 0.0) THEN

*        calculate magnification for maximum image dimension

	  IF( DEVICE_NAME .EQ. 'T5688') THEN

*          set magnification for Sigma 5688 device working in quadrant mode

	    MAGNIF_X = REAL(MAX_X)/2.0/REAL(NX)
	    MAGNIF_Y = REAL(MAX_Y)/2.0/REAL(NY)

	    MAGNIF = MIN( MAGNIF_X, MAGNIF_Y)

	  ELSE

*          set magnification for other devices

	    MAGNIF_X = REAL(MAX_X)/REAL(NX)
	    MAGNIF_Y = REAL(MAX_Y)/REAL(NY)

	    MAGNIF = MIN( MAGNIF_X, MAGNIF_Y)

	  END IF

*        set magnification to 80% display area for annotation space

          MAGNIF = 0.80 * MAGNIF

*        test for above maximum value for magnification

	  IF( MAGNIF .GT. MAX_MAGNIF) THEN

	    MAGNIF = MAX_MAGNIF

	  END IF

*      put the magnification to the output parameter

	  CALL  PAR_PUT0R( 'IMAGE_CALMAG', MAGNIF, STATUS)

	END IF

*      get option to cursor position image displayed

	CALL PAR_GET0C( 'USE_CURSOR', USE_CURSOR, STATUS)

*      test value of cursor use option

	IF( USE_CURSOR .EQ. 'YES' .OR. USE_CURSOR .EQ. 'Y') THEN

*        display cursor to select position of the centre of the image

	  CALL CURSOR_POSITION( STATUS)

*        get start,end in X and Y for image position

	  CALL PAR_GET0R( 'X_CUR_REAL', TEMP_X, STATUS)
	  CALL PAR_GET0R( 'Y_CUR_REAL', TEMP_Y, STATUS)

	  CALL PAR_GET0C( 'CURSOR_WHERE', CURSOR_WHERE, STATUS)

*        test where user wants cursor positioning to work on ...

	  IF( CURSOR_WHERE .EQ. 'BOTTOM_LEFT') THEN

*          define start and end pixels for image plot

	    IM_XST = REAL(INT( TEMP_X + 0.5))
	    IM_YST = REAL(INT( TEMP_Y + 0.5))

	    IM_XEN = REAL((IM_XST - 1.0) + INT(MAGNIF*NX))
	    IM_YEN = REAL((IM_YST - 1.0) + INT(MAGNIF*NY))

	  ELSE IF( CURSOR_WHERE .EQ. 'TOP_RIGHT') THEN

*          define start and end pixels for image plot

	    IM_XST = REAL(INT( TEMP_X + 0.5) - INT(MAGNIF*NX) + 1.0)
	    IM_YST = REAL(INT( TEMP_Y + 0.5) - INT(MAGNIF*NY) + 1.0)

	    IM_XEN = REAL((IM_XST - 1.0) + INT(MAGNIF*NX))
	    IM_YEN = REAL((IM_YST - 1.0) + INT(MAGNIF*NY))

	  ELSE

*          define start and end pixels for image plot

	    IM_XST = REAL(INT( TEMP_X + 0.5) - INT(MAGNIF*(NX/2)) + 1.0)
	    IM_YST = REAL(INT( TEMP_Y + 0.5) - INT(MAGNIF*(NY/2)) + 1.0)

	    IM_XEN = REAL((IM_XST - 1.0) + INT(MAGNIF*NX))
	    IM_YEN = REAL((IM_YST - 1.0) + INT(MAGNIF*NY))

	  END IF

*        set real X,Y centre variables

	  XIMCEN = INT( IM_XST + ( IM_XEN - IM_XST)/2.0 + 0.5)
	  YIMCEN = INT( IM_YST + ( IM_YEN - IM_YST)/2.0 + 0.5)

*      else get specific position for image display

	ELSE

*        get start,end in X and Y for image position

	  CALL PAR_GET0I( 'IM_XCEN', XIMCEN, STATUS)
	  CALL PAR_GET0I( 'IM_YCEN', YIMCEN, STATUS)

*        test if image centres are zero; if so then calculate centre of screen

	  IF( XIMCEN .LE. 0 .OR. YIMCEN .LE. 0) THEN

	    IF( DEVICE_NAME .EQ. 'T5688') THEN

	      XIMCEN = MAX_X/4
	      YIMCEN = MAX_Y/4

	    ELSE

	      XIMCEN = MAX_X/2
	      YIMCEN = MAX_Y/2

	    END IF

	  END IF

*        define start and end pixels for image plot

	  IM_XST = REAL(XIMCEN - INT(MAGNIF*(NX/2)) + 1.0)
	  IM_YST = REAL(YIMCEN - INT(MAGNIF*(NY/2)) + 1.0)

	  IM_XEN = (IM_XST - 1.0) + MAGNIF*NX
	  IM_YEN = (IM_YST - 1.0) + MAGNIF*NY

	END IF

d      write (6,*) 'got max x,y:  ', max_x, max_y
d      write (6,*) 'got x start, end:  ', im_xst, im_xen
d      write (6,*) 'got y start, end:  ', im_yst, im_yen

*      test if device is Args and check bounds of plot since if you attempt
*      to plot outside plotting area now the task crashes ... GKS7 problem

	IF( DEVICE_NAME .EQ. 'ARGS') THEN

	  IF( IM_XST .LT. 1.0) IM_XST = 1.0
	  IF( IM_YST .LT. 1.0) IM_YST = 1.0
	  IF( IM_XEN .GT. 512.0) IM_XEN = 512.0
	  IF( IM_YEN .GT. 512.0) IM_YEN = 512.0

	END IF

*      check to see if image goes off screen and change if does

	IF( IM_XST .LT. 1 .OR. IM_YST .LT. 1 .OR.
     :	    IM_XEN .GT. MAX_X .OR. IM_YEN .GT. MAX_Y) THEN
          CALL MSG_OUT( 'ERR',
     :  'With current magnification image exceeds display  - QUITING',
     :	    STATUS)
	  IF( SUBIM_OPTION .EQ. 'S' .OR. SUBIM_OPTION .EQ. 'SUBIM' .OR.
     :	      SUBIM_OPTION .EQ. 'SUB_IMAGE') THEN
	    CALL NDF_ANNUL( LOC_SUBIM, STATUS)
	  END IF
	  CALL NDF_ANNUL( LOCSR, STATUS)
	  CALL NDF_ANNUL( LOC_IMAGE, STATUS)
	  STATUS = SAI__OK
	  RETURN
	END IF

*      set the x,y centre parameters

	CALL PAR_PUT0I( 'IM_XCEN', XIMCEN, STATUS)
	CALL PAR_PUT0I( 'IM_YCEN', YIMCEN, STATUS)

*      put image start,end coordinates to parameter system

	CALL PAR_PUT0R( 'IM_XST', IM_XST, STATUS)
	CALL PAR_PUT0R( 'IM_YST', IM_YST, STATUS)

	CALL PAR_PUT0R( 'IM_XEN', IM_XEN, STATUS)
	CALL PAR_PUT0R( 'IM_YEN', IM_YEN, STATUS)

*      test if an error occurred

	IF( STATUS .NE. SAI__OK) THEN

          CALL ERR_REP('ERR', 'Error : just before GCA ...', STATUS )

	  IF( SUBIM_OPTION .EQ. 'S' .OR. SUBIM_OPTION .EQ. 'SUBIM' .OR.
     :	      SUBIM_OPTION .EQ. 'SUB_IMAGE') THEN

	    CALL NDF_ANNUL( LOC_SUBIM, STATUS)

	  END IF

	  CALL NDF_ANNUL( LOCSR, STATUS)
	  CALL NDF_ANNUL( LOC_IMAGE, STATUS)
	  RETURN

	END IF

*      plot data image after scaling

	IF( PLOT_WHICH .NE. 'FLASH' .AND.
     :	    PLOT_WHICH .NE. 'CFLASH') THEN


d      write (6, *) 'NSIGMA plot about to call GCA routines with ...'
d 	type *, 'xst, yst = ', im_xst, im_yst
d	type *, 'xen, yen = ', im_xen, im_yen
d	type *, 'nx, ny   = ', naxis(1), naxis( 2)

	  CALL GCA( IM_XST, 			! X upper left
     :	            IM_YEN, 			! Y upper left
     :	            IM_XEN,			! X lower right
     :	            IM_YST, 			! Y lower right
     :	            NAXIS( 1),			! X size of image
     :	            NAXIS( 2),			! Y size of image
     :              1, 1,               ! subarray start
     :	            NAXIS( 1),	NAXIS( 2),		! size of subarray
     :	            %VAL( POINTER_SCRATCH))	! scaled image array

	ELSE

	  CALL GCA( IM_XST,
     :	            IM_YEN,
     :	            IM_XEN,
     :	            IM_YST,
     :	            NAXIS( 1),
     :	            NAXIS( 2),
     :              1, 1,
     :	            NAXIS( 1),	NAXIS( 2),
     :	            %VAL( POINTER_IMAGE))

	END IF

*      flush buffer of residual output

	CALL SGS_FLUSH


*      set the image position start and end parameters in interface file

	CALL PAR_PUT0R( 'IM_XST', REAL( IM_XST), STATUS)
	CALL PAR_PUT0R( 'IM_XEN', REAL( IM_XEN), STATUS)
	CALL PAR_PUT0R( 'IM_YST', REAL( IM_YST), STATUS)
	CALL PAR_PUT0R( 'IM_YEN', REAL( IM_YEN), STATUS)

*      release data depending on whether full and sub-image plotted

	IF( SUBIM_OPTION .EQ. 'S' .OR. SUBIM_OPTION .EQ. 'SUBIM' .OR.
     :	    SUBIM_OPTION .EQ. 'SUB_IMAGE') THEN

 	  CALL NDF_ANNUL( LOC_SUBIM, STATUS)

	END IF

*      delete the temporary working space

	IF( PLOT_WHICH .NE. 'FLASH' .AND.
     :	    PLOT_WHICH .NE. 'CFLASH') THEN

	  CALL NDF_ANNUL( LOCSR, STATUS)

	END IF

*      annul top locator to data

	CALL NDF_ANNUL( LOC_IMAGE, STATUS)

	END
