        SUBROUTINE CONTOUR_PLOT( STATUS)

* Description : To plot a contour map on a GKS/SGS workstation

* =========================================================================

* Invocation : Invoked by the PLT2D

* Parameters : Defined in interface module

* Options : Defined in interface module

* Authors : C.Aspin (UOE)

* History :
* 1) CAA :  1stNov85 : Modified to work under PLT2D
* 2) CAA : 14thNov85 : Added image slice option to data set
* 3) CAA : 29thSep86 : Added auto contour level option
*    SKL   17thFeb94   Changed DAT and CMP routines to NDF
*    SKL   26THJul94   Changed error reporting to use ERR_, removed VALUE
*    SKL   26thOct94   Changed MAGNIF from INT to REAL
*    SKL   04thNov94   Added option of auto scaling if magnif=0
* Endhistory

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

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

* Import :

        INTEGER STATUS

* Import-Export :

* Export :

* External references :

* Global variables

        INCLUDE 'PLT2DCOM'

* Local Constants :

* Local variables :

        INTEGER NAXIS( 2)
        INTEGER CONTOUR_NUMBER
        REAL CONTOUR_MAGNIF
        INTEGER POINTER_IMAGE
        INTEGER SUBIM_EN( 2)
        INTEGER SUBIM_ST( 2)
        INTEGER LOC_IMAGE
        INTEGER LOC_SUBIM
        INTEGER NELEMENTS
        INTEGER NDIM

        REAL CONTOUR_BASE
        REAL CONTOUR_INTERVAL
        REAL MAGNIF_X
        REAL MAGNIF_Y
        REAL MAX_MAGNIF

        PARAMETER ( MAX_MAGNIF = 500.0 )

        CHARACTER*80 CONTOUR_AUTO
        CHARACTER*80 CONTOUR_ANNOT
        CHARACTER*40 CONTOUR_TITLE
        CHARACTER*1 SUBIM_OPTION

* Internal References :

* Local data :

* =========================================================================

* Test of input status OK and act if incorrect

        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'ERR', 'Error : CONTOUR_PLOT : On entry',
     :                   STATUS )
          RETURN
        END IF

* get contour start, auto mode, levels variables from parameter system

        CALL PAR_GET0C( 'CONTOUR_AUTO', CONTOUR_AUTO, STATUS)
        CALL PAR_GET0I( 'CONTOUR_NUMBER', CONTOUR_NUMBER, STATUS)
        CALL PAR_GET0R( 'CONTOUR_MAGNIF', CONTOUR_MAGNIF, STATUS)
        CALL PAR_GET0R( 'CONTOUR_BASE', CONTOUR_BASE, STATUS)
        CALL PAR_GET0R( 'CONTOUR_STEP', CONTOUR_INTERVAL, STATUS)

* get plot title from parameter system

        CALL PAR_GET0C( 'CONTOUR_TITLE', CONTOUR_TITLE, STATUS)

* get the option to plot annotation or not as the case may be ...

        CALL PAR_GET0C( 'CONTOUR_ANNOTAT', CONTOUR_ANNOT, STATUS)

* get sub-image option

        CALL PAR_GET0C( 'SUBIM_OPTION', SUBIM_OPTION, STATUS)

        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'ERR',
     :                  'Error : CONTOUR_PLOT : after PAR_GETs',
     :                   STATUS )
          RETURN
        END IF

* get data from HDS container file

        CALL NDF_ASSOC( 'CONTOUR_IMAGE', 'READ', LOC_IMAGE, STATUS)

        IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP( 'ERR',
     :                  'Error : CONTOUR_PLOT : During NDF_ASSOC',
     :                   STATUS )
          RETURN
        END IF

* test sub-image option for whole or slice of image

        IF( SUBIM_OPTION .NE. 'S') THEN

* map data in 'DATA_ARRAY' to obtain memory pointer POINTER_IMAGE
* and size of 2D image in NAXIS

          CALL NDF_MAP( LOC_IMAGE, 'Data', '_REAL', 'READ',
     :                  POINTER_IMAGE, NELEMENTS, STATUS)

          IF( STATUS. NE. SAI__OK)THEN
            CALL ERR_REP( 'ERR',
     :                'Error : CONTOUR_PLOT : During NDF_MAP image',
     :                     STATUS )
            RETURN
          END IF

          CALL NDF_DIM(LOC_IMAGE, 2, NAXIS, NDIM, STATUS)

D       write (6,*) 'NDF_DIM dimensions returned:     '
D       write (6,*)  naxis(1), '  and  ', naxis(2)


        ELSE

* here if want to plot slice of current image : GET slice area

          CALL PAR_GET0I( 'SUBIM_XST', SUBIM_ST( 1), STATUS)
          CALL PAR_GET0I( 'SUBIM_XEN', SUBIM_EN( 1), STATUS)
          CALL PAR_GET0I( 'SUBIM_YST', SUBIM_ST( 2), STATUS)
          CALL PAR_GET0I( 'SUBIM_YEN', SUBIM_EN( 2), STATUS)

* GET slice locator

          CALL NDF_SECT( LOC_IMAGE, 2, SUBIM_ST, SUBIM_EN, LOC_SUBIM,
     :                    STATUS)

* map data in 'DATA_ARRAY' to obtain memory pointer POINTER_IMAGE
* and size of 2D image in NAXIS

          CALL NDF_MAP( LOC_SUBIM, 'Data', '_REAL', 'READ',
     :                   POINTER_IMAGE, NELEMENTS, STATUS)

          IF( STATUS. NE. SAI__OK)THEN
            CALL ERR_REP( 'ERR',
     :            'Error : CONTOUR_PLOT : During NDF_MAP sub-image',
     :                     STATUS )
            RETURN
          END IF

          NAXIS( 1) = ( SUBIM_EN( 1) - SUBIM_ST( 1) + 1)
          NAXIS( 2) = ( SUBIM_EN( 2) - SUBIM_ST( 2) + 1)

        END IF

* put size of data image into common variables

        NX = NAXIS( 1)
        NY = NAXIS( 2)


*      if magnification is 0 then need to auto scale to fill screen

	IF( CONTOUR_MAGNIF .EQ. 0.0) THEN

*        calculate magnification for maximum image dimension

	  IF( DEVICE_NAME .EQ. 'T5688') THEN

*          set magnification for Sigma 5688 device working in quadrant mode

	    MAGNIF_X = REAL(MAX_X)/2.0/REAL(NX)
	    MAGNIF_Y = REAL(MAX_Y)/2.0/REAL(NY)

	    CONTOUR_MAGNIF = MIN( MAGNIF_X, MAGNIF_Y)

	  ELSE

*          set magnification for other devices

	    MAGNIF_X = REAL(MAX_X)/REAL(NX)
	    MAGNIF_Y = REAL(MAX_Y)/REAL(NY)

	    CONTOUR_MAGNIF = MIN( MAGNIF_X, MAGNIF_Y)

	  END IF

*        set magnification to 80% display area for annotation space

          CONTOUR_MAGNIF = 0.80 * CONTOUR_MAGNIF

*        test for above maximum value for magnification

	  IF( CONTOUR_MAGNIF .GT. MAX_MAGNIF) THEN

	    CONTOUR_MAGNIF = MAX_MAGNIF

	  END IF

*        put calculated magnification to parameter

	  CALL PAR_PUT0R( 'CONTOUR_CALMAG', CONTOUR_MAGNIF, STATUS)

	END IF

* call routine to plot contour plot

        CALL CONTOUR_DRAW(   %VAL( POINTER_IMAGE),
     :                       CONTOUR_ANNOT,
     :                       CONTOUR_NUMBER,
     :                       CONTOUR_AUTO,
     :                       CONTOUR_BASE,
     :                       CONTOUR_INTERVAL,
     :                       CONTOUR_MAGNIF,
     :                       CONTOUR_TITLE,
     :                       STATUS)

* release data depending on whether full and sub-image plotted

        IF( SUBIM_OPTION .EQ. 'S') THEN

* unmap sub-image data and release locator

          CALL NDF_ANNUL( LOC_SUBIM, STATUS)
          CALL NDF_ANNUL( LOC_IMAGE, STATUS)

          IF( STATUS .NE. SAI__OK) THEN
            CALL ERR_REP( 'ERR',
     :            'Error : CONTOUR_PLOT : after NDF_ANNUL sub-image',
     :                     STATUS )
            RETURN
          END IF

        ELSE

* unmap image data and release locator

          CALL NDF_ANNUL( LOC_IMAGE,   STATUS)

          IF( STATUS .NE. SAI__OK) THEN
            CALL ERR_REP( 'ERR',
     :                    'Error : CONTOUR_PLOT : after NDF_ANNUL',
     :                     STATUS )
            RETURN
          END IF

        END IF

        END
