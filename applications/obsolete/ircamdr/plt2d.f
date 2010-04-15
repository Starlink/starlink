        SUBROUTINE PLT2D( STATUS)
*
* Description : D-task to plot images and related phenomena on workstation
*               using SGS/GKS 7.2 software under ADAM/SSE implementation.
* =========================================================================
*
* Invocation : Invoked by the ADAM D-task environment
*
* Subroutines called : NONE
*
* Parameters : As defined in interface module PLT2D.IFL
*
* Options : DEVINIT
*
* Authors : C.Aspin (UOE) - 1stFeb85
* History
*   Top level DEVINIT
* 17-FEB-1994 SKL@JACH: Scratch file created using NDF call
* 17-Aug-1994 SKL@JACH: Changed DEVINIT and ACT routines to new ADAM
*                       Instrument task style single routine
*   ACT subroutine
*  1) CAA : 28thJun85 : rationalized routine to make more general and ADAM
*                       compatible
*  2) CAA : 01stJul85 : added GRID, XLINE, YLINE options
*  3) CAA : 31stOct85 : rationalized actions and modified cursoring
*  4) CAA : 03rdNov85 : added FLASH option and renamed several others
*  5) CAA : 06thNov85 : added LINE, CURLINE, BOX, CURBOX, COMMENT, CURCOM
*                       options
*  6) CAA : 07thNov85 : added CIRCLE, CURCIR, CROSS, CURCRO options
*  7) CAA : 08thNov85 : moved SGS/GKS open/closed to OPEN,CLOSE options
*  8) CAA : 14thNov85 : added CUT,CURCUT options
*  9) CAA : 03rdFeb86 : added plus-minus Nsigma plot option called NSIGMA
* 10) CAA : 16thApr86 : added CURPLOT action
* 11) CAA : 25thApr86 : added CURBLOCK action
* 12) CAA : 25thApr86 : added CURNSIGMA action
* 13) CAA : 01stMay86 : added LABEL action
* 14) CAA : 20thMay86 : added RANPLOT action
* 15) CAA : 17thJun86 : added IMCLEAR action
* 16) CAA : 23rdJun86 : added CURANPLOT action
* 17) CAA : 21stSep87 : started conversion to GKS 7.2
* 18) CAA : 14thSep88 : changed CURCUT to use CUT with CURSOR definition
* 19) CAA : 25thJan89 : added DEVICE_OPENED check to see if device opened
* 20) CAA : 09thAug90 : added VMONNSIGMA/VMONNSIGMA2 actions
* 21) CAA : 29thAug93 : added WELCOME action
* 22) CAA : 07thSep93 : added CONTPARS action
* SKL@JACH  24thFeb94 : Removed MON calls, ANNOTATE_DEFAULT and LABEL_PLOT
* SKL@JACH  24thFeb94 : Added ROPARS and RONEXT
* SKL@JACH  25thJul94 : Removed 'value' from action arguments, errors
*                       reported using ERR_ system within routines
* TIMJ@JACH 11Aug2004 : Use ADAM_DEFNS
* Endhistory
*
* Type Definitions
        IMPLICIT NONE
*
* Global constants :
        include 'SAE_PAR'
        INCLUDE 'PAR_PAR'               ! Necessary for non-VMS
        INCLUDE 'ADAM_DEFNS'
        INCLUDE 'ACT_ERR'
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

*
* Import :
        INTEGER STATUS
*
* Import-Export :
*
* Export :
*
* External references :
*
* Global variables
        INCLUDE 'PLT2DCOM'
*
* Local Constants :
*
* Local variables :

        CHARACTER*(PAR__SZNAM)  NAME         ! Action name
        CHARACTER*10 DEVICE_OPENED

        INTEGER CONTEXT            ! Should be OBEY
        INTEGER SEQ                ! SEQ = 0 on first call to routine
        INTEGER BASE_ZONE
        INTEGER LOCSR              ! Locator to scratch file
        INTEGER PNTSR              ! pointer to scratch file
        INTEGER NELEMENTS          ! Number of elements mapped by NDF_MAP
        INTEGER TEMP_LBND(2)       ! Lower bound scratch array
        INTEGER TEMP_UBND(2)       ! Upper bound scratch array

*
* Internal References :
*
* Local data :

        DATA TEMP_LBND / 1, 1 /
        DATA TEMP_UBND / 2, 2 /
*
*

* =========================================================================
*
* check status for ok value
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'ERR', 'Error : PLT2D On entry', STATUS )
          CALL ERR_FLUSH( STATUS )
          RETURN
        END IF


* Pick up required ACT parameters

        CALL TASK_GET_NAME( NAME, STATUS )
        CALL TASK_GET_CONTEXT( CONTEXT, STATUS )
        CALL TASK_GET_SEQ( SEQ, STATUS )


        IF (SEQ .EQ. 0) THEN

           CALL NDF_BEGIN

*       See if scratch image PLT2D_SCRATCH in ADAM_USER already exits

           CALL NDF_EXIST( 'SCRATCH_NAME', 'WRITE', LOCSR, STATUS)

*       If it doesn't exist then create a small image as for subsequent assocs

           IF ( LOCSR .EQ. NDF__NOID ) THEN

              CALL NDF_CREAT ( 'SCRATCH_NAME', '_INTEGER', 2, TEMP_LBND,
     :                         TEMP_UBND, LOCSR, STATUS)

           END IF

           CALL NDF_MAP( LOCSR, 'DATA', '_INTEGER', 'WRITE', PNTSR,
     :                   NELEMENTS, STATUS )



*    Just create file for now, and define data array, can release locator

           CALL NDF_END( STATUS )

        END IF



*
* test if context is OBEY
        IF ( CONTEXT .EQ. OBEY) THEN
*
* get parameter defining whether device has been openned or not
          CALL PAR_GET0C( 'DEVICE_OPENED', DEVICE_OPENED, STATUS)
*
* test parameter to see if action allowed for opened/closed device
          IF( DEVICE_OPENED .EQ. 'NO' .AND.
     :        NAME .NE. 'OPEN' .AND.
     :        NAME .NE. 'WELCOME' .AND.
     :        NAME .NE. 'RONEXT' .AND.
     :        NAME .NE. 'ROPARS' .AND.
     :        NAME .NE. 'CONTPARS') THEN
            CALL MSG_OUT('ERR',
     :           'Error, need to OPEN plotting before proceeding',
     :           STATUS )
            RETURN
          ELSE IF( DEVICE_OPENED .NE. 'NO' .AND. NAME .EQ. 'OPEN') THEN
            CALL MSG_OUT('ERR', 'Plotting ALREADY opened',
     :           STATUS )
          END IF


* start NDF context

          CALL NDF_BEGIN


*
* =========================================================================
*
* obey BLOCK option - plot colour table block
          IF( NAME .EQ. 'BLOCK') THEN
*
* call subroutine to plot colour table block on workstation
            CALL PAR_PUT0C( 'USE_CURSOR', 'NO', STATUS)
            CALL COLOUR_BLOCK( STATUS)
*
* =========================================================================
*
* obey BORDER option - plots border around current image
          ELSE IF( NAME .EQ. 'BORDER') THEN
*
* call subroutine to plot border
            CALL PLOT_BORDER( STATUS)
*
* =========================================================================
*
* obey BOX option - plots BOX with corners xst,yst to xen,yen
          ELSE IF( NAME .EQ. 'BOX') THEN
*
* call subroutine to plot BOX
            CALL PLOT_BOX( STATUS)
*
* =========================================================================
*
* obey CFLASH option - CFLASHES an image on wkstn
          ELSE IF( NAME .EQ. 'CFLASH') THEN
*
* call subroutine to plot CFLASH image to workstation
            CALL PAR_PUT0C( 'PLOT_WHICH', 'CFLASH', STATUS)
            CALL PAR_PUT0C( 'USE_CURSOR', 'YES', STATUS)
            CALL IMAGE_DISPLAY( STATUS)
*
* =========================================================================
*
* obey CIRCLE option - plots CIRCLE with centre defined USER
          ELSE IF( NAME .EQ. 'CIRCLE') THEN
*
* call subroutine to plot CIRCLE
            CALL PLOT_CIRCLE( STATUS)
*
* =========================================================================
*
* obey CLEAR option - clear current display without loss of colour table
          ELSE IF( NAME .EQ. 'CLEAR') THEN
*
* call subroutine to clear workstation
            CALL CLEAR_SCREEN( STATUS)
*
* =========================================================================
*
* obey CLOSE option - CLOSES workstation on selected device
          ELSE IF( NAME .EQ. 'CLOSE') THEN
*
* call subroutine CLOSE workstation on selected device DEVICE_NAME
            CALL DEVICE_CLOSE( BASE_ZONE, STATUS)
*
* set parameter defining device to be closed
            CALL PAR_PUT0C( 'DEVICE_OPENED', 'NO', STATUS)
*
* =========================================================================
*
* obey COMMENT option - puts user defined annotation on workstation
          ELSE IF( NAME .EQ. 'COMMENT') THEN
*
* calls subroutine to put up annotation on screen
            CALL PLOT_COMMENT( STATUS)
*
* =========================================================================
*
* obey COLTAB option - writes colour table to workstation
          ELSE IF( NAME .EQ. 'COLTAB') THEN
*
* call subroutine to write colour table
            CALL COLOUR_TABLE( STATUS)
*
* =========================================================================
*
* obey CONTOUR option - draws a contour map on workstation
          ELSE IF( NAME .EQ. 'CONTOUR') THEN
*
* calls subroutine to draw contour map type 1 or type 2
            CALL CONTOUR_PLOT( STATUS)
*
* =========================================================================
*
* obey CONVAL option - converts pixel coordinates to device raster coords
          ELSE IF( NAME .EQ. 'CONVAL') THEN
*
* calls subroutine to take pixel values and convert them to raster units
            CALL CONVERT_VALUES( STATUS)
*
* =========================================================================
*
* obey CROSS option - plots CROSS
          ELSE IF( NAME .EQ. 'CROSS') THEN
*
* call subroutine to plot CROSS
            CALL PLOT_CROSS( STATUS)
*
* =========================================================================
*
* obey CURANPLOT option - plots image (ranged) using cursor
          ELSE IF( NAME .EQ. 'CURANPLOT') THEN
*
* call subroutine to plot CURANPLOT image to workstation
            CALL PAR_PUT0C( 'PLOT_WHICH', 'RANPLOT', STATUS)
            CALL PAR_PUT0C( 'USE_CURSOR', 'YES', STATUS)
            CALL IMAGE_DISPLAY( STATUS)
*
* =========================================================================
*
* obey CURBLOCK option - plot colour table block at cursor position
          ELSE IF( NAME .EQ. 'CURBLOCK') THEN
*
* call subroutine to plot colour table block on workstation
            CALL PAR_PUT0C( 'USE_CURSOR', 'YES', STATUS)
            CALL COLOUR_BLOCK( STATUS)
*
* =========================================================================
*
* obey CURBOX option - plots BOX with corners xst,yst to xen,yen
          ELSE IF( NAME .EQ. 'CURBOX') THEN
*
* call subroutine to plot BOX
            CALL PLOT_CURBOX( STATUS)
*
* =========================================================================
*
* obey CURCIR option - plots CIRCLE with centre defined by cursor
          ELSE IF( NAME .EQ. 'CURCIR') THEN
*
* call subroutine to plot CIRCLE
            CALL PLOT_CURCIRCLE( STATUS)
*
* =========================================================================
*
* obey CURCOM option - puts user defined annotation on workstation from cursor
          ELSE IF( NAME .EQ. 'CURCOM') THEN
*
* calls subroutine to put up annotation on screen from cursor input
            CALL PLOT_CURCOMMENT( STATUS)
*
* =========================================================================
*
* obey CURCROSS option - plots CROSS at position x,y of length a
          ELSE IF( NAME .EQ. 'CURCRO') THEN
*
* call subroutine to plot CROSS
            CALL PLOT_CURCROSS( STATUS)
*
* =========================================================================
*
* obey CURCUT option - plots a cut/slice through an image on workstation
          ELSE IF( NAME .EQ. 'CURCUT') THEN
*
* select to use cursor and call subroutine to plot cut on workstation
            CALL PAR_PUT0C( 'CUT_USE_CURSOR', 'Y', STATUS)
            CALL CUT_DISPLAY( STATUS)
*
* =========================================================================
*
* obey CURLINE option - draws a line from CURSOR 1 to CURSOR 2
          ELSE IF(NAME .EQ. 'CURLIN') THEN
*
* call subroutine to get cursor positions and draw line
            CALL PLOT_CURLINE( STATUS)
*
* =========================================================================
*
* obey CURNSIGMA option - plots image +-Nsigma at cursor position
          ELSE IF( NAME .EQ. 'CURNSIGMA') THEN
*
* call subroutine to plot NSIGMA image to workstation
            CALL PAR_PUT0C( 'PLOT_WHICH', 'NSIGMA', STATUS)
            CALL PAR_PUT0C( 'USE_CURSOR', 'YES', STATUS)
            CALL IMAGE_DISPLAY( STATUS)
*
* =========================================================================
*
* obey CURPLOT option - plot image at cursor position
          ELSE IF( NAME .EQ. 'CURPLOT') THEN
*
* call subroutine to plot image at cursor position
            CALL PAR_PUT0C( 'PLOT_WHICH', 'PLOT', STATUS)
            CALL PAR_PUT0C( 'USE_CURSOR', 'YES', STATUS)
            CALL IMAGE_DISPLAY( STATUS)
*
* =========================================================================
*
* obey CURVARGREY option - CURVARGREY scales an image on wkstn
          ELSE IF( NAME .EQ. 'CURVARGREY') THEN
*
* call subroutine to plot CURVARGREY image to workstation
            CALL PAR_PUT0C( 'PLOT_WHICH', 'VARGREY', STATUS)
            CALL PAR_PUT0C( 'USE_CURSOR', 'YES', STATUS)
            CALL IMAGE_DISPLAY( STATUS)
*
* =========================================================================
*
* obey CURSOR option - puts up cursor and return position and value
          ELSE IF( NAME .EQ. 'CURSOR') THEN
*
* calls subroutine to put up cursor on workstation and return X,Y and
* value at chosen pixel
            CALL CURSOR_DISPLAY( STATUS)
*
* =========================================================================
*
* obey CURSOR option - puts up cursor and return position and value
          ELSE IF( NAME .EQ. 'CURPOS') THEN
*
* calls subroutine to put up cursor on workstation and return X,Y and
* value at chosen pixel
            CALL CURSOR_POSITION( STATUS)
*
* =========================================================================
*
* obey CUT option - plots a cut/slice through an image on workstation
          ELSE IF( NAME .EQ. 'CUT') THEN
*
* select not to use cursor and call subroutine to plot cut on workstation
            CALL PAR_PUT0C( 'CUT_USE_CURSOR', 'N', STATUS)
            CALL CUT_DISPLAY(  STATUS)
*
* =========================================================================
*
* obey CLEARIT option - clears an area
          ELSE IF( NAME .EQ. 'CLEARIT') THEN
*
* call subroutine to clear an area on the workstation
            CALL CLEAR_AREA(  STATUS)
*
* =========================================================================
*
* obey ELLIPSE option - plots an ellipse at specified location
          ELSE IF( NAME .EQ. 'ELLIPSE') THEN
*
* call subroutine to plot ellipse on the workstation
            CALL PLOT_ELLIPSE( STATUS)
*
* =========================================================================
*
* obey CURRELLIPSE option - plots an ellipse at cursor location
          ELSE IF( NAME .EQ. 'CURELLIPSE') THEN
*
* call subroutine to plot ellipse on the workstation
            CALL PLOT_CURELLIPSE( STATUS)
*
* =========================================================================
*
* obey FLASH option - FLASHES an image on wkstn
          ELSE IF( NAME .EQ. 'FLASH') THEN
*
* call subroutine to plot FLASH image to workstation
            CALL PAR_PUT0C( 'PLOT_WHICH', 'FLASH', STATUS)
            CALL PAR_PUT0C( 'USE_CURSOR', 'NO', STATUS)
            CALL IMAGE_DISPLAY(  STATUS)
*
* =========================================================================
*
* obey GLITCHMARK option - puts up cursor and writes X,Y value to text file
*
          ELSE IF( NAME .EQ. 'GLITCHMARK') THEN
*
* calls subroutine to put up cursor on workstation and write the X,Y
* value at chosen pixel to a text file for GLITCH to read
*
            CALL GLITCH_MARK(  STATUS)
*
* =========================================================================
*
* obey GRID option - puts GRID of lines on image
*
          ELSE IF( NAME .EQ. 'GRID') THEN
*
* calls subroutine to plot grid of lines
*
            CALL PLOT_GRID(  STATUS)
*
* =========================================================================
*
* obey IMCLEAR option - clears an image
*
          ELSE IF( NAME .EQ. 'IMCLEAR') THEN
*
* call subroutine to clear an image of the workstation
*
            CALL IMAGE_CLEAR(  STATUS)
*
* =========================================================================
*
* obey LINE option - draws a line from xst,yst to xen,yen
*
          ELSE IF( NAME .EQ. 'LINE') THEN
*
* call subroutine to draw line
*
            CALL PLOT_LINE(  STATUS)
*
* =========================================================================
*
* obey LINE_WIDTH option - sets the line width for ln03 laser printer META
*
          ELSE IF( NAME .EQ. 'LINE_WIDTH') THEN
*
* call subroutine to set width
*
            CALL SET_LINEWIDTH( STATUS)
*
* =========================================================================
*
* obey NSIGMA option - plots image
*
          ELSE IF( NAME .EQ. 'NSIGMA') THEN
*
* call subroutine to plot NSIGMA image to workstation
*
            CALL PAR_PUT0C( 'PLOT_WHICH', 'NSIGMA', STATUS)
            CALL PAR_PUT0C( 'USE_CURSOR', 'NO', STATUS)

            CALL IMAGE_DISPLAY(  STATUS)
*
* =========================================================================
*
* obey CONTPARS option - get container file paramters and stuff into
* parameter system  (for old style IRCAM frames)
*
          ELSE IF( NAME .EQ. 'CONTPARS') THEN
*
* call subroutine to get container file parameters
            CALL OLDPARS( STATUS)
*
* =========================================================================
*
* obey OPEN option - opens workstation on selected device
*
          ELSE IF( NAME .EQ. 'OPEN') THEN
*
* call subroutine open workstation on selected device DEVICE_NAME
*
            CALL DEVICE_OPEN( BASE_ZONE, STATUS)
*
* set parameter defining device to be open
*
            CALL PAR_PUT0C( 'DEVICE_OPENED', 'YES', STATUS)
*
* =========================================================================
*
* obey PLOT option - plots image
*
          ELSE IF( NAME .EQ. 'PLOT') THEN
*
* call subroutine to plot image to workstation
*
            CALL PAR_PUT0C( 'PLOT_WHICH', 'PLOT', STATUS)
            CALL PAR_PUT0C( 'USE_CURSOR', 'NO', STATUS)

            CALL IMAGE_DISPLAY( STATUS)
*
* =========================================================================
*
* obey POLAX option - plots polarization vector map
*
          ELSE IF( NAME .EQ. 'POLAX') THEN

* call subroutine to plot vector map to workstation
*
            CALL POLAX(  STATUS)
*
* =========================================================================
*
* obey RANPLOT option - plots image
*
          ELSE IF( NAME .EQ. 'RANPLOT') THEN
*
* call subroutine to plot RANPLOT image to workstation
*
            CALL PAR_PUT0C( 'PLOT_WHICH', 'RANPLOT', STATUS)
            CALL PAR_PUT0C( 'USE_CURSOR', 'NO', STATUS)

            CALL IMAGE_DISPLAY( STATUS)
*
* =========================================================================
*
* obey SETCOL option - set the colour of a pen in range 0-255
*
          ELSE IF( NAME .EQ. 'SETCOL') THEN
*
* call subroutine set pen colour
*
            CALL SET_PENCOL( STATUS)
*
* =========================================================================
*
* obey RONEXT option - get container file extension info
          ELSE IF( NAME .EQ. 'RONEXT') THEN
*
* call subroutine to get container file parameters
            CALL RONEXT(  STATUS)
*
* =========================================================================
*
* obey ROPARS option - get container file paramters and stuff into
* parameter system
          ELSE IF( NAME .EQ. 'ROPARS') THEN
*
* call subroutine to get container file parameters
            CALL ROPARS(  STATUS)
*
* =========================================================================
*
* obey SETCOL2 option - set the colour of a pen in range 0-255
*
          ELSE IF( NAME .EQ. 'SETCOL2') THEN
*
* call subroutine set pen colour
*
            CALL SET_PENCOL2(  STATUS)
*
* =========================================================================
*
* obey SURROUND option - puts DEFAULT border and ticks around an image
*
          ELSE IF( NAME .EQ. 'SURROUND') THEN
*
* calls subroutine to write ticks/border/numbers
*
            CALL SURROUND_ANNOTATE(  STATUS)
*
* =========================================================================
*
* obey VARGREY option - VARGREY scales an image on wkstn
*
          ELSE IF( NAME .EQ. 'VARGREY') THEN
*
* call subroutine to plot VARGREY image to workstation
*
            CALL PAR_PUT0C( 'PLOT_WHICH', 'VARGREY', STATUS)
            CALL PAR_PUT0C( 'USE_CURSOR', 'NO', STATUS)

            CALL IMAGE_DISPLAY(  STATUS)
*
* =========================================================================
*
* obey WELCOME option - auto load action
*
          ELSE IF( NAME .EQ. 'WELCOME') THEN
*
* =========================================================================
*
* here if NAME with OPTION invalid
*
          ELSE
*
* only valid commands should be passed to ACT so this section should
* never occur
*
            CALL MSG_SETC('NAME', NAME)
            CALL MSG_OUT( 'UNIMP', 'Action ^NAME not implemented',
     :                     STATUS )

          ENDIF


* make sure all NDF locators are released

          CALL NDF_END( STATUS )


        ELSE

          CALL MSG_OUT('ERR',
     :     'ERROR : PLT2D : Unknown context - not OBEY/GET/SET',
     :     STATUS )

        END IF

        END
