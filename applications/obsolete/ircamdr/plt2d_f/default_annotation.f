        SUBROUTINE DEFAULT_ANNOTATION( STATUS)

* Description : Routine to plot default anotation on an image
* makes the IR data look nice and neat for photographing the
* screen

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters :

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  26-Jul-1994 Changed error reporting to use ERR_, removed VALUE,
*              changed IFIX to INT (SKL@JACH
*  26-Oct-1994 Changed MAGNIF from INT to REAL
* Endhistory

* Type Definitions

        IMPLICIT NONE

* Global constants :

        INCLUDE 'ADAM_DEFNS'
        INCLUDE 'SAE_PAR'
        INCLUDE 'DTDEFNS'
        INCLUDE 'DTERRS'

* Status :

        INTEGER STATUS

* Import :

* Import-Export :

* Export :

* External references :

* Global variables

        INCLUDE 'PLT2DCOM'

* Local variables :

        INTEGER DATE
        INTEGER DEC_DEGREES
        INTEGER DEC_MINUTES
        INTEGER DEC_SECONDS
        INTEGER CHIP_TEMP
        REAL MAGNIF
        INTEGER NF
        INTEGER NPR
        INTEGER NUMBER_COADDED
        INTEGER DEFANN_PEN
        INTEGER RA_HOURS
        INTEGER RA_MINUTES
        INTEGER UT_HOURS
        INTEGER UT_MINUTES
        INTEGER UT_SECONDS
        INTEGER YEAR

        REAL AR
        REAL DEFANN_MAX
        REAL DEFANN_MIN
        REAL EPOCH
        REAL EXPOSURE_TIME
        REAL HM
        REAL HT
        REAL PIXEL_SCALE
        REAL RA_SECONDS
        REAL SP
        REAL TEXT_SIZE
        REAL X_EXTENT
        REAL XTPOS
        REAL XU
        REAL XUP
        REAL Y_EXTENT
        REAL YTPOS
        REAL YU
        REAL YUP
        REAL XTICKINT
        REAL YTICKINT
        REAL AXRAT

        CHARACTER CHIP_TYPE*10
        CHARACTER DEFANN_COL*1
        CHARACTER DEFANN_TITLE*60
        CHARACTER DOUBLE_QUOTES*1
        CHARACTER FILTER*1
        CHARACTER MODE*10
        CHARACTER MONTH*3
        CHARACTER OBJECT_NAME*60
        CHARACTER TXJ*2

* Local Constants :

* Internal References :

* =====================================================================

* check status on entry

        IF( STATUS .NE. SAI__OK)THEN

          RETURN

        END IF

* get all current text attributes

        CALL SGS_IMTX( HM, NF, NPR, HT, AR, XU, YU, SP, TXJ)

* get parameters defining text plot from parameter system

        CALL PAR_GET0C( 'DEFANN_TITLE', DEFANN_TITLE, STATUS)
        CALL PAR_GET0C( 'DEFANN_COLOUR', DEFANN_COL, STATUS)

        CALL PAR_GET0I( 'DEFANN_PEN', DEFANN_PEN, STATUS)

        CALL PAR_GET0R( 'MAXIMUM', DEFANN_MAX, STATUS)
        CALL PAR_GET0R( 'MINIMUM', DEFANN_MIN, STATUS)

        IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :                 'Error : DEFAULT_ANNOTATION : after PAR_GETS',
     :                 STATUS )
          RETURN
        END IF

* calculate magnification factor used in last image display

        MAGNIF = ( IM_XEN - IM_XST)/REAL(NX)

* set text size

        TEXT_SIZE = 15.0*MAGNIF/16.0*REAL(NY)/64.0
        CALL SGS_SHTX( TEXT_SIZE)

* text orientation is required

        XUP = 0.0
        YUP = 1.0

* set text orientation

        CALL SGS_SUPTX( XUP, YUP)

* set text font

        CALL SGS_SFONT( 1)

* set annotation colour

        CALL SET_COLOUR( DEFANN_PEN, DEFANN_COL)

* plot border lines

        CALL SGS_BOX( IM_XST, IM_XEN, IM_YST, IM_YEN)
        CALL SGS_BOX( IM_XST-1, IM_XEN+1, IM_YST-1, IM_YEN+1)

* call subroutine to plot tick marks

        CALL PAR_GET0R( 'CONTOUR_TICKINT', XTICKINT, STATUS)
        CALL PAR_GET0R( 'CONTOUR_YTICKIN', YTICKINT, STATUS)
        CALL PAR_GET0R( 'CONTOUR_AXRAT', AXRAT, STATUS)

        CALL CONTOUR_TICKS( XTICKINT, YTICKINT, MAGNIF, AXRAT, STATUS)

* call subroutine to plot numbers

        CALL CONTOUR_NUMBERS( XTICKINT, YTICKINT, MAGNIF, AXRAT, STATUS)

* set text X,Y positioning id...(centre in X and Y)

        CALL SGS_STXJ( 'CC')

* write out TITLE string

        XTPOS = IM_XST + ( IM_XEN - IM_XST)/2.0
        YTPOS = IM_YEN + 35.84*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATXL( DEFANN_TITLE)

* set text X,Y positioning id...(BOTTOM LEFT in X and Y)

        CALL SGS_STXJ( 'BL')

* write out Side titles

* Object name

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 51.2*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Object = ')
        CALL SGS_ATXL( OBJECT_NAME)

* Right Ascension

        RA_HOURS = 6
        RA_MINUTES = 37
        RA_SECONDS = 24.3

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 128.0*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'RA  = ')
        CALL SGS_ATXI( RA_HOURS, 0)
        CALL SGS_ATEXT( ',')
        CALL SGS_ATXI( RA_MINUTES, 0)
        CALL SGS_ATEXT( ',')
        CALL SGS_ATXR( RA_SECONDS, 0, 1)

* Declination

        DEC_DEGREES = 10
        DEC_MINUTES = 46
        DEC_SECONDS = 11

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 179.2*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'DEC = ')
        CALL SGS_ATXI( DEC_DEGREES, 0)
        CALL SGS_ATEXT( ',')
        CALL SGS_ATXI( DEC_MINUTES, 0)
        CALL SGS_ATEXT( ',')
        CALL SGS_ATXI( DEC_SECONDS, 0)

* Epoch

        EPOCH = 1950.0

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 230.4*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Epoch = ')
        CALL SGS_ATEXT( '(')
        CALL SGS_ATXR( EPOCH, 0, 1)
        CALL SGS_ATEXT( ')')

* Date

        DATE = 11
        MONTH = 'NOV'
        YEAR = 84

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 281.6*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Date = ')
        CALL SGS_ATXI( DATE, 0)
        CALL SGS_ATXL( MONTH)
        CALL SGS_ATXI( YEAR, 0)

* UT

        UT_HOURS = 21
        UT_MINUTES = 19
        UT_SECONDS = 34

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 332.8*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'UT = ')
        CALL SGS_ATXI( UT_HOURS, 0)
        CALL SGS_ATEXT( ',')
        CALL SGS_ATXI( UT_MINUTES, 0)
        CALL SGS_ATEXT( ',')
        CALL SGS_ATXI( UT_SECONDS, 0)

* chip releated parameters

* chip type

        CHIP_TYPE = 'SBRC 1'

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 409.6*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Chip = ')
        CALL SGS_ATXL( CHIP_TYPE)

* filter

        FILTER = 'K'

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 460.8*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Filter = ')
        CALL SGS_ATXL( FILTER)

* pixel scale

        PIXEL_SCALE = 2.5
        DOUBLE_QUOTES = CHAR( 34)

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 512.0*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Scale = ')
        CALL SGS_ATXR( PIXEL_SCALE, 0, 1)
        CALL SGS_ATXL( DOUBLE_QUOTES)
        CALL SGS_ATEXT( '/pxl')

* number of frames coadded

        NUMBER_COADDED = 50

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 563.2*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Coadded = ')
        CALL SGS_ATXI( NUMBER_COADDED, 0)

* exposure time

        EXPOSURE_TIME = 1.5

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 614.4*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Exposure = ')
        CALL SGS_ATXR( EXPOSURE_TIME, 0, 1)
        CALL SGS_ATEXT( 's')

* mode

        MODE = 'STARE'

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 665.6*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Mode = ')
        CALL SGS_ATXL( MODE)

* temperature

        CHIP_TEMP = 50

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 716.8*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Temp = ')
        CALL SGS_ATXI( CHIP_TEMP, 0)
        CALL SGS_ATEXT( 'K')

* X,Y axis size

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 793.6*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Size = ')
        CALL SGS_ATXI( NX, 0)
        CALL SGS_ATEXT( ',')
        CALL SGS_ATXI( NY, 0)

* extent

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 844.8*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Ext  = ')

        X_EXTENT = NX*PIXEL_SCALE
        Y_EXTENT = NY*PIXEL_SCALE

        CALL SGS_ATXR( X_EXTENT, 0, 1)
        CALL SGS_ATEXT( ',')
        CALL SGS_ATXR( Y_EXTENT, 0, 1)

* max value

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 921.6*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Max = ')
        CALL SGS_ATXI( INT(DEFANN_MAX), 0)

* min value

        XTPOS = IM_XEN + 30.0*MAGNIF/16.0*REAL(NX)/64.0
        YTPOS = IM_YEN - 972.8*MAGNIF/16.0*REAL(NY)/64.0

        CALL SGS_BTEXT( XTPOS, YTPOS)
        CALL SGS_ATEXT( 'Min = ')
        CALL SGS_ATXI( INT(DEFANN_MIN), 0)

* reset text attributes to old values

        CALL SGS_SHTX( HT)
        CALL SGS_SUPTX( XU, YU)
        CALL SGS_SFONT( 1)
        CALL SGS_STXJ( TXJ)

        CALL SGS_FLUSH

        END
