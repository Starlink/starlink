      SUBROUTINE SLIDE( STATUS )
*+
*  Name:
*     SLIDE

*  Purpose:
*     Realigns an NDF using a translation.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SLIDE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The pixels of an NDF are shifted in its current co-ordinate
*     Frame, so that the pixel co-ordinates of a given feature will
*     change but its co-ordinates in user-added Frames in the WCS
*     component will not.  The translation is specified by either an 
*     absolute vector given by the ABS parameter or by the difference
*     between a fiducial point and a standard object given by the FID
*     and OBJ parameters respectively.  In each case the co-ordinates
*     are specified in the NDF's current co-ordinate Frame.
*
*     The shift is done by resampling the NDF pixels onto a new grid.
*     By default this is done using linear interpolation, but
*     nearest-neighbour or other schemes may be selected using the
*     METHOD parameter.

*  Usage:
*     slide in out abs method

*  ADAM Parameters:
*     ABS = LITERAL (Read)
*        Absolute shift in the current co-ordinate Frame (supplying a 
*        colon ":" will display details of the current Frame).  The
*        position should be supplied as a list of formatted axis
*        values separated by spaces or commas.  Only used if 
*        STYPE="Absolute".
*     FID = LITERAL (Read)
*        Position of the fiducial point in the current co-ordinate Frame
*        (supplying a colon ":" will display details of the current Frame).
*        The position should be supplied as a list of formatted axis
*        values separated by spaces or commas.  Only used if 
*        STYPE="Relative".
*     IN = NDF (Read)
*        The NDF to be translated.
*     METHOD = LITERAL (Read)
*        The interpolation method used to perform the translation. 
*        The following values are permitted:
*
*        -  "Nearest"   -- Nearest neighbour sampling.
*
*        -  "Linear"    -- Linear interpolation.
*
*        -  "Sinc"      -- Sum of surrounding pixels weighted using
*                          a 1-d sinc(pi*x) kernel.
*
*        -  "SincSinc"  -- Sum of surrounding pixels weighted using
*                          a 1-d sinc(pi*x)*sinc(k*pi*x) kernel.
*
*        -  "SincCos"   -- Sum of surrounding pixels weighted using
*                          a 1-d sinc(pi*x)*cos(k*pi*x) kernel.
*
*        -  "SincGauss" -- Sum of surrounding pixels weighted using
*                          a 1-d sinc(pi*x)*exp(-k*x*x) kernel.
*
*        -  "BlockAve"  -- Block averaging over all pixels in the
*                          surrounding N-dimensional cube.
*
*        In the above, sinc(z)=sin(z)/z.  Some of these schemes will
*        require additional parameters to be supplied via the PARAMS
*        parameter.  A more detailed discussion of these schemes is
*        given in the "Sub-Pixel Interpolation Schemes" section below.
*        ["Linear"]
*     OBJ = LITERAL (Read)
*        Position of the standard object in the current co-ordinate Frame
*        (supplying a colon ":" will display details of the current Frame).
*        The position should be supplied as a list of formatted axis
*        values separated by spaces or commas.  Only used if 
*        STYPE="Relative".
*     OUT = NDF (Write)
*        The translated NDF.
*     PARAMS( ) = _DOUBLE (Read)
*        Parameters required to control the resampling scheme.  One or
*        more values may be required to specify the exact resampling
*        behaviour, according to the value of the METHOD parameter.
*        See the section on "Sub-Pixel Interpolation Schemes".
*     STYPE = LITERAL (Read)
*        The sort of shift to be used.  The choice is "Relative" or
*        "Absolute". ["Absolute"]
*     TITLE = LITERAL (Read)
*        Title for the output NDF. A null (!) value will cause the input
*        title to be used. [!]

*  Examples:
*     slide m31 m31_acc "0:0:0 0:0:15.8"
*        The pixels in the NDF m31, whose current co-ordinate Frame 
*        is in the SKY domain, are shifted by 15.8 seconds in 
*        declination and the result is written to the NDF m31_acc.
*        Linear interpolation is used to produce the output data 
*        (and, if present, variance) array.
*     slide m31 m31_acc "0:0:0 0:0:15.8" nearest
*        The same as the previous example except that nearest neighbour
*        resampling is used.  This will be somewhat faster, but may
*        result in features shifted by up to half a pixel. 
*     slide speca specb stype=rel fid=3968.9 obj=3960.1
*        The pixels in the NDF speca are shifted by 3968.9-3960.1 
*        units in the current co-ordinate Frame, and the output NDF is 
*        written as specb.
*     slide speca specb stype=abs abs=8.8
*        This does just the same as the previous example.

*  Sub-Pixel Interpolation Schemes:
*     When performing the translation the pixels are resampled from
*     the input grid to the output grid by default using linear
*     interpolation.  For many purposes this default scheme will
*     be adequate, but for greater control over the resampling 
*     process the METHOD and PARAMS parameters can be used.  Detailed
*     discussion of the use of these parameters can be found in the
*     "Sub-pixel Interpolation Schemes" section of the REGRID task
*     documentation.

*  Related Applications:
*     KAPPA: REGRID, SQORST, WCSADD

*  Authors:
*     MBT: Mark Taylor (Starlink)

*  History:
*     7-JAN-2002 (MBT):
*        Original version.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST definitions and declarations
      INCLUDE 'NDF_PAR'          ! NDF system constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER KPG1_FLOOR         ! Most positive integer .LE. a given real
      INTEGER KPG1_CEIL          ! Most negative integer .GE. a given real

*  Local Variables:
      CHARACTER DTYPE * ( NDF__SZFTP ) ! Full data type name
      CHARACTER ITYPE * ( NDF__SZTYP ) ! HDS Data type name
      CHARACTER METHOD * ( 16 )  ! Name of resampling scheme
      CHARACTER STYPE * ( 16 )   ! Type of shift to be supplied
      DOUBLE PRECISION BC( NDF__MXDIM ) ! Base Frame co-ordinates (dummy)
      DOUBLE PRECISION DLBNDI( NDF__MXDIM ) ! Lower bounds of input NDF
      DOUBLE PRECISION DLBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      DOUBLE PRECISION DUBNDI( NDF__MXDIM ) ! Upper bounds of input NDF
      DOUBLE PRECISION DUBNDO( NDF__MXDIM ) ! Upper bounds of output NDF
      DOUBLE PRECISION FID( NDF__MXDIM ) ! Co-ordinates of fiducial point
      DOUBLE PRECISION OBJ( NDF__MXDIM ) ! Co-ordinates of standard object
      DOUBLE PRECISION PARAMS( 8 ) ! Auxiliary parameters for resampling
      DOUBLE PRECISION PIA( NDF__MXDIM ) ! First input point
      DOUBLE PRECISION PIB( NDF__MXDIM ) ! Second input point
      DOUBLE PRECISION POA( NDF__MXDIM ) ! First output point
      DOUBLE PRECISION POB( NDF__MXDIM ) ! Second output point
      DOUBLE PRECISION SHIFT( NDF__MXDIM ) ! Translation vector
      DOUBLE PRECISION TOL       ! Linear approximation tolerance
      DOUBLE PRECISION XL( NDF__MXDIM ) ! Position of lower outliers (dummy)
      DOUBLE PRECISION XU( NDF__MXDIM ) ! Position of upper outliers (dummy)
      INTEGER ELI                ! Number of elements in input NDF
      INTEGER ELO                ! Number of elements in output NDF
      INTEGER FLAGS              ! Flags for resampling routine
      INTEGER I                  ! Loop variable
      INTEGER INTERP             ! Resampling scheme
      INTEGER IPDATI             ! Pointer to input data array
      INTEGER IPDATO             ! Pointer to output data array
      INTEGER IPVARI             ! Pointer to input variance array
      INTEGER IPVARO             ! Pointer to output variance array
      INTEGER LBNDI( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      INTEGER MAP                ! AST Mapping for resampling
      INTEGER MAPBC              ! AST Mapping from base to current Frame
      INTEGER MAPCB              ! AST Mapping from current to base Frame
      INTEGER MAPSHF             ! AST Mapping representing simple translation
      INTEGER MAXPIX             ! Maximum extent of linear approximation
      INTEGER NAXC               ! Number of axes in current Frame
      INTEGER NBAD               ! Number of bad pixels written
      INTEGER NDFI               ! Input NDF identifier
      INTEGER NDFO               ! Output NDF identifier
      INTEGER NDIM               ! Number of dimensions of NDF
      INTEGER NPARAM             ! Number of auxiliary resampling parameters
      INTEGER UBNDI( NDF__MXDIM ) ! Upper bounds of input NDF
      INTEGER UBNDO( NDF__MXDIM ) ! Upper bounds of output NDF
      INTEGER WCSI               ! WCS FrameSet of input NDF
      INTEGER WCSO               ! WCS FrameSet of output NDF
      LOGICAL BAD                ! May there be bad pixels?
      LOGICAL HASVAR             ! Do we have a variance component?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new AST context.
      CALL AST_BEGIN( STATUS )

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Open and enquire attributes of the input NDF.
*  =============================================

*  Open the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Find out if we have a variance component
      CALL NDF_STATE( NDFI, 'VARIANCE', HASVAR, STATUS )

*  Determine a data type which can be used for operations on its Data
*  and possibly Variance components.
      CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,' //
     :                '_DOUBLE', 1, NDFI, 'DATA,VARIANCE', ITYPE,
     :                DTYPE, STATUS )

*  Get its shape.
      CALL NDF_BOUND( NDFI, NDF__MXDIM, LBNDI, UBNDI, NDIM, STATUS )

*  See if it has bad pixels in the Data or Variance component.
      CALL NDF_BAD( NDFI, 'DATA', .FALSE., BAD, STATUS )
      IF ( HASVAR .AND. .NOT. BAD ) THEN
         CALL NDF_BAD( NDFI, 'VARIANCE', .FALSE., BAD, STATUS )
      END IF

*  Get its WCS component.
      CALL KPG1_GTWCS( NDFI, WCSI, STATUS )

*  Get the number of axes in the current Frame.
      NAXC = AST_GETI( WCSI, 'Naxes', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Determine the Mapping to be used for the resample operation.
*  ============================================================

*  See if we want a relative or absolute shift.
      CALL PAR_CHOIC( 'STYPE', 'ABSOLUTE', 'ABSOLUTE,RELATIVE', .TRUE.,
     :                STYPE, STATUS )

*  Get the absolute shift directly as the value of the ABS parameter.
      IF ( STYPE .EQ. 'ABSOLUTE' ) THEN
         CALL KPG1_GTPOS( 'ABS', WCSI, .FALSE., SHIFT, BC, STATUS )

*  Or get it as the difference between the FID and OBJ parameters.
      ELSE

*  Get the co-ordinates of the fiducial point.
         CALL KPG1_GTPOS( 'FID', WCSI, .FALSE., FID, BC, STATUS )

*  Get the co-ordinates of the standard object.
         CALL KPG1_GTPOS( 'OBJ', WCSI, .FALSE., OBJ, BC, STATUS )

*  Set the shift as the difference between the two.
         DO I = 1, NAXC
            SHIFT( I ) = FID( I ) - OBJ( I )
         END DO
      END IF

*  Construct a Mapping corresponding to this shift.
      DO I = 1, NDIM
         PIA( I ) = 0D0
         PIB( I ) = 1D0
         POA( I ) = PIA( I ) + SHIFT( I )
         POB( I ) = PIB( I ) + SHIFT( I )
      END DO
      MAPSHF = AST_WINMAP( NAXC, PIA, PIB, POA, POB, ' ', STATUS )

*  Construct the Mapping to be used for the resampling by sandwiching
*  the translation in the current Frame by a Mapping from base
*  (GRID domain) to current and a Mapping from current back to base.
*  This will constitute the Mapping either from GRID to GRID Frames
*  or from PIXEL to PIXEL Frames.
      MAPBC = AST_GETMAPPING( WCSI, AST__BASE, AST__CURRENT, STATUS )
      MAPCB = AST_GETMAPPING( WCSI, AST__CURRENT, AST__BASE, STATUS )
      MAP = AST_CMPMAP( MAPBC, MAPSHF, .TRUE., ' ', STATUS )
      MAP = AST_CMPMAP( MAP, MAPCB, .TRUE., ' ', STATUS )
      MAP = AST_SIMPLIFY( MAP, STATUS )

*  Create and configure the output NDF.
*  ====================================

*  Propagate the input to the output NDF.
      CALL LPG_PROP( NDFI, 'UNIT', 'OUT', NDFO, STATUS )

*  Get a title for the new NDF from the parameter system.
      CALL NDF_CINP( 'TITLE', NDFO, 'TITLE', STATUS )

*  Set the Data and possibly Variance component data types.
      CALL NDF_STYPE( ITYPE, NDFO, 'DATA', STATUS )
      IF ( HASVAR ) THEN
         CALL NDF_STYPE( ITYPE, NDFO, 'VARIANCE', STATUS )
      END IF

*  Work out the bounds of an array which would contain the resampled
*  copy of the whole input array.
      DO I = 1, NDIM
         DLBNDI( I ) = DBLE( LBNDI( I ) - 1 )
         DUBNDI( I ) = DBLE( UBNDI( I ) )
      END DO
      DO I = 1, NDIM
         CALL AST_MAPBOX( MAP, DLBNDI, DUBNDI, .TRUE., I, DLBNDO( I ),
     :                    DUBNDO( I ), XL, XU, STATUS )
      END DO

*  Work out the corresponding shape of the output NDF.
      DO I = 1, NDIM
         LBNDO( I ) = KPG1_FLOOR( REAL( DLBNDO( I ) ) ) + 1
         UBNDO( I ) = KPG1_CEIL( REAL( DUBNDO( I ) ) )
      END DO

*  Set the shape of the output NDF.
      CALL NDF_SBND( NDIM, LBNDO, UBNDO, NDFO, STATUS )

*  Fix it up according to the changes we will make.
      CALL KPG1_ASFIX( MAP, NDFI, NDFO, STATUS )


*  Resample data from the input to output NDF.
*  ===========================================

*  Map the input and output data arrays.
      CALL NDF_MAP( NDFI, 'DATA', ITYPE, 'READ', IPDATI, ELI, STATUS )
      CALL NDF_MAP( NDFO, 'DATA', ITYPE, 'WRITE', IPDATO, ELO, STATUS )

*  If necessary map the input and output variance arrays.
      IF ( HASVAR ) THEN
         CALL NDF_MAP( NDFI, 'VARIANCE', ITYPE, 'READ', IPVARI, ELI,
     :                 STATUS )
         CALL NDF_MAP( NDFO, 'VARIANCE', ITYPE, 'WRITE', IPVARO, ELO,
     :                 STATUS )
      END IF

*  Set flags for the resampling routine.
      FLAGS = 0
      IF ( HASVAR ) FLAGS = FLAGS + AST__USEVAR
      IF ( BAD ) FLAGS = FLAGS + AST__USEBAD
      TOL = 0.1D0
      MAXPIX = 500

*  Determine the resampling scheme to use.
      CALL PAR_CHOIC( 'METHOD', 'LINEAR', 'LINEAR,NEAREST,SINC,' //
     :                'SINCSINC,SINCCOS,SINCGAUSS,BLOCKAVE', .TRUE.,
     :                METHOD, STATUS )
      IF ( METHOD .EQ. 'NEAREST' ) THEN
         INTERP = AST__NEAREST
         NPARAM = 0
      ELSE IF ( METHOD .EQ. 'LINEAR' ) THEN
         INTERP = AST__LINEAR
         NPARAM = 0
      ELSE IF ( METHOD .EQ. 'SINC' ) THEN
         INTERP = AST__SINC
         NPARAM = 1
      ELSE IF ( METHOD .EQ. 'SINCSINC' ) THEN
         INTERP = AST__SINCSINC
         NPARAM = 2
      ELSE IF ( METHOD .EQ. 'SINCCOS' ) THEN
         INTERP = AST__SINCCOS
         NPARAM = 2
      ELSE IF ( METHOD .EQ. 'SINCGAUSS' ) THEN
         INTERP = AST__SINCGAUSS
         NPARAM = 2
      ELSE IF ( METHOD .EQ. 'BLOCKAVE' ) THEN
         INTERP = AST__BLOCKAVE
         NPARAM = 1
      END IF

*  Get an additional parameter vector if required.
      IF ( NPARAM .GT. 0 ) THEN
         CALL PAR_EXACD( 'PARAMS', NPARAM, PARAMS, STATUS )
      END IF

*  Perform the resampling.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         NBAD = AST_RESAMPLEB( MAP, NDIM, LBNDI, UBNDI, %VAL( IPDATI ),
     :                         %VAL( IPVARI ), INTERP, AST_NULL, 
     :                         PARAMS, FLAGS, TOL, MAXPIX, VAL__BADB,
     :                         NDIM, LBNDO, UBNDO, LBNDO, UBNDO, 
     :                         %VAL( IPDATO ), %VAL( IPVARO ), STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         NBAD = AST_RESAMPLEUB( MAP, NDIM, LBNDI, UBNDI, %VAL( IPDATI ),
     :                          %VAL( IPVARI ), INTERP, AST_NULL, 
     :                          PARAMS, FLAGS, TOL, MAXPIX, VAL__BADUB,
     :                          NDIM, LBNDO, UBNDO, LBNDO, UBNDO, 
     :                          %VAL( IPDATO ), %VAL( IPVARO ), STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         NBAD = AST_RESAMPLEW( MAP, NDIM, LBNDI, UBNDI, %VAL( IPDATI ),
     :                         %VAL( IPVARI ), INTERP, AST_NULL, 
     :                         PARAMS, FLAGS, TOL, MAXPIX, VAL__BADW,
     :                         NDIM, LBNDO, UBNDO, LBNDO, UBNDO, 
     :                         %VAL( IPDATO ), %VAL( IPVARO ), STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         NBAD = AST_RESAMPLEUW( MAP, NDIM, LBNDI, UBNDI, %VAL( IPDATI ),
     :                          %VAL( IPVARI ), INTERP, AST_NULL, 
     :                          PARAMS, FLAGS, TOL, MAXPIX, VAL__BADUW,
     :                          NDIM, LBNDO, UBNDO, LBNDO, UBNDO, 
     :                          %VAL( IPDATO ), %VAL( IPVARO ), STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         NBAD = AST_RESAMPLEI( MAP, NDIM, LBNDI, UBNDI, %VAL( IPDATI ),
     :                         %VAL( IPVARI ), INTERP, AST_NULL, 
     :                         PARAMS, FLAGS, TOL, MAXPIX, VAL__BADI,
     :                         NDIM, LBNDO, UBNDO, LBNDO, UBNDO, 
     :                         %VAL( IPDATO ), %VAL( IPVARO ), STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         NBAD = AST_RESAMPLER( MAP, NDIM, LBNDI, UBNDI, %VAL( IPDATI ),
     :                         %VAL( IPVARI ), INTERP, AST_NULL, 
     :                         PARAMS, FLAGS, TOL, MAXPIX, VAL__BADR,
     :                         NDIM, LBNDO, UBNDO, LBNDO, UBNDO, 
     :                         %VAL( IPDATO ), %VAL( IPVARO ), STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         NBAD = AST_RESAMPLED( MAP, NDIM, LBNDI, UBNDI, %VAL( IPDATI ),
     :                         %VAL( IPVARI ), INTERP, AST_NULL, 
     :                         PARAMS, FLAGS, TOL, MAXPIX, VAL__BADD,
     :                         NDIM, LBNDO, UBNDO, LBNDO, UBNDO, 
     :                         %VAL( IPDATO ), %VAL( IPVARO ), STATUS )
      END IF

*  We can set the bad pixels flag according to the bad pixel count 
*  returned from AST_RESAMPLE<X>.
      BAD = NBAD .GT. 0
      CALL NDF_SBAD( BAD, NDFO, 'DATA', STATUS )
      IF ( HASVAR ) THEN
         CALL NDF_SBAD( BAD, NDFO, 'VARIANCE', STATUS )
      END IF

*  Tidy up.
*  ========

*  Annul (and unmap) the input and output NDFs.
      CALL NDF_ANNUL( NDFI, STATUS )
      CALL NDF_ANNUL( NDFO, STATUS )

*  Error exit label.
  999 CONTINUE

*  Exit the NDF context.
      CALL NDF_END( STATUS )

*  Exit the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SLIDE_ERR1', 'SLIDE: Unable to translate NDF',
     :                 STATUS )
      END IF

      END
