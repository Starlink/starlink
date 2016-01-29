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
*     The pixels of an NDF are shifted by a given number of pixels along
*     each pixel axis.  The shift need not be an integer number of
*     pixels, and pixel interpolation will be performed if necessary
*     using the scheme selected by Parameter METHOD.  The shifts to use
*     are specified either by an absolute vector given by the ABS
*     parameter or by the difference between a fiducial point and a
*     standard object given by the FID and OBJ parameters respectively.
*     In each case the co-ordinates are specified in the NDF's pixel
*     co-ordinate Frame.

*  Usage:
*     slide in out abs method

*  ADAM Parameters:
*     ABS( ) = _DOUBLE (Read)
*        Absolute shifts in pixels.  The number of values supplied must
*        match the number of pixel axes in the NDF.  It is only used if
*        STYPE="Absolute".
*     FID( ) = _DOUBLE (Read)
*        Position of the fiducial point in pixel co-ordinates.  The
*        number of values supplied must match the number of pixel axes
*        in the NDF.  It is only used if STYPE="Relative".
*
*        An object centred at the pixel co-ordinates given by Parameter
*        OBJ in the input NDF will be centred at the pixel co-ordinates
*        given by Parameter FID in the output NDF.
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
*        parameter.  A more-detailed discussion of these schemes is
*        given in the "Sub-Pixel Interpolation Schemes" section below.
*        the initial default is "Linear".  [current value]
*     OBJ = LITERAL (Read)
*        Position of the standard object in pixel co-ordinates.  The
*        number of values supplied must match the number of pixel axes
*        in the NDF.   It is only used if STYPE="Relative".
*
*        An object centred at the pixel co-ordinates given by Parameter
*        OBJ in the input NDF will be centred at the pixel co-ordinates
*        given by Parameter FID in the output NDF.
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
*        Title for the output NDF.  A null (!) value will cause the
*        input title to be used. [!]

*  Examples:
*     slide m31 m31_acc [3.2,2.3]
*        The pixels in the NDF m31 are shifted by 3.2 pixels in X and
*        2.3 pixels in Y, and written to NDF m31_acc.  Linear
*        interpolation  is used to produce the output data (and, if
*        present, variance) array.
*     slide m31 m31_acc [3.2,2.3] nearest
*        The same as the previous example except that nearest-neighbour
*        resampling is used.  This will be somewhat faster, but may
*        result in features shifted by up to half a pixel.
*     slide speca specb stype=rel fid=11.2 obj=11.7
*        The pixels in the NDF speca are shifted by 0.5 (i.e.
*        11.7 - 11.2) pixels and the output NDF is written as specb.
*     slide speca specb stype=abs abs=0.5
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

*  Notes:
*     -  If the NDF is shifted by a whole number of pixels along each
*     axis, this application merely changes the pixel origin in the NDF.
*     It can thus be compared to the SETORIGIN command.
*     -  Resampled axis centres that are beyond the bounds of the
*     input NDF are given extrapolated values from the first (or last)
*     pair of valid centres.

*  Related Applications:
*     KAPPA: REGRID, SQORST, WCSADD.

*  Implementation Status:
*     -  The LABEL, UNITS, and HISTORY components, and all extensions
*     are propagated. TITLE is controlled by the TITLE parameter.  DATA,
*     VARIANCE, AXIS and WCS are propagated after appropriate
*     modification.  QUALITY component is also propagated if
*     nearest-neighbour interpolation is being used.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  There can be an arbitrary number of NDF dimensions.

*  Copyright:
*     Copyright (C) 2002 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MBT: Mark Taylor (Starlink)
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     7-JAN-2002 (MBT):
*        Original version.
*     15-JAN-2002 (DSB):
*        Modified so that positions are always obtained in pixel coords.
*        Added propagation of QUALITY.  Also changed code to look more
*        like REGRID.  Added propagation of the AXIS component.
*     2005 October 13 (MJC):
*        Replace bad axis centres arising during resampling with
*        extrapolated values.
*     2012 May 9 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST definitions and declarations
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'NDF_PAR'          ! NDF system constants
      INCLUDE 'PAR_ERR'          ! Parameter system error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER KPG1_FLOOR         ! Most positive integer .LE. a given
                                 ! real
      INTEGER KPG1_CEIL          ! Most negative integer .GE. a given
                                 ! real

*  Local Variables:
      CHARACTER DTYPE * ( NDF__SZFTP ) ! Full data type name
      CHARACTER ITYPE * ( NDF__SZTYP ) ! HDS Data type name
      CHARACTER METHOD * ( 16 )  ! Name of resampling scheme
      CHARACTER STYPE * ( 16 )   ! Type of shift to be supplied
      DOUBLE PRECISION DUBNDI( NDF__MXDIM ) ! Upper bounds of input
                                 ! array
      DOUBLE PRECISION FID( NDF__MXDIM ) ! Co-ordinates of fiducial
                                 ! point
      DOUBLE PRECISION LPO       ! Lower bound in output array
      DOUBLE PRECISION OBJ( NDF__MXDIM ) ! Co-ordinates of standard
                                 ! object
      DOUBLE PRECISION PARAMS( 4 ) ! Additional parameters for resampler
      DOUBLE PRECISION PIA( NDF__MXDIM ) ! First input point
      DOUBLE PRECISION PIB( NDF__MXDIM ) ! Second input point
      DOUBLE PRECISION POA( NDF__MXDIM ) ! First output point
      DOUBLE PRECISION POB( NDF__MXDIM ) ! Second output point
      DOUBLE PRECISION PT1I( NDF__MXDIM ) ! First input point
      DOUBLE PRECISION PT1O( NDF__MXDIM ) ! First output point
      DOUBLE PRECISION PT2I( NDF__MXDIM ) ! Second input point
      DOUBLE PRECISION PT2O( NDF__MXDIM ) ! Second output point
      DOUBLE PRECISION SHIFT( NDF__MXDIM ) ! Translation vector
      DOUBLE PRECISION DLBNDI( NDF__MXDIM ) ! Lower bounds of input
                                 ! array
      DOUBLE PRECISION TOL       ! Tolerance for linear transform
                                 ! approximation
      DOUBLE PRECISION UPO       ! Upper bound in output array
      DOUBLE PRECISION XL( NDF__MXDIM ) ! Position of lowest value
      DOUBLE PRECISION XU( NDF__MXDIM ) ! Position of highest value
      INTEGER ELI                ! Number of elements in input NDF
      INTEGER ELO                ! Number of elements in output NDF
      INTEGER FLAGS              ! Flags sent to AST_RESAMPLE<X>
      INTEGER I                  ! Loop variable
      INTEGER INTERP             ! Resampling scheme identifier
      INTEGER IPAI               ! Pointer to input AXIS Centre array
      INTEGER IPAO               ! Pointer to output AXIS Centre array
      INTEGER IPDATI             ! Pointer to input Data array
      INTEGER IPDATO             ! Pointer to output Data array
      INTEGER IPQUAI             ! Pointer to input Quality array
      INTEGER IPQUAO             ! Pointer to output Quality array
      INTEGER IPVARI             ! Pointer to input Variance array
      INTEGER IPVARO             ! Pointer to output Variance array
      INTEGER LBNDI( NDF__MXDIM ) ! Lower bounds of input NDF pixel
                                 ! co-ordinates
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      INTEGER MAPA               ! Mapping from i/ axis centre to o/p
                                 ! axis centre
      INTEGER MAPHI              ! Half-pixel shift Mapping at input end
      INTEGER MAPHIO             ! Mapping with half-pixel shifts at
                                 ! both ends
      INTEGER MAPHO              ! Half-pixel shift Mapping at output
                                 ! end
      INTEGER MAPIO              ! Mapping from input to output NDF
      INTEGER MAXPIX             ! Max size of linear approximation
                                 ! region
      INTEGER NBAD               ! Number of bad pixels
      INTEGER NDFI               ! NDF identifier of input NDF
      INTEGER NDFO               ! NDF identifier of output NDF
      INTEGER NDIM               ! Number of dimensions of input and
                                 ! output NDF
      INTEGER NPARAM             ! Number of parameters required for
                                 ! resampler
      INTEGER NREP               ! Number of bad pixels replaced
      INTEGER OUTPRM( NDF__MXDIM ) ! Output axis permutation array for
                                 ! PMAP1
      INTEGER PMAP1              ! A PermMap which selects the current
                                 ! axis
      INTEGER PMAP2              ! The inverse of PMAP1
      INTEGER UBNDI( NDF__MXDIM ) ! Upper bounds of input NDF pixel
                                 ! co-ordinates
      INTEGER UBNDO( NDF__MXDIM ) ! Upper bounds of output NDF
      LOGICAL BAD                ! May there be bad pixels?
      LOGICAL HASQUA             ! Does the input NDF have Quality
                                 ! component?
      LOGICAL HASVAR             ! Does the input NDF have Variance
                                 ! component?
      LOGICAL HASAX              ! Does the input NDF have an AXIS
                                 ! Centre array?
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

*  Get its dimensions.
      CALL NDF_BOUND( NDFI, NDF__MXDIM, LBNDI, UBNDI, NDIM, STATUS )

*  See if it has a Variance component.
      CALL NDF_STATE( NDFI, 'VARIANCE', HASVAR, STATUS )

*  Get the input PIXEL -> output PIXEL Mapping.
*  ============================================

*  See if we want a relative or absolute shift.
      CALL PAR_CHOIC( 'STYPE', 'ABSOLUTE', 'ABSOLUTE,RELATIVE', .TRUE.,
     :                STYPE, STATUS )

*  Get the absolute shift directly as the value of the ABS parameter.
      IF ( STYPE .EQ. 'ABSOLUTE' ) THEN
         CALL PAR_EXACD( 'ABS', NDIM, SHIFT, STATUS )

*  Or get it as the difference between the FID and OBJ parameters.
      ELSE

*  Get the co-ordinates of the fiducial point.
         CALL PAR_EXACD( 'FID', NDIM, FID, STATUS )

*  Get the co-ordinates of the standard object.
         CALL PAR_EXACD( 'OBJ', NDIM, OBJ, STATUS )

*  Set the shift as the difference between the two.
         DO I = 1, NDIM
            SHIFT( I ) = FID( I ) - OBJ( I )
         END DO
      END IF

*  Construct a Mapping from input PIXEL to output PIXEL corresponding to
*  the given shifts.
      DO I = 1, NDIM
         PIA( I ) = 0D0
         PIB( I ) = 1D0
         POA( I ) = PIA( I ) + SHIFT( I )
         POB( I ) = PIB( I ) + SHIFT( I )
      END DO
      MAPIO = AST_WINMAP( NDIM, PIA, PIB, POA, POB, ' ', STATUS )

*  Get the qualifications to the transformation.
*  =============================================

*  Initialise the resampling routine control flags.
      FLAGS = 0

*  Get the method for calculating the output array value from the
*  input values.
      CALL PAR_CHOIC( 'METHOD', 'NEAREST', 'NEAREST,LINEAR,SINC,'//
     :                'SINCSINC,SINCCOS,SINCGAUSS,BLOCKAVE', .FALSE.,
     :                METHOD, STATUS )

      IF ( STATUS .NE. SAI__OK ) GO TO 999
      IF ( METHOD .EQ. 'NEAREST' ) THEN
         CALL MSG_SETC( 'M', 'Nearest Neighbour' )
         INTERP = AST__NEAREST
         NPARAM = 0
      ELSE IF ( METHOD .EQ. 'LINEAR' ) THEN
         CALL MSG_SETC( 'M', 'Bilinear' )
         INTERP = AST__LINEAR
         NPARAM = 0
      ELSE IF ( METHOD .EQ. 'SINC' ) THEN
         CALL MSG_SETC( 'M', 'Sinc' )
         INTERP = AST__SINC
         NPARAM = 1
      ELSE IF ( METHOD .EQ. 'SINCSINC' ) THEN
         CALL MSG_SETC( 'M', 'SincSinc' )
         INTERP = AST__SINCSINC
         NPARAM = 2
      ELSE IF ( METHOD .EQ. 'SINCCOS' ) THEN
         CALL MSG_SETC( 'M', 'SincCos' )
         INTERP = AST__SINCCOS
         NPARAM = 2
      ELSE IF ( METHOD .EQ. 'SINCGAUSS' ) THEN
         CALL MSG_SETC( 'M', 'SincGauss' )
         INTERP = AST__SINCGAUSS
         NPARAM = 2
      ELSE IF ( METHOD .EQ. 'BLOCKAVE' ) THEN
         CALL MSG_SETC( 'M', 'BlockAve' )
         INTERP = AST__BLOCKAVE
         NPARAM = 1
      END IF
      CALL MSG_OUT( 'SLIDE_MSG1', '  Using ^M interpolation.',
     :              STATUS )

*  Get an additional parameter vector if required.
      IF ( NPARAM .GT. 0 ) THEN
         CALL PAR_EXACD( 'PARAMS', NPARAM, PARAMS, STATUS )
      END IF

*  Set the tolerance for Mapping linear approximation.
      TOL = 0.1D0

*  Set the initial scale size for the Mapping linear approximation
*  algorithm (see AST_RESAMPLE<X> documentation).
      MAXPIX = 500

*  Get the bounds of the output NDF.
*  =================================

*  Work out the bounds of an array which would contain the resampled
*  copy of the whole input array.
      DO I = 1, NDIM
         DLBNDI( I ) = DBLE( LBNDI( I ) - 1 )
         DUBNDI( I ) = DBLE( UBNDI( I ) )
      END DO
      DO I = 1, NDIM
         CALL AST_MAPBOX( MAPIO, DLBNDI, DUBNDI, .TRUE., I, LPO, UPO,
     :                    XL, XU, STATUS )
         LBNDO( I ) = KPG1_FLOOR( REAL( LPO ) ) + 1
         UBNDO( I ) = KPG1_CEIL( REAL( UPO ) )
      END DO

*  Create and configure the output NDF.
*  ====================================

*  Create a new NDF by propagation from the input one.
      CALL LPG_PROP( NDFI, 'UNIT', 'OUT', NDFO, STATUS )

*  Get a title for the new NDF from the parameter system.
      CALL NDF_CINP( 'TITLE', NDFO, 'TITLE', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set the shape of the output NDF.
      CALL NDF_SBND( NDIM, LBNDO, UBNDO, NDFO, STATUS )

*  Determine a data type which can be used for operations on the
*  Data and possibly Variance components of the NDF.
      CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'//
     :                '_REAL,_DOUBLE', 1, NDFI, 'DATA,VARIANCE', ITYPE,
     :                DTYPE, STATUS )

*  Set the Data and possibly Variance component data types.
      CALL NDF_STYPE( ITYPE, NDFO, 'DATA', STATUS )
      IF ( HASVAR ) THEN
         CALL NDF_STYPE( ITYPE, NDFO, 'VARIANCE', STATUS )
      END IF

*  Store a suitably modified copy of the input WCS in the output.
      CALL KPG1_ASFIX( MAPIO, NDFI, NDFO, STATUS )

*  Map the array components.
*  =========================

*  Map the Data array of the input and output NDFs.
      CALL NDF_MAP( NDFI, 'DATA', ITYPE, 'READ', IPDATI, ELI, STATUS )
      CALL NDF_MAP( NDFO, 'DATA', ITYPE, 'WRITE', IPDATO, ELO,
     :              STATUS )

*  Find out if there may be bad pixels in the mapped Data array.
      CALL NDF_BAD( NDFI, 'DATA', .FALSE., BAD, STATUS )

*  Map the Variance component of the input and output NDFs if we are
*  processing variances.
      IF ( HASVAR ) THEN
         CALL NDF_MAP( NDFI, 'VARIANCE', ITYPE, 'READ', IPVARI, ELI,
     :                 STATUS )
         CALL NDF_MAP( NDFO, 'VARIANCE', ITYPE, 'WRITE', IPVARO,
     :                 ELO, STATUS )

*  Unless we already know of bad values in the Data component, see
*  whether the Variance component may contain them.
         IF ( .NOT. BAD ) THEN
            CALL NDF_BAD( NDFI, 'VARIANCE', .FALSE., BAD, STATUS )
         END IF

*  Record the fact that variances should be processed.
         FLAGS = FLAGS + AST__USEVAR
      END IF

*  If either the Data or Variance component of the input NDF may have
*  bad values, record this fact.
      IF ( BAD ) THEN
         FLAGS = FLAGS + AST__USEBAD
      END IF

*  Perform the resampling.
*  =======================

*  Since AST_RESAMPLE<X> requires the centre of pixels to be represented
*  by integers (the LBND and UBND arrays) it is necessary to add a
*  half-pixel shift onto both ends of the Mapping prior to executing
*  the resample.  First construct a Mapping which transforms minus a
*  half pixel in every input dimension.
      DO I = 1, NDIM
         PT1I( I ) = 0D0
         PT2I( I ) = 1D0
         PT1O( I ) = PT1I( I ) - 0.5D0
         PT2O( I ) = PT2I( I ) - 0.5D0
      END DO
      MAPHI = AST_WINMAP( NDIM, PT1I, PT2I, PT1O, PT2O, ' ', STATUS )

*  Then one which transforms plus a half-pixel in every output
*  dimension.
      DO I = 1, NDIM
         PT1I( I ) = 0D0
         PT2I( I ) = 1D0
         PT1O( I ) = PT1I( I ) + 0.5D0
         PT2O( I ) = PT2I( I ) + 0.5D0
      END DO
      MAPHO = AST_WINMAP( NDIM, PT1I, PT2I, PT1O, PT2O, ' ', STATUS )

*  Combine these to get a Mapping which does what we want it to,
*  correcting for the half pixel at either end.
      MAPHIO = AST_CMPMAP( MAPHI, MAPIO, .TRUE., ' ', STATUS )
      MAPHIO = AST_CMPMAP( MAPHIO, MAPHO, .TRUE., ' ', STATUS )
      MAPHIO = AST_SIMPLIFY( MAPHIO, STATUS )

*  Perform the resampling according to data type.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         NBAD = AST_RESAMPLEB( MAPHIO, NDIM, LBNDI, UBNDI,
     :                         %VAL( CNF_PVAL( IPDATI ) ),
     :                         %VAL( CNF_PVAL( IPVARI ) ), INTERP,
     :                         AST_NULL, PARAMS, FLAGS, TOL, MAXPIX,
     :                         VAL__BADB, NDIM, LBNDO, UBNDO, LBNDO,
     :                         UBNDO, %VAL( CNF_PVAL( IPDATO ) ),
     :                         %VAL( CNF_PVAL( IPVARO ) ),
     :                         STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         NBAD = AST_RESAMPLEUB( MAPHIO, NDIM, LBNDI, UBNDI,
     :                          %VAL( CNF_PVAL( IPDATI ) ),
     :                          %VAL( CNF_PVAL( IPVARI ) ), INTERP,
     :                          AST_NULL, PARAMS, FLAGS, TOL, MAXPIX,
     :                          VAL__BADUB, NDIM, LBNDO, UBNDO, LBNDO,
     :                          UBNDO, %VAL( CNF_PVAL( IPDATO ) ),
     :                          %VAL( CNF_PVAL( IPVARO ) ),
     :                          STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         NBAD = AST_RESAMPLEW( MAPHIO, NDIM, LBNDI, UBNDI,
     :                         %VAL( CNF_PVAL( IPDATI ) ),
     :                         %VAL( CNF_PVAL( IPVARI ) ), INTERP,
     :                         AST_NULL, PARAMS, FLAGS, TOL, MAXPIX,
     :                         VAL__BADW, NDIM, LBNDO, UBNDO, LBNDO,
     :                         UBNDO, %VAL( CNF_PVAL( IPDATO ) ),
     :                         %VAL( CNF_PVAL( IPVARO ) ),
     :                         STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         NBAD = AST_RESAMPLEUW( MAPHIO, NDIM, LBNDI, UBNDI,
     :                          %VAL( CNF_PVAL( IPDATI ) ),
     :                          %VAL( CNF_PVAL( IPVARI ) ), INTERP,
     :                          AST_NULL, PARAMS, FLAGS, TOL, MAXPIX,
     :                          VAL__BADUW, NDIM, LBNDO, UBNDO, LBNDO,
     :                          UBNDO, %VAL( CNF_PVAL( IPDATO ) ),
     :                          %VAL( CNF_PVAL( IPVARO ) ),
     :                          STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         NBAD = AST_RESAMPLEI( MAPHIO, NDIM, LBNDI, UBNDI,
     :                         %VAL( CNF_PVAL( IPDATI ) ),
     :                         %VAL( CNF_PVAL( IPVARI ) ), INTERP,
     :                         AST_NULL, PARAMS, FLAGS, TOL, MAXPIX,
     :                         VAL__BADI, NDIM, LBNDO, UBNDO, LBNDO,
     :                         UBNDO, %VAL( CNF_PVAL( IPDATO ) ),
     :                         %VAL( CNF_PVAL( IPVARO ) ),
     :                         STATUS )

      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         NBAD = AST_RESAMPLEK( MAPHIO, NDIM, LBNDI, UBNDI,
     :                         %VAL( CNF_PVAL( IPDATI ) ),
     :                         %VAL( CNF_PVAL( IPVARI ) ), INTERP,
     :                         AST_NULL, PARAMS, FLAGS, TOL, MAXPIX,
     :                         VAL__BADK, NDIM, LBNDO, UBNDO, LBNDO,
     :                         UBNDO, %VAL( CNF_PVAL( IPDATO ) ),
     :                         %VAL( CNF_PVAL( IPVARO ) ),
     :                         STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         NBAD = AST_RESAMPLER( MAPHIO, NDIM, LBNDI, UBNDI,
     :                         %VAL( CNF_PVAL( IPDATI ) ),
     :                         %VAL( CNF_PVAL( IPVARI ) ), INTERP,
     :                         AST_NULL, PARAMS, FLAGS, TOL, MAXPIX,
     :                         VAL__BADR, NDIM, LBNDO, UBNDO, LBNDO,
     :                         UBNDO, %VAL( CNF_PVAL( IPDATO ) ),
     :                         %VAL( CNF_PVAL( IPVARO ) ),
     :                         STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         NBAD = AST_RESAMPLED( MAPHIO, NDIM, LBNDI, UBNDI,
     :                         %VAL( CNF_PVAL( IPDATI ) ),
     :                         %VAL( CNF_PVAL( IPVARI ) ), INTERP,
     :                         AST_NULL, PARAMS, FLAGS, TOL, MAXPIX,
     :                         VAL__BADD, NDIM, LBNDO, UBNDO, LBNDO,
     :                         UBNDO, %VAL( CNF_PVAL( IPDATO ) ),
     :                         %VAL( CNF_PVAL( IPVARO ) ),
     :                         STATUS )
      END IF

*  We can set the bad pixels flag according to the bad pixel count
*  returned from AST_RESAMPLE<X>.
      BAD = NBAD .GT. 0
      CALL NDF_SBAD( BAD, NDFO, 'DATA', STATUS )
      IF ( HASVAR ) THEN
         CALL NDF_SBAD( BAD, NDFO, 'VARIANCE', STATUS )
      END IF

*  If using Nearest Neighbour interpolation, resample any QUALITY array.
*  =====================================================================
      CALL NDF_STATE( NDFI, 'QUALITY', HASQUA, STATUS )
      IF( INTERP .EQ. AST__NEAREST .AND. HASQUA ) THEN

*  Map the QUALITY array of the input and output NDFs. Note, QUALITY
*  arrays should always be mapped as _UBYTE.
         CALL NDF_MAP( NDFI, 'QUALITY', '_UBYTE', 'READ', IPQUAI, ELI,
     :                 STATUS )
         CALL NDF_MAP( NDFO, 'QUALITY', '_UBYTE', 'WRITE', IPQUAO, ELO,
     :                 STATUS )

*  Do the resampling.
         NBAD = AST_RESAMPLEUB( MAPHIO, NDIM, LBNDI, UBNDI,
     :                          %VAL( CNF_PVAL( IPQUAI ) ),
     :                          %VAL( CNF_PVAL( IPQUAI ) ), INTERP,
     :                          AST_NULL, PARAMS, 0, TOL, MAXPIX,
     :                          VAL__BADUB, NDIM, LBNDO, UBNDO, LBNDO,
     :                          UBNDO, %VAL( CNF_PVAL( IPQUAO ) ),
     :                          %VAL( CNF_PVAL( IPQUAO ) ),
     :                          STATUS )

      END IF

*  Resample any AXIS arrays.
*  =========================

*  Initialize the output axis permutation array to hold zeros,
*  indicating that none of the outputs from the PermMap are connected to
*  an input.
      DO I = 1, NDIM
         OUTPRM( I ) = 0
      END DO

*  Loop round each NDF axis which has a defined AXIS Centre array.
      DO I = 1, NDIM
         CALL NDF_ASTAT( NDFI, 'Centre', I, HASAX, STATUS )
         IF( HASAX ) THEN

*  Modify the Mapping to select this axis at both ends.
            OUTPRM( I ) = 1
            PMAP1 = AST_PERMMAP( 1, I, NDIM, OUTPRM, 0.0D0, ' ',
     :                           STATUS )
            OUTPRM( I ) = 0

            PMAP2 = AST_COPY( PMAP1, STATUS )
            CALL AST_INVERT( PMAP2, STATUS )

            MAPA = AST_CMPMAP( AST_CMPMAP( PMAP1, MAPHIO, .TRUE., ' ',
     :                                     STATUS ),
     :                         PMAP2, .TRUE., ' ', STATUS )

*  Get the data type of the Centre array and map it in both input and
*  output NDFs.
            CALL NDF_ATYPE( NDFI, 'Centre', I, ITYPE, STATUS )

            CALL NDF_AMAP( NDFI, 'Centre', I, ITYPE, 'READ', IPAI, ELI,
     :                     STATUS )
            CALL NDF_AMAP( NDFO, 'Centre', I, ITYPE, 'WRITE', IPAO, ELO,
     :                     STATUS )

*  Tell AST_RESAMPLE to recognize bad values.
            FLAGS = AST__USEBAD

*  Perform the resampling according to data type.  Replace any bad
*  values introduced at the start and/or end of the axis array,
*  extrapolating from the next interior pair.
            IF ( ITYPE .EQ. '_BYTE' ) THEN
               NBAD = AST_RESAMPLEB( MAPA, 1, LBNDI( I ), UBNDI( I ),
     :                               %VAL( CNF_PVAL( IPAI ) ),
     :                               %VAL( CNF_PVAL( IPAI ) ), INTERP,
     :                               AST_NULL, PARAMS, FLAGS, TOL,
     :                               MAXPIX, VAL__BADB, 1, LBNDO( I ),
     :                               UBNDO( I ), LBNDO( I ), UBNDO( I ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               STATUS )

               IF ( NBAD .GT. 0 ) THEN
                  CALL KPS1_SLAEB( LBNDO( I ), UBNDO( I ),
     :                             %VAL( CNF_PVAL( IPAO ) ), NREP,
     :                             STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               NBAD = AST_RESAMPLEUB( MAPA, 1, LBNDI( I ), UBNDI( I ),
     :                               %VAL( CNF_PVAL( IPAI ) ),
     :                               %VAL( CNF_PVAL( IPAI ) ), INTERP,
     :                               AST_NULL, PARAMS, FLAGS, TOL,
     :                               MAXPIX, VAL__BADUB, 1, LBNDO( I ),
     :                               UBNDO( I ), LBNDO( I ), UBNDO( I ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               STATUS )

               IF ( NBAD .GT. 0 ) THEN
                  CALL KPS1_SLAEUB( LBNDO( I ), UBNDO( I ),
     :                              %VAL( CNF_PVAL( IPAO ) ), NREP,
     :                              STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               NBAD = AST_RESAMPLEW( MAPA, 1, LBNDI( I ), UBNDI( I ),
     :                               %VAL( CNF_PVAL( IPAI ) ),
     :                               %VAL( CNF_PVAL( IPAI ) ), INTERP,
     :                               AST_NULL, PARAMS, FLAGS, TOL,
     :                               MAXPIX, VAL__BADW, 1, LBNDO( I ),
     :                               UBNDO( I ), LBNDO( I ), UBNDO( I ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               STATUS )

               IF ( NBAD .GT. 0 ) THEN
                  CALL KPS1_SLAEW( LBNDO( I ), UBNDO( I ),
     :                             %VAL( CNF_PVAL( IPAO ) ), NREP,
     :                             STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               NBAD = AST_RESAMPLEUW( MAPA, 1, LBNDI( I ), UBNDI( I ),
     :                               %VAL( CNF_PVAL( IPAI ) ),
     :                               %VAL( CNF_PVAL( IPAI ) ), INTERP,
     :                               AST_NULL, PARAMS, FLAGS, TOL,
     :                               MAXPIX, VAL__BADUW, 1, LBNDO( I ),
     :                               UBNDO( I ), LBNDO( I ), UBNDO( I ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               STATUS )

               IF ( NBAD .GT. 0 ) THEN
                  CALL KPS1_SLAEUW( LBNDO( I ), UBNDO( I ),
     :                              %VAL( CNF_PVAL( IPAO ) ), NREP,
     :                              STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               NBAD = AST_RESAMPLEI( MAPA, 1, LBNDI( I ), UBNDI( I ),
     :                               %VAL( CNF_PVAL( IPAI ) ),
     :                               %VAL( CNF_PVAL( IPAI ) ), INTERP,
     :                               AST_NULL, PARAMS, FLAGS, TOL,
     :                               MAXPIX, VAL__BADI, 1, LBNDO( I ),
     :                               UBNDO( I ), LBNDO( I ), UBNDO( I ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               STATUS )
               IF ( NBAD .GT. 0 ) THEN
                  CALL KPS1_SLAEI( LBNDO( I ), UBNDO( I ),
     :                             %VAL( CNF_PVAL( IPAO ) ), NREP,
     :                             STATUS )
               END IF


            ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
               NBAD = AST_RESAMPLEK( MAPA, 1, LBNDI( I ), UBNDI( I ),
     :                               %VAL( CNF_PVAL( IPAI ) ),
     :                               %VAL( CNF_PVAL( IPAI ) ), INTERP,
     :                               AST_NULL, PARAMS, FLAGS, TOL,
     :                               MAXPIX, VAL__BADK, 1, LBNDO( I ),
     :                               UBNDO( I ), LBNDO( I ), UBNDO( I ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               STATUS )
               IF ( NBAD .GT. 0 ) THEN
                  CALL KPS1_SLAEK( LBNDO( I ), UBNDO( I ),
     :                             %VAL( CNF_PVAL( IPAO ) ), NREP,
     :                             STATUS )
               END IF


            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               NBAD = AST_RESAMPLER( MAPA, 1, LBNDI( I ), UBNDI( I ),
     :                               %VAL( CNF_PVAL( IPAI ) ),
     :                               %VAL( CNF_PVAL( IPAI ) ), INTERP,
     :                               AST_NULL, PARAMS, FLAGS, TOL,
     :                               MAXPIX, VAL__BADR, 1, LBNDO( I ),
     :                               UBNDO( I ), LBNDO( I ), UBNDO( I ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               STATUS )

               IF ( NBAD .GT. 0 ) THEN
                  CALL KPS1_SLAER( LBNDO( I ), UBNDO( I ),
     :                             %VAL( CNF_PVAL( IPAO ) ), NREP,
     :                             STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               NBAD = AST_RESAMPLED( MAPA, 1, LBNDI( I ), UBNDI( I ),
     :                               %VAL( CNF_PVAL( IPAI ) ),
     :                               %VAL( CNF_PVAL( IPAI ) ), INTERP,
     :                               AST_NULL, PARAMS, FLAGS, TOL,
     :                               MAXPIX, VAL__BADD, 1, LBNDO( I ),
     :                               UBNDO( I ), LBNDO( I ), UBNDO( I ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               %VAL( CNF_PVAL( IPAO ) ),
     :                               STATUS )

               IF ( NBAD .GT. 0 ) THEN
                  CALL KPS1_SLAED( LBNDO( I ), UBNDO( I ),
     :                             %VAL( CNF_PVAL( IPAO ) ), NREP,
     :                             STATUS )
               END IF
            END IF

*  Unmap the Centre arrays.
            CALL NDF_AUNMP( NDFI, 'Centre', I, STATUS )
            CALL NDF_AUNMP( NDFO, 'Centre', I, STATUS )

         END IF
      END DO

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
         CALL ERR_REP( 'SLIDE_ERR1', 'SLIDE: Unable to shift pixels '//
     :                 'in an NDF.', STATUS )
      END IF

      END
