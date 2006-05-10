      SUBROUTINE SQORST( STATUS )
*+
*  Name:
*     SQORST

*  Purpose:
*     Squashes or stretches an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SQORST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     An output NDF is produced by squashing or stretching an input 
*     NDF along one or more of its dimensions.  The shape of the
*     output NDF can be specified in one of two ways, according to
*     the value of the MODE parameter; either a distortion factor 
*     is given for each dimension, or its lower and upper pixel 
*     bounds are given explicitly.

*  Usage:
*     sqorst in out factors

*  ADAM Parameters:
*     FACTORS( ) = _DOUBLE (Read)
*        The factor by which each dimension will be distorted to 
*        produce the output NDF.  A factor greater than one is a stretch
*        and less than one is a squash.  The number of values supplied
*        must be the same as the number of dimensions of the NDF.  
*        Only used if MODE="Factors".
*     IN = NDF (Read)
*        The NDF to be squashed or stretched.
*     LBOUND( ) = _INTEGER
*        The lower pixel index values of the output NDF.  The number of
*        values supplied must be the same as the number of dimensions
*        of the NDF.  If null (!) is given, the lower pixel bounds of
*        the input NDF will be used.  Only used if MODE="Bounds".
*     METHOD = LITERAL (Read)
*        The interpolation method used to perform the 1-dimensional
*        resampling operations which constitute the squash or stretch.
*        The following values are permitted:
*
*        -  "Auto"      -- Equivalent to "BlockAve" with an appropriate
*                          PARAMS for squashes by a factor of 2 or more,
*                          otherwise equivalent to "Linear".
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
*        -  "BlockAve"  -- Block averaging over surrounding pixels.
*
*        In the above, sinc(z)=sin(z)/z.  Some of these schemes will
*        require additional parameters to be supplied via the PARAMS
*        parameter.  A more detailed discussion of these schemes is
*        given in the "Sub-Pixel Interpolation Schemes" section below.
*        ["Auto"]
*     MODE = LITERAL (Read)
*        Determines how the shape of the output NDF is to be specified.
*        If MODE="Factors" (the default) then the FACTORS parameter
*        will be used to determine the factor by which each dimension
*        should be multiplied.  If MODE="Bounds" then the LBOUND and
*        UBOUND parameters will be used to get the lower and upper
*        pixel bounds of the output NDF. ["Factors"]
*     OUT = NDF (Write)
*        The squashed or stretched NDF.
*     PARAMS( ) = _DOUBLE (Read)
*        Parameters required to control the resampling scheme.  One or
*        more values may be required to specify the exact resampling
*        behaviour, according to the value of the METHOD parameter.
*        See the section on "Sub-Pixel Interpolation Schemes".
*     TITLE = LITERAL (Read)
*        Title for the output NDF. A null (!) value causes the input
*        title to be used. [!]
*     UBOUND( ) = _INTEGER
*        The upper pixel index values of the output NDF.  The number of
*        values supplied must be the same as the number of dimensions
*        of the NDF.  If null (!) is given, the upper pixel bounds of
*        the input NDF will be used.  Only used if MODE="Bounds".

*  Examples:
*     sqorst block blocktall [1,2,1]
*        The 3-dimensional NDF called block is stretched by a factor 
*        of two along its second axis to produce an NDF called
*        blocktall with twice as many pixels.  The same data block
*        is represented, but each pixel in the output NDF corresponds
*        to half a pixel in the input NDF.  The default resampling
*        scheme, linear interpolation in the stretch direction, is used.
*     sqorst block blocktall [1,2,1] method=sincsinc params=[2,2]
*        The same operation as the previous example is performed, 
*        except that a Lanczos kernel is used for the interpolation.
*     sqorst cygnus1 squish1 mode=bounds lbound=[1,1] ubound=[50,50]
*        This turns the 2-dimensional NDF cygnus1 into a new NDF squish1 
*        which has 50 pixels along each side.  The same region of sky
*        is represented, but the input image is squashed along both
*        axes to fit the specified dimensions.
*
*  Sub-Pixel Interpolation Schemes:
*     When squashing or stretching an NDF, a separate one-dimensional 
*     resampling operation is performed for each of the dimensions
*     in which a resize is being done.  By default (when METHOD="Auto")
*     this is done using linear interpolation, unless it is a 
*     squash of a factor of FACT=2 or more, in which case a block
*     averaging scheme which averages over FACT pixels.  For many 
*     purposes this default scheme will be adequate, but for greater
*     control over the resampling process the METHOD and PARAMS 
*     parameters can be used.  Detailed discussion of the use of these 
*     parameters can be found in the "Sub-pixel Interpolation Schemes"
*     section of the REGRID task documentation.

*  Notes:
*     If the input NDF contains a Variance component, a Variance
*     component will be written to the output NDF.  It will be
*     calculated on the assumption that errors on the input data
*     values are statistically independent and that their variance
*     estimates may simply be summed (with appropriate weighting
*     factors) when several input pixels contribute to an output data
*     value. If this assumption is not valid, then the output error
*     estimates may be biased. In addition, note that the statistical
*     errors on neighbouring output data values (as well as the
*     estimates of those errors) may often be correlated, even if the
*     above assumption about the input data is correct, because of
*     the sub-pixel interpolation schemes employed.

*  Related Applications:
*     KAPPA: REGRID, SLIDE, WCSADD.

*  Implementation Status:
*     -  The LABEL, UNITS, and HISTORY components, and all extensions are 
*     propagated. TITLE is controlled by the TITLE parameter. DATA,
*     VARIANCE, AXIS and WCS are propagated after appropriate modification.
*     QUALITY component is also propagated if Nearest Neighbour
*     interpolation is being used. 
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  There can be an arbitrary number of NDF dimensions.

*  Copyright:
*     Copyright (C) 2002, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     MBT: Mark Taylor (Starlink)
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2-JAN-2002 (MBT):
*        Original version.
*     16-JAN-2002 (DSB):
*        Added propagation of QUALITY and AXIS.
*     3-SEP-2002 (DSB):
*        Avoid use of PSX_CALLOC since it cannot handle all HDS data types.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST definitions and declarations
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'NDF_PAR'          ! NDF system constants
      INCLUDE 'PAR_ERR'          ! PAR system error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DTYPE * ( NDF__SZFTP ) ! Full data type name
      CHARACTER ITYPE * ( NDF__SZTYP ) ! HDS Data type name
      CHARACTER METHOD * ( 16 )  ! Resampling method
      CHARACTER MODE * ( 10 )    ! Mode for getting output bounds
      DOUBLE PRECISION FACTS( NDF__MXDIM ) ! Expansion factors
      DOUBLE PRECISION PARAMS( 8 ) ! Auxiliary parameters for resampling
      DOUBLE PRECISION PIA( NDF__MXDIM ) ! First input point
      DOUBLE PRECISION PIB( NDF__MXDIM ) ! Second input point
      DOUBLE PRECISION POA( NDF__MXDIM ) ! First output point
      DOUBLE PRECISION POB( NDF__MXDIM ) ! Second output point
      DOUBLE PRECISION ZOOM      ! Zoom factor for 1d expansion
      INTEGER BMIN( NDF__MXDIM ) ! Invalid values for pixel index bound defaults
      INTEGER BMAX( NDF__MXDIM ) ! Maximum values for pixel index bounds
      INTEGER BPV                ! Bytes per value for selected data type
      INTEGER DIM1( NDF__MXDIM ) ! Dimensions of intermediate input array
      INTEGER DIM2( NDF__MXDIM ) ! Dimensions of intermediate output array
      INTEGER DIMI( NDF__MXDIM ) ! Dimensions of input NDF
      INTEGER DIMO( NDF__MXDIM ) ! Dimensions of output NDF
      INTEGER EL                 ! Number of elements in array
      INTEGER EL2                ! Number of elements in output array
      INTEGER I                  ! Loop variable
      INTEGER INTERP             ! Resampling scheme identifier
      INTEGER IPAXI              ! Pointer to input AXIS Centre array
      INTEGER IPAXO              ! Pointer to output AXIS Centre array
      INTEGER IPDAT1             ! Pointer to intermediate input data array
      INTEGER IPDAT2             ! Pointer to intermediate output data array
      INTEGER IPDATI             ! Pointer to data array of input NDF
      INTEGER IPDATO             ! Pointer to data array of output NDF
      INTEGER IPQUA1             ! Pointer to intermediate input quality array
      INTEGER IPQUA2             ! Pointer to intermediate output quality array
      INTEGER IPQUAI             ! Pointer to quality array of input NDF
      INTEGER IPQUAO             ! Pointer to quality array of output NDF
      INTEGER IPVAR1             ! Pointer to intermediate input variance array
      INTEGER IPVAR2             ! Pointer to intermediate output variance array
      INTEGER IPVARI             ! Pointer to variance array of input NDF
      INTEGER IPVARO             ! Pointer to variance array of output NDF
      INTEGER IPWD1              ! Pointer to workspace
      INTEGER IPWD2              ! Pointer to workspace
      INTEGER IPWQ1              ! Pointer to workspace
      INTEGER IPWQ2              ! Pointer to workspace
      INTEGER IPWV1              ! Pointer to workspace
      INTEGER IPWV2              ! Pointer to workspace
      INTEGER J                  ! Loop variable
      INTEGER LASTDM             ! Index of last dimension needing resampling
      INTEGER LBNDI( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      INTEGER MAP                ! AST Mapping representing modifications
      INTEGER NDFI               ! Input NDF identifier
      INTEGER NDFO               ! Output NDF identifier
      INTEGER NDIM               ! Number of dimensions of NDFs
      INTEGER NPARAM             ! Number of auxiliary resampling parameters
      INTEGER UBNDI( NDF__MXDIM ) ! Upper bounds of input NDF
      INTEGER UBNDO( NDF__MXDIM ) ! Upper bounds of output NDF
      LOGICAL BAD                ! May there be bad pixels?
      LOGICAL HASAXI             ! Do we have an AXIS Centre component?
      LOGICAL HASQUA             ! Do we have a quality component?
      LOGICAL HASVAR             ! Do we have a variance component?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new AST context.
      CALL AST_BEGIN( STATUS )

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Get input parameters and open NDFs.
*  ===================================

*  Open the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Find out if we have a Variance component.
      CALL NDF_STATE( NDFI, 'VARIANCE', HASVAR, STATUS )

*  Find out if we have a Quality component.
      CALL NDF_STATE( NDFI, 'QUALITY', HASQUA, STATUS )

*  Determine a data type which can be used for operations on its Data
*  and possibly Variance components.
      CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,' //
     :                '_DOUBLE', 1, NDFI, 'DATA,VARIANCE', ITYPE,
     :                DTYPE, STATUS )

*  Determine the number of bytes per value for the selected data type.
*  We do this becaise PSX_CALLOC cannot accept the whole range of HDS
*  data types. So we work out the numberof bytes needed and use 
*  PSX_MALLOC instead.
      IF( ITYPE .EQ. '_BYTE' ) THEN
	BPV = VAL__NBB
      ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
	BPV = VAL__NBUB
      ELSE IF( ITYPE .EQ. '_WORD' ) THEN
	BPV = VAL__NBW
      ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
	BPV = VAL__NBUW
      ELSE IF( ITYPE .EQ. '_INTEGER' ) THEN
	BPV = VAL__NBI
      ELSE IF( ITYPE .EQ. '_REAL' ) THEN
	BPV = VAL__NBR
      ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
	BPV = VAL__NBD
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( 'SQORST_ERR1', 'Data type '//ITYPE//' not yet '//
     :                'supported fully.', STATUS )
      END IF  

*  Get its pixel index bounds.
      CALL NDF_BOUND( NDFI, NDF__MXDIM, LBNDI, UBNDI, NDIM, STATUS )

*  Find out how the output shape will be supplied.
      CALL PAR_CHOIC( 'MODE', 'FACTORS', 'FACTORS,BOUNDS', .FALSE.,
     :                MODE, STATUS )

*  Work out the output shape either by expansion factors or by 
*  explicitly supplied bounds.
      IF ( MODE .EQ. 'FACTORS' ) THEN

*  Get the expansion factors.
         CALL PAR_EXACD( 'FACTORS', NDIM, FACTS, STATUS )

*  Calclulate the upper and lower output bounds from the factors.
         DO I = 1, NDIM
            LBNDO( I ) = NINT( DBLE( LBNDI( I ) - 1 ) * FACTS( I ) )
     :                   + 1
            UBNDO( I ) = NINT( DBLE( UBNDI( I ) ) * FACTS( I ) )
         END DO
      ELSE

*  Get the lower bounds.
         IF ( STATUS .NE. SAI__OK ) GO TO 999
         CALL PAR_EXACI( 'LBOUND', NDIM, LBNDO, STATUS )

*  If null was supplied, use the input NDF lower bounds.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            DO I = 1, NDIM
               LBNDO( I ) = LBNDI( I )
            END DO
         END IF

*  Get the upper bounds.
         DO I = 1, NDIM
            BMIN( I ) = VAL__MINI
            BMAX( I ) = VAL__MAXI
         END DO
         IF ( STATUS .NE. SAI__OK ) GO TO 999
         CALL PAR_GRM1I( 'UBOUND', NDIM, BMIN, LBNDO, BMAX, .FALSE.,
     :                   UBNDO, STATUS )

*  If null was supplied, use the input NDF upper bounds.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            DO I = 1, NDIM
               UBNDO( I ) = UBNDI( I )
            END DO
         END IF
      END IF

*  Create a new NDF by propagation from the input one. 
      CALL LPG_PROP( NDFI, 'AXIS', 'OUT', NDFO, STATUS )

*  Get a title from the parameter system.
      CALL NDF_CINP( 'TITLE', NDFO, 'TITLE', STATUS )

*  Reshape it according to the reqested lower and upper bounds.
      CALL NDF_SBND( NDIM, LBNDO, UBNDO, NDFO, STATUS )

*  Set the Data and possibly Variance component data types.
      CALL NDF_STYPE( ITYPE, NDFO, 'DATA', STATUS )
      IF ( HASVAR ) THEN
         CALL NDF_STYPE( ITYPE, NDFO, 'VARIANCE', STATUS )
      END IF

*  Exit if there has been an error so far.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Calculate the input and output dimensions for convenience.
      DO I = 1, NDIM
         DIMI( I ) = UBNDI( I ) - LBNDI( I ) + 1
         DIMO( I ) = UBNDO( I ) - LBNDO( I ) + 1
      END DO

*  Determine the highest-numbered dimension which needs resampling.
      LASTDM = 0
      DO I = 1, NDIM
         IF ( DIMO( I ) .NE. DIMI( I ) ) THEN
            LASTDM = I
         END IF
      END DO

*  Get the resampling method if necessary.
      IF ( LASTDM .GT. 0 ) THEN
         NPARAM = 0
         METHOD = 'AUTO'
         CALL PAR_CHOIC( 'METHOD', 'AUTO', 'AUTO,NEAREST,LINEAR,'//
     :                   'SINC,SINCSINC,SINCCOS,SINCGAUSS,BLOCKAVE',
     :                   .TRUE., METHOD, STATUS )

*  Set the values to pass to AST_RESAMPLE<X> accordingly.  If AUTO
*  is selected these will not be required.
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
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Put the correct WCS FrameSet in the new NDF.
*  ============================================

*  Construct a Mapping representing the transformation between the 
*  old pixel coordinates and the new ones.  This is just used to
*  fix up the WCS FrameSet, the resampling routine generates its 
*  own Mappings.
      DO I = 1, NDIM
         PIA( I ) = DBLE( LBNDI( I ) - 1 ) 
         PIB( I ) = DBLE( UBNDI( I ) )
         POA( I ) = DBLE( LBNDO( I ) - 1 )
         POB( I ) = DBLE( UBNDO( I ) )
      END DO
      MAP = AST_WINMAP( NDIM, PIA, PIB, POA, POB, ' ', STATUS )

*  Fix it up according to the changes we will make.
      CALL KPG1_ASFIX( MAP, NDFI, NDFO, STATUS )

*  Check if any resampling is required.
      IF ( LASTDM .GT. 0 ) THEN

*  Resample the NDF into its new shape.
*  ====================================

*  Map the input data array and possibly variance array.
         CALL NDF_MAP( NDFI, 'DATA', ITYPE, 'READ', IPDATI, EL, STATUS )
         IF ( HASVAR ) THEN
            CALL NDF_MAP( NDFI, 'VARIANCE', ITYPE, 'READ', IPVARI, EL,
     :                    STATUS )
         END IF

*  See if we have bad pixels.
         CALL NDF_BAD( NDFI, 'DATA', .FALSE., BAD, STATUS )
         IF ( HASVAR .AND. .NOT. BAD ) THEN
            CALL NDF_BAD( NDFI, 'VARIANCE', .FALSE., BAD, STATUS )
         END IF

*  If using Nearest Neighbour interpolation, also map the QUALITY array.
         IF( HASQUA .AND. INTERP .EQ. AST__NEAREST ) THEN
            CALL NDF_MAP( NDFI, 'QUALITY', '_UBYTE', 'READ', IPQUAI, EL, 
     :                    STATUS )
         ELSE
            HASQUA = .FALSE.
         END IF        

*  Initialise the per-iteration input data to be the input NDF.
         IPDAT1 = IPDATI
         IPVAR1 = IPVARI
         IPQUA1 = IPQUAI
         DO I = 1, NDIM
            DIM1( I ) = DIMI( I )
            DIM2( I ) = DIMI( I )
         END DO

*  Iterate over each dimension applying non-Unit Mappings in turn.
*  We do this one dimension at a time rather than using a single 
*  invocation of AST_RESAMPLE<X> to prevent leakage of pixel values in 
*  unaltered dimensions, which can't be done in some (all?) of the
*  provided sub-pixel resampling schemes.  This will be a bit less 
*  efficient, but in most cases SQORST will probably be only
*  used along one dimension at a time in any case.
         DO I = 1, LASTDM 

*  Do we need to resample in this dimension?
            IF ( DIMO( I ) .NE. DIMI( I ) ) THEN

*  Set the per-iteration output dimensions.
               DIM2( I ) = DIMO( I )

*  Calculate the factor of output to input size along this dimension.
               ZOOM = DBLE( DIMO( I ) ) / DBLE( DIMI( I ) )

*  Select an appropriate resampling scheme.
               IF ( METHOD .EQ. 'AUTO' ) THEN
                  IF ( ZOOM .LT. 0.5D0 ) THEN
                     INTERP = AST__BLOCKAVE
                     PARAMS( 1 ) = 0.5D0 / ZOOM
                  ELSE
                     INTERP = AST__LINEAR
                  END IF
               END IF

*  If this axis has a defined AXIS Centre array, we need to over-write the 
*  AXIS Centre array propagated from the input NDF, by resampling the input 
*  AXIS Centre array. See if it is defined. If so, map the input and
*  output AXIS Centre array as _DOUBLE. Reset other AXIS arrays in the
*  output.
               CALL NDF_ASTAT( NDFI, 'Centre', I, HASAXI, STATUS ) 
               IF( HASAXI ) THEN
                  CALL NDF_AMAP( NDFI, 'Centre', I, '_DOUBLE', 'READ',
     :                           IPAXI, EL, STATUS ) 
                  CALL NDF_AMAP( NDFO, 'Centre', I, '_DOUBLE', 'WRITE',
     :                           IPAXO, EL, STATUS ) 
                  CALL NDF_AREST( NDFO, 'Variance,Width', I, STATUS )                
               END IF            

*  Get a pointer to the array into which the results will be written.
*  This may be either a temporary workspace array allocated for the
*  purpose, or if this is the last transformation which needs to be
*  done, the output NDF arrays themselves.
               IF ( I .EQ. LASTDM ) THEN
                  CALL NDF_MAP( NDFO, 'DATA', ITYPE, 'WRITE', IPDAT2,
     :                          EL2, STATUS )
                  IF ( HASVAR ) THEN
                     CALL NDF_MAP( NDFO, 'VARIANCE', ITYPE, 'WRITE',
     :                             IPVAR2, EL2, STATUS )
                  END IF
                  IF ( HASQUA ) THEN
                     CALL NDF_MAP( NDFO, 'QUALITY', '_UBYTE', 'WRITE',
     :                             IPQUA2, EL2, STATUS )
                  END IF
               ELSE
                  EL2 = 1
                  DO J = 1, NDIM
                     EL2 = EL2 * DIM2( J )
                  END DO
                  CALL PSX_MALLOC( EL2*BPV, IPDAT2, STATUS )
                  IF ( HASVAR ) THEN
                     CALL PSX_MALLOC( EL2*BPV, IPVAR2, STATUS )
                  END IF
                  IF ( HASQUA ) THEN
                     CALL PSX_MALLOC( EL2*VAL__NBUB, IPQUA2, STATUS )
                  END IF
               END IF

*  Allocate additional temporary workspace.
               CALL PSX_MALLOC( DIM1( I )*BPV, IPWD1, STATUS )
               CALL PSX_MALLOC( DIM2( I )*BPV, IPWD2, STATUS )
               IF ( HASVAR ) THEN
                  CALL PSX_MALLOC( DIM1( I )*BPV, IPWV1, STATUS )
                  CALL PSX_MALLOC( DIM2( I )*BPV, IPWV2, STATUS )
               END IF
               IF ( HASQUA ) THEN
                  CALL PSX_MALLOC( DIM1( I )*VAL__NBUB, IPWQ1, STATUS )
                  CALL PSX_MALLOC( DIM2( I )*VAL__NBUB, IPWQ2, STATUS )
               END IF

*  Do the resampling along the current dimension.
               IF ( ITYPE .EQ. '_BYTE' ) THEN
                  CALL KPS1_RS1B( NDIM, I, DIM1, DIM2, INTERP, PARAMS,
     :                            HASVAR, HASQUA, HASAXI, 
     :                            %VAL( CNF_PVAL( IPDAT1 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR1 ) ),
     :                            %VAL( CNF_PVAL( IPQUA1 ) ), 
     :                            %VAL( CNF_PVAL( IPAXI ) ), BAD,
     :                            %VAL( CNF_PVAL( IPWD1 ) ), 
     :                            %VAL( CNF_PVAL( IPWV1 ) ),
     :                            %VAL( CNF_PVAL( IPWQ1 ) ), 
     :                            %VAL( CNF_PVAL( IPWD2 ) ),
     :                            %VAL( CNF_PVAL( IPWV2 ) ), 
     :                            %VAL( CNF_PVAL( IPWQ2 ) ),
     :                            %VAL( CNF_PVAL( IPDAT2 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR2 ) ),
     :                            %VAL( CNF_PVAL( IPQUA2 ) ), 
     :                            %VAL( CNF_PVAL( IPAXO ) ),
     :                            STATUS )

               ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                  CALL KPS1_RS1UB( NDIM, I, DIM1, DIM2, INTERP, PARAMS,
     :                            HASVAR, HASQUA, HASAXI, 
     :                            %VAL( CNF_PVAL( IPDAT1 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR1 ) ),
     :                            %VAL( CNF_PVAL( IPQUA1 ) ), 
     :                            %VAL( CNF_PVAL( IPAXI ) ), BAD,
     :                            %VAL( CNF_PVAL( IPWD1 ) ), 
     :                            %VAL( CNF_PVAL( IPWV1 ) ),
     :                            %VAL( CNF_PVAL( IPWQ1 ) ), 
     :                            %VAL( CNF_PVAL( IPWD2 ) ),
     :                            %VAL( CNF_PVAL( IPWV2 ) ), 
     :                            %VAL( CNF_PVAL( IPWQ2 ) ),
     :                            %VAL( CNF_PVAL( IPDAT2 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR2 ) ),
     :                            %VAL( CNF_PVAL( IPQUA2 ) ), 
     :                            %VAL( CNF_PVAL( IPAXO ) ),
     :                            STATUS )

               ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                  CALL KPS1_RS1W( NDIM, I, DIM1, DIM2, INTERP, PARAMS,
     :                            HASVAR, HASQUA, HASAXI, 
     :                            %VAL( CNF_PVAL( IPDAT1 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR1 ) ),
     :                            %VAL( CNF_PVAL( IPQUA1 ) ), 
     :                            %VAL( CNF_PVAL( IPAXI ) ), BAD,
     :                            %VAL( CNF_PVAL( IPWD1 ) ), 
     :                            %VAL( CNF_PVAL( IPWV1 ) ),
     :                            %VAL( CNF_PVAL( IPWQ1 ) ), 
     :                            %VAL( CNF_PVAL( IPWD2 ) ),
     :                            %VAL( CNF_PVAL( IPWV2 ) ), 
     :                            %VAL( CNF_PVAL( IPWQ2 ) ),
     :                            %VAL( CNF_PVAL( IPDAT2 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR2 ) ),
     :                            %VAL( CNF_PVAL( IPQUA2 ) ), 
     :                            %VAL( CNF_PVAL( IPAXO ) ),
     :                            STATUS )

               ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                  CALL KPS1_RS1UW( NDIM, I, DIM1, DIM2, INTERP, PARAMS,
     :                            HASVAR, HASQUA, HASAXI, 
     :                            %VAL( CNF_PVAL( IPDAT1 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR1 ) ),
     :                            %VAL( CNF_PVAL( IPQUA1 ) ), 
     :                            %VAL( CNF_PVAL( IPAXI ) ), BAD,
     :                            %VAL( CNF_PVAL( IPWD1 ) ), 
     :                            %VAL( CNF_PVAL( IPWV1 ) ),
     :                            %VAL( CNF_PVAL( IPWQ1 ) ), 
     :                            %VAL( CNF_PVAL( IPWD2 ) ),
     :                            %VAL( CNF_PVAL( IPWV2 ) ), 
     :                            %VAL( CNF_PVAL( IPWQ2 ) ),
     :                            %VAL( CNF_PVAL( IPDAT2 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR2 ) ),
     :                            %VAL( CNF_PVAL( IPQUA2 ) ), 
     :                            %VAL( CNF_PVAL( IPAXO ) ),
     :                            STATUS )

               ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                  CALL KPS1_RS1I( NDIM, I, DIM1, DIM2, INTERP, PARAMS,
     :                            HASVAR, HASQUA, HASAXI, 
     :                            %VAL( CNF_PVAL( IPDAT1 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR1 ) ),
     :                            %VAL( CNF_PVAL( IPQUA1 ) ), 
     :                            %VAL( CNF_PVAL( IPAXI ) ), BAD,
     :                            %VAL( CNF_PVAL( IPWD1 ) ), 
     :                            %VAL( CNF_PVAL( IPWV1 ) ),
     :                            %VAL( CNF_PVAL( IPWQ1 ) ), 
     :                            %VAL( CNF_PVAL( IPWD2 ) ),
     :                            %VAL( CNF_PVAL( IPWV2 ) ), 
     :                            %VAL( CNF_PVAL( IPWQ2 ) ),
     :                            %VAL( CNF_PVAL( IPDAT2 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR2 ) ),
     :                            %VAL( CNF_PVAL( IPQUA2 ) ), 
     :                            %VAL( CNF_PVAL( IPAXO ) ),
     :                            STATUS )

               ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPS1_RS1R( NDIM, I, DIM1, DIM2, INTERP, PARAMS,
     :                            HASVAR, HASQUA, HASAXI, 
     :                            %VAL( CNF_PVAL( IPDAT1 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR1 ) ),
     :                            %VAL( CNF_PVAL( IPQUA1 ) ), 
     :                            %VAL( CNF_PVAL( IPAXI ) ), BAD,
     :                            %VAL( CNF_PVAL( IPWD1 ) ), 
     :                            %VAL( CNF_PVAL( IPWV1 ) ),
     :                            %VAL( CNF_PVAL( IPWQ1 ) ), 
     :                            %VAL( CNF_PVAL( IPWD2 ) ),
     :                            %VAL( CNF_PVAL( IPWV2 ) ), 
     :                            %VAL( CNF_PVAL( IPWQ2 ) ),
     :                            %VAL( CNF_PVAL( IPDAT2 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR2 ) ),
     :                            %VAL( CNF_PVAL( IPQUA2 ) ), 
     :                            %VAL( CNF_PVAL( IPAXO ) ),
     :                            STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPS1_RS1D( NDIM, I, DIM1, DIM2, INTERP, PARAMS,
     :                            HASVAR, HASQUA, HASAXI, 
     :                            %VAL( CNF_PVAL( IPDAT1 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR1 ) ),
     :                            %VAL( CNF_PVAL( IPQUA1 ) ), 
     :                            %VAL( CNF_PVAL( IPAXI ) ), BAD,
     :                            %VAL( CNF_PVAL( IPWD1 ) ), 
     :                            %VAL( CNF_PVAL( IPWV1 ) ),
     :                            %VAL( CNF_PVAL( IPWQ1 ) ), 
     :                            %VAL( CNF_PVAL( IPWD2 ) ),
     :                            %VAL( CNF_PVAL( IPWV2 ) ), 
     :                            %VAL( CNF_PVAL( IPWQ2 ) ),
     :                            %VAL( CNF_PVAL( IPDAT2 ) ), 
     :                            %VAL( CNF_PVAL( IPVAR2 ) ),
     :                            %VAL( CNF_PVAL( IPQUA2 ) ), 
     :                            %VAL( CNF_PVAL( IPAXO ) ),
     :                            STATUS )
               END IF

*  Free the temporary workspace.
               CALL PSX_FREE( IPWD1, STATUS )
               CALL PSX_FREE( IPWD2, STATUS )
               IF ( HASVAR ) THEN
                  CALL PSX_FREE( IPWV1, STATUS )
                  CALL PSX_FREE( IPWV2, STATUS )
               END IF
               IF ( HASQUA ) THEN
                  CALL PSX_FREE( IPWQ1, STATUS )
                  CALL PSX_FREE( IPWQ2, STATUS )
               END IF

*  Release the input array, which may be the original mapped NDF component
*  or intermediately allocated workspace.
               IF ( IPDAT1 .EQ. IPDATI ) THEN
                  CALL NDF_UNMAP( NDFI, 'DATA', STATUS )
                  IF ( HASVAR ) THEN
                     CALL NDF_UNMAP( NDFI, 'VARIANCE', STATUS )
                  END IF
                  IF ( HASQUA ) THEN
                     CALL NDF_UNMAP( NDFI, 'QUALITY', STATUS )
                  END IF
               ELSE
                  CALL PSX_FREE( IPDAT1, STATUS )
                  IF ( HASVAR ) THEN
                     CALL PSX_FREE( IPVAR1, STATUS )
                  END IF
                  IF ( HASQUA ) THEN
                     CALL PSX_FREE( IPQUA1, STATUS )
                  END IF
               END IF

               IF ( HASAXI ) THEN 
                  CALL NDF_AUNMP( NDFI, 'Centre', I, STATUS ) 
                  CALL NDF_AUNMP( NDFO, 'Centre', I, STATUS ) 
               END IF

*  The output values for array shapes and pointers for this iteration 
*  become input values for the next iteration.
               DIM1( I ) = DIM2( I )
               IPDAT1 = IPDAT2
               IPVAR1 = IPVAR2
               IPQUA1 = IPQUA2
            END IF
         END DO

*  Update the bad pixel flag for the mapped output arrays.
         CALL NDF_SBAD( BAD, NDFO, 'DATA', STATUS )
         IF ( HASVAR ) THEN
            CALL NDF_SBAD( BAD, NDFO, 'VARIANCE', STATUS )
         END IF

*  Copy data directly between arrays.
*  ==================================

*  If no resampling had to be done (this includes the case where 
*  LBOUND and UBOUND are changed by the same amount) then we just need
*  to copy the data from the input arrays to the output arrays.
      ELSE

*  Map the input and output data arrays.
         CALL NDF_MAP( NDFI, 'DATA', ITYPE, 'READ', IPDATI, EL, 
     :                 STATUS )
         CALL NDF_MAP( NDFO, 'DATA', ITYPE, 'WRITE', IPDATO, EL,
     :                 STATUS )

*  Copy the data array across.
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_CPNDB( 1, 1, EL, %VAL( CNF_PVAL( IPDATI ) ), 
     :                       1, EL,
     :                       %VAL( CNF_PVAL( IPDATO ) ), EL2, STATUS )
         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_CPNDUB( 1, 1, EL, %VAL( CNF_PVAL( IPDATI ) ), 
     :                        1, EL,
     :                        %VAL( CNF_PVAL( IPDATO ) ), EL2, STATUS )
         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_CPNDW( 1, 1, EL, %VAL( CNF_PVAL( IPDATI ) ), 
     :                       1, EL,
     :                       %VAL( CNF_PVAL( IPDATO ) ), EL2, STATUS )
         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_CPNDUW( 1, 1, EL, %VAL( CNF_PVAL( IPDATI ) ), 
     :                        1, EL,
     :                        %VAL( CNF_PVAL( IPDATO ) ), EL2, STATUS )
         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_CPNDI( 1, 1, EL, %VAL( CNF_PVAL( IPDATI ) ), 
     :                       1, EL,
     :                       %VAL( CNF_PVAL( IPDATO ) ), EL2, STATUS )
         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_CPNDR( 1, 1, EL, %VAL( CNF_PVAL( IPDATI ) ), 
     :                       1, EL,
     :                       %VAL( CNF_PVAL( IPDATO ) ), EL2, STATUS )
         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_CPNDD( 1, 1, EL, %VAL( CNF_PVAL( IPDATI ) ), 
     :                       1, EL,
     :                       %VAL( CNF_PVAL( IPDATO ) ), EL2, STATUS )
         END IF

*  Unmap the data arrays.
         CALL NDF_UNMAP( NDFI, 'DATA', STATUS )
         CALL NDF_UNMAP( NDFO, 'DATA', STATUS )

*  Map the input and output variance arrays.
         IF ( HASVAR ) THEN
            CALL NDF_MAP( NDFI, 'VARIANCE', ITYPE, 'READ', IPVARI, EL,
     :                    STATUS )
            CALL NDF_MAP( NDFO, 'VARIANCE', ITYPE, 'WRITE', IPVARO, EL,
     :                    STATUS )

*  Copy the variance array across.
            IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL KPG1_CPNDB( 1, 1, EL, %VAL( CNF_PVAL( IPVARI ) ), 
     :                          1, EL,
     :                          %VAL( CNF_PVAL( IPVARO ) ), 
     :                          EL2, STATUS )
            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               CALL KPG1_CPNDUB( 1, 1, EL, %VAL( CNF_PVAL( IPVARI ) ), 
     :                           1, EL,
     :                           %VAL( CNF_PVAL( IPVARO ) ), 
     :                           EL2, STATUS )
            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL KPG1_CPNDW( 1, 1, EL, %VAL( CNF_PVAL( IPVARI ) ), 
     :                          1, EL,
     :                          %VAL( CNF_PVAL( IPVARO ) ), 
     :                          EL2, STATUS )
            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               CALL KPG1_CPNDUW( 1, 1, EL, %VAL( CNF_PVAL( IPVARI ) ), 
     :                           1, EL,
     :                           %VAL( CNF_PVAL( IPVARO ) ), 
     :                           EL2, STATUS )
            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPG1_CPNDI( 1, 1, EL, %VAL( CNF_PVAL( IPVARI ) ), 
     :                          1, EL,
     :                          %VAL( CNF_PVAL( IPVARO ) ), 
     :                          EL2, STATUS )
            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_CPNDR( 1, 1, EL, %VAL( CNF_PVAL( IPVARI ) ), 
     :                          1, EL,
     :                          %VAL( CNF_PVAL( IPVARO ) ), 
     :                          EL2, STATUS )
            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_CPNDD( 1, 1, EL, %VAL( CNF_PVAL( IPVARI ) ), 
     :                          1, EL,
     :                          %VAL( CNF_PVAL( IPVARO ) ), 
     :                          EL2, STATUS )
            END IF

*  Unmap the variance arrays.
            CALL NDF_UNMAP( NDFI, 'VARIANCE', STATUS )
            CALL NDF_UNMAP( NDFO, 'VARIANCE', STATUS )
         END IF

*  Map the input and output quality arrays.
         IF ( HASQUA ) THEN
            CALL NDF_MAP( NDFI, 'QUALITY', '_UBYTE', 'READ', IPQUAI, EL,
     :                    STATUS )
            CALL NDF_MAP( NDFO, 'QUALITY', '_UBYTE', 'WRITE', IPQUAO, 
     :                    EL, STATUS )

*  Copy the quality array across.
            CALL KPG1_CPNDUB( 1, 1, EL, %VAL( CNF_PVAL( IPQUAI ) ), 
     :                        1, EL,
     :                        %VAL( CNF_PVAL( IPQUAO ) ), EL2, STATUS )

*  Unmap the quality arrays.
            CALL NDF_UNMAP( NDFI, 'QUALITY', STATUS )
            CALL NDF_UNMAP( NDFO, 'QUALITY', STATUS )
         END IF

      END IF

*  Come here if something has gone wrong.
  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Report a contextual message if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SQORST_ERR2', 
     :                 'SQORST: Unable to squash/stretch NDF.', STATUS )
      END IF

      END
