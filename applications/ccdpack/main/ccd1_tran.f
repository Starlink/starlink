      SUBROUTINE CCD1_TRAN( NDF, NNDF, NIN, INGRP, CFRAME, FRCUR,
     :                      NVAR, GETS, GETZ, SCALE, ZERO,
     :                      STATUS )

*+
*  Name:
*     CCD1_TRAN

*  Purpose:
*     Calculates SCALE and ZERO for the DRIZZLE A-task

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_TRAN( STATUS )

*  Description:
*     Cut down version of TRANNDF (does nearest neighbour only). It produces
*     a bunch of NDFs which are then run though the oberlap code, ripped
*     directly from MAKEMOS, which calculates the SCALE and ZERo point
*     corrections for the input frames. The transformed frames are then
*     deleted.

*  Arguments:
*     NDF( CCD1__MXNDF + 1 ) = INTEGER (Given)
*        Array of input NDF identifiers
*     NNDF = INTEGER (Given)
*        Number of NDFs supplied
*     NIN = INTEGER (Given)
*        Number of input NDFs
*     INGRP = INTEGER (Given)
*        ID for group of input NDFs
*     CFRAME( CCD1__MXNDF + 1 ) = INTEGER (Given)
*        Index value of the current AST Frame
*     FRCUR( CCD1__MXNDF + 1 ) = INTEGER (Given)
*        Pointer to the Current AST Frame
*     NVAR = INTEGER (Given)
*        Number of input variance arrays
*     GETS = LOGICAL (Given)
*        Make scale factor corrections?
*     GETZ = LOGICAL (Given)
*        Make zero point corrections?
*     SCALE( CCD1__MXNDF + 1 ) = DOUBLE PRECISION (Returned)
*        Scale factor correction
*     ZERO( CCD1__MXNDF + 1 ) =  DOUBLE PRECISION (Returned)
*        Zero point correction
*     STATUS =    INTEGER (Given & Returned)
*        Global status

*  Copyright:
*     Copyright (C) 1999-2000 Central Laboratory of the Research
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AA: Alasdair Allan (STARLINK, Keele University)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     08-SEP-1999 (AA):
*        Original version based on TRANNDF and MAKEMOS code.
*     9-NOV-1999 (MBT):
*        Modified warnings.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'CCD1_PAR'         ! General CCDPACK constants
      INCLUDE 'CCD1_MOSPR'       ! Constants specific to MAKEMOS & DRIZZLE
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'AST_PAR'          ! AST parameters

*  Global Variables
      INCLUDE 'CCD1_MOSCM'       ! Global variables for MAKEMOS & DRIZZLE
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INGRP             ! ID for group of input NDFs
      INTEGER NNDF              ! total number of NDFs
      INTEGER NIN               ! number of input NDFs
      INTEGER NVAR              ! number of NDFs with variances

      INTEGER NDF( NNDF )       ! pointers to the NDFs
      INTEGER CFRAME( NNDF )    ! index of the current AST frame
      INTEGER FRCUR( NNDF )     ! pointer to the current AST frame

      LOGICAL GETS              ! Do scaling?
      LOGICAL GETZ              ! Do zero-point correction?

*  Arguments Returned:
      DOUBLE PRECISION SCALE( CCD1__MXNDF + 1 )  ! Scale factor correction
      DOUBLE PRECISION ZERO( CCD1__MXNDF + 1 )   ! Zero point correction

*  Status:
      INTEGER STATUS              ! Global status

*  Local constants
      INTEGER NACOMP             ! Number NDF array components
      PARAMETER ( NACOMP = 3 )

*  Local Variables:
      CHARACTER * ( 8 ) COMP( NACOMP ) ! NDF array component names
      CHARACTER * ( AST__SZCHR ) DMN ! Domain of frame
      CHARACTER * ( 9 ) EBUFS    ! Buffer for formatting scale error
      CHARACTER * ( 9 ) EBUFZ    ! Buffer for formatting zero error
      CHARACTER * ( 1 ) REFFLG   ! Character to flag reference NDF
      CHARACTER * ( DAT__SZTYP ) ITYPE ! NDF array implementation type
      CHARACTER * ( 80 ) TXT     ! Output text buffer

      INTEGER ADIMS( NDF__MXDIM )  ! number of elements in axis to be pasted
      INTEGER AEL( NDF__MXDIM )    ! axis-array dimensions
      INTEGER AXOFFS( NDF__MXDIM ) ! offset in concatenated axis vector
      INTEGER AXPNTR( NDF__MXDIM ) ! pointers to the axis centre arrays
      INTEGER CADIMS( NDF__MXDIM ) ! no. of elements in concatenated axis vector
      INTEGER CAXPTR               ! pointer to the concatenated axes
      INTEGER EL                   ! Number of elements in higher dims
      INTEGER ELIN                 ! Number of elements in input array
      INTEGER ELOUT                ! Number of elements in output array
      INTEGER FRM                  ! AST pointer to frame under consideration
      INTEGER I                    ! loop counter for transform loop
      INTEGER IAXIS                ! loop counter through the axes
      INTEGER ICOMP              ! Loop counter through array components
      INTEGER IDIMS( NDF__MXDIM )  ! dimensions of the input NDF
      INTEGER IGNORE             ! I/O error status (ignored)
      INTEGER INDID                ! Workspace identifier
      INTEGER INPNTR               ! Pointer to nearest-neighbour list
      INTEGER IPNTR( 2 )           ! Pointers to the input arrays
      INTEGER ILBND( NDF__MXDIM )  ! lower bounds of the input NDF
      INTEGER IUBND( NDF__MXDIM )  ! upper bounds of the input NDF
      INTEGER IWCS                 ! pointer to the input NDF WCS component
      INTEGER J                    ! just another loop counter (output dim)
      INTEGER JPIX                 ! pointer to the PIXEL frame
      INTEGER K                    ! loop counter around the FrameSet
      INTEGER LBND( NDF__MXDIM , CCD1__MXNDF ) ! NDF lower bounds
      INTEGER LBNDX( NDF__MXDIM ) ! Minimum (overall) lower bound
      INTEGER LW                   ! Size of workspace
      INTEGER MAPCUR               ! pointer to the current mapping
      INTEGER MAXIT                ! Maximum number of iterations
      INTEGER NBAD                 ! Number of bad elements in output array
      INTEGER NCS                ! No. characters in scale factor error
      INTEGER NCZ                ! No. characters in zero point error
      INTEGER NCMP               ! Number of inter-comparisons
      INTEGER NCMP0              ! Total number of NDF overlaps
      INTEGER NCMP1              ! No. of successful inter-comparisons
      INTEGER NDIM                 ! Number of NDF dimensions
      INTEGER NDIMI                ! number of dimensions in input NDF
      INTEGER NDIMS                ! No. of significant output dimensions
      INTEGER NDIMX                ! Maximum (overall) no. of dimensions
      INTEGER NFRM                 ! numebr of frames in the FrameSet
      INTEGER NMAX                 ! Maximum possible dimension of workspace
      INTEGER NPIX( CCD1__MXCMP )  ! Number of intersecting pixels
      INTEGER NVIN                 ! number of input variables in the map
      INTEGER NVOUT                ! number of output variables in the map
      INTEGER ODIMS( NDF__MXDIM )  ! Dimensions of the output NDF
      INTEGER OPNTR( 2 )           ! Pointers to the output arrays
      INTEGER OPNTRW               ! Pointer to the workspace for flux conserv
      INTEGER OPTOV                ! Optimum number of NDF overlaps
      INTEGER OLBND( NDF__MXDIM )  ! Lower bounds of the output NDF
      INTEGER OUBND( NDF__MXDIM )  ! Upper bounds of the output NDF
      INTEGER OUTGRP               ! ID for the output group
      INTEGER OUT( NNDF )          ! pointers to the temporary output NDFs
      INTEGER UBND( NDF__MXDIM, CCD1__MXNDF ) ! NDF upper bounds
      INTEGER UBNDX( NDF__MXDIM )  ! Maximum (overall) upper bound
      INTEGER WDIMS( 2 )           ! Workspace dimensions
      INTEGER WPNTR1               ! Pointer to coordinate workspace
      INTEGER WPNTR2               ! Pointer to indices workspace
      INTEGER WPNTR3               ! Pointer to coordinate workspace
      INTEGER WRK1                 ! Workspace pointer
      INTEGER WRK2                 ! Workspace pointer
      INTEGER WRK3                 ! Workspace pointer
      INTEGER WRK4                 ! Workspace pointer
      INTEGER WRK5                 ! Workspace pointer

      DOUBLE PRECISION AEND( NDF__MXDIM )   ! End co-ord of each axis
      DOUBLE PRECISION ASTART( NDF__MXDIM ) ! Start co-ord of each axis
      DOUBLE PRECISION DDLBND( NDF__MXDIM ) ! Data coord lower bnds of out NDF
      DOUBLE PRECISION DDUBND( NDF__MXDIM ) ! Data coord upper bnds of out NDF
      DOUBLE PRECISION DDXL( NDF__MXDIM ) ! Co-ord of input pnt gives lower bnd
      DOUBLE PRECISION DDXU( NDF__MXDIM ) ! Co-ord of input pnt gives upper bnd
      DOUBLE PRECISION DSCALE( CCD1__MXNDF + 1 ) ! Scale factor error
      DOUBLE PRECISION DZERO( CCD1__MXNDF + 1 ) ! Zero point error
      DOUBLE PRECISION FLUX               ! Flux conservation factor
      DOUBLE PRECISION ORIGIN( CCD1__MXNDF + 1 ) ! False origin value


      REAL TOLS                  ! Fraction scale factor tolerance
      REAL TOLZ                  ! Zero point tolerance
      REAL SKYSUP                ! Sky noise suppression factor

      LOGICAL CMPVAR      ! Use variance in inter-comparisons?
      LOGICAL CONSRV      ! If true, the flux will be altered
      LOGICAL THERE       ! If true, the array component is present in the NDF
      LOGICAL VAR         ! Variance array present?
      LOGICAL DOITER      ! Do we have to do iterations?

*  Local Data:
      DATA COMP / 'Data', 'Variance', 'Quality' /

*  External references
      EXTERNAL VAL_DTOI      ! INTEGER to DOUBLE with exception checking
      INTEGER VAL_DTOI       ! DBLE to nearest INTEGER with exception checking
      INTEGER CHR_LEN        ! Significant length of a string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  And get the names of the corresponding output NDFs.
      CALL CCD1_NDFPG( 'TMPFILE', INGRP, NNDF, OUTGRP, STATUS )

*  Start the routine
      CALL CCD1_MSG( ' ',  ' ', STATUS )
      CALL CCD1_MSG( ' ',
     :   '    NDF remapping for intercomparison:', STATUS )
      CALL CCD1_MSG( ' ',
     :   '    ---------------------------------', STATUS )
      CALL CCD1_MSG( ' ',
     :   '    Data resampling method: Nearest neighbour', STATUS )

*=======================================================================
*  1st Main Code Block -- Transform the input NDFs
*=======================================================================
      DO 1 I = 1, NIN

*  Write out name of this NDF. And which loop this is.
         CALL CCD1_MSG( ' ',  ' ', STATUS )
         CALL NDF_MSG( 'CURRENT_NDF', NDF( I ) )
         CALL CCD1_MSG( ' ', '    +++ Processing NDF: ^CURRENT_NDF',
     :                  STATUS )
         CALL MSG_SETI( 'CURRENT_NUM', I )
         CALL MSG_SETI( 'MAX_NUM', NIN )
         CALL CCD1_MSG( ' ', '    (Number ^CURRENT_NUM of ^MAX_NUM)',
     :                  STATUS )
         CALL CCD1_MSG( ' ',  ' ', STATUS )

*  Get pointer to the NDF
         CALL CCD1_GTWCS( NDF( I ), IWCS, STATUS )

*  Lets find out which frame contains the PIXEL domain (its going to be
*  frame 2, but we may as well do it properly) in the output WCS frameset.
         NFRM = AST_GETI( IWCS, 'Nframe', STATUS )
         DO K = 1, NFRM
            FRM = AST_GETFRAME( IWCS, K, STATUS )
            IF( AST_GETC( FRM, 'Domain', STATUS )
     :          .EQ. 'PIXEL' ) JPIX = K
         END DO

*  Get the current mapping
         MAPCUR = AST_GETMAPPING( IWCS, JPIX, CFRAME( I ), STATUS )
         MAPCUR = AST_SIMPLIFY( MAPCUR, STATUS )

*  Tell the user which co-ordinate frame we'll be resampling into.
         FRM = AST_GETFRAME( IWCS, CFRAME( I ), STATUS )
         DMN = AST_GETC( FRM, 'Domain', STATUS )
         CALL MSG_SETC( 'DMN', DMN )
         CALL CCD1_MSG( ' ', '    Resampling into the ^DMN '
     :                     //'coordinate system', STATUS )

*  If it's neither CCD_REG nor CCD_WCSREG then issue a mild warning.
         IF ( DMN .NE. 'CCD_REG ' .AND. DMN .NE. 'CCD_WCSREG ' ) THEN
            CALL CCD1_MSG( ' ', '      (Warning: this is not a '//
     :      'default CCDPACK registration coordinate system)', STATUS )
         END IF

*  Obtain the number of input and output co-ordinates for a Mapping
         NVIN = AST_GETI( MAPCUR, 'Nin', STATUS )
         NVOUT = AST_GETI( MAPCUR, 'Nout', STATUS )


*  Get the properties of the NDF.
*  ==============================
*  Dimensions.
         CALL NDF_DIM( NDF( I ), NDF__MXDIM, IDIMS, NDIMI, STATUS )

*  Bounds.
         CALL NDF_BOUND( NDF( I ), NDF__MXDIM, ILBND, IUBND, NDIMI,
     :                   STATUS )

         IF ( STATUS .NE. SAI__OK ) GO TO 940

*  Validate dimensions and number of input coordinates.
*  =====================================================
*
*  Check that processing is possible.  The number of dimensions in the
*  NDF must be at least the number of input variables for the
*  transformation to be applied.
         IF ( NVIN .GT. NDIMI .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDF( I ) )
            CALL MSG_SETI( 'NVIN', NVIN )
            CALL MSG_SETI( 'NDIMI', NDIMI )
            CALL ERR_REP( 'DRIZZLE_MISMATCH',
     :     'DRIZZLE: There is a mismatch between the transformation '/
     :     /'which expects ^NVIN variables, and the '/
     :     /'NDF ^NDF, which has only ^NDIMI dimensions.', STATUS )
            GO TO 940
         END IF

*  Define the input coordinate system.
*  ====================================

*  Generate a pseudo axis array to satisfy later calls (which will keep
*  this application as simiar as possible to the KAPPA version).
         DO IAXIS = 1, NVIN

*  Get workspace for the axes.
            AEL( IAXIS ) = IUBND( IAXIS ) - ILBND( IAXIS ) + 1
            CALL CCD1_MALL( AEL( IAXIS ), '_DOUBLE', AXPNTR( IAXIS ),
     :                      STATUS )
            CALL CCG1_AXIND( ILBND( IAXIS ), IUBND( IAXIS ),
     :                       %VAL( CNF_PVAL( AXPNTR( IAXIS ) ) ),
     :                       STATUS )
         END DO

*  Find the coordinate bounds of the output NDF.
*  ==============================================

*  The transformed coordinates will not, in general, be in pixel
*  coordinates, so we shall need to derive and then join a
*  transformation that converts between the output coordinates and
*  pixel coordinates.  The first stage is to estimate the bounds.
*  Axis end-points are pixel coordinates.
         DO IAXIS = 1, NVIN
            ASTART( IAXIS ) = DBLE( ILBND( IAXIS ) )
            AEND( IAXIS ) = DBLE( IUBND( IAXIS ) )
         END DO

*  Use test points at the extremes and midpoints of each axis to obtain
*  an estimate of the extent of the output NDF's coordinates.  This
*  assumes that the transformation does not move the innards of the
*  input array to the outside of the output array.
         DO IAXIS = 1, NVIN
            CALL AST_MAPBOX( MAPCUR, ASTART, AEND, .TRUE., IAXIS,
     :                       DDLBND(IAXIS), DDUBND(IAXIS), DDXL,
     :                       DDXU, STATUS )
         END DO

*  Set the bounds of the temporary output NDF
*  ==========================================

*  Autosize the output NDF.
         DO IAXIS = 1, NVOUT
            OLBND( IAXIS ) = VAL_DTOI( .FALSE.,
     :                       DDLBND( IAXIS ) - 0.5D0, STATUS )
            OUBND( IAXIS ) = VAL_DTOI( .FALSE., DDUBND( IAXIS ),
     :                       STATUS )
         END DO

*  Derive the output NDF's dimensions.
         DO IAXIS = 1, NVOUT
            ODIMS( IAXIS ) = OUBND( IAXIS ) - OLBND( IAXIS ) + 1
         END DO
         IF ( STATUS .NE. SAI__OK ) GO TO 940

*  Create a concatenated input axis array.
*  =======================================
*
*  The subroutines that perform the resampling need the axis arrays
*  to convert coordinates into pixel indices in the input array.
*  For convenience the axis centres are passed in a single vector
*  and required values are found using offsets equal to the sum of
*  the lower axis dimensions.
*
*  Find the length of the concatenated axes.
         CADIMS( 1 ) = 0
         DO IAXIS = 1, NVIN
           CADIMS( 1 ) = CADIMS( 1 ) + AEL( IAXIS )
         END DO

*  Create some workspace for the concatenated array.
         CALL CCD1_MALL( CADIMS( 1 ), '_DOUBLE', CAXPTR, STATUS )

*  Pasting routine needs filled dimension arrays of NDF__MXDIM values.
         DO IAXIS = 2, NDF__MXDIM
            ADIMS( IAXIS ) = 1
            CADIMS( IAXIS ) = 1
            AXOFFS( IAXIS ) = 0
         END DO

         AXOFFS( 1 ) = 0

*  Loop around the axis
         DO IAXIS = 1, NVIN

*  Assign the axis dimension.
            ADIMS( 1 ) = AEL( IAXIS )

*  Paste each axis into the work array.
            CALL KPG1_PASTD( .FALSE., .TRUE., AXOFFS, ADIMS, ADIMS( 1 ),
     :                       %VAL( CNF_PVAL( AXPNTR( IAXIS ) ) ),
     :                       CADIMS,
     :                       CADIMS( 1 ), %VAL( CNF_PVAL( CAXPTR ) ),
     :                       STATUS )

*  Increment the offsets for the next axis.
            AXOFFS( 1 ) = AXOFFS( 1 ) + AEL( IAXIS )
         END DO

*  Tidy the axis centres.
*  ======================
         DO IAXIS = 1, NVIN
            CALL CCD1_MFREE( AXPNTR( IAXIS ), STATUS )
         END DO

*  Create the output NDF.
*  ======================
         CALL NDG_NDFPR( NDF( I ), 'Axis,Units', OUTGRP, I, OUT( I ),
     :                   STATUS )

*  Tell user the name of the output NDF.
         CALL NDF_MSG( 'TMPNDF', OUT( I ) )
         CALL CCD1_MSG( ' ', '    Temporary NDF: ^TMPNDF', STATUS )

*  Format and display the pixel index bounds of the input NDF.
      DO  IAXIS = 1, NVIN
         IF ( IAXIS .NE. 1 ) CALL MSG_SETC( 'BOUNDS', ',' )
         CALL MSG_SETI( 'BOUNDS', ILBND( IAXIS ) )
         CALL MSG_SETC( 'BOUNDS', ':' )
         CALL MSG_SETI( 'BOUNDS', IUBND( IAXIS ) )
      END DO
      CALL CCD1_MSG( ' ',
     :   '    Pixel bounds of input file: (^BOUNDS)', STATUS )

*  Format and display the pixel index bounds of the output NDF.
      DO  IAXIS = 1, NVOUT
         IF ( IAXIS .NE. 1 ) CALL MSG_SETC( 'BOUNDS', ',' )
         CALL MSG_SETI( 'BOUNDS', OLBND( IAXIS ) )
         CALL MSG_SETC( 'BOUNDS', ':' )
         CALL MSG_SETI( 'BOUNDS', OUBND( IAXIS ) )
      END DO
      CALL CCD1_MSG( ' ',
     :   '    Pixel bounds of output mosaic: (^BOUNDS)', STATUS )

*  Change its shape to the required output shape.
      CALL NDF_SBND( NDIMI, OLBND, OUBND, OUT( I ), STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 940

*  No flux conservation.
      FLUX = 1.0D0
      CONSRV = .FALSE.
      CALL CCD1_MSG( ' ', '    Flux is not conserved', STATUS )

*  Resample using the nearest-neighbour technique.
*  ===============================================

*  This method is applicable to the data, variance and quality arrays.
*  First determine the vector indices of the nearest neighbours applying
*  the transformation.  To do this we need some work space for the
*  input and output coordinates, and input fractional indices.
            WDIMS( 1 ) = ODIMS( 1 )
            WDIMS( 2 ) = NDIMI
            CALL CCD1_MALL( WDIMS( 1 ) * WDIMS( 2 ), '_DOUBLE', WPNTR1,
     :                      STATUS )
            CALL CCD1_MALL( WDIMS( 1 ) * WDIMS( 2 ), '_DOUBLE', WPNTR2,
     :                      STATUS )
            WDIMS( 2 ) = NDIMI
            CALL CCD1_MALL( WDIMS( 1 ) * WDIMS( 2 ), '_DOUBLE', WPNTR3,
     :                      STATUS )

*  Get workspace to hold the indices of the nearest neighbours. There
*  is one for each output array element.
            EL = 1
            DO J = 1, NVOUT
               EL = EL * ODIMS( J )
            END DO
            CALL CCD1_MKTMP( EL, '_INTEGER', INDID, STATUS )
            CALL CCD1_MPTMP( INDID, 'WRITE', INPNTR, STATUS )

*  Generate the list of vector indices for the resampling.
            CALL CCG1_ASPID( NDIMI, IDIMS, MAPCUR,
     :                       %VAL( CNF_PVAL( CAXPTR ) ),
     :                       WDIMS( 1 ), NVOUT, OLBND, ODIMS,
     :                       %VAL( CNF_PVAL( WPNTR1 ) ),
     :                       %VAL( CNF_PVAL( WPNTR3 ) ),
     :                       %VAL( CNF_PVAL( WPNTR2 ) ),
     :                       %VAL( CNF_PVAL( INPNTR ) ),
     :                       STATUS )

*  Free the workspace that is no longer needed.
            CALL CCD1_MFREE( WPNTR1, STATUS )
            CALL CCD1_MFREE( WPNTR2, STATUS )
            CALL CCD1_MFREE( WPNTR3, STATUS )

*  Apply the transformation with n-n resampling.
*  =============================================
*  Loop through all the components.
            DO ICOMP = 1, NACOMP

*  See if the component is present.
               IF ( COMP( ICOMP ) .NE. 'Data' ) THEN
                  CALL NDF_STATE( NDF(I), COMP( ICOMP ), THERE, STATUS )
               ELSE
                  THERE = .TRUE.
               END IF

*  Can only process when the array component is present.
               IF ( THERE ) THEN

*  Get the type of the array.
                  CALL NDF_TYPE( NDF(I), COMP( ICOMP ), ITYPE, STATUS )

*  Map the input and output arrays.
                  CALL NDF_MAP( NDF(I), COMP( ICOMP ), ITYPE, 'READ',
     :                          IPNTR, ELIN, STATUS )
                  CALL NDF_MAP( OUT(I), COMP( ICOMP ), ITYPE,
     :                          'WRITE/BAD', OPNTR, ELOUT, STATUS )

*  No flux conservation (we don't care) so use the same pointer and
*  save some workspace. See TRANNDF (line 1099) for details of what
*  happens when we do care about flux conservation.
                  OPNTRW = OPNTR( 1 )

*  Perform the transformation on the data array for the numeric data
*  type.  First for a byte array
                  IF ( ITYPE .EQ. '_BYTE' ) THEN
                     CALL KPG1_VASVB( ELOUT, %VAL( CNF_PVAL( INPNTR ) ),
     :                                ELIN,
     :                                %VAL( CNF_PVAL( IPNTR( 1 ) ) ),
     :                                %VAL( CNF_PVAL( OPNTRW ) ),
     :                                NBAD, STATUS )

*  Transform a double-precision array.
                  ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPG1_VASVD( ELOUT, %VAL( CNF_PVAL( INPNTR ) ),
     :                                ELIN,
     :                                %VAL( CNF_PVAL( IPNTR( 1 ) ) ),
     :                                %VAL( CNF_PVAL( OPNTRW ) ),
     :                                NBAD, STATUS )

*  Transform an integer array.
                  ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                     CALL KPG1_VASVI( ELOUT, %VAL( CNF_PVAL( INPNTR ) ),
     :                                ELIN,
     :                                %VAL( CNF_PVAL( IPNTR( 1 ) ) ),
     :                                %VAL( CNF_PVAL( OPNTRW ) ),
     :                                NBAD, STATUS )

*  Transform a single-precision array.
                  ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                     CALL KPG1_VASVR( ELOUT, %VAL( CNF_PVAL( INPNTR ) ),
     :                                ELIN,
     :                                %VAL( CNF_PVAL( IPNTR( 1 ) ) ),
     :                                %VAL( CNF_PVAL( OPNTRW ) ),
     :                                NBAD, STATUS )

*  Transform an unsigned-byte array.
                  ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                     CALL KPG1_VASVUB( ELOUT,
     :                                 %VAL( CNF_PVAL( INPNTR ) ), ELIN,
     :                                 %VAL( CNF_PVAL( IPNTR( 1 ) ) ),
     :                                 %VAL( CNF_PVAL( OPNTRW ) ),
     :                                 NBAD, STATUS )

*  Transform an unsigned-word array.
                  ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                     CALL KPG1_VASVUW( ELOUT,
     :                                 %VAL( CNF_PVAL( INPNTR ) ), ELIN,
     :                                 %VAL( CNF_PVAL( IPNTR( 1 ) ) ),
     :                                 %VAL( CNF_PVAL( OPNTRW ) ),
     :                                 NBAD, STATUS )

*  Transform a word array.
                  ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                     CALL KPG1_VASVW( ELOUT, %VAL( CNF_PVAL( INPNTR ) ),
     :                                ELIN,
     :                                %VAL( CNF_PVAL( IPNTR( 1 ) ) ),
     :                                %VAL( CNF_PVAL( OPNTRW ) ),
     :                                NBAD, STATUS )
                  END IF

*  Unmap the output array.
                  CALL NDF_UNMAP( OUT( I ), COMP( ICOMP ), STATUS )

*  Unmap the input NDF array.
                  CALL NDF_UNMAP( NDF( I ), COMP( ICOMP ), STATUS )
               END IF
            END DO

*  Free the workspace holding the resampled vector indices.
            CALL CCD1_FRTMP( INDID, STATUS )

*  Free concatenated axes workspace.
         CALL CCD1_MFREE( CAXPTR, STATUS )

*=======================================================================
*  End of 1st Main Code Block
*=======================================================================
1     CONTINUE
      CALL CCD1_MSG( ' ',  ' ', STATUS )

*  We have now created a bunch of temporary NDFs, OUT(*), which have been
*  rotated, or squashed, or whatever. We now need to calculate the overlaps
*  between them.

*=======================================================================
*  2nd Main Code Block  -- Calculate overlaps and SCALE and ZERO
*=======================================================================

*  Loop through the NDFs
      DO 3 I = 1, NIN

*  Obtain the NDF bounds.
         CALL NDF_BOUND( OUT( I ), NDF__MXDIM, LBND( 1, I ),
     :                   UBND( 1, I ), NDIM, STATUS )

*  Test to see whether the NDF contains variance information and count
*  the number which do.
         CALL NDF_STATE( OUT( I ), 'Variance', VAR, STATUS )
         IF ( VAR ) NVAR = NVAR + 1

*  Accumulate the minimum lower bound value, the maximum upper bound
*  value and the maximum number of dimensions. These determine the
*  shape of the output mosaic.
         DO 2 IAXIS = 1, NDF__MXDIM
            IF ( I .EQ. 1 ) THEN
               LBNDX( IAXIS ) = LBND( IAXIS, I )
               UBNDX( IAXIS ) = UBND( IAXIS, I )
               NDIMX = NDIM
            ELSE
               LBNDX( IAXIS ) = MIN( LBNDX( IAXIS ), LBND( IAXIS, I ) )
               UBNDX( IAXIS ) = MAX( UBNDX( IAXIS ), UBND( IAXIS, I ) )
               NDIMX = MAX( NDIMX, NDIM )
            END IF
 2       CONTINUE
         IF ( STATUS .NE. SAI__OK ) GO TO 940
 3    CONTINUE

      VAR = .FALSE.
      IF ( CCD1_IREF .NE. 0 ) CALL NDF_STATE( NDF( CCD1_IREF ),
     :                                   'Variance', VAR, STATUS )

*  If at least two of the NDFs supplied (including an additional
*  reference NDF, if given) contain variance information, then see if
*  variance values should be used when inter-comparing NDFs. Otherwise,
*  there is no possibility of using variance information.
      IF ( ( NVAR .GE. 2 ) .OR.
     :        ( ( NVAR .EQ. 1 ) .AND. VAR ) ) THEN
         CALL PAR_GET0L( 'CMPVAR', CMPVAR, STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) GO TO 940

*  If inter-comparisons will be made, then obtain the optimum number of
*  overlaps to be used per input NDF.
      CALL PAR_GDR0I( 'OPTOV', 3, 1, CCD1__MXNDF, .FALSE., OPTOV,
     :                   STATUS )

*  See if any inter-comparisons to be made between the input NDFs may
*  involve iteration. If so, then obtain the required accuracy
*  tolerances and the maximum number of iterations to allow.
      DOITER = ( GETS .AND. ( GETZ .OR. CMPVAR ) )
      IF ( DOITER ) THEN
         IF ( GETS ) CALL PAR_GDR0R( 'TOLS', 0.001, 0.0, NUM__MAXR,
     :                               .FALSE., TOLS, STATUS )
         IF ( GETZ ) CALL PAR_GDR0R( 'TOLZ', 0.05, 0.0, NUM__MAXR,
     :                               .FALSE., TOLZ, STATUS )
         CALL PAR_GDR0I( 'MAXIT', 20, 1, NUM__MAXI, .FALSE., MAXIT,
     :                   STATUS )
      END IF

*  If scale factor adjustments will be made, then obtain the sky noise
*  suppression factor, suggesting a suitable default based on the
*  number of output dimensions with a pixel index range exceeding
*  unity.
      IF ( GETS ) THEN
         NDIMS = 0
         DO IAXIS = 1, NDIMX
            IF ( UBNDX( IAXIS ) .GT. LBNDX( IAXIS ) ) THEN
               NDIMS = NDIMS + 1
            ENDIF
         END DO
         CALL PAR_GDR0R( 'SKYSUP', 10.0 ** ( 0.5 * NDIMS ), 0.0,
     :                   NUM__MAXR, .FALSE., SKYSUP, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 940

*  Determine which input NDFs to inter-compare.
*  ===========================================
*  If corrections are to be made, then allocate workspace and obtain a
*  list of pair-wise overlaps between the input NDFs.  Include an
*  additional reference NDF, if provided.
      CALL PSX_CALLOC( NNDF, '_INTEGER', WRK1, STATUS )
      CALL CCD1_GTCMP( NNDF, OUT, CCD1_IPAIR, NPIX, NCMP,
     :                    %VAL( CNF_PVAL( WRK1 ) ), STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Allocate further workspace and prune the list of overlaps to remove
*  unnecessary multiple overlaps. Deallocate the workspace when done.
         NCMP0 = NCMP
         CALL PSX_CALLOC( NCMP0, '_INTEGER', WRK2, STATUS )
         CALL CCD1_PRUNE( OPTOV, NCMP0, CCD1_IPAIR, NPIX, NNDF,
     :                       %VAL( CNF_PVAL( WRK1 ) ), NCMP,
     :                    %VAL( CNF_PVAL( WRK2 ) ), STATUS )
         CALL PSX_FREE( WRK2, STATUS )
      END IF
      CALL PSX_FREE( WRK1, STATUS )

*  Check that there is at least the minimum number of overlaps
*  required.  If not, then report an error and abort.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NCMP .LT. NNDF - 1 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'NCMP', NCMP )
            CALL MSG_SETI( 'NNDF', NNDF )
            CALL ERR_REP( 'DRIZZLE_OVLAP',
     :                    '^NNDF NDFs have been supplied with ' //
     :                    'only ^NCMP overlap(s) between them; ' //
     :                    'not adequate to determine the ' //
     :                    'required scale-factor/zero-point ' //
     :                    'corrections.', STATUS )
         GOTO 940
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 940

*  Display general information about the inter-comparisons to be made.
*  ==================================================================
*  Display a heading.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ',
     :'   Parameters for NDF inter-comparisons:', STATUS )
      CALL CCD1_MSG( ' ',
     :'   ------------------------------------', STATUS )

*  Show how many inter-comparisons will be made.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL MSG_SETI( 'NCMP0', NCMP0 )
      CALL CCD1_MSG( ' ',
     :'      Number of NDF overlaps available:   ^NCMP0',
     :   STATUS )
      CALL MSG_SETI( 'OPTOV', OPTOV )
      CALL CCD1_MSG( ' ',
     :'      Optimum no. of overlaps per NDF:    ^OPTOV',
     :   STATUS )
      CALL MSG_SETI( 'NCMP', NCMP )
      CALL CCD1_MSG( ' ',
     :'      No. inter-comparisons to be made:   ^NCMP',
     :   STATUS )

*  Display the function being fitted and the parameter(s) being
*  estimated.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      IF ( GETS .AND. GETZ ) THEN
         CALL CCD1_MSG( ' ',
     :'      Function being fitted:              a = b * S + Z',
     :      STATUS )
         CALL CCD1_MSG( ' ',
     :'      Parameters being estimated:         Scale-factor (S)',
     :      STATUS )
         CALL CCD1_MSG( ' ',
     :'                                          Zero-point (Z)',
     :      STATUS )
      ELSE IF ( GETS ) THEN
         CALL CCD1_MSG( ' ',
     :'      Function being fitted:              a = b * S',
     :      STATUS )
         CALL CCD1_MSG( ' ',
     :'      Parameter being estimated:          Scale-factor (S)',
     :      STATUS )
      ELSE IF ( GETZ ) THEN
         CALL CCD1_MSG( ' ',
     :'      Function being fitted:              a = b + Z',
     :       STATUS )
         CALL CCD1_MSG( ' ',
     :'      Parameter being estimated:          Zero-point (Z)',
     :      STATUS )
      END IF

*  If iterations may be needed during the inter-comparison, then show
*  the parameter values controlling the iterations.
      IF ( DOITER ) THEN
         IF ( NVAR .EQ. NNDF ) THEN
            CALL CCD1_MSG( ' ',
     :'      Estimation will be iterative', STATUS )
         ELSE
            CALL CCD1_MSG( ' ',
     :'      Estimation may be iterative', STATUS )
         END IF
         CALL CCD1_MSG( ' ',
     :'         Convergence criteria:', STATUS )
         IF ( GETS ) THEN
            CALL MSG_SETR( 'TOLS', ABS( TOLS ) )
            CALL CCD1_MSG( ' ',
     :'            Scale-factor tolerance:       ^TOLS * S', STATUS )
         END IF
         IF ( GETZ ) THEN
            CALL MSG_SETR( 'TOLZ', ABS( TOLZ ) )
            CALL CCD1_MSG( ' ',
     :'            Zero-point tolerance:         ^TOLZ * S', STATUS )
         END IF
         CALL MSG_SETI( 'MAXIT', MAXIT )
         CALL CCD1_MSG( ' ',
     :'         Maximum number of iterations:    ^MAXIT', STATUS )
      END IF

*  Display the sky noise suppression factor, if appropriate.
      IF ( GETS ) THEN
         IF ( SKYSUP .GT. 0.0 ) THEN
            CALL MSG_SETR( 'SKYSUP', SKYSUP )
         ELSE
            CALL MSG_SETC( 'SKYSUP', 'Not in use' )
         END IF
         CALL CCD1_MSG( ' ',
     :'      Sky noise suppression factor:       ^SKYSUP', STATUS )
      END IF

*  Display the name of the reference NDF, if supplied.
      IF ( CCD1_IREF .NE. 0 ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL NDF_MSG( 'NDFREF', NDF( CCD1_IREF ) )
         IF ( NNDF .GT. NIN ) THEN
            CALL MSG_SETC( 'STATE', 'not an input NDF' )
         ELSE
            CALL MSG_SETC( 'STATE', 'also an input NDF' )
         END IF
         CALL CCD1_MSG( ' ',
     :'      Reference NDF: ^NDFREF (^STATE)', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 940


*  Inter-compare the NDFs in pairs to determine their zero point and/or
*  scale factor differences. Update the number of inter-comparisons to
*  exclude any that failed.
      CALL CCD1_DOCMP( GETS, GETZ, CMPVAR, SKYSUP, TOLS, TOLZ,
     :                 MAXIT, NNDF, OUT, CCD1_IREF, NCMP, CCD1_IPAIR,
     :                 NPIX, CCD1_DIFS, CCD1_DDIFS, CCD1_DIFZ,
     :                 CCD1_DDIFZ, CCD1_ORIG, NCMP1, STATUS )
      NCMP = NCMP1
      IF ( STATUS .NE. SAI__OK ) GO TO 940

*  Find the optimised corrections.
*  ==============================
*  Display a heading.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '   Solving for optimum corrections:',
     :              STATUS )
      CALL CCD1_MSG( ' ', '   -------------------------------',
     :                 STATUS )

*  Display the correction formula and headings for the table of
*  results. The form of this depends on which corrections are being
*  applied.
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Both scale factor and zero point corrections:
      IF ( GETS .AND. GETZ ) THEN
         CALL CCD1_MSG( ' ',
     :'      Correction formula is: new = old * S + Z', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :'                           Scale-Factor    ' //
     :'           Zero-Point', STATUS )
         CALL CCD1_MSG( ' ',
     :'         NDF             Correction S (dS)' //
     :'         Correction Z (dZ)', STATUS )
         CALL CCD1_MSG( ' ',
     :'         ---             -----------------' //
     :'         -----------------', STATUS )

*  Scale factor corrections only:
      ELSE IF ( GETS ) THEN
         CALL CCD1_MSG( ' ',
     :'      Correction formula is: new = old * S', STATUS )
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL CCD1_MSG( ' ',
     :'                                         Scale-Factor', STATUS )
            CALL CCD1_MSG( ' ',
     :'                   NDF                 Correction S (dS)',
     :       STATUS )
            CALL CCD1_MSG( ' ',
     :'                   ---                 -----------------',
     :      STATUS )

*  Zero point corrections only:
      ELSE IF ( GETZ ) THEN
         CALL CCD1_MSG( ' ',
     :'      Correction formula is: new = old + Z', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :'                                       Zero-Point', STATUS )
         CALL CCD1_MSG( ' ',
     :'                   NDF              Correction Z (dZ)',
     :      STATUS )
         CALL CCD1_MSG( ' ',
     :'                   ---              -----------------',
     :      STATUS )
      END IF

*  Allocate workspace.
      IF ( GETS .AND. GETZ ) THEN
         NMAX = MAX( 2 * ( NNDF + 1 ), 2 * ( NCMP + 1 ) )
         CALL PSX_CALLOC( NMAX, '_DOUBLE', WRK1, STATUS )
         LW = 17 * NMAX + 40
         CALL PSX_CALLOC( LW, '_INTEGER', WRK2, STATUS )
         LW = ( NMAX + 2 ) * ( NMAX + 2 )
         CALL PSX_CALLOC( LW, '_DOUBLE', WRK3, STATUS )
         LW = 4 * NMAX * NMAX + 86 * NMAX + 141
         CALL PSX_CALLOC( LW, '_DOUBLE', WRK4, STATUS )
         LW = NMAX * NMAX
         CALL PSX_CALLOC( LW, '_DOUBLE', WRK5, STATUS )
      ELSE
         NMAX = 1
         CALL PSX_CALLOC( 2 * ( NNDF + 1 ), '_DOUBLE', WRK1, STATUS )
      END IF

*  Solve to find globally optimised corrections for each NDF consistent
*  with the inter-comparison results obtained above. Include an
*  additional reference NDF, if provided.
      CALL CCD1_SZSLV( GETS, GETZ, NNDF, NCMP, NMAX, SCALE, DSCALE,
     :                 ZERO, DZERO, ORIGIN, %VAL( CNF_PVAL( WRK1 ) ),
     :                 %VAL( CNF_PVAL( WRK2 ) ),
     :                 %VAL( CNF_PVAL( WRK3 ) ),
     :                 %VAL( CNF_PVAL( WRK4 ) ),
     :                 %VAL( CNF_PVAL( WRK5 ) ), STATUS )

*  Release the workspace.
      CALL PSX_FREE( WRK1, STATUS )
      IF ( GETS .AND. GETZ ) THEN
         CALL PSX_FREE( WRK2, STATUS )
         CALL PSX_FREE( WRK3, STATUS )
         CALL PSX_FREE( WRK4, STATUS )
         CALL PSX_FREE( WRK5, STATUS )
      END IF

*  Loop to display the optimised results. Format the error estimate(s).
      DO 11 I = 1, NNDF
         IF ( GETS ) THEN
            WRITE( EBUFS, '(G9.2)', IOSTAT = IGNORE )
     :         SNGL( DSCALE( I ) )
            CALL CHR_LDBLK( EBUFS )
            NCS = CHR_LEN( EBUFS )
         END IF
         IF ( GETZ ) THEN
            WRITE( EBUFZ, '(G9.2)', IOSTAT = IGNORE )
     :         SNGL( DZERO( I ) )
            CALL CHR_LDBLK( EBUFZ )
            NCZ = CHR_LEN( EBUFZ )
         END IF

*  Note if this is the reference NDF.
         REFFLG = ' '
         IF ( I .EQ. CCD1_IREF ) REFFLG = '*'

*  Display the results using an appropriate format.
         TXT = ' '
         IF ( GETS .AND. GETZ ) THEN
            WRITE( TXT, 1001, IOSTAT = IGNORE ) I, REFFLG,
     :      SNGL( SCALE( I ) ), EBUFS( : NCS ),
     :      SNGL( ZERO( I ) - SCALE( I ) * ORIGIN( I ) ),
     :      EBUFZ( : NCZ )
         ELSE IF ( GETS ) THEN
            WRITE( TXT, 1002, IOSTAT = IGNORE ) I, REFFLG,
     :      SNGL( SCALE( I ) ), EBUFS( : NCS )
         ELSE IF ( GETZ ) THEN
            WRITE( TXT, 1002, IOSTAT = IGNORE ) I, REFFLG,
     :      SNGL( ZERO( I ) - SCALE( I ) * ORIGIN( I ) ),
     :      EBUFZ( : NCZ )
         END IF
         CALL CCD1_MSG( ' ', TXT, STATUS )
 1001    FORMAT( I11, A1, T22, G12.5, ' (', A, ')',
     :                    T51, G12.5, ' (', A, ')' )
 1002    FORMAT( T11, I11, A1, T36, G12.5, ' (', A, ')' )
 11   CONTINUE

*  Add a footnote identifying the reference NDF if necessary.
      IF ( CCD1_IREF .NE. 0 ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :'                              * = Reference NDF', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 940

      DO I = 1, NIN
         ZERO( I ) = ZERO( I ) - SCALE( I ) * ORIGIN( I )
      END DO

*=======================================================================
*  End of 2nd Main Code Block
*=======================================================================

* We've now generated the scaling and zero-point corrections, all that
* remains is to delete the temporary NDFs

      DO I = 1, NIN
         CALL NDF_DELET( OUT( I ), STATUS )
      END DO

*  Jump to here on error
940   CONTINUE
      CALL CCD1_GRDEL( OUTGRP, STATUS )

*  Time at the bar please...
999   END


