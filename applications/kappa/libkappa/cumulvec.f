       SUBROUTINE CUMULVEC( STATUS )
*+
*  Name:
*     CUMULVEC

*  Purpose:
*     Sums the values cumulatively in a one-dimensional NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CUMULVEC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application forms the cumulative sum of the values of a
*     one-dimensional NDF starting from the first to the last element.
*     thus the first output pixel will be unchanged but the second will
*     be the sum of the first two input pixels, third output pixel is the
*     sum of the first three input pixels and so on.  Anomalous values
*     may be excluded from the summation by setting a threshold.

*  Usage:
*     cumulvec in out [thresh]

*  ADAM Parameters:
*     IN = NDF (Read)
*        The one-dimensional NDF containing the vector to be summed.
*     OUT = NDF (Write)
*        The NDF to contain the summed image.
*     THRESH = _DOUBLE (Read)
*        The maximum difference between adjacent elements for the
*        summation to ocur.  For increments outside the allowed
*        range, the increment becomes zero.  If null, !, is given, then
*        there is no limit.  [!]
*     TITLE = LITERAL (Read)
*        The title of the output NDF.  A null (!) value means using the
*        title of the input NDF.  [!]

*  Examples:
*     cumulvec gradient profile
*       The one-dimensional NDF called gradient is summed cumulatively to
*       form NDF profile.
*     cumulvec in=gradient out=profile thresh=20
*       As above but only adjacent values separated by less than 20
*       are included in the summation.

*  Related Applications:
*     KAPPA: HISTOGRAM.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS, and HISTORY components of an
*     NDF data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.  Bad pixels are propagated and excluded from the
*     summation.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single- or double-precision floating point as
*     appropriate.

*  Copyright:
*     Copyright (C) 2006 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2006 October 6 (MJC):
*        Original version.
*     2006 December 18 (MJC):
*        Added support for VARIANCE.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! PRM public constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Dimensionality of input array
      PARAMETER( NDIM = 1 )      ! One-dimensional data

*  Local Variables:
      CHARACTER * ( 13 ) COMP    ! The array component(s) of NDF
      INTEGER DIM( NDF__MXDIM )  ! Size of the image in each dimension
      DOUBLE PRECISION DTHDEF    ! Suggested default threshold for
                                 ! cleaning a d.p. array
      DOUBLE PRECISION DTHRES    ! Threshold for cleaning a d.p. array
                                 ! image
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Numeric type for output arrays
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Numeric type for processing
      INTEGER NDFI               ! NDF Identifier for input image
      INTEGER NDFO               ! NDF Identifier for output image
      INTEGER NDIMS              ! Actual number of the dimension of
                                 ! the image
      INTEGER NEL                ! Number of mapped elements
      INTEGER PNTIN( 2 )         ! Pointer to the inpyut mapped data
      INTEGER PNTOUT( 2 )        ! Pointer to output mapped data
      INTEGER SDIM( NDF__MXDIM ) ! Significant NDF dimensions
      REAL THRDEF                ! Suggested default threshold for
                                 ! cleaning a single-precision array
      REAL THRESH                ! Threshold for cleaning a
                                 ! single-precision array
      LOGICAL VAR                ! NDF contains a variance array?

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the input NDF.
*  =====================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get identifier for the NDF containing the input image.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Find whether or not there are no more than one significant
*  dimension and which ones it is.
      CALL KPG1_SDIMP( NDFI, NDIM, SDIM, STATUS )

*  Exit if an error occurred.  This is needed because the significant
*  dimensions are used as array indices.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine its dimensions (note that only one significant dimension
*  can be accommodated).  Then ignore non-significant dimensions.
      CALL NDF_DIM( NDFI, SDIM( NDIM ), DIM, NDIMS, STATUS )
      DIM( 1 ) = DIM( SDIM( 1 ) )

*  See if the input NDF has a Variance component.
      CALL NDF_STATE( NDFI, 'VARIANCE', VAR, STATUS )

*  Store a list of components to be accessed.
      IF ( VAR ) THEN
         COMP = 'DATA,VARIANCE'
      ELSE
         COMP = 'DATA'
      END IF

*  Determine the numeric type to be used for processing the input
*  arrays.  This application supports single- and double-precision
*  floating point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFI, NDFI, COMP, ITYPE, DTYPE,
     :                STATUS )

*  Create the output NDF and map arrays.
*  =====================================

*  Create a new output NDF to contain the cleaned image, which
*  inheriting all the attributes of the input NDF.  Set an appropriate
*  numeric type for the output arrays.
      CALL LPG_PROP( NDFI, 'WCS,Axis,Units,History,Quality', 'OUT',
     :               NDFO, STATUS )
      CALL NDF_STYPE( DTYPE, NDFO, COMP, STATUS )

*  Map the input and output data arrays.
      CALL KPG1_MAP( NDFI, COMP, ITYPE, 'READ', PNTIN, NEL, STATUS )
      CALL KPG1_MAP( NDFO, COMP, ITYPE, 'WRITE', PNTOUT, NEL, STATUS )

*  Exit if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  If variance is absent, ensure that there a valid pointer for the main
*  processing subroutine.
      IF ( .NOT. VAR ) THEN
         PNTIN( 2 ) = PNTIN( 1 )
         PNTOUT( 2 ) = PNTOUT( 1 )
      END IF

*  Obtain parameters to control the filtering.
*  ===========================================

*  Obtain the range of threshold in the appropriate type.  If a null
*  value is returned the full range of the implementation type are
*  used.  Set the suggested default arguments to bad values in order to
*  have no dynamic defaults.
      CALL ERR_MARK
      IF ( ITYPE .EQ. '_DOUBLE' )THEN
         DTHDEF = VAL__BADD
         CALL PAR_GDR0D( 'THRESH', DTHDEF, 0D0, VAL__MAXD,
     :                   .FALSE., DTHRES, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            DTHRES = VAL__MAXD
         END IF
      ELSE
         THRDEF = VAL__BADR
         CALL PAR_GDR0R( 'THRESH', THRDEF, 0.0, VAL__MAXR,
     :                   .FALSE., THRESH, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            THRESH = VAL__MAXR
         END IF
      END IF
      CALL ERR_RLSE

*  Perform the filtering.
*  ======================

*  Form cumulative sum.  Reject increments when adjacent pixels deviate
*  from by more than the threshold, calling the routine of the
*  appropriate data type.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPS1_CPWDR( NEL, %VAL( CNF_PVAL( PNTIN( 1 ) ) ), VAR,
     :                    %VAL( CNF_PVAL( PNTIN( 2 ) ) ),
     :                    THRESH, %VAL( CNF_PVAL( PNTOUT( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTOUT( 2 ) ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_CPWDD( NEL, %VAL( CNF_PVAL( PNTIN( 1 ) ) ), VAR,
     :                    %VAL( CNF_PVAL( PNTIN( 2 ) ) ),
     :                    DTHRES, %VAL( CNF_PVAL( PNTOUT( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTOUT( 2 ) ) ), STATUS )

      END IF

*  Get the new title for the output image and insert it into the output
*  NDF.  The input NDF's title was already propagated by the LPG_PROP
*  call and so a null value will leave it unaltered.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CUMULVEC_ERR',
     :     'CUMULVEC: Error summing values of an NDF data structure '/
     :     /'cumulatively.', STATUS )
      END IF

*  End the routine.
      END
