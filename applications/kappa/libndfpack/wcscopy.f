      SUBROUTINE WCSCOPY( STATUS )
*+
*  Name:
*     WCSCOPY

*  Purpose:
*     Copies WCS information from one NDF to another.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSCOPY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application copies the WCS component from one NDF to
*     another, optionally modifying it to take account of a linear
*     mapping between the pixel co-ordinates in the two NDFs. It can be
*     used, for instance, to rectify the loss of WCS information
*     produced by older applications which do not propagate the WCS
*     component.

*  Usage:
*     wcscopy ndf like [tr] [confirm]

*  ADAM Parameters:
*     CONFIRM = _LOGICAL (Read)
*        If TRUE, the user is asked for confirmation before replacing
*        any existing WCS component within the input NDF. No
*        confirmation is required if the there is no WCS component in
*        the input NDF. [TRUE]
*     LIKE = NDF (Read)
*        The reference NDF data structure from which WCS information is
*        to be copied.
*     NDF = NDF (Read and Write)
*        The input NDF data structure in which the WCS information is to
*        be stored. Any existing WCS component is over-written (see
*        Parameter CONFIRM).
*     OK = _LOGICAL (Read)
*        This parameter is used to get a confirmation that an existing
*        WCS component within the input NDF can be over-written.
*     TR( ) = _DOUBLE (Read)
*        The values of this parameter are the coefficients of a linear
*        transformation from pixel co-ordinates in the reference NDF
*        given for Parameter LIKE, to pixel co-ordinates in the input
*        NDF given for Parameter NDF. For instance, if a feature has
*        pixel co-ordinates (X,Y,Z,...) in the reference NDF, and pixel
*        co-ordinates (U,V,W,...) in the input NDF, then the following
*        transformations would be used, depending on how many axes each
*        NDF has:
*
*        - 1-dimensional:
*
*              U = TR(1) + TR(2)*X
*
*        - 2-dimensional:
*
*              U = TR(1) + TR(2)*X + TR(3)*Y
*
*              V = TR(4) + TR(5)*X + TR(6)*Y
*
*        - 3-dimensional:
*
*              U = TR(1) + TR(2)*X + TR(3)*Y + TR(4)*Z
*
*              V = TR(5) + TR(6)*X + TR(7)*Y + TR(8)*Z
*
*              W = TR(9) + TR(10)*X + TR(11)*Y + TR(12)*Z
*
*        If a null value (!) is given it is assumed that the pixel
*        co-ordinates of a given feature are identical in the two NDFs.
*        [!]

*  Examples:
*     wcscopy m51_sim m51
*        This copies the WCS component from the NDF called m51 to the
*        NDF called m51_sim, which may hold the results of a numerical
*        simulation for instance. It is assumed that the two NDFs are
*        aligned (i.e. the pixel co-ordinates of any feature are the
*        same in both NDFs).
*     wcscopy m51_sqorst m51 [125,0.5,0.0,125,0.0,0.5]
*        This example assumes that an application similar to SQORST has
*        previously been used to change the size of a 2-dimensional NDF
*        called m51, producing a new NDF called m51_sqorst. It is
*        assumed that this SQORST-like application does not propagate
*        WCS and also resets the pixel origin to [1,1]. In fact, this is
*        what KAPPA:SQORST actually did, prior to version 1.0. This
*        example shows how WCSCOPY can be used to rectify this by
*        copying the WCS component from the original NDF m51 to the
*        squashed NDF m51_sqorst, modifying it in the process to take
*        account of both the squashing and the resetting of the pixel
*        origin produced by SQORST. To do this, you need to work out the
*        transformation in pixel co-ordinates produced by SQORST, and
*        specify this when running WCSCOPY using the TR parameter. Let's
*        assume the first axis of NDF m51 has pixel-index bounds of
*        I1:I2 (these values can be found using NDFTRACE). If the first
*        axis in the squashed NDF m51_sqorst spans M pixels (where M is
*        the value assigned to SQORST Parameter XDIM), then it will have
*        pixel-index bounds of 1:M. Note, the lower bound is 1 since the
*        pixel origin has been reset by SQORST. The squashing factor for
*        the first axis is then:
*
*                FX = M/(I2 - I1 + 1)
*
*        and the shift in the pixel origin is:
*
*                SX = FX*( 1 - I1 )
*
*        Likewise, if the bounds of the second axis in m51 are J1:J2,
*        and SQORST Parameter YDIM is set to N, then the squashing
*        factor for the second axis is:
*
*                FY = N/(J2 - J1 + 1)
*
*        and the shift in the pixel origin is:
*
*                SY = FY*( 1 - J1 )
*
*        You would then use the following values for Parameter TR when
*        running WCSCOPY:
*
*                TR = [SX, FX, 0.0, SY, 0.0, FY]
*
*        Note, the zero terms indicate that the axes are independent
*        (i.e. there is no rotation of the image). The numerical values
*        in the example are for an image with pixel-index bounds of
*        52:251 on both axes which was squashed by SQORST to produce an
*        image with 100 pixels on each axis.

*  Notes:
*     -  An error is reported if the transformation supplied using
*     Parameter TR is singular.
*     -  The pixel with pixel index I spans a range of pixel co-ordinate
*     from (I - 1.0) to (I).
*     -  The pixel indices of the bottom left pixel in an NDF is called
*     the "pixel origin" of the NDF, and can take any value. The pixel
*     origin can be examined using application NDFTRACE and set using
*     application SETORIGIN. WCSCOPY takes account of the pixel origins
*     in the two NDFs when modifying the WCS component. Thus, if a null
*     value is given for Parameter TR, the supplied WCS component may
*     still be modified if the two NDFs have different pixel origins.

*  Related Applications:
*     KAPPA: NDFTRACE, WCSFRAME, WCSREMOVE, WCSADD, WCSATTRIB

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION DET       ! Matrix determinant
      DOUBLE PRECISION MATRIX( NDF__MXDIM*NDF__MXDIM ) ! Pure matrix (no offset)
      DOUBLE PRECISION MTEST( NDF__MXDIM*NDF__MXDIM ) ! Pure matrix (no offset)
      DOUBLE PRECISION OFFSET( NDF__MXDIM ) ! Pixel offset vector
      DOUBLE PRECISION OTEST( NDF__MXDIM )  ! Pixel offset vector
      DOUBLE PRECISION TR( NDF__MXDIM*( NDF__MXDIM + 1 ) ) ! Mapping co-effs
      INTEGER ACTVAL             ! No. of transformation co-efficients supplied
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions (not used)
      INTEGER I                  ! Row index
      INTEGER INDF1              ! NDF identifier for NDF being modified
      INTEGER INDF2              ! NDF identifier for NDF supplying the WCS
      INTEGER J                  ! Column index
      INTEGER K                  ! Index within supplied list of co-efficients
      INTEGER L                  ! Index within vectorised matrix array
      INTEGER NCOEF              ! Required no. of transformation co-efficients
      INTEGER NDIM1              ! No. of pixel axes in INDF1
      INTEGER NDIM2              ! No. of pixel axes in INDF2
      INTEGER SING               ! Non-zero if matrix is singular
      INTEGER WORK( NDF__MXDIM ) ! Work space
      LOGICAL CONF               ! Confirm replacement of existing WCS ?
      LOGICAL THERE              ! Does the WCS component already exist?
      LOGICAL OK                 ! Is it OK to replace the existing WCS?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF to be modified.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF1, STATUS )

*  Get the number of pixel axes in the NDF.
      CALL NDF_DIM( INDF1, NDF__MXDIM, DIM, NDIM1, STATUS )

*  Obtain an identifier for the NDF containing the WCS component to be
*  copied.
      CALL LPG_ASSOC( 'LIKE', 'READ', INDF2, STATUS )

*  Get the number of pixel axes in the NDF.
      CALL NDF_DIM( INDF2, NDF__MXDIM, DIM, NDIM2, STATUS )

*  Report an error if the NDFs do not have the same number of axes.
      IF( NDIM1 .NE. NDIM2 .AND. STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETI( 'ND1', NDIM1 )
         CALL NDF_MSG( 'NDF1', INDF1 )
         CALL MSG_SETI( 'ND2', NDIM2 )
         CALL NDF_MSG( 'NDF2', INDF2 )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'WCSCOPY_ERR1', 'The number of pixel axes '//
     :                 '(^ND1) in ''^NDF1'' is not the same as the '//
     :                 'number of pixel axes (^ND2) in ''^NDF2''.',
     :                 STATUS )
         GO TO 999
      END IF

*  Ask for confirmation before over-writing an existing WCS component.
*  We do not check for IRAS90 or FITS WCS information since the extensions
*  containing these will not be changed by this application. More over, it
*  is expensive to examine these extensions to see if they contain usable
*  WCS information.
      CALL NDF_STATE( INDF1, 'WCS', THERE, STATUS )
      IF( THERE ) THEN

         CALL PAR_GET0L( 'CONFIRM', CONF, STATUS )
         IF( CONF ) THEN

            CALL NDF_MSG( 'NDF', INDF1 )
            CALL MSG_OUT( 'WCSCOPY_MSG1', 'There is an existing WCS '//
     :                    'component in ''^NDF''.', STATUS )

            CALL PAR_GET0L( 'OK', OK, STATUS )

            IF( .NOT. OK ) THEN
               CALL MSG_OUT( 'WCSCOPY_MSG2', 'The WCS information '//
     :                       'has not been copied.', STATUS )
               CALL MSG_BLANK( STATUS )
               GO TO 999
            END IF

            CALL MSG_BLANK( STATUS )

         END IF
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the co-efficients of the linear transformation from the pixel
*  Frame in INDF1 to the pixel Frame in INDF2. Ensure the exact required
*  number are supplied.
      ACTVAL = 0
      NCOEF = ( NDIM1 + 1 )*NDIM1
      DO WHILE( ACTVAL .NE. NCOEF .AND. STATUS .EQ. SAI__OK )
         CALL PAR_GET1D( 'TR', NCOEF, TR, ACTVAL, STATUS )
         IF( ACTVAL .NE. NCOEF .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETI( 'N', NCOEF )
            CALL MSG_OUT( 'WCSCOPY_MSG', 'Please supply exactly ^N '//
     :                    'co-efficient values (or a null value) for '//
     :                    'parameter %TR.', STATUS )
            CALL PAR_CANCL( 'TR', STATUS )
         END IF
      END DO

*  If a null value was given, annul the error and store a unit matrix and
*  zero offset.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

         L = 1

         DO I = 1, NDIM1
            OFFSET( I ) = 0.0D0

            DO J = 1, NDIM1
               IF( I .EQ. J ) THEN
                  MATRIX( L ) = 1.0D0
               ELSE
                  MATRIX( L ) = 0.0D0
               END IF

               L = L + 1

            END DO

         END DO

*  Otherwise, if no error has occurred, extract the offset and matrix
*  from the supplied list of co-efficients.
      ELSE

*  Extract the offset into a separate vector, making two copies.
         DO I = 1, NDIM1
            OFFSET( I ) = TR( 1 + ( I - 1 )*( NDIM1 + 1 ) )
            OTEST( I ) = OFFSET( I )
         END DO

*  Extract the matrix into a separate vector, making two copies.
         K = 1
         L = 1
         DO I = 1, NDIM1
            K = K + 1

            DO J = 1, NDIM1
               MATRIX( L ) = TR( K )
               MTEST( L ) = TR( K )
               L = L + 1
               K = K + 1
            END DO

         END DO

*  See if the matrix is singular. The MTEST and OTEST arrays are changed
*  by this call, This is why we took two copies above.
         CALL SLA_DMAT( NDIM1, MTEST, OTEST, DET, SING, WORK )

*  Report na error if the matrix is singular.
         IF( SING .NE. 0 .AND. STATUS .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'WCSCOPY_ERR2', 'The supplied '//
     :                    'transformation matrix is singular, and '//
     :                    'therefore cannot be inverted.', STATUS )
         END IF

      END IF

*  Copy the WCS component from INDF2 to INDF1, re-mapping the GRID
*  Frame in the process in order to produce the required Mapping between
*  pixel co-ordinates.
      CALL KPG1_ASPRP( NDIM1, INDF2, INDF1, MATRIX, OFFSET, STATUS )

*  Tidy up.
*  ========
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WCSCOPY_ERR3', 'WCSCOPY: Failed to copy '//
     :                 'WCS information from one NDF to another.',
     :                 STATUS )
      END IF

      END
