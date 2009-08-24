      SUBROUTINE KPG1_ASGET( INDF, NDIM, EXACT, TRIM, REQINV, SDIM, 
     :                       SLBND, SUBND, IWCS, STATUS )
*+
*  Name:
*     KPG1_ASGET

*  Purpose:
*     Gets an AST FrameSet from the WCS component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASGET( INDF, NDIM, EXACT, TRIM, REQINV, SDIM, SLBND, SUBND, 
*                      IWCS, STATUS )

*  Description:
*     This routine determines the axes to be used from an NDF and returns a
*     FrameSet representing the WCS information in the NDF. 
*
*     Each axis of the supplied NDF is checked to see if it is significant 
*     (i.e. has a size greater than 1).  The index of each significant axis 
*     is returned in SDIM, and the bounds of the axis are returned in SLBND 
*     and SUBND.  If EXACT is .TRUE., an error is reported if the number of 
*     significant axes is not exactly NDIM.  This mode is intended for case 
*     where (say) the user has supplied a single plane from a 3D data cube
*     to an application which requires a 2D array.
*
*     If EXACT is .FALSE. an error is only reported if the number of 
*     significant dimensions is higher than NDIM.  If there are less than 
*     NDIM significant dimensions then the insignificant dimensions are 
*     used (starting from the lowest) to ensure that the required number 
*     of dimensions are returned. This mode is intended for cases where (say)
*     the user supplies a 1D data stream to an application which requires a
*     2D array.
*
*     The GRID Frame (i.e. the Base Frame) obtained from the NDFs WCS 
*     component is modified so that it has NDIM axes corresponding to the 
*     axes returned in SDIM (the value 1.0 is used for the other axes). 
*
*     Likewise, the PIXEL Frame obtained from the NDFs WCS component is 
*     modified so that it has NDIM axes corresponding to the axes returned 
*     in SDIM (the lower pixel bound is used for the other axes). The
*     original PIXEL Frame is retained, but with Domain changed to
*     NDF_PIXEL.
*
*     If TRIM is .TRUE., then the Current Frame obtained from the NDFs WCS
*     component is also modified so that it has NDIM axes. If the original
*     Current Frame has more than NDIM axes, then the axes to use are 
*     obtained from the environment using parameter USEAXIS. A new Current 
*     Frame is then made by picking these axes from the original Current
*     Frame, assigning the value AST__BAD to the axes which have not been 
*     chosen.
*
*     If the original Current Frame has less than NDIM axes, then simple
*     axes are added into the new Current Frame to make up a total of
*     NDIM. These axes are given the value 1.0.
*
*     Various environment parameters may be used to obtain options, etc. The
*     names of these parameters are hard-wired into this subroutine in
*     order to ensure conformity between applications. 

*  Environment Parameters:
*     USEAXIS = LITERAL (Read)
*        A set of NDIM axes to be selected from the Current Frame. Each 
*        axis can be specified either by giving its index within the Current 
*        Frame in the range 1 to the number of axes in the Frame, or by
*        giving its symbol. This parameter is only accessed if TRIM is
*        .TRUE. and the original Current Frame in the supplied NDF has
*        too many axes. The dynamic default selects the axes with the same
*        indices as the selected NDF axes. The value should be given as a
*        GRP group expression, with default control characters. 

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier for the NDF.
*     NDIM = INTEGER (Given)
*        The number of dimensions required by the application.
*     EXACT = LOGICAL (Given)
*        Must the NDF have exactly NDIM significant axes? Otherwise it is
*        acceptable for the NDIM axes to include some insignificant axes.
*     TRIM = LOGICAL (Given)
*        Should the returned FrameSet be trimmed to ensure that the
*        Current Frame has NDIM axes? Otherwise, the Current Frame read
*        from the NDF is not changed.
*     REQINV = LOGICAL (Given)
*        Is the inverse mapping (from Current Frame to Base Frame)
*        required? If it is, an error is reported if the inverse mapping
*        is not available. REQINV should be supplied .TRUE. in most cases.
*     SDIM( NDIM ) = INTEGER (Returned)
*        The indices of the significant dimensions.
*     SLBND( NDIM ) = INTEGER (Returned)
*        The lower pixel index bounds of the significant dimensions.  These 
*        are stored in the same order as the indices in SDIM.
*     SUBND( NDIM ) = INTEGER (Returned)
*        The upper pixel index bounds of the significant dimensions.  These
*        are stored in the same order as the indices in SDIM.
*     IWCS = INTEGER (Returned)
*        An AST pointer to the WCS FrameSet. Returned equal to AST__NULL
*        if an error occurs. The Base Frame is an NDIM-dimensional GRID 
*        Domain.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the Current Frame in the returned FrameSet has no Title, then
*     the Title is set to the value of the NDF TITLE component (so long
*     as the NDF TITLE is not blank or undefined).

*  Copyright:
*     Copyright (C) 1998, 1999, 2000, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     30-JUN-1998 (DSB):
*        Original version.
*     22-JUN-1999 (DSB):
*        Remove call to NDF_SECT which set the pixel bounds fo all
*        insignificant axes to (1:1).
*     15-JAN-2000 (DSB):
*        Do not remove insignificant axes from the current Frame if TRIM
*        indicates that a precise number of axes is required in the
*        current Frame.
*     30-AUG-2004 (DSB):
*        Replaced hardwired "3" by "NDIMS" in call to KPG1_ASSIG!
*     2-DEC-2005 (DSB):
*        Added INPRM to argument list for KPG1_ASTRM.
*     14-FEB-2006 (DSB):
*        Ensure INPRM is initialised even if NDIM is equal to NBAX. Lack
*        of initialisation caused KPG1_ASTRM to crash in the case where 
*        NDIM and NBAX are equal.
*     12-SEP-2006 (DSB):
*        If the current Frame is AXIS and the AXIS structures are
*        non-monotonic, reset the current Frame to PIXEL.
*     16-NOV-2006 (PWD):
*        Stop modfication of NDIM given argument in call to NDF_BOUND.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL

*  Arguments Given:
      INTEGER INDF
      INTEGER NDIM
      LOGICAL EXACT
      LOGICAL TRIM
      LOGICAL REQINV

*  Arguments Returned:
      INTEGER SDIM( NDIM )
      INTEGER SLBND( NDIM )
      INTEGER SUBND( NDIM )
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER SZFMT              ! Max. characters in formatted value
      PARAMETER ( SZFMT = 2 * VAL__SZD )

*  Local Variables:
      CHARACTER COSTR*( NDF__MXDIM * ( SZFMT + 1 ) + 1 ) 
                                   ! Formatted co-ordinate string
      CHARACTER DOM*30             ! Current Frame domain
      CHARACTER PAXIS*( VAL__SZI ) ! Buffer for new axis number
      CHARACTER QAXIS*( VAL__SZI ) ! Buffer for original axis number
      CHARACTER TTL*80             ! NDF title
      DOUBLE PRECISION CONST( NDF__MXDIM )! Constants for unassigned axes
      DOUBLE PRECISION OFFSET      ! Axis offset scale factor
      DOUBLE PRECISION SCALE       ! Axis scale factor
      INTEGER AX( NDF__MXDIM )     ! Axis indices to check for monotonicity
      INTEGER DIM                  ! Pixel dimension
      INTEGER FRM                  ! Pointer to a Frame in IWCS
      INTEGER I                    ! Axis index
      INTEGER IBASE                ! Index of original Base Frame in IWCS
      INTEGER ICURR                ! Index of original Current Frame in IWCS
      INTEGER INPRM( NDF__MXDIM )  ! Input axis permutation array
      INTEGER IPIX                 ! Index of original PIXEL Frame in IWCS
      INTEGER IPWORK               ! Pointer to work space
      INTEGER LBND( NDF__MXDIM )   ! Original NDF bounds
      INTEGER LTTL                 ! Used length of TTL
      INTEGER MXDIM                ! Largest selected pixel dimension
      INTEGER NBAX                 ! Number of axes in GRID Frame
      INTEGER NC                   ! No. of characters in text buffer
      INTEGER NCP                  ! No. of characters in PAXIS text buffer
      INTEGER NCQ                  ! No. of characters in QAXIS text buffer
      INTEGER NDIMS                ! No. of genuine axes in the NDF
      INTEGER NEWBAS               ! Pointer to the new Base Frame
      INTEGER NEWFS                ! Pointer to a FrameSet with new Base Frame
      INTEGER NEWPIX               ! Pointer to the new PIXEL Frame
      INTEGER OUTPRM( NDF__MXDIM ) ! Output axis permutation array
      INTEGER PMAP                 ! AST pointer to a PermMap
      INTEGER UBND( NDF__MXDIM )   ! Original NDF bounds
      LOGICAL MONO                 ! Are all axes monotonic?
*.

*  Initialise.
      IWCS = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Choose the NDF pixel axes to be used by the application.
*  ==================================================

*  Find whether or not there are exactly the required number of
*  significant dimensions in the NDF pixel array and which ones they are.
      IF ( EXACT ) THEN
         CALL KPG1_SGDIM( INDF, NDIM, SDIM, STATUS )

*  If insignificant dimensions can be used, pad out the returned
*  dimensions with insignificant dimensions if the required number
*  of significant dimensions is not present.
      ELSE
         CALL KPG1_SDIMP( INDF, NDIM, SDIM, STATUS )
      END IF

*  Obtain the bounds of the NDF.  
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*  Return the bounds of the chosen pixel axes. Also find the largest
*  chosen pixel dimension, and ininitialise the base Frame axis
*  permutation array (INPRM). 
      MXDIM = 0
      DO I = 1, NDIM
         SLBND( I ) = LBND( SDIM( I ) )
         SUBND( I ) = UBND( SDIM( I ) )
         DIM = SUBND( I ) - SLBND( I ) + 1
         IF( DIM .GT. MXDIM ) MXDIM = DIM
         INPRM( I ) = I
      END DO
         
*  Get a pointer to the WCS FrameSet.
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Remove any insignificant axes from the Current Frame. We do not do
*  this if later code will trim the current Frame to have the required
*  number of axes.
      IF( .NOT. TRIM ) CALL KPG1_ASSIG( IWCS, NDIMS, LBND, UBND, 
     :                                  STATUS )

*  Re-map the Base (GRID) Frame by selecting the chosen axes.
*  ==========================================================
*  Save the number of axes in the Base Frame.
      NBAX = AST_GETI( IWCS, 'NIN', STATUS ) 

*  If the Base Frame has the wrong number of axes, create a new Base
*  Frame by picking axes from the original.
      IF( NBAX .NE. NDIM ) THEN

*  Create a new GRID Frame with NDIM axes.
         NEWBAS = AST_FRAME( NDIM, 'DOMAIN=GRID', STATUS )

*  Create a title for it, including the grid co-ordinates of the first
*  pixel, i.e. (1.0,1.0,...) 
         NC = 0
         CALL CHR_PUTC( '(', COSTR, NC )
         DO I = 1, NDIM
            IF ( I .GT. 1 ) CALL CHR_PUTC( ',', COSTR, NC )
            CALL CHR_PUTC( '1.0', COSTR, NC )
         END DO
         CALL CHR_PUTC( ')', COSTR, NC )

*  Store the title in the Frame.
         IF ( NDIM .EQ. 1 ) THEN
            CALL AST_SETC( NEWBAS, 'Title', 'Data grid index; first '//
     :                     'pixel at ' // COSTR( : NC ), STATUS )
         ELSE
            CALL AST_SETC( NEWBAS, 'Title', 'Data grid indices; '//
     :                     'first pixel at ' // COSTR( : NC ), STATUS ) 
         END IF

*  For each axis, set up a label, symbol and unit value. Use the original
*  NDF axis numbers.
         DO I = 1, NDIM
            NCP = 0
            NCQ = 0

            CALL CHR_PUTI( I, PAXIS, NCP )
            CALL CHR_PUTI( SDIM( I ), QAXIS, NCQ )

            CALL AST_SETC( NEWBAS, 'Label(' // PAXIS( : NCP ) // ')',
     :                     'Data grid index ' // QAXIS( : NCQ ), 
     :                     STATUS )
            CALL AST_SETC( NEWBAS, 'Symbol(' // PAXIS( : NCP ) // ')',
     :                     'g' // QAXIS( : NCQ ), STATUS )
            CALL AST_SETC( NEWBAS, 'Unit(' // PAXIS( : NCP ) // ')',
     :                     'pixel', STATUS )

         END DO

*  Create a PermMap which goes from this new NDIM-dimensional GRID Frame 
*  to the original NBAX-dimensional GRID Frame. First, initialise the axis 
*  permutation arrays so that all input and output axes take the value of 
*  the first constant supplied to AST_PERMMAP (i.e. 1.0).
         DO I = 1, NBAX
            OUTPRM( I ) = -1
         END DO

         DO I = 1, NDIM
            INPRM( I ) = -1
         END DO

*  Now over-write elements of the axis permutation arrays which correspond to 
*  genuine axes.
         DO I = 1, NDIM
            IF( SDIM( I ) .LE. NBAX ) THEN
               INPRM( I ) = SDIM( I )
               OUTPRM( SDIM( I ) ) = I
            END IF 
         END DO

*  Create the PermMap.
         PMAP = AST_PERMMAP( NDIM, INPRM, NBAX, OUTPRM, 1.0D0, ' ', 
     :                       STATUS ) 

*  Create a new FrameSet holding just the new GRID Frame.
         NEWFS = AST_FRAMESET( NEWBAS, ' ', STATUS )

*  Note the indices of the original Base and Current Frames.
         IBASE = AST_GETI( IWCS, 'BASE', STATUS )
         ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Make the GRID Frame the Current Frame (AST_ADDFRAME uses the Current
*  Frame).
         CALL AST_SETI( IWCS, 'CURRENT', IBASE, STATUS )
         
*  Add in the FrameSet read from the NDF, using the PermMap created above
*  to connect the Base (GRID) Frame to the new FrameSet. 
         CALL AST_ADDFRAME( NEWFS, AST__BASE, PMAP, IWCS, STATUS )
         CALL AST_ANNUL( PMAP, STATUS )

*  Remove the original GRID Frame. Its index will have increased by 1
*  because of the single Frame which was already in NEWFS.
         CALL AST_REMOVEFRAME( NEWFS, IBASE + 1, STATUS )

*  Re-instate the original Current Frame.
         CALL AST_SETI( NEWFS, 'CURRENT', ICURR, STATUS )

*  Annul the original FrameSet and use the new one instead.
         CALL AST_ANNUL( IWCS, STATUS )
         IWCS = NEWFS

      END IF       

*  Re-map the PIXEL Frame by selecting the chosen axes.
*  ====================================================

*  If the PIXEL Frame has the wrong number of axes, create a new PIXEL
*  Frame by picking axes from the original.
      IF( NBAX .NE. NDIM ) THEN

*  Create a new PIXEL Frame with NDIM axes.
         NEWPIX = AST_FRAME( NDIM, 'DOMAIN=PIXEL', STATUS )

*  Create a title for it, including the pixel co-ordinates of the first
*  pixel.
         NC = 0
         CALL CHR_PUTC( '(', COSTR, NC )
         DO I = 1, NDIM
            IF ( I .GT. 1 ) CALL CHR_PUTC( ',', COSTR, NC )
            CALL CHR_PUTR( REAL( SLBND( I ) ) - 0.5, COSTR, NC )
         END DO
         CALL CHR_PUTC( ')', COSTR, NC )

*  Store the title in the Frame.
         IF ( NDIM .EQ. 1 ) THEN
            CALL AST_SETC( NEWPIX, 'Title', 'Pixel co-ordinate; '//
     :                     'first pixel at ' // COSTR( : NC ), STATUS )
         ELSE
            CALL AST_SETC( NEWPIX, 'Title', 'Pixel co-ordinates; '//
     :                     'first pixel at ' // COSTR( : NC ), STATUS ) 
         END IF

*  For each axis, set up a label, symbol and unit value. Use the original
*  NDF axis numbers.
         DO I = 1, NDIM
            NCP = 0
            NCQ = 0

            CALL CHR_PUTI( I, PAXIS, NCP )
            CALL CHR_PUTI( SDIM( I ), QAXIS, NCQ )

            CALL AST_SETC( NEWPIX, 'Label(' // PAXIS( : NCP ) // ')',
     :                     'Pixel co-ordinate ' // QAXIS( : NCQ ), 
     :                     STATUS )
            CALL AST_SETC( NEWPIX, 'Symbol(' // PAXIS( : NCP ) // ')',
     :                     'p' // QAXIS( : NCQ ), STATUS )
            CALL AST_SETC( NEWPIX, 'Unit(' // PAXIS( : NCP ) // ')',
     :                     'pixel', STATUS )

         END DO

*  Store the pixel co-ordinate at the centre of the low bound pixel on each
*  axis. These are the constants used by AST_PERMMAP.
         DO I = 1, NDF__MXDIM
            CONST( I ) = DBLE( LBND( I ) ) - 0.5D0
         END DO

*  Create a PermMap which goes from this new NDIM-dimensional PIXEL Frame 
*  to the original NBAX-dimensional PIXEL Frame. First, initialise the axis 
*  permutation arrays so that all input and output axes take the value of 
*  the corresponding axis lower bound. Nagative values index the array of
*  constants set up above.
         DO I = 1, NBAX
            OUTPRM( I ) = -I
         END DO

         DO I = 1, NDIM
            INPRM( I ) = -SDIM( I )
         END DO

*  Now over-write elements of the axis permutation arrays which correspond to 
*  genuine axes.
         DO I = 1, NDIM
            IF( SDIM( I ) .LE. NBAX ) THEN
               INPRM( I ) = SDIM( I )
               OUTPRM( SDIM( I ) ) = I
            END IF 
         END DO

*  Create the PermMap.
         PMAP = AST_PERMMAP( NDIM, INPRM, NBAX, OUTPRM, CONST, ' ', 
     :                       STATUS ) 

*  Find the original PIXEL Frame, and change its Domain to NDF_PIXEL.
         IPIX = 0
         DO I = 1, AST_GETI( IWCS, 'NFRAME', STATUS )
            FRM = AST_GETFRAME( IWCS, I, STATUS )
            IF( AST_GETC( FRM, 'DOMAIN', STATUS ) .EQ. 'PIXEL' ) THEN
               CALL AST_SETC( FRM, 'DOMAIN', 'NDF_PIXEL', STATUS )
               CALL AST_ANNUL( FRM, STATUS )
               IPIX = I
               GO TO 10
            END IF
            CALL AST_ANNUL( FRM, STATUS )
         END DO
 10      CONTINUE

*  Report an error if no PIXEL Frame was found.
         IF( IPIX .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'KPG1_ASGET_1', 'No PIXEL Frame found '//
     :                    'in WCS component of ''^NDF'' (possible '//
     :                    'programming error).', STATUS )      
         END IF

*  Invert the PermMap so that its forward mapping goes from the original 
*  PIXEL Frame to the new one. This is the direction required by
*  AST_ADDFRAME.
         CALL AST_INVERT( PMAP, STATUS )

*  Save the index of the Current Frame (changed by AST_ADDFRAME).
         ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )
         
*  Add the new PIXEL Frame on to the end of the FrameSet, using the above
*  PermMap to connect it to the original PIXEL Frame. The new PIXEL Frame
*  becomes the Current Frame.
         CALL AST_ADDFRAME( IWCS, IPIX, PMAP, NEWPIX, STATUS )

*  Re-instate the original Current Frame, unless this was the original PIXEL 
*  Frame (in which case the new PIXEL Frame is left as the Current Frame).
         IF( ICURR .NE. IPIX ) CALL AST_SETI( IWCS, 'CURRENT', ICURR,
     :                                        STATUS ) 

      END IF       



*  Now modify the Current Frame if required to have exactly NDIM axes.
*  ===================================================================
      IF( TRIM ) THEN
         CALL PSX_CALLOC( MXDIM*2, '_DOUBLE', IPWORK, STATUS )
         CALL KPG1_ASTRM( IWCS, INPRM, SLBND, SUBND, 
     :                    %VAL( CNF_PVAL(IPWORK) ), STATUS )
         CALL PSX_FREE( IPWORK, STATUS )
      END IF


*  Tidy up
*  =======

*  If the Current Frame has no Title, use the Title from the NDF (if any).
      TTL = ' '
      CALL NDF_CGET( INDF, 'TITLE', TTL, STATUS )
      IF( TTL .NE. ' ' .AND. 
     :    .NOT. AST_TEST( IWCS, 'TITLE', STATUS ) ) THEN
         LTTL = CHR_LEN( TTL )
         CALL AST_SETC( IWCS, 'TITLE', TTL( : LTTL ), STATUS )
      END IF

*  Report an error if the inverse mapping is required, but is not
*  available.
      IF( REQINV .AND. .NOT. AST_GETL( IWCS, 'TRANINVERSE', STATUS ) 
     :    .AND. STATUS .EQ. SAI__OK ) THEN

         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF ) 
         CALL ERR_REP( 'KPG1_ASGET_2', 'The mapping from the current '//
     :                 'co-ordinate Frame in ^NDF to the pixel '//
     :                 'co-ordinate Frame is not defined.', STATUS )

         CALL NDF_MSG( 'NDF', INDF ) 
         CALL ERR_REP( 'KPG1_ASGET_3', 'It may be possible to avoid '//
     :                 'this problem by changing the current '//
     :                 'co-ordinate Frame in ^NDF using WCSFRAME.', 
     :                  STATUS )

      END IF

*  If the current Frame is AXIS, check that the AXIS structures are
*  monotonic. If not, reset the current Frame to PIXEL.
      DOM = AST_GETC( IWCS, 'Domain', STATUS )
      IF( DOM .EQ. 'AXIS' ) THEN      

         CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )
         DO I = 1, NDIMS
            AX( I ) = I
         END DO

         CALL KPG1_CHAXD( INDF, NDIMS, AX, MONO, SCALE, OFFSET,
     :                    STATUS )

         IF( .NOT. MONO ) THEN
            CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )
            CALL AST_SETI( IWCS, 'Current', IPIX, STATUS )
            CALL MSG_OUT( 'KPG1_GTWCS_MSG4', 'PIXEL co-ordinates will'//
     :                    ' be used instead of AXIS co-ordinates.', 
     :                    STATUS )
         END IF

      END IF

*  Export the returned FrameSet from the current AST context so that it is
*  not annulled by the following call to AST_END.
      CALL AST_EXPORT( IWCS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )
    
      END
