      SUBROUTINE PERMAXES( STATUS )
*+
*  Name:
*     PERMAXES

*  Purpose:
*     Permute an NDF's pixel axes.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PERMAXES( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application re-orders the pixel axes of an NDF, together with
*     all related information (AXIS structures, and the axes of all
*     co-ordinate Frames stored in the WCS component of the NDF).

*  Usage:
*     permaxes in out perm

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF data structure.
*     OUT = NDF (Write)
*        The output NDF data structure.
*     PERM() = _INTEGER (Read)
*        A list of integers defining how the pixel axes are to be
*        permuted.  The list must contain one element for each pixel
*        axis in the NDF.  The first element is the index of the pixel
*        axis within the input NDF which is to become axis 1 in the
*        output NDF.  The second element is the index of the pixel axis
*        within the input NDF which is to become axis 2 in the output
*        NDF, etc.  Axes are numbered from 1.
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null value will cause the title
*        of the NDF supplied for parameter IN to be used instead.  [!]

*  Examples:
*     permaxes a b [2,1]
*        Swaps the axes in the 2-dimensional NDF called "a", to produce
*        a new two-dimensional NDF called "b".
*     permaxes a b [3,1,2]
*        Creates a new three-dimensional NDF called "b" in which axis 1
*        corresponds to axis 3 in the input three-dimensional NDF called
*        "a", axis 2 corresponds to input axis 1, axis 3 corresponds to
*        input axis 2.

*  Notes:
*     - If any WCS co-ordinate Frame has more axes then the number of
*     pixel axes in the NDF, then the high numbered surplus axes in the
*     WCS Frame are left unchanged.
*     - If any WCS co-ordinate Frame has fewer axes then the number of
*     pixel axes in the NDF, then the Frame is left unchanged if the
*     specified permutation would change any of the high numbered
*     surplus pixel axes.  A warning message is issued if this occurs.

*  Related Applications:
*     KAPPA: ROTATE, FLIP; Figaro: IREVX, IREVY, IROT90.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS, and HISTORY components of the
*     input NDF and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  The data
*     type of the input pixels is preserved in the output NDF.
*     -  Huge NDF are supported.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008, 2012 Science and Technology Facilities
*     Council.
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-FEB-2001 (DSB):
*        Original version.
*     15-MAR-2001 (DSB):
*        Take surplus axes into account.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 12 (MJC):
*        Remove unused variable and wrapped long lines.
*     2008 June 17 (MJC):
*        Trim trailing blanks from output NDF character components.
*     2012 May 9 (MJC):
*        Add _INT64 support.
*     24-FEB-2020 (DSB):
*        Support huge NDFs.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      BYTE BB                    ! Quality bad-bits value
      CHARACTER ACCOMP( 2 )*5    ! Axis character components to process
      CHARACTER ACOMP( 3 )*8     ! Axis array components to process
      CHARACTER COMP( 3 )*8      ! Array components to process
      CHARACTER DOM*30           ! Frame domain
      CHARACTER FORM*( NDF__SZFRM ) ! Form of the NDF array
      CHARACTER TYPE*( NDF__SZTYP ) ! Array component numeric type
      CHARACTER VALUE*80         ! Axis character component value
      DOUBLE PRECISION MATRIX( NDF__MXDIM*NDF__MXDIM ) ! Matrix
                                 ! component of linear mapping
      DOUBLE PRECISION OFFSET( NDF__MXDIM ) ! Translation component of
                                 ! linear mapping
      INTEGER*8 DIM( NDF__MXDIM )! Input NDF dimension sizes
      INTEGER*8 DIMO( NDF__MXDIM ) ! Output NDF dimension sizes
      INTEGER*8 EL               ! Number of elements mapped
      INTEGER I                  ! Axis index
      INTEGER IBASE              ! Index of Base Frame
      INTEGER ICOMP              ! Loop counter for array components
      INTEGER ICURR              ! Index of original Current Frame
      INTEGER IDIM               ! Input axis index
      INTEGER*8 IERR             ! Index of first numerical error
      INTEGER INDF1              ! Input NDF identifier
      INTEGER INDF2              ! Output NDF identifier
      INTEGER IP1                ! Pointer to mapped input array
      INTEGER IP2                ! Pointer to mapped output array
      INTEGER IWCS               ! WCS FrameSet pointer
      INTEGER J                  ! Axis index
      INTEGER*8 LBND( NDF__MXDIM ) ! Lower pixel index bounds in input
      INTEGER*8 LBNDO( NDF__MXDIM )! Lower pixel index bounds in output
      INTEGER MINAX              ! Minimum allowed number of Frame axes
      INTEGER NAX                ! Number of Frame axes
      INTEGER NC                 ! No. characters in text buffer
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER*8 NERR             ! Number of numerical errors
      INTEGER PERM( NDF__MXDIM ) ! Axis permutation array
      INTEGER*8 UBND( NDF__MXDIM ) ! Upper pixel index bounds in input
      INTEGER*8 UBNDO( NDF__MXDIM )! Upper pixel index bounds in output
      LOGICAL BAD                ! Bad-pixel flag
      LOGICAL THERE              ! Component is defined?

*  Local Data:
      DATA COMP / 'Data', 'Variance', 'Quality' /
      DATA ACOMP / 'Centre', 'Width', 'Variance' /
      DATA ACCOMP / 'Label', 'Units' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF and determine its bounds.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )
      CALL NDF_BOUND8( INDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Get the axis permutation array.
      PERM( 1 ) = -1
      CALL PAR_GDR1I( 'PERM', NDIM, PERM, 1, NDIM, .FALSE., PERM,
     :                STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Verify that each axis is used only once.
      DO I = 1, NDIM - 1
         DO J = I + 1, NDIM
            IF( PERM( I ) .EQ. PERM( J ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'AX', PERM( I ) )
               CALL ERR_REP( 'PERMAXES_ERR1', 'Axis ^AX is included '//
     :                       'more than once in the list supplied for'//
     :                       ' Parameter PERM.', STATUS )
               GO TO 999
            END IF
         END DO
      END DO

*  Get the bounds of the output NDF. Also store the dimensions for input
*  and output.
      DO I = 1, NDIM
         LBNDO( I ) = LBND( PERM( I ) )
         UBNDO( I ) = UBND( PERM( I ) )
         DIM( I ) = UBND( I ) - LBND( I ) + 1
         DIMO( I ) = UBNDO( I ) - LBNDO( I ) + 1
      END DO

*  Create the output NDF, propagating the units values.
      CALL LPG_PROP( INDF1, 'Units', 'OUT', INDF2, STATUS )

*  Set the bounds of the output NDF.
      CALL NDF_SBND8( NDIM, LBNDO, UBNDO, INDF2, STATUS )

*  Process the main NDF array components.
*  =====================================

*  Disable automatic quality masking, since the quality array (if
*  present) will be handled explicitly.
      CALL NDF_SQMF( .FALSE., INDF1, STATUS )

*  Loop to process the data, variance and quality arrays in turn.
      DO ICOMP = 1, 3

*  Determine if the input array is defined.
         CALL NDF_STATE( INDF1, COMP( ICOMP ), THERE, STATUS )

*  If so, then determine its numeric type and map the input and output
*  arrays for access using this type.
         IF ( THERE ) THEN
            CALL NDF_TYPE( INDF1, COMP( ICOMP ), TYPE, STATUS )
            CALL NDF_MAP8( INDF1, COMP( ICOMP ), TYPE, 'READ', IP1,
     :                     EL, STATUS )
            CALL NDF_MAP8( INDF2, COMP( ICOMP ), TYPE, 'WRITE', IP2,
     :                     EL, STATUS )

*  Call the appropriate routine to process the array, depending on its
*  numeric type.
            IF ( TYPE .EQ. '_BYTE' ) THEN
               CALL KPS1_PRMXB( NDIM, DIM, DIMO, PERM,
     :                          %VAL( CNF_PVAL( IP1 ) ),
     :                          %VAL( CNF_PVAL( IP2 ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
               CALL KPS1_PRMXUB( NDIM, DIM, DIMO, PERM,
     :                           %VAL( CNF_PVAL( IP1 ) ),
     :                           %VAL( CNF_PVAL( IP2 ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_PRMXD( NDIM, DIM, DIMO, PERM,
     :                          %VAL( CNF_PVAL( IP1 ) ),
     :                          %VAL( CNF_PVAL( IP2 ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
               CALL KPS1_PRMXI( NDIM, DIM, DIMO, PERM,
     :                          %VAL( CNF_PVAL( IP1 ) ),
     :                          %VAL( CNF_PVAL( IP2 ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_INT64' ) THEN
               CALL KPS1_PRMXK( NDIM, DIM, DIMO, PERM,
     :                          %VAL( CNF_PVAL( IP1 ) ),
     :                          %VAL( CNF_PVAL( IP2 ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_REAL' ) THEN
               CALL KPS1_PRMXR( NDIM, DIM, DIMO, PERM,
     :                          %VAL( CNF_PVAL( IP1 ) ),
     :                          %VAL( CNF_PVAL( IP2 ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_WORD' ) THEN
               CALL KPS1_PRMXW( NDIM, DIM, DIMO, PERM,
     :                          %VAL( CNF_PVAL( IP1 ) ),
     :                          %VAL( CNF_PVAL( IP2 ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
               CALL KPS1_PRMXUW( NDIM, DIM, DIMO, PERM,
     :                           %VAL( CNF_PVAL( IP1 ) ),
     :                           %VAL( CNF_PVAL( IP2 ) ), STATUS )
            END IF

*  If a quality array is being processed, then transfer the quality
*  component's bad-bits value.
            IF ( COMP( ICOMP ) .EQ. 'Quality' ) THEN
               CALL NDF_BB( INDF1, BB, STATUS )
               CALL NDF_SBB( BB, INDF2, STATUS )

*  Otherwise, transfer the bad-pixel flag from the input to the output
*  array unless its storage format is primitive.
            ELSE
               CALL NDF_FORM( INDF2, COMP( ICOMP ), FORM, STATUS )
               CALL NDF_BAD( INDF1, COMP( ICOMP ), .FALSE., BAD,
     :                       STATUS )
               IF ( FORM .NE. 'PRIMITIVE' ) THEN
                  CALL NDF_SBAD( BAD, INDF2, COMP( ICOMP ), STATUS )
               END IF
            END IF

*  Unmap the input and output arrays.
            CALL NDF_UNMAP( INDF1, COMP( ICOMP ), STATUS )
            CALL NDF_UNMAP( INDF2, COMP( ICOMP ), STATUS )
         END IF

      END DO

*  Process the axis arrays.
*  =======================

*  Loop to process each axis.  I refers to the output NDF, and IDIM
*  refers to the input NDF.
      DO I = 1, NDIM
         IDIM = PERM( I )

*  Loop to process the axis centre, width and variance arrays.
         DO ICOMP = 1, 3

*  Determine if the input axis array is defined.
            CALL NDF_ASTAT( INDF1, ACOMP( ICOMP ), IDIM, THERE, STATUS )

*  If so, then determine its numeric type and map the input and output
*  axis arrays for access using this type.
            IF ( THERE ) THEN
               CALL NDF_ATYPE( INDF1, ACOMP( ICOMP ), IDIM, TYPE,
     :                         STATUS )
               CALL NDF_AMAP8( INDF1, ACOMP( ICOMP ), IDIM, TYPE,
     :                         'READ', IP1, EL, STATUS )
               CALL NDF_AMAP8( INDF2, ACOMP( ICOMP ), I, TYPE, 'WRITE',
     :                         IP2, EL, STATUS )

*  Call the appropriate routine to process the array, depending on its
*  numeric type.
               IF ( TYPE .EQ. '_BYTE' ) THEN
                  CALL VEC8_BTOB( .FALSE., EL, %VAL( CNF_PVAL(IP1) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                  CALL VEC8_UBTOUB( .FALSE., EL, %VAL( CNF_PVAL(IP1) ),
     :                             %VAL( CNF_PVAL( IP2 ) ),
     :                             IERR, NERR, STATUS )

               ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                  CALL VEC8_DTOD( .FALSE., EL, %VAL( CNF_PVAL(IP1) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                  CALL VEC8_ITOI( .FALSE., EL, %VAL( CNF_PVAL(IP1) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                  CALL VEC8_KTOK( .FALSE., EL, %VAL( CNF_PVAL(IP1) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                  CALL VEC8_RTOR( .FALSE., EL, %VAL( CNF_PVAL(IP1) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                  CALL VEC8_WTOW( .FALSE., EL, %VAL( CNF_PVAL(IP1) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                  CALL VEC8_UWTOUW( .FALSE., EL, %VAL( CNF_PVAL(IP1) ),
     :                             %VAL( CNF_PVAL( IP2 ) ),
     :                             IERR, NERR, STATUS )

               END IF

*  Unmap the input and output axis arrays.
               CALL NDF_AUNMP( INDF1, ACOMP( ICOMP ), IDIM, STATUS )
               CALL NDF_AUNMP( INDF2, ACOMP( ICOMP ), I, STATUS )

            END IF

         END DO

*  Loop to process the axis label and units components.
         DO ICOMP = 1, 2

*  Determine if the input component is defined.
            CALL NDF_ASTAT( INDF1, ACCOMP( ICOMP ), IDIM, THERE,
     :                      STATUS )

*  If so, then copy it to the output  Note that NDF_ACPUT does not
*  truncate trailing blanks.
            IF ( THERE ) THEN
               CALL NDF_ACGET( INDF1, ACCOMP( ICOMP ), IDIM, VALUE,
     :                         STATUS )
               NC = CHR_LEN( VALUE )
               CALL NDF_ACPUT( VALUE( :NC ), INDF2, ACCOMP( ICOMP ), I,
     :                         STATUS )
            END IF

         END DO

      END DO

*  Propagate the WCS component.
*  ============================
*  Set up a matrix and offset vector describing the linear mapping from
*  input pixel co-ordinates to output pixel co-ordinates.  First of all
*  set the matrix and vector to a unit transformation.
      DO I = 1, NDIM*NDIM
         MATRIX( I ) = 0.0D0
      END DO

      DO I = 1, NDIM
         OFFSET( I ) = 0.0D0
         MATRIX( NDIM*( I - 1 ) + PERM( I ) ) = 1.0D0
      END DO

*  Propagate the WCS component.
      CALL KPG1_ASPRP( NDIM, INDF1, INDF2, MATRIX, OFFSET, STATUS )

*  We now need to permute axes in all the WCS Frames (except the Base
*  GRID Frame).  To do this make each Frame in the frameSet current in
*  turn and then permute the axes of the FrameSet using AST_PERMAXES.
*  Doing it this way (rather than simply getting a pointer to each Frame
*  and using AST_PERMAXES on the Frame) is better because the FrameSet
*  class includes code for automatically modifying the Mappings in the
*  FrameSet to take account of any changes in the Frames properties
*  ("integrity checking").  If we used AST_PERMAXES on the Frame instead
*  of the FrameSet, we would have to manually remap each frame using a
*  PermMap.

*  Get the output WCS FrameSet.
      CALL KPG1_GTWCS( INDF2, IWCS, STATUS )

*  Save the original index of the current and base Frames.
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )
      IBASE = AST_GETI( IWCS, 'BASE', STATUS )

*  Pad the permutation array with values which leave any surplus WCS
*  axes unchanged.
      DO I = NDIM + 1, NDF__MXDIM
         PERM( I ) = I
      END DO

*  Find the lowest number of axes allowed in a WCS Frame for the
*  permutation to be possible. This is just equal to the index of the
*  highest changed pixel axis.
      MINAX = 0
      DO I = 1, NDIM
         IF( PERM( I ) .NE. I ) THEN
            MINAX = I
         END IF
      END DO

*  Loop round each Frame in the FrameSet, ignoring the Base Frame.
      DO I = 1, AST_GETI ( IWCS, 'NFRAME', STATUS )
         IF( I .NE. IBASE ) THEN

*  Make this Frame Current.
            CALL AST_SETI( IWCS, 'CURRENT', I, STATUS )

*  Get the number of axes in the Frame.
            NAX = AST_GETI( IWCS, 'NOUT', STATUS )

*  If there not enough WCS axes, issue a warning and do not permute the
*  axes.
            IF( NAX .LT. MINAX ) THEN
               DOM = AST_GETC( IWCS, 'DOMAIN', STATUS )
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETI( 'N', NAX )
               IF( DOM .NE. ' ' ) THEN
                  CALL MSG_SETC( 'D', DOM )
                  CALL MSG_OUT( 'PERMAXES_MSG1', 'WCS frame number ^I'//
     :                          '(^D) has only ^N axes and so cannot '//
     :                          'be permuted.', STATUS )
               ELSE
                  CALL MSG_OUT( 'PERMAXES_MSG2', 'WCS frame number ^I'//
     :                          'has only ^N axes and so cannot be '//
     :                          'permuted.', STATUS )
               END IF

*  Otherwise, permute the order of the axes in the current Frame.
            ELSE
               CALL AST_PERMAXES( IWCS, PERM, STATUS )
            END IF

         END IF
      END DO

*  Re-instate the original Current Frame.
      CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Store the new output WCS FrameSet.
      CALL NDF_PTWCS( IWCS, INDF2, STATUS )

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', INDF2, 'Title', STATUS )

 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PERMAXES_ERR', 'PERMAXES: Error permuting the'//
     :                 ' pixels axes of an NDF.', STATUS )
      END IF

      END
