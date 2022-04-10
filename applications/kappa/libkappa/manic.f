      SUBROUTINE MANIC( STATUS )
*+
*  Name:
*     MANIC

*  Purpose:
*     Change the dimensionality of all or part of an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MANIC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application manipulates the dimensionality of an NDF.  The
*     input NDF can be projected on to any n-dimensional surface (line,
*     plane, etc.) by averaging or taking the median the pixels in
*     perpendicular directions, or grown into new dimensions by
*     duplicating an existing n-dimensional surface.  The order of the
*     axes can also be changed at the same time.  Any combination of
*     these operations is also possible.
*
*     The shape of the output NDF is specified using parameter AXES.
*     This is a list of integers, each element of which identifies the
*     source of the corresponding axis of the output---either the index
*     of one ofthe pixel axes of the input, or a zero indicating that
*     the input should be expanded with copies of itself along that
*     axis.   If any axis of the input NDF is not referenced in the AXES
*     list, the missing dimensions will be collapsed to form the
*     resulting data.  Dimensions are collapsed by averaging all the
*     non-bad pixels along the relevant pixel axis (or axes).

*  Usage:
*     manic in out axes

*  ADAM Parameters:
*     AXES( ) = _INTEGER (Read)
*        An array of integers which define the pixel axes of the output
*        NDF.  The array should contain one value for each pixel axis in
*        the output NDF.  Each value can be either a positive integer or
*        zero.  If positive, it is taken to be the index of a pixel axis
*        within the input NDF which is to be used as the output axis.
*        If zero, the output axis will be formed by replicating the
*        entire output NDF a specified number of times (see parameters
*        LBOUND and UBOUND).  At least one non-zero value must appear
*        in the list, and no input axis may be used more than once.
*     ESTIMATOR = LITERAL (Read)
*        The method by which data values in collapsed axes are combined.
*        The permittted options are "Mean" to form the average, or
*         "Median" to use the median.  ["Mean"]
*     IN = NDF (Read)
*        The input NDF.
*     LBOUND( ) = _INTEGER (Read)
*        An array holding the lower pixel bounds of any new axes in the
*        output NDF (that is, output axes which have a zero value in the
*        corresponding element of the AXES parameter).  One element must
*        be given for each zero-valued element within AXES, in order of
*        appearance within AXES.  The dynamic default is to use 1 for
*        every element.  []
*     OUT = NDF (Write)
*        The output NDF.
*     TITLE = LITERAL (Read)
*        Title for the output NDF.  A null (!) means use the title from
*        the input NDF.  [!]
*     UBOUND( ) = _INTEGER (Read)
*        An array holding the upper pixel bounds of any new axes in the
*        output NDF (that is, output axes which have a zero value in the
*        corresponding element of the AXES parameter).  One element must
*        be given for each zero-valued element within AXES, in order of
*        appearance within AXES.  The dynamic default is to use 1 for
*        every element.  []

*  Examples:
*     manic image transim [2,1]
*        This transposes the two-dimensional NDF image so that its X
*        pixel co-ordinates are in the Y direction and vice versa.  The
*        ordering of the axes within the current WCS Frame will only be
*        changed if the Domain of the current Frame is PIXEL or AXES.
*        For instance, if the current Frame has Domain "SKY", with axis
*        1 being RA and axis 2 being DEC, then these will be unchanged
*        in the output NDF.  However, the Mapping which is used to
*        relate (RA,DEC) positions to pixel positions will be modified
*        to take the permutation of the pixel axes into account.
*     manic cube summ 3
*        This creates a one-dimensional output NDF called summ, in which
*        the single pixel axis corresponds to the Z (third) axis in an
*        input NDF called (cube).  Each element in the output is equal
*        to the average data value in the corresponding XY plane of the
*        input.
*     manic in=cube out=summ axis=3 estimator=median
*        The same as the previous example, except each output value is 
*        equal to the median data value in the corresponding XY plane of
*        the input cube.
*     manic line plane [0,1] lbound=1 ubound=25
*        This takes a one-dimensional NDF called line and expands it
*        into a two-dimensional NDF called plane.  The second pixel axis
*        of the output NDF corresponds to the first (and only) pixel
*        axis in the input NDF.  The first pixel axes of the output is
*        formed by replicating the the input NDF 25 times.
*     manic line plane [1,0] lbound=1 ubound=25
*        This does the same as the last example except that the output
*        NDF is transposed.  That is, the input NDF is copied into the
*        output NDF so that it is parallel to pixel axis 1 (X) in the
*        output NDF, instead of pixel axis 2 (Y) as before.
*     manic cube hyper [1,0,0,0,0,0,3] ubound=[2,4,2,2,1] accept
*        This manic example projects the second dimension of an input
*        three-dimensional NDF on to the plane formed by its first and
*        third dimensions by averaging, and grows the resulting plane
*        up through five new dimensions with a variety of extents.

*  Notes:
*     - This application permutes the NDF pixel axes, and any associated
*     AXIS structures.  It does not change the axes of the current WCS
*     co-ordinate Frame, either by permuting, adding or deleting, unless
*     that frame has Domain "PIXEL" or "AXES".  See the first example
*     in the "Examples" section.

*  Related Applications:
*     KAPPA: COLLAPSE, PERMAXES.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     LABEL, TITLE, UNITS, WCS, and HISTORY components of the input NDF
*     and propagates all extensions.  QUALITY is also propagated if
*     possible (i.e. if no axes are collapsed).
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported, up to a maximum of
*     7.

*  Copyright:
*     Copyright (C) 2001-2002, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2012, 2022 Science & Technology Facilities Council.
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
*     MBT: Mark Taylor (STARLINK)
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     8-NOV-2001 (MBT):
*        Original version (parts were heavily informed by COLLAPSE).
*     23-NOV-2001 (DSB):
*        Minor mods to the prologue.  Check that UBOUND values are not
*        less than LBOUND values.
*     11-JAN-2002 (DSB):
*        Added "Implementation Status" to prologue.  Correct the
*        annullment of LOC5.  Changed TITLE default to null.
*     12-JAN-2004 (DSB):
*        Changed logic used for copying AXIS structures form input to
*        output, because the old system did not take account of the fact
*        that HDS locators for NDF components can become incorrect if
*        the NDF is changed (because certain NDF calls create new HDS
*        objects).
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     2022 April 10 (MJC):
*        Add ESTIMATOR parameter to permit collapsing using the median.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'         ! Global SSE definitions
      INCLUDE  'AST_PAR'         ! AST constants and functions
      INCLUDE  'NDF_PAR'         ! NDF constants
      INCLUDE  'DAT_PAR'         ! HDS system constants
      INCLUDE  'PRM_PAR'         ! VAL constants
      INCLUDE  'CNF_PAR'         ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER  ESTIM*( 6 )     ! Combination method, collapsed axes 
      CHARACTER ITYPE*( NDF__SZTYP )! Numeric type of NDF data component
      CHARACTER LOC1*( DAT__SZLOC ) ! Locator to the whole NDF
      CHARACTER LOC2*( DAT__SZLOC ) ! Locator to NDF AXIS array
      CHARACTER LOC3*( DAT__SZLOC ) ! Locator to copy of original AXIS
                                 ! array
      CHARACTER LOC4*( DAT__SZLOC ) ! Locator to a cell of the new AXIS
                                 ! array
      CHARACTER LOC5*( DAT__SZLOC ) ! Locator to a cell of the old AXIS
                                 ! array
      CHARACTER LOC6*( DAT__SZLOC ) ! Locator to a component of the old
                                 ! cell
      CHARACTER NAME*( DAT__SZNAM ) ! HDS component name
      INTEGER AXES( NDF__MXDIM ) ! Axis indices forming the output NDF
      INTEGER BFRM               ! AST pointer to Base Frame in WCS
                                 ! FrameSet
      INTEGER DFLS( NDF__MXDIM ) ! Default lower bounds
      INTEGER DIMI( NDF__MXDIM ) ! Dimensions of input NDF
      INTEGER DIMO( NDF__MXDIM ) ! Dimensions of output NDF
      INTEGER DUMMY              ! Pointer to dummy work array
      INTEGER I                  ! Loop variable
      INTEGER IBASE              ! Index of Base Frame in WCS FrameSet
      INTEGER ICURR              ! Index of Current Frame in WCS
                                 ! FrameSet
      INTEGER INDF1              ! NDF identifier of input NDF
      INTEGER INDF2              ! NDF identifier of output NDF
      INTEGER INEW               ! Index of the new Base Frame in WCS
                                 ! FrameSet
      INTEGER INPRM( NDF__MXDIM )! Output axis assigned to each i/p axis
      INTEGER IPDI               ! Pointer to mapped input data array
      INTEGER IPDO               ! Pointer to mapped output data array
      INTEGER IPDUM              ! Pointer to dummy mapped data
      INTEGER IPQI               ! Pointer to mapped input quality array
      INTEGER IPQO               ! Pointer to mapped o/p quality array
      INTEGER IPVI               ! Pointer to mapped i/p variance array
      INTEGER IPVO               ! Pointer to mapped output variance
                                 ! array
      INTEGER IPWKC              ! Pointer to workspace
      INTEGER IPWKE              ! Pointer to workspace
      INTEGER IPWKP              ! Pointer to workspace
      INTEGER IWCS               ! AST pointer to WCS FrameSet of NDF
      INTEGER J                  ! Loop variable
      INTEGER LBNDI( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      INTEGER MAXS( NDF__MXDIM ) ! Max. legal values for AXES elements
      INTEGER MAXUB( NDF__MXDIM )! Max. legal values for UBOUND elements
      INTEGER MINS( NDF__MXDIM ) ! Min. legal values for AXES elements
      INTEGER NBFRM              ! AST pointer to new Base Frame
      INTEGER NCOMP              ! Number of HDS components
      INTEGER NDIMI              ! Dimensionality of input NDF
      INTEGER NDIMO              ! Dimensionality of output NDF
      INTEGER NEL                ! Number of elements
      INTEGER NEWLB( NDF__MXDIM ) ! Lower bounds of new dimensions
      INTEGER NEWUB( NDF__MXDIM ) ! Upper bounds of new dimensions
      INTEGER NEXPAN             ! Number of new dimensions in output
                                 ! array
      INTEGER NUSED              ! Number of input array axes used in
                                 ! output array
      INTEGER NWKC               ! Number of in pixels collapsing on
                                 ! each out pixel
      INTEGER NWKE               ! Number of copied out pixels for each
                                 ! unique one
      INTEGER OUTPRM( NDF__MXDIM ) ! Input axis assigned to each output
                                 ! axis
      INTEGER PMAP               ! AST pointer to PermMap between Base
                                 ! frames
      INTEGER UBNDI( NDF__MXDIM ) ! Upper bounds of input NDF
      INTEGER UBNDO( NDF__MXDIM ) ! Upper bounds of output NDF
      LOGICAL GOTAX              ! Does an AXIS component exist?
      LOGICAL GOTQUL             ! Does a QUALITY component exist?
      LOGICAL GOTVAR             ! Does a VARIANCE component exist?
      LOGICAL USED( NDF__MXDIM ) ! Is input array axis used in output
                                 ! array?
*.

*  Check the global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Get the bounds of the NDF.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBNDI, UBNDI, NDIMI, STATUS )

*  Get the list of axes which is to form the output NDF.  Make sure
*  the parameter system will only return values in a meaningful range
*  (1..NDIMI).
      CALL KPG1_FILLI( 0, NDF__MXDIM, MINS, STATUS )
      CALL KPG1_FILLI( NDIMI, NDF__MXDIM, MAXS, STATUS )
      CALL PAR_GRMVI( 'AXES', NDF__MXDIM, MINS, MAXS, AXES, NDIMO,
     :                STATUS )

*  Obtain the method to use for estimating the pixel values from
*  collapsed axes.
      CALL PAR_CHOIC( 'ESTIMATOR', 'Mean', 'Mean,Median',  .TRUE.,
     :                ESTIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  See which dimensions of the input array are used in the output array
*  (i.e. are not to be collapsed).
      DO I = 1, NDIMI
         USED( I ) = .FALSE.
      END DO

      NUSED = 0
      DO I = 1, NDIMO
         IF ( AXES( I ) .GT. 0 ) THEN
            IF ( USED( AXES( I ) ) ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'MANIC_ERR1', 'MANIC: Axis numbers '//
     :                       'cannot be used more than once each.',
     :                       STATUS )
               GO TO 999
            END IF
            USED( AXES( I ) ) = .TRUE.
            NUSED = NUSED + 1
         END IF
      END DO

*  Check that the requested action is reasonable.
      IF ( NUSED .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'MANIC_ERR2', 'MANIC: At least one dimension '//
     :                 'must be copied from the input NDF.', STATUS )
         GO TO 999
      END IF

*  Get the extent of expanded dimensions if any are required.
      NEXPAN = NDIMO - NUSED
      IF ( NEXPAN .GT. 0 ) THEN

*  Set the default for the lower bounds dynamically to all 1s.
         CALL KPG1_FILLI( 1, NEXPAN, DFLS, STATUS )
         CALL PAR_DEF1I( 'LBOUND', NEXPAN, DFLS, STATUS )

*  Store maximum values for the upper bound (VAL__MAXI).
         CALL KPG1_FILLI( VAL__MAXI, NEXPAN, MAXUB, STATUS )

*  Get the values from the environment.
         CALL PAR_EXACI( 'LBOUND', NEXPAN, NEWLB, STATUS )
         CALL PAR_GRM1I( 'UBOUND', NEXPAN, NEWLB, NEWLB, MAXUB, .FALSE.,
     :                   NEWUB, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set the upper and lower bounds of the output NDF.
      INEW = 0
      DO I = 1, NDIMO
         IF ( AXES( I ) .EQ. 0 ) THEN
            INEW = INEW + 1
            LBNDO( I ) = NEWLB( INEW )
            UBNDO( I ) = NEWUB( INEW )
         ELSE
            LBNDO( I ) = LBNDI( AXES( I ) )
            UBNDO( I ) = UBNDI( AXES( I ) )
         END IF
      END DO

*  Create the output NDF by propagation from the input NDF.  This
*  results in history, etc. being passed on.  The shape and
*  dimensionality will be wrong but this will be corrected later.
      CALL LPG_PROP( INDF1, 'Axis,Units', 'OUT', INDF2, STATUS )

*  Set the title of the output NDF.
      CALL NDF_CINP( 'TITLE', INDF2, 'TITLE', STATUS )

*  The shape and size of the output NDF created above will be wrong, so
*  we need to correct it by removing the collapse axis.  This is easy if
*  it is the final axis (we would just use NDF_SBND with specifying
*  NDIM-1 axes), but is not so easy if the collapse axis is not the
*  final axis.  In this case, we do the following:
*    1) - Save copies of an AXIS structures in the output NDF (because
*         the following step will change their lengths to match the new
*         bounds).
*    2) - Change the bounds and dimensionality of the NDF to the
*         appropriate values.
*    3) - Restore the saved AXIS structures, permuting them so that they
*         apply to the correct axis.
*    4) - Adjust the WCS FrameSet to pick the required axis from the
*         original Base Frame.

*  First see if the AXIS component is defined.
      CALL NDF_STATE( INDF2, 'AXIS', GOTAX, STATUS )

*  If so, we need to save copies of the AXIS structures.
      IF ( GOTAX ) THEN

*  Get an HDS locator to the NDF structure.
         CALL NDF_LOC( INDF2, 'UPDATE', LOC1, STATUS )

*  Get a locator for the AXIS component.
         CALL DAT_FIND( LOC1, 'AXIS', LOC2, STATUS )

*  Take a copy of the AXIS component and call it OLDAXIS.
         CALL DAT_COPY( LOC2, LOC1, 'OLDAXIS', STATUS )

*  Annul the other locators since the following call to NDF_SBND will
*  create new HDS objects.
         CALL DAT_ANNUL( LOC1, STATUS )
         CALL DAT_ANNUL( LOC2, STATUS )

      END IF

*  Set the output NDF bounds to the required values.  This will change
*  the lengths of the current AXIS arrays.
      CALL NDF_SBND( NDIMO, LBNDO, UBNDO, INDF2, STATUS )

*  We now reinstate any AXIS structures, in their new order.
      IF ( GOTAX ) THEN

*  First erase the axis structures inherited from the input NDF, and
*  then create default axis components for each dimension of the new
*  NDF.  Some of these will be overwritten with new values, but without
*  this step the ones corresponding to new dimensions would be missing.
         CALL NDF_RESET( INDF2, 'AXIS', STATUS )
         CALL NDF_ACRE( INDF2, STATUS )

*  Now get an HDS locator to the modified NDF structure.
         CALL NDF_LOC( INDF2, 'UPDATE', LOC1, STATUS )

*  And get a locator for the default AXIS component created above.
         CALL DAT_FIND( LOC1, 'AXIS', LOC2, STATUS )

*  Get a locator for the OLDAXIS component added above.
         CALL DAT_FIND( LOC1, 'OLDAXIS', LOC3, STATUS )

*  Promote the NDF locator to a primary locator so that the HDS
*  container file is not closed when the NDF identifier is annulled.
         CALL DAT_PRMRY( .TRUE., LOC1, .TRUE., STATUS )

*  The DATA array of the output NDF will not yet be in a defined state.
*  This would result in NDF_ANNUL reporting an error, so we temporarily
*  map the DATA array (which puts it in a defined state) to prevent
*  this.  (Shame--this is potentially, though perhaps not usually,
*  expensive).
         CALL NDF_TYPE( INDF2, 'DATA', ITYPE, STATUS )
         CALL NDF_MAP( INDF2, 'DATA', ITYPE, 'WRITE', IPDUM, NEL,
     :                 STATUS )

*  Annul the supplied NDF identifier so that we can change the contents
*  of the NDF using HDS, without getting out of step with the NDF
*  library's description of the NDF.
         CALL NDF_ANNUL( INDF2, STATUS )

*  Loop round each cell in the returned AXIS structure.
         DO I = 1, NDIMO

*  If this is a copy of an axis from the input NDF, copy the OLDAXIS
*  components into it.
            IF ( AXES( I ) .GT. 0 ) THEN

*  Get a locator to this cell in the NDF's AXIS array.
               CALL DAT_CELL( LOC2, 1, I, LOC4, STATUS )

*  Empty it of any components.
               CALL DAT_NCOMP( LOC4, NCOMP, STATUS )
               DO J = NCOMP, 1, -1
                  CALL DAT_INDEX( LOC4, J, LOC5, STATUS )
                  CALL DAT_NAME( LOC5, NAME, STATUS )
                  CALL DAT_ANNUL( LOC5, STATUS )
                  CALL DAT_ERASE( LOC4, NAME, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GO TO 999
               END DO

*  Get a locator to the corresponding cell in the OLDAXIS array.
               CALL DAT_CELL( LOC3, 1, AXES( I ), LOC5, STATUS )

*  Now copy all the components of the OLDAXIS cell into the AXIS cell.
*  Find the number of components, and loop round them.
               CALL DAT_NCOMP( LOC5, NCOMP, STATUS )
               DO J = NCOMP, 1, -1

*  Get a locator to this component in the original OLDAXIS cell.
                  CALL DAT_INDEX( LOC5, J, LOC6, STATUS )

*  Get its name.
                  CALL DAT_NAME( LOC6, NAME, STATUS )

*  Copy it into the new AXIS structure.
                  CALL DAT_COPY( LOC6, LOC4, NAME, STATUS )

*  Annul the locators.
                  CALL DAT_ANNUL( LOC6, STATUS )

*  Abort if an error has occurred.
                  IF ( STATUS .NE. SAI__OK ) GO TO 999
               END DO

*  Annul the locators.
               CALL DAT_ANNUL( LOC4, STATUS )
               CALL DAT_ANNUL( LOC5, STATUS )
            END IF
         END DO

*  Annul the locator to the OLDAXIS structure and then erase the object.
         CALL DAT_ANNUL( LOC3, STATUS )
         CALL DAT_ERASE( LOC1, 'OLDAXIS', STATUS )

*  Annul the AXIS array locator.
         CALL DAT_ANNUL( LOC2, STATUS )

*  Import the modified NDF back into the NDF system.
         CALL NDF_FIND( LOC1, ' ', INDF2, STATUS )

*  Annul the NDF locator.
         CALL DAT_ANNUL( LOC1, STATUS )
      END IF

*  Now to store a suitable WCS frameset in the output NDF.  This will be
*  a copy of the input NDF's WCS FrameSet but with the Base frame
*  modified according to the AXES argument.

*  Get the WCS FrameSet from the input NDF.
      CALL KPG1_GTWCS( INDF1, IWCS, STATUS )

*  Get its Base frame.
      BFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )

*  Get the index of its Current frame, since this will be disturbed by
*  what we are about to do and we must restore it.
      IBASE = AST_GETI( IWCS, 'BASE', STATUS )
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Create a new base frame by picking axes from the original base frame.
*  The format of the AXES array required for AST_PICKAXES is, happily,
*  the same as used by this program.  This also creates a new PermMap
*  which goes from the original Base frame to the new one.  However,
*  this PermMap assigns bad values to new axes, which will cause many
*  WCS applications to fail.  We really want a PermMap which assigns
*  suitable contant pixel values to any new axes.
      NBFRM = AST_PICKAXES( BFRM, NDIMO, AXES, PMAP, STATUS )

*  Unfortunately, the PermMap created by AST_PICKAXES assigns bad values
*  to any new or missing axes, which will cause many WCS applications to
*  fail.  We really want a PermMap which assigns suitable constant pixel
*  values to any new or missing axes.  We create a better PermMap now.
*  The AXES parameter corresponds to the OUTPERM argument of the PermMap
*  constructor.  Form a copy in which zero values are changed to -1 (so
*  that they will be assigned the constant value (1.0) supplied to
*  AST_PERMMAP, instead of AST__BAD).  Also form the corresponding
*  INPERM array at the same time.
      DO I = 1, NDIMI
         INPRM( I ) = -1
      END DO

      DO I = 1, NDIMO
         IF( AXES( I ) .GT. 0 ) THEN
            OUTPRM( I ) = AXES( I )
            IF( AXES( I ) .LE. NDIMI ) INPRM( AXES( I ) ) = I
         ELSE
            OUTPRM( I ) = -1
         END IF
      END DO

*  Now create the PermMap assigning the constant value 1.0 to any unused
*  axes (1.0 is the GRID value of the first element on the pixel axis).
      PMAP = AST_PERMMAP( NDIMI, INPRM, NDIMO, OUTPRM, 1.0D0, ' ',
     :                    STATUS )

*  Now add this Frame into the FrameSet, using the PermMap created
*  above.
      CALL AST_ADDFRAME( IWCS, AST__BASE, PMAP, NBFRM, STATUS )

*  Get the frame index of the newly created frame.
      INEW = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Reinstate the original Current Frame.
      CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Make the new frame the Base Frame, and remove the old Base Frame.
      CALL AST_SETI( IWCS, 'BASE', INEW, STATUS )
      CALL AST_REMOVEFRAME( IWCS, IBASE, STATUS )

*  Save the modified WCS FrameSet in the output NDF.
      CALL NDF_PTWCS( IWCS, INDF2, STATUS )

*  Now do the main part of the work: reshape and transfer the data
*  array from the input NDF to the output one.

*  Get the dimensions of the input and output NDFs in a convenient form.
      CALL NDF_DIM( INDF1, NDF__MXDIM, DIMI, NDIMI, STATUS )
      CALL NDF_DIM( INDF2, NDF__MXDIM, DIMO, NDIMO, STATUS )

*  We need to calculate some numbers for allocating workspace.  Find
*  out how many pixels in the input array collapse on to each pixel in
*  the output array.
      NWKC = 1
      DO I = 1, NDIMI
         IF ( .NOT. USED( I ) ) THEN
            NWKC = NWKC * DIMI( I )
         END IF
      END DO

*  Find out how many pixels in the output array correspond to each
*  unique one.
      NWKE = 1
      DO I = 1, NDIMO
         IF ( AXES( I ) .EQ. 0 ) THEN
            NWKE = NWKE * DIMO( I )
         END IF
      END DO

*  Get the data type of the NDF's DATA component.
      CALL NDF_TYPE( INDF1, 'DATA', ITYPE, STATUS )

*  Map the data arrays of the input and output NDFs.
      CALL NDF_MAP( INDF1, 'DATA', ITYPE, 'READ', IPDI, NEL, STATUS )
      CALL NDF_MAP( INDF2, 'DATA', ITYPE, 'WRITE', IPDO, NEL, STATUS )

*  Allocate workspace.
      CALL PSX_CALLOC( NWKC, '_INTEGER', IPWKC, STATUS )
      CALL PSX_CALLOC( NWKE, '_INTEGER', IPWKE, STATUS )
      CALL PSX_CALLOC( NWKC, ITYPE, IPWKP, STATUS )

*  Do the data pixel copying.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_MANIB( NDIMI, DIMI, %VAL( CNF_PVAL( IPDI ) ),
     :                    NDIMO, DIMO,
     :                    AXES, ESTIM, %VAL( CNF_PVAL( IPWKC ) ),
     :                    %VAL( CNF_PVAL( IPWKE ) ),
     :                    %VAL( CNF_PVAL( IPWKP ) ),
     :                    %VAL( CNF_PVAL( IPDO ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_MANIUB( NDIMI, DIMI, %VAL( CNF_PVAL( IPDI ) ),
     :                     NDIMO, DIMO,
     :                     AXES, ESTIM, %VAL( CNF_PVAL( IPWKC ) ),
     :                     %VAL( CNF_PVAL( IPWKE ) ),
     :                     %VAL( CNF_PVAL( IPWKP ) ),
     :                     %VAL( CNF_PVAL( IPDO ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPG1_MANIW( NDIMI, DIMI, %VAL( CNF_PVAL( IPDI ) ),
     :                    NDIMO, DIMO,
     :                    AXES, ESTIM, %VAL( CNF_PVAL( IPWKC ) ),
     :                    %VAL( CNF_PVAL( IPWKE ) ),
     :                    %VAL( CNF_PVAL( IPWKP ) ),
     :                    %VAL( CNF_PVAL( IPDO ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_MANIUW( NDIMI, DIMI, %VAL( CNF_PVAL( IPDI ) ),
     :                     NDIMO, DIMO,
     :                     AXES, ESTIM, %VAL( CNF_PVAL( IPWKC ) ),
     :                     %VAL( CNF_PVAL( IPWKE ) ),
     :                     %VAL( CNF_PVAL( IPWKP ) ),
     :                     %VAL( CNF_PVAL( IPDO ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_MANII( NDIMI, DIMI, %VAL( CNF_PVAL( IPDI ) ),
     :                    NDIMO, DIMO,
     :                    AXES, ESTIM, %VAL( CNF_PVAL( IPWKC ) ),
     :                    %VAL( CNF_PVAL( IPWKE ) ),
     :                    %VAL( CNF_PVAL( IPWKP ) ),
     :                    %VAL( CNF_PVAL( IPDO ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         CALL KPG1_MANIK( NDIMI, DIMI, %VAL( CNF_PVAL( IPDI ) ),
     :                    NDIMO, DIMO,
     :                    AXES, ESTIM, %VAL( CNF_PVAL( IPWKC ) ),
     :                    %VAL( CNF_PVAL( IPWKE ) ),
     :                    %VAL( CNF_PVAL( IPWKP ) ),
     :                    %VAL( CNF_PVAL( IPDO ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_MANIR( NDIMI, DIMI, %VAL( CNF_PVAL( IPDI ) ),
     :                    NDIMO, DIMO,
     :                    AXES, ESTIM, %VAL( CNF_PVAL( IPWKC ) ),
     :                    %VAL( CNF_PVAL( IPWKE ) ),
     :                    %VAL( CNF_PVAL( IPWKP ) ),
     :                    %VAL( CNF_PVAL( IPDO ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_MANID( NDIMI, DIMI, %VAL( CNF_PVAL( IPDI ) ),
     :                    NDIMO, DIMO,
     :                    AXES, ESTIM, %VAL( CNF_PVAL( IPWKC ) ),
     :                    %VAL( CNF_PVAL( IPWKE ) ),
     :                    %VAL( CNF_PVAL( IPWKP ) ),
     :                    %VAL( CNF_PVAL( IPDO ) ), STATUS )
      END IF

*  Unmap the data arrays.
      CALL NDF_UNMAP( INDF1, 'DATA', STATUS )
      CALL NDF_UNMAP( INDF2, 'DATA', STATUS )

*  If necessary do the same for the variance array.
      CALL NDF_STATE( INDF1, 'VARIANCE', GOTVAR, STATUS )
      IF ( GOTVAR ) THEN

*  Get the data type of the NDF's VARIANCE component.
         CALL NDF_TYPE( INDF1, 'VARIANCE', ITYPE, STATUS )

*  Map the variance arrays of the input and output NDFs.
         CALL NDF_MAP( INDF1, 'VARIANCE', ITYPE, 'READ', IPVI, NEL,
     :                 STATUS )
         CALL NDF_MAP( INDF2, 'VARIANCE', ITYPE, 'WRITE', IPVO, NEL,
     :                 STATUS )

*  Do the variance pixel copying.
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_MANIB( NDIMI, DIMI, %VAL( CNF_PVAL( IPVI ) ),
     :                       NDIMO, DIMO,
     :                       AXES, 'MEAN', %VAL( CNF_PVAL( IPWKC ) ),
     :                       %VAL( CNF_PVAL( IPWKE ) ),
     :                       %VAL( CNF_PVAL( IPWKP ) ),
     :                       %VAL( CNF_PVAL( IPVO ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_MANIUB( NDIMI, DIMI, %VAL( CNF_PVAL( IPVI ) ),
     :                        NDIMO, DIMO,
     :                        AXES, 'MEAN', %VAL( CNF_PVAL( IPWKC ) ),
     :                        %VAL( CNF_PVAL( IPWKE ) ),
     :                        %VAL( CNF_PVAL( IPWKP ) ),
     :                        %VAL( CNF_PVAL( IPVO ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_MANIW( NDIMI, DIMI, %VAL( CNF_PVAL( IPVI ) ),
     :                       NDIMO, DIMO,
     :                       AXES, 'MEAN', %VAL( CNF_PVAL( IPWKC ) ),
     :                       %VAL( CNF_PVAL( IPWKE ) ),
     :                       %VAL( CNF_PVAL( IPWKP ) ),
     :                       %VAL( CNF_PVAL( IPVO ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_MANIUW( NDIMI, DIMI, %VAL( CNF_PVAL( IPVI ) ),
     :                        NDIMO, DIMO,
     :                        AXES, 'MEAN', %VAL( CNF_PVAL( IPWKC ) ),
     :                        %VAL( CNF_PVAL( IPWKE ) ),
     :                        %VAL( CNF_PVAL( IPWKP ) ),
     :                        %VAL( CNF_PVAL( IPVO ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_MANII( NDIMI, DIMI, %VAL( CNF_PVAL( IPVI ) ),
     :                       NDIMO, DIMO,
     :                       AXES, 'MEAN', %VAL( CNF_PVAL( IPWKC ) ),
     :                       %VAL( CNF_PVAL( IPWKE ) ),
     :                       %VAL( CNF_PVAL( IPWKP ) ),
     :                       %VAL( CNF_PVAL( IPVO ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL KPG1_MANIK( NDIMI, DIMI, %VAL( CNF_PVAL( IPVI ) ),
     :                       NDIMO, DIMO,
     :                       AXES, 'MEAN', %VAL( CNF_PVAL( IPWKC ) ),
     :                       %VAL( CNF_PVAL( IPWKE ) ),
     :                       %VAL( CNF_PVAL( IPWKP ) ),
     :                       %VAL( CNF_PVAL( IPVO ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_MANIR( NDIMI, DIMI, %VAL( CNF_PVAL( IPVI ) ),
     :                       NDIMO, DIMO,
     :                       AXES, 'MEAN', %VAL( CNF_PVAL( IPWKC ) ),
     :                       %VAL( CNF_PVAL( IPWKE ) ),
     :                       %VAL( CNF_PVAL( IPWKP ) ),
     :                       %VAL( CNF_PVAL( IPVO ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_MANID( NDIMI, DIMI, %VAL( CNF_PVAL( IPVI ) ),
     :                       NDIMO, DIMO,
     :                       AXES, 'MEAN', %VAL( CNF_PVAL( IPWKC ) ),
     :                       %VAL( CNF_PVAL( IPWKE ) ),
     :                       %VAL( CNF_PVAL( IPWKP ) ),
     :                       %VAL( CNF_PVAL( IPVO ) ), STATUS )
         END IF

*  Unmap the variance arrays.
         CALL NDF_UNMAP( INDF1, 'VARIANCE', STATUS )
         CALL NDF_UNMAP( INDF2, 'VARIANCE', STATUS )
      END IF

*  If necessary do the same for the quality array.  This is only
*  possible if no axes are being collapsed.
      CALL NDF_STATE( INDF1, 'QUALITY', GOTQUL, STATUS )
      IF ( GOTQUL .AND. NUSED .EQ. NDIMI ) THEN

*  Map the variance arrays of the input and output NDFs.  Quality values
*  are always unsigned bytes.
         CALL NDF_MAP( INDF1, 'QUALITY', '_UBYTE', 'READ', IPQI, NEL,
     :                 STATUS )
         CALL NDF_MAP( INDF2, 'QUALITY', '_UBYTE', 'WRITE', IPQO, NEL,
     :                 STATUS )

*  Do the quality pixel copying.  Use a dummy argument for the unused
*  pixel work array.
         CALL KPG1_MANIUB( NDIMI, DIMI, %VAL( CNF_PVAL( IPQI ) ),
     :                     NDIMO, DIMO,
     :                     AXES, 'MEAN', %VAL( CNF_PVAL( IPWKC ) ),
     :                     %VAL( CNF_PVAL( IPWKE ) ), DUMMY,
     :                     %VAL( CNF_PVAL( IPQO ) ), STATUS )

*  Unmap the quality arrays.
         CALL NDF_UNMAP( INDF1, 'QUALITY', STATUS )
         CALL NDF_UNMAP( INDF2, 'QUALITY', STATUS )
      END IF

*  Release the workspace.
      CALL PSX_FREE( IPWKC, STATUS )
      CALL PSX_FREE( IPWKE, STATUS )
      CALL PSX_FREE( IPWKP, STATUS )

*  Come here if something has gone wrong.
  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Report a contextual message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MANIC_ERR3', 'MANIC: Unable to convert NDF.',
     :                 STATUS )
      END IF

      END
