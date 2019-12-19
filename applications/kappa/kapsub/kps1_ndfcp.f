      SUBROUTINE KPS1_NDFCP( INDF1, COMP, TRIM, TRMWCS, PARAM, OPLACE,
     :                       INDF2, STATUS )
*+
*  Name:
*     KPS1_NDFCP

*  Purpose:
*     Copies an NDF section to a new location.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_NDFCP( INDF1, TRIM, TRMWCS, PARAM, OPLACE, INDF2,
*                      STATUS )

*  Description:
*     This routine copies a given NDF (or NDF section) to a new
*     location.  It performs most of the work for the NDFCOPY
*     application.
*
*     It also permits the VARIANCE, ERROR or QUALITY components to
*     become the DATA_ARRAY in the output NDF (see the COMP argument),
*     thus losing the original DATA_ARRAY and omitting the respective
*     VARIANCE or QUALITY too.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the input NDF.
*     COMP = CHARACTER * ( * ) (Given)
*        The array component to be the DATA_ARRAY in the output NDF.
*
*        If this is set to 'DATA' all the array components are copied to
*        their counterparts in the output NDF.

*        If this is set to 'VARIANCE' or 'QUALITY' the VARIANCE or
*        QUALITY component respectively becomes the new DATA_ARRAY in
*        the output NDF.  If it is set to 'ERROR' the square root
*        VARIANCE component becomes the new DATA_ARRAY.  The chosen
*        array component is absent from the output NDF.  The information
*        stored within the input DATA_ARRAY is not transferred.  Also
*        any variance is not propagated when 'QUALITY' is supplied, as
*        the variance applies to the DATA_ARRAY.
*     TRIM = LOGICAL (Given)
*        Should insignificant pixel axes be removed form the output NDF?
*     TRMWCS = LOGICAL (Given)
*        Should insignificant WCS axes be removed form the output NDF?
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of a parameter to use to get the output NDF name. If
*        this is blank, then OPLACE is used instead.
*     OPLACE = INTEGER (Given)
*        A place-holder for the output NDF. Only used if PARAM is blank.
*     INDF2 = INTEGER (Returned)
*        An identifier for the output NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Environment Parameters:
*     USEAXIS = LITERAL (Read)
*        This parameter is only accessed if TRIM and TRIMWCS are both
*        TRUE, and some axes need to be removed from the current WCS
*        Frame of the output NDF.  It gives the axes which are to be
*        retained in the current WCS Frame of the output NDF.  Each axis
*        can be specified using one of the following options.
*
*        - An integer index of an axis within the current Frame of the
*        input NDF (in the range 1 to the number of axes in the current
*        Frame).
*
*        - An axis symbol string such as "RA" or "VRAD".
*
*        - A generic option where "SPEC" requests the spectral axis,
*        "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the
*        sky longitude and latitude axes respectively.  Only those axis
*        domains present are available as options.

*        The dynamic default selects the axes with the same indices as
*        the pixel axes being copied.  The value should be given as a
*        comma-separated list.

*  Copyright:
*     Copyright (C) 2009, 2012 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-FEB-2009 (DSB):
*        Original version
*     2009 July 17 (MJC):
*        Permit VARIANCE and QUALITY to become the DATA_ARRAY via
*        a new COMP argument.
*     18-SEP-2009 (DSB):
*        Allow excess WCS axes to be trimmed.
*     2009-12-03 (TIMJ):
*        Allow COMP=ERROR.
*     2012 May 11 (MJC):
*        Add support for _INT64.
*     19-APR-2016 (DSB):
*        UTRIM is initialised to FALSE, so set it TRUE if there are WCS
*        axes to be trimmed (rather than setting it FALSE if there are
*        no WCS axes to be trimmed).
*     19-DEC-2019 (DSB):
*        Support huge NDFs.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'DAT_PAR'          ! HDS_ public constants
      INCLUDE 'AST_PAR'          ! AST functions and constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDF1
      CHARACTER COMP*(*)
      LOGICAL TRIM
      LOGICAL TRMWCS
      CHARACTER PARAM*(*)
      INTEGER OPLACE

*  Arguments Returned:
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER CLIST*35         ! List of components to propagate
      CHARACTER ICOMP( 3 )*(DAT__SZNAM) ! Input array-component names
      CHARACTER ICOMPN( 3 )*(DAT__SZNAM) ! Input array-component names for NDF STYPE
      CHARACTER LOC1*(DAT__SZLOC) ! Locator to the output NDF
      CHARACTER LOC2*(DAT__SZLOC) ! Locator to output AXIS array
      CHARACTER LOC2C*(DAT__SZLOC)! Loc. for a single o/p AXIS structure
      CHARACTER LOC3*(DAT__SZLOC) ! Locator to the input NDF
      CHARACTER LOC4*(DAT__SZLOC) ! Locator to input AXIS array
      CHARACTER LOC4C*(DAT__SZLOC)! Locator for a single i/p AXIS
                                  ! structure
      CHARACTER LOC5*(DAT__SZLOC)! Locator to AXIS component
      CHARACTER NAME*(DAT__SZNAM)! Name of AXIS component
      CHARACTER OCOMP( 3 )*(DAT__SZNAM) ! Output array-component names
      CHARACTER TTL*(AST__SZCHR) ! Frame title
      CHARACTER TYPE*(DAT__SZTYP)! Numerical type of array component
      INTEGER CAXES( NDF__MXDIM )! Non-degenerate current Frame axes
      INTEGER*8 EL               ! Number of elements in mapped array
      INTEGER*8 I                ! Loop index
      INTEGER IAXIS( NDF__MXDIM )! Current Frame axes to retain
      INTEGER ICURR              ! Index of original current Frame
      INTEGER IERR               ! Index of first numerical error
      INTEGER INDF3              ! Temporary NDF identifier
      INTEGER IP1                ! Pointer to mapped input array
      INTEGER IP2                ! Pointer to mapped output array
      INTEGER IPERM( NDF__MXDIM )! Output axis index for each input axis
      INTEGER IWCS               ! WCS FrameSet for output
      INTEGER*8 LBND( NDF__MXDIM ) ! Template NDF lower bounds
      INTEGER LTTL               ! Length of title
      INTEGER MAP                ! Original base->current Mapping
      INTEGER MAP2               ! Non-degenerate axes component of MAP
      INTEGER MAP3               ! Axis permutation Mapping
      INTEGER NCOMP              ! No. of components in AXIS structure
      INTEGER NDIM               ! Number of template dimensions
      INTEGER NERR               ! Number of numerical errors
      INTEGER NEWAX              ! New output axis index
      INTEGER NEWFRM             ! Replacement Frame
      INTEGER NFC                ! Number of frame axes
      INTEGER OLDAX              ! Old output axis index
      INTEGER OPERM( NDF__MXDIM )! Input axis index for each output axis
      INTEGER PLACE              ! Place holder for a temporary NDF
      INTEGER PM                 ! Axis permutation Mapping
      INTEGER SCOMP              ! Index of first component to copy
      INTEGER SIGDIM             ! Number of significant pixel indices
      INTEGER*8 SLBND( NDF__MXDIM )! Significant axis lower bounds
      INTEGER*8 SUBND( NDF__MXDIM )! Significant axis upper bounds
      INTEGER*8 UBND( NDF__MXDIM ) ! Template NDF upper bounds
      LOGICAL BAD                ! Bad values in the array component?
      LOGICAL DIRECT             ! One-to-one copy of array components?
      LOGICAL ISBAS              ! Is the NDF identifier for a base NDF?
      LOGICAL THERE              ! Does object exists?
      LOGICAL USEPRM             ! Use PermMap to select Frame axes?
      LOGICAL UTRIM              ! Remove insignificant pixel axes?
      LOGICAL UTRWCS             ! Remove corresponding WCS axes?

*  Local Data:
      CHARACTER ACOMP( 3 )*(DAT__SZNAM) ! NDF array component names
      DATA ACOMP /'DATA', 'VARIANCE', 'QUALITY' /

*.
      UTRIM = .FALSE.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Find the number of significant axes (i.e. axes panning more than 1
*  pixel). First find the number of dimensions.
      CALL NDF_BOUND8( INDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Loop round each dimension, counting the significant axes. Also set up
*  axis permutation arrays which can be used to create an AST PermMap if
*  required.
      SIGDIM = 0
      DO I = 1, NDIM
         IF ( LBND( I ) .LT. UBND( I ) ) THEN
            SIGDIM = SIGDIM + 1
            SLBND( SIGDIM ) = LBND( I )
            SUBND( SIGDIM ) = UBND( I )
            OPERM( SIGDIM ) = I
            IPERM( I ) = SIGDIM
         ELSE
            IPERM( I ) = -1
         END IF
      END DO

*  If there are no insignificant pixel axes, but there are excess WCS
*  axes (i.e. more WCS axes than pixel axes), then there is something
*  to trim.
      IF( SIGDIM .EQ. NDIM ) THEN
         CALL KPG1_GTWCS( INDF1, IWCS, STATUS )
         IF( AST_GETI( IWCS, 'Nin', STATUS ) .LT.
     :       AST_GETI( IWCS, 'NOUT', STATUS ) ) UTRIM = .TRUE.

*  If there are no significant axes, trimming is not possible.
      ELSE IF( SIGDIM .EQ. 0 .AND. TRIM ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Cannot trim insignificant output '//
     :                    'pixel axes since all output pixel axes '//
     :                    'are insignificant.', STATUS )
         END IF

*  Otherwise, use the supplied TRIM value.
      ELSE
         IWCS = AST__NULL
         UTRIM = TRIM
      END IF

*  Determine whether it is a one-to-one transfer of array components,
*  or VARIANCE or QUALITY is to replace the DATA_ARRAY.
      DIRECT = .NOT. ( COMP .EQ. 'VARIANCE' .OR.
     :                 COMP .EQ. 'ERROR' .OR. COMP .EQ. 'QUALITY' )

*  If not, copy all components from the input NDF (or section) to
*  create  the output NDF.
      IF( .NOT. UTRIM .AND. DIRECT ) THEN
         IF( PARAM .NE. ' ' ) THEN
            CALL LPG_PROP( INDF1, 'Title,Label,Units,Data,Variance,'//
     :                     'Quality,Axis,History,WCS', 'OUT', INDF2,
     :                     STATUS )
         ELSE
            CALL NDF_SCOPY( INDF1, 'Title,Label,Units,Data,Variance,'//
     :                      'Quality,Axis,History,WCS', OPLACE, INDF2,
     :                      STATUS )
         END IF

*  Otherwise, we do not need to copy the array components, or the WCS
*  or AXIS components since we will be copying these explicitly.  An
*  exception is when merely switching the data array without trimming,
*  then we can simply propagate the AXIS and WCS components.
      ELSE
         IF ( UTRIM ) THEN
            CLIST = 'Title,Label,Units,History'
         ELSE
            CLIST = 'Title,Label,Units,Axis,History,WCS'
         END IF

         IF( PARAM .NE. ' ' ) THEN
            CALL LPG_PROP( INDF1, CLIST, 'OUT', INDF2, STATUS )
         ELSE
            CALL NDF_SCOPY( INDF1, CLIST, OPLACE, INDF2, STATUS )
         END IF

*  Only need to manipulate the WCS if we are trimming, as opposed to
*  replacement of the DATA_ARRAY component.
         IF ( UTRIM ) THEN

*  Get the WCS FrameSet from the input NDF.
            IF( IWCS .EQ. AST__NULL ) CALL KPG1_GTWCS( INDF1, IWCS,
     :                                                 STATUS )

*  Get the Mapping from base (GRID) Frame to current Frame.
            MAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT,
     :                            STATUS )

*  Create a PermMap which goes from the NDIM-dimensional GRID Frame to
*  a SIGDIM_dimensional copy of the GRID Frame, supplying a value of 1.0
*  for the insignificant axes.
            PM = AST_PERMMAP( NDIM, IPERM, SIGDIM, OPERM, 1.0D0, ' ',
     :                        STATUS )

*  Create a SIGDIM-dimensional GRID Frame, then add it into the WCS
*  FrameSet, deleting the original.  Note, the Mapping returned by
*  AST_PICKAXES supplies AST__BAD for the insignificant axes and so we
*  use the better PermMap created above.
            CALL AST_INVERT( IWCS, STATUS )
            NEWFRM = AST_PICKAXES( IWCS, SIGDIM, OPERM, MAP2, STATUS )
            ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )
            CALL AST_ADDFRAME( IWCS, AST__CURRENT, PM, NEWFRM, STATUS )
            CALL AST_REMOVEFRAME( IWCS, ICURR, STATUS )
            CALL AST_INVERT( IWCS, STATUS )

*  Get the number of axes in the original Current Frame.
            NFC = AST_GETI( IWCS, 'NAXES', STATUS )

*  See if the current WCS co-ordinate Frame is to be modified so that it
*  has the same number of axes as the pixel Frame. If there are no
*  excess WCS axes, there is nothing to trim.
            IF( NFC .LE. SIGDIM ) THEN
               UTRWCS = .FALSE.
            ELSE
               UTRWCS = TRMWCS
            END IF

*  If required, remove WCS axes.
            IF( UTRWCS ) THEN

*  See if the non-degenerate GRID axes correspond to a distinct and
*  independent group of current Frame axes. If so, MAP2 is returned
*  holding the Mapping from the non-degenerate GRID axes to the
*  corresponding WCS axes, and CAXES is returned holding the indices of
*  these WCS axes.
               CALL AST_MAPSPLIT( MAP, SIGDIM, OPERM, CAXES, MAP2,
     :                            STATUS )

*  Now see which WCS axes are to be retained.  If the base->current
*  Mapping can be split into two parallel components, we use a default
*  USEAXIS value which includes the WCS axes corresponding to the
*  non-degenerate pixel axes.  If the base->current Mapping cannot be
*  split, we use a default USEAXIS which assumes that the non-degenerate
*  WCS axes simply have the same index as the non-degenerate pixel axes.
               DO I = 1, SIGDIM
                  IAXIS( I ) = OPERM( I )
               END DO

               USEPRM = .TRUE.
               IF( MAP2 .NE. AST__NULL ) THEN
                  IF( AST_GETI( MAP2, 'NOUT',
     :                          STATUS ) .EQ. SIGDIM ) THEN
                     USEPRM = .FALSE.
                     DO I = 1, SIGDIM
                        IAXIS( I ) = CAXES( I )
                     END DO
                  END IF
               END IF

*  Get a value for USEAXIS from the user using the above default.
               CALL KPG1_GTAXI( 'USEAXIS', IWCS, SIGDIM, IAXIS, STATUS )

*  If the base->current Mapping can be split, check that the user has
*  not chosen to retain any degenerate WCS axes.  If so, we will have to
*  use a PermMap to selected the required WCS axes (rather than
*  replacing the base->current Mapping by the component Mapping found
*  above).
               IF( .NOT. USEPRM ) THEN
                  DO I = 1, SIGDIM
                     USEPRM = IAXIS( I ) .NE. CAXES( I )
                  END DO
               END IF

*  Create a new Frame by picking the selected axes from the original
*  Current Frame.  This also returns a PermMap which goes from the
*  original Frame to the new one, using AST__BAD values for the
*  un-selected axes.  We will only use this Mapping if the base->current
*  Mapping was not succesfully split by AST_MAPSPLIT.
               NEWFRM = AST_PICKAXES( IWCS, SIGDIM, IAXIS, MAP3,
     :                                STATUS )

*  If the original Current Frame is a CmpFrame, the Frame created from
*  the above call to AST_PICKAXES may not have inherited its Title.  If
*  the Frame created above has no Title, but the original Frame had,
*  then copy the original Frame's Title to the new Frame.
               IF( AST_TEST( IWCS, 'TITLE', STATUS ) .AND.
     :             .NOT. AST_TEST( NEWFRM, 'TITLE', STATUS ) ) THEN
                  TTL = AST_GETC( IWCS, 'TITLE', STATUS )
                  LTTL = MAX( 1, CHR_LEN( TTL ) )
                  CALL AST_SETC( NEWFRM, 'TITLE', TTL( : LTTL ),
     :                           STATUS )
               END IF

*  If the base->current Mapping cannot be split into two parallel
*  component Mappings, or if the user wants to select WCS axes which
*  depend on the degenerate pixel axes, then we use the PermMap created
*  by AST_PICKAXES above to select the required WCS axes.  Add this new
*  Frame into the FrameSet. It becomes the Current Frame.
               IF( USEPRM ) THEN
                  CALL AST_ADDFRAME( IWCS, AST__CURRENT, MAP3, NEWFRM,
     :                               STATUS )

*  If the degenerate pixel axes correspond to a distinct subset of the
*  current Frame axes, then we can use the simpler base->current Mapping
*  returned by AST_MAPSPLIT above.
               ELSE
                  CALL AST_ADDFRAME( IWCS, AST__BASE, MAP2, NEWFRM,
     :                               STATUS )

               END IF

            END IF

*  Modify the bounds of the output NDF so that the significant axes
*  span Axes 1 to SIGDIM.
            CALL NDF_SBND8( SIGDIM, SLBND, SUBND, INDF2, STATUS )

*  Store the new WCS FrameSet in the output NDF.
            CALL NDF_PTWCS( IWCS, INDF2, STATUS )

*  We now need to copy any AXIS structures into the output NDF so that
*  they refer to the re-ordered axes.
            CALL NDF_STATE( INDF1, 'AXIS', THERE, STATUS )
            IF( THERE ) THEN

*  Since we will be using HDS to modify the output NDF, we need to take
*  care that the internal representation of the NDF stored within the
*  common blocks of the NDF library does not get out of step with the
*  actual HDS structure of the NDF.  For this reason, we get an HDS
*  locator to the NDF and then annul the NDF identifier. We will
*  re-import the modified NDF back into the NDF library once all the
*  changes have been made.  Before annulling the NDF, we need to map the
*  DATA array to put it into a defined state since we are not allowed to
*  release an NDF with an undefined DATA array.
               CALL NDF_MAP8( INDF2, 'DATA', '_BYTE', 'WRITE', IP2, EL,
     :                        STATUS )
               CALL NDF_LOC( INDF2, 'READ', LOC1, STATUS )
               CALL DAT_PRMRY( .TRUE., LOC1, .TRUE., STATUS )
               CALL NDF_ANNUL( INDF2, STATUS )

*  Create a new array of axis structures within the output NDF, and get
*  a locator to it.
               CALL DAT_NEW( LOC1, 'AXIS', 'AXIS', 1, SIGDIM, STATUS )
               CALL DAT_FIND( LOC1, 'AXIS', LOC2, STATUS )

*  Get a locator to the array of AXIS structures in the input NDF. If
*  the supplied NDF is not a base NDF we need to take a copy of it so
*  that NDF_LOC will return a locator for a structure describing the
*  selected section rather than the base NDF.
               CALL NDF_ISBAS( INDF1, ISBAS, STATUS )
               IF( .NOT. ISBAS ) THEN
                  CALL NDF_TEMP( PLACE, STATUS )
                  CALL NDF_SCOPY( INDF1, 'AXIS,NOEXTENSION()', PLACE,
     :                            INDF3, STATUS )
                  CALL NDF_LOC( INDF3, 'READ', LOC3, STATUS )
               ELSE
                  CALL NDF_LOC( INDF1, 'READ', LOC3, STATUS )
               END IF

               CALL DAT_FIND( LOC3, 'AXIS', LOC4, STATUS )

*  Loop round each re-ordered axis in the output NDF.
               DO NEWAX = 1, SIGDIM
                  OLDAX = OPERM( NEWAX )

*  Get locators to the appropriate cells of the old and new AXIS arrays.
                  CALL DAT_CELL( LOC2, 1, NEWAX, LOC2C, STATUS )
                  CALL DAT_CELL( LOC4, 1, OLDAX, LOC4C, STATUS )

*  Copy all components of the old axis structure into the new axis
*  structure.
                  CALL DAT_NCOMP( LOC4C, NCOMP, STATUS )
                  IF( STATUS .EQ. SAI__OK ) THEN
                     DO I = 1, NCOMP
                        CALL DAT_INDEX( LOC4C, I, LOC5, STATUS )
                        CALL DAT_NAME( LOC5, NAME, STATUS )
                        CALL DAT_COPY( LOC5, LOC2C, NAME, STATUS )
                        CALL DAT_ANNUL( LOC5, STATUS )
                     END DO
                  END IF

*  Annul the locators to the cells.
                  CALL DAT_ANNUL( LOC4C, STATUS )
                  CALL DAT_ANNUL( LOC2C, STATUS )

               END DO

*  Re-import the modified NDF into the NDF Library.
               CALL NDF_FIND( LOC1, ' ', INDF2, STATUS )

*  Annul the remaining Locators.
               CALL DAT_ANNUL( LOC4, STATUS )
               CALL DAT_ANNUL( LOC3, STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )
               CALL DAT_ANNUL( LOC1, STATUS )

            END IF

*  End of block for trimming.
         END IF

*  Define the input and output array components.  The direct one-to-one
*  transfer is straightforward.
         SCOMP = 0
         IF ( DIRECT ) THEN
            SCOMP = 1
            DO I = SCOMP, 3
               ICOMP( I ) = ACOMP( I )
               ICOMPN( I ) = ICOMP( I )
               OCOMP( I ) = ACOMP( I )
            END DO

*  For VARIANCE we can retain any QUALITY but lose the DATA_ARRAY.
*  So we transfer a maximum of two array components starting at Index 2.
*  Make the input VARIANCE the ouptut DATA_ARRAY.
         ELSE IF ( COMP .EQ. 'VARIANCE' .OR. COMP .EQ. 'ERROR' ) THEN
            SCOMP = 2
            DO I = SCOMP, 3
               ICOMP( I ) = ACOMP( I )
               ICOMPN( I ) = ICOMP( I )
               IF ( COMP .EQ. ACOMP( I ) ) THEN
                  OCOMP( I ) = 'DATA'

               ELSE IF ( COMP .EQ. 'ERROR' .AND.
     :                   ACOMP( I ) .EQ. 'VARIANCE' ) THEN
                  ICOMP( I ) = COMP
                  ICOMPN( I ) = 'VARIANCE'
                  OCOMP( I ) = 'DATA'
               ELSE
                  OCOMP( I ) = ACOMP( I )
               END IF
            END DO

*  For QUALITY we can only transfer the QUALITY array.
         ELSE IF ( COMP .EQ. 'QUALITY' ) THEN
            SCOMP = 3
            ICOMP( SCOMP ) = 'QUALITY'
            ICOMPN( SCOMP ) = ICOMP( SCOMP )
            OCOMP( SCOMP ) = 'DATA'
         ELSE
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'C', COMP )
               CALL ERR_REP( ' ',
     :              'Could not understand supplied component ^C',
     :              STATUS )
            END IF
         END IF

*  Copy all the selected array components that exist.
         DO I = SCOMP, 3
            CALL NDF_STATE( INDF1, ICOMPN( I ), THERE, STATUS )

            IF( THERE ) THEN

*  Obtain the data type of the array component and determine whether it
*  contains any bad pixels without reading all its elements.
               CALL NDF_TYPE( INDF1, ICOMPN( I ), TYPE, STATUS )
               CALL NDF_MAP8( INDF1, ICOMP( I ), TYPE, 'READ', IP1, EL,
     :                        STATUS )
               CALL NDF_BAD( INDF1, ICOMPN( I ), .FALSE., BAD, STATUS )

*  The data type may have changed if there's is a new DATA_ARRAY.  NDF
*  must be told about the potential change of data type.  Even though
*  the data are mapped and copied using the desired type, upon unmapping
*  the DATA_ARRAY would revert to the input component's data type.
               IF ( .NOT. DIRECT .AND. OCOMP( I ) .NE. 'QUALITY' )
     :           CALL NDF_STYPE( TYPE, INDF2, OCOMP( I ), STATUS )
               CALL NDF_MAP8( INDF2, OCOMP( I ), TYPE, 'WRITE', IP2, EL,
     :                        STATUS )

*  Now actually copy the array components from input to output.
               IF( TYPE .EQ. '_DOUBLE' ) THEN
                  CALL VEC8_DTOD( BAD, EL, %VAL( CNF_PVAL( IP1 ) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF( TYPE .EQ. '_REAL' ) THEN
                  CALL VEC8_RTOR( BAD, EL, %VAL( CNF_PVAL( IP1 ) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
                  CALL VEC8_ITOI( BAD, EL, %VAL( CNF_PVAL( IP1 ) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF( TYPE .EQ. '_INT64' ) THEN
                  CALL VEC8_KTOK( BAD, EL, %VAL( CNF_PVAL( IP1 ) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF( TYPE .EQ. '_WORD' ) THEN
                  CALL VEC8_WTOW( BAD, EL, %VAL( CNF_PVAL( IP1 ) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF( TYPE .EQ. '_UWORD' ) THEN
                  CALL VEC8_UWTOUW( BAD, EL, %VAL( CNF_PVAL( IP1 ) ),
     :                             %VAL( CNF_PVAL( IP2 ) ),
     :                             IERR, NERR, STATUS )

               ELSE IF( TYPE .EQ. '_BYTE' ) THEN
                  CALL VEC8_BTOB( BAD, EL, %VAL( CNF_PVAL( IP1 ) ),
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
                  CALL VEC8_UBTOUB( BAD, EL, %VAL( CNF_PVAL( IP1 ) ),
     :                             %VAL( CNF_PVAL( IP2 ) ),
     :                             IERR, NERR, STATUS )
               END IF

            END IF

         END DO

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
