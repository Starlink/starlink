      SUBROUTINE COF_WCSIM( FC, INDF, NENCOD, ENCODS, STATUS )
*+
*  Name:
*     COF_WCSIM

*  Purpose:
*     Imports WCS information from the supplied FitsChan into the supplied
*     NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_WCSIM( FC, INDF, NENCOD, ENCODS, STATUS )

*  Description:
*     An attempt is made to read an AST FrameSet (see SUN/210) from the
*     supplied FitsChan. If succesful, it is added into the FrameSet
*     representing the NDF's WCS component. This is done by connecting
*     the base Frames of the two FrameSets with a UnitMap (on the
*     assumption that they are equivalent). The modified FrameSet is
*     then stored back in the NDF (the NDF library will automatically
*     remove any PIXEL, FRACTION, GRID and AXIS Frames from the FrameSet
*     as these are generated afresh each time NDF_GTWCS is called).
*
*     The supplied FitsChan may contain more than one description (or
*     "encoding") of the FrameSet to be added to the NDF, each
*     encoding using a different set of header cards. These encodings
*     may not all be consistent with each other. For instance, if a
*     Starlink application stores a FrameSet twice in a FITS header using
*     FITS-IRAF and AST native encodings, an IRAF application may then
*     modify the FITS-IRAF encoding without making equivalent modifications
*     to the native encoding. In this case, we should use the FITS-IRAF
*     encoding in preference to the native encoding when reconstructing
*     the NDFs WCS component. On the other hand, if the two encodings were
*     still consistent, it would be preferable to use the native encoding
*     since the FITS-IRAF encoding may not give a complete description of the
*     original FrameSet.
*
*     The choice of encoding has several stages:
*
*     o  If the caller has supplied a list of preferred encodings in the
*     ENCODS argument, then the first available encoding in this list is
*     used.
*     o  If no preferred encodings are supplied, then a check is made to
*     see if a native encoding is available. If there is no native encoding,
*     then the default encoding supplied by AST is used. This will be a
*     non-native encoding selected on the basis of the header cards available
*     in the FitsChan.
*     o  If a native encoding is available, and is the only available
*     encoding, then it is used.
*     o  If both native and non-native encodings are available, then the
*     first non-native encoding to be found which is inconsistent with the
*     native encoding is used. If all encodings are consistent, then the
*     native encoding is used. The first inconsistent encoding is used on
*     the assumption that software which modifies the native encoding
*     (i.e. mainly Starlink software) will also modify the non-native
*     encodings so that they remain consistent. Foreign software however
*     (i.e. non-AST software) will probably not bother to modify the native
*     encoding when the non-native encoding is modified.

*  Arguments:
*     FC = INTEGER (Given)
*        An AST pointer to the FitsChan in which to store the NDF's WCS
*        information.
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     NENCOD = INTEGER (Given)
*        The number of encodings supplied in ENCODS.
*     ENCODS( NENCOD ) = CHARACTER * ( * ) (Given)
*        The preferred encodings to use, in order of preference (most
*        preferable first). Ignored if NENCOD is zero.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997-1998, 2000-2001, 2003-2004 Central Laboratory
*     of the Research Councils. All Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     11-NOV-1997 (DSB):
*        Original version.
*     9-NOV-1998 (DSB):
*        Modified to include references to the FITS-IRAF encoding.
*     11-APR-2000 (DSB):
*        Added FITS-IRAF and FITS-PC WCS encodings.
*     20-SEP-2000 (DSB):
*        Allow for different numbers of axes in the Base Frames of the
*        NDF FrameSet and FitsChan FrameSet.
*     3-APR-2001 (DSB):
*        Report an error if none of the supplied encodings can be used.
*     4-FEB-2003 (DSB):
*        Modified so that 1.0 is used for unconnected GRID axes instead
*        of AST__BAD.
*     27-AUG-2004 (DSB):
*        - Added FITS-CLASS encoding.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'AST_PAR'          ! AST_ public constants

*  Arguments Given:
      INTEGER FC
      INTEGER INDF
      INTEGER NENCOD
      CHARACTER ENCODS( NENCOD )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER DEFNCD             ! Number of non-native AST encodings.
      PARAMETER ( DEFNCD = 7 )

*  External References:
      LOGICAL COF_WCSDF

*  Local Variables:
      CHARACTER ASTCOD( DEFNCD )*13 ! The non-native AST encoding names
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER FC2                ! AST identifier for temporary FitsChan
      INTEGER I                  ! Axis count
      INTEGER IBASE              ! Index of the Base Frame.
      INTEGER ICURR              ! Index of Current Frame
      INTEGER IENCOD             ! Index of the current encoding
      INTEGER INPRM( NDF__MXDIM )! FITS axis for each NDF axis
      INTEGER IWCS               ! AST identifier for NDF's WCS information
      INTEGER MAP                ! NDF Base to FitsChan Base Mapping
      INTEGER NATOBJ             ! AST identifier for the native FrameSet
      INTEGER NBFITS             ! No. of axes in FitsChan Base Frame
      INTEGER NDIM               ! Dimensionality of the NDF
      INTEGER NFRAME             ! Total no. of Frames to be written out
      INTEGER OBJ                ! AST identifier for the new FrameSet
      INTEGER OUTPRM( 100 )      ! NDF axis for each FITS axis
      INTEGER TMPF               ! Template 2D Frame

*  Store names of non-native AST encodings to use.
      DATA ASTCOD / 'FITS-IRAF', 'FITS-WCS', 'DSS', 'FITS-AIPS',
     :              'FITS-AIPS++', 'FITS-PC', 'FITS-CLASS' /
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the supplied FitsChan contains a native encoding, then reset any
*  Axis structures which are currently defined in the NDF. This is done
*  because earlier routines may have created Axis structures based
*  on the values of the CRVAL, CDELT, CRPIX keywords, but the native
*  AST FrameSet is the prefered origin for Axis information.
      IF( AST_GETC( FC, 'ENCODING', STATUS ) .EQ. 'NATIVE' ) THEN
         CALL NDF_RESET( INDF, 'AXIS', STATUS )
      END IF

*  First, read a FrameSet from the supplied FitsChan. If the FitsChan
*  contains more than encoding we need to be careful about out choice
*  of encoding because the available encodings may be inconsistent (eg
*  if foreign software has modified one encoding without modifying all
*  the others.
*  ==================================================================

*  If a list of encodings preferences was supplied, attempt to read an
*  Object using each encoding in turn until an Object (FrameSet) is read
*  succesfully.
      IF( NENCOD .GT. 0 ) THEN

         DO IENCOD = 1, NENCOD

*  Use a copy of the FitsChan in order to leave the supplied FitsChan
*  unchanged (AST reads are destructive, removing the cards read from
*  the FitsChan).
            FC2 = AST_COPY( FC, STATUS )

*  Set the encoding.
            CALL AST_SETC( FC2, 'ENCODING', ENCODS( IENCOD ), STATUS )

*  Some encodings (eg NATIVE) can store more than one Object in a FITS header.
*  We use the first FrameSet in the header.
            CALL COF_FNDFS( FC2, OBJ, STATUS )

*  Annul the FitsChan.
            CALL AST_ANNUL( FC2, STATUS )

*  Break out of the loop if we have found a usable Object.
            IF( OBJ .NE. AST__NULL ) GO TO 10

         END DO

 10      CONTINUE

*  Report an error if none of the encodings produced good WCS.
         IF( OBJ .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'ENC', ENCODS( 1 ) )

            IF( NENCOD .EQ. 1 ) THEN
               CALL ERR_REP( 'COF_WCSIM_ERR1', 'WCS information could'//
     :                       ' not be read using the ^ENC encoding.',
     :                       STATUS )
            ELSE
               DO IENCOD = 2, NENCOD
                  CALL MSG_SETC( 'ENC', ',' )
                  CALL MSG_SETC( 'ENC', ENCODS( IENCOD ) )
               END DO
               CALL ERR_REP( 'COF_WCSIM_ERR2', 'WCS information could'//
     :                       ' not be read using any of the supplied '//
     :                       'encodings (^ENC).', STATUS )

            END IF
         END IF

*  If no preferences for encodings were supplied, we have to decide which
*  of the available encodings to use.
      ELSE

*  See if an object can be read from the FitsChan using AST native
*  encoding. Take a copy of the supplied FitsChan, set its encoding,
*  attempt to read the object, and then annul the FitsChan copy.
         FC2 = AST_COPY( FC, STATUS )
         CALL AST_SETC( FC2, 'ENCODING', 'NATIVE', STATUS )
         CALL COF_FNDFS( FC2, NATOBJ, STATUS )
         CALL AST_ANNUL( FC2, STATUS )

*  If a FrameSet could not be read using native encoding, use the default
*  non-native encoding.
         IF( NATOBJ .EQ. AST__NULL ) THEN

            OBJ = AST_READ( FC, STATUS )
            IF( OBJ .NE. AST__NULL ) THEN
               IF( .NOT. AST_ISAFRAMESET( OBJ, STATUS ) ) THEN
                  CALL AST_ANNUL( OBJ, STATUS )
               END IF
            END IF

*  If a FrameSet was read using native encoding...
         ELSE

*  Consider each possible non-native encoding in turn...
            DO IENCOD = 1, DEFNCD

*  Attempt to read an Object using this encoding. AST_READ will report
*  an error if keywords required for the specified encoding are not
*  found. Therefore annul any errors generated by AST_READ.
               FC2 = AST_COPY( FC, STATUS )
               CALL AST_SETC( FC2, 'ENCODING', ASTCOD( IENCOD ),
     :                        STATUS )

               CALL ERR_BEGIN( STATUS )
               OBJ = AST_READ( FC2, STATUS )
               IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
               CALL ERR_END( STATUS )

               CALL AST_ANNUL( FC2, STATUS )

*  If any type of Object other than a FrameSet was read, annul it.
               IF( OBJ .NE. AST__NULL ) THEN
                  IF( .NOT. AST_ISAFRAMESET( OBJ, STATUS ) ) THEN
                     CALL AST_ANNUL( OBJ, STATUS )
                  END IF
               END IF

*  If a FrameSet was obtained...
               IF( OBJ .NE. AST__NULL ) THEN

*  See if the FrameSet is inconsistent with the FrameSet read earlier using
*  native encoding. If so, annul the native FrameSet and break out of the
*  loop. If the native and non-native FrameSets are consistent, then
*  annul the non-native FrameSet, and go on to consider the next encoding.
                  IF( COF_WCSDF( ASTCOD( IENCOD ), NATOBJ, OBJ,
     :                           STATUS ) ) THEN
                     CALL AST_ANNUL( NATOBJ, STATUS )
                     GO TO 20
                  ELSE
                     CALL AST_ANNUL( OBJ, STATUS )
                  END IF

               END IF

            END DO

 20         CONTINUE

*  If all the non-native encodings are consistent with the native encoding,
*  use the FrameSet read using native encoding.
            IF( OBJ .EQ. AST__NULL ) OBJ = NATOBJ

         END IF

      END IF

*  If we have managed to read a FrameSet from the FitsChan, insert it into
*  the NDFs FrameSet. The base Frames of the two FrameSets are assume to
*  be equivalent.
*=========================================================================
      IF ( OBJ .NE. AST__NULL ) THEN

*  Get the dimensionality of the NDF.
         CALL NDF_DIM( INDF, NDF__MXDIM, DIM, NDIM, STATUS )

*  Create a Frame which can be used as a template when searching for
*  other Frames.
         TMPF = AST_FRAME( NDIM, ' ', STATUS )

*  Get the current NDF FrameSet. This will contain Frames generated
*  automatically for the GRID, FRACTION, AXIS and PIXEL domains.
         CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  Remove the AXIS Frame from the FrameSet. This is done to ensure that the
*  merged FrameSet passed to COF_WCSAX will not contain two AXIS Frames.
*  AST_FINDFRAME will make the AXIS Frame the Current Frame if it finds it,
*  so we need just remove the Current Frame.
         IF( AST_FINDFRAME( IWCS, TMPF, 'AXIS', STATUS )
     :       .NE. AST__NULL ) THEN
            CALL AST_REMOVEFRAME( IWCS, AST__CURRENT, STATUS )
         END IF

*  Note the number of Frames in the NDF FrameSet.
         NFRAME = AST_GETI( IWCS, 'Nframe', STATUS )

*  Get the indices of the base and current Frames in the FrameSet read
*  from the FITS file.
         ICURR = AST_GETI( OBJ, 'Current', STATUS )
         IBASE = AST_GETI( OBJ, 'Base', STATUS )

*  Get the number of axes described in the Base Frame of the FrameSet
*  read from the FitsChan.
         NBFITS = AST_GETI( OBJ, 'NIN', STATUS )

*  We now get the Mapping to use when adding the FrameSet read from the
*  FitsChan into the FrameSet read from the NDF. This Mapping goes from the
*  Base Frame of the NDFs FrameSet to the Base Frame of the FitsChan
*  FrameSet. Usually this could be just a UnitMap since the two Base
*  Frames are considered to be equivalent. But sometimes, the FITS header
*  may not include descriptions for all axes. In these cases we use a PermMap
*  with 1.0 being used for any unconnected axes.
         IF( NDIM .EQ. NBFITS ) THEN
            MAP = AST_UNITMAP( NDIM, ' ', STATUS )

*  If the two frames have different numbers of axes, we use a PermMap.
         ELSE

*  First set up the arrays describing the connections between input and
*  output axes in the PermMap.
            DO I = 1, NDIM
               IF( I .LE. NBFITS ) THEN
                  INPRM( I ) = I
               ELSE
                  INPRM( I ) = -1
               END IF
            END DO

            DO I = 1, NBFITS
               IF( I .LE. NDIM ) THEN
                  OUTPRM( I ) = I
               ELSE
                  OUTPRM( I ) = -1
               END IF
            END DO

*  Now create the PermMap.
            MAP = AST_PERMMAP( NDIM, INPRM, NBFITS, OUTPRM, 1.0D0, ' ',
     :                         STATUS )
         END IF

*  Add the new FrameSet into the NDF's FrameSet, using the above Mapping
*  to connect the two base Frames. AST_ADDFRAME uses the current Frame
*  in the FrameSet being added, so first set the current Frame to be
*  the base Frame.
         CALL AST_SETI( OBJ, 'Current', IBASE, STATUS )
         CALL AST_ADDFRAME( IWCS, AST__BASE, MAP, OBJ, STATUS )
         CALL AST_ANNUL( MAP, STATUS )

*  We now need to re-instate the original current Frame. The index of the
*  original current Frame will have increased to take account of all the
*  Frames in the original NDF FrameSet.
         CALL AST_SETI( IWCS, 'Current', ICURR + NFRAME, STATUS )

*  We now have two copies of the original base Frame (one from the NDF
*  FrameSet and one from the new FrameSet). Remove the one from the new
*  FrameSet.
         CALL AST_REMOVEFRAME( IWCS, IBASE + NFRAME, STATUS )

*  Any AXIS Frame in the FrameSet read from the FITS file should take
*  priority over Axis structures established earlier by this application.
*  Check that there was an AXIS Frame in the FITS file.
         IF( AST_FINDFRAME( OBJ, TMPF, 'AXIS', STATUS )
     :       .NE. AST__NULL ) THEN

*  If so, there will be a copy of it in the NDF's FrameSet (because the
*  two FrameSets have now been merged). Use this copy to create an Axis
*  structure in the NDF, since the one read from the FITS file will never
*  be equivalent to the PIXEL Frame (NDF2FITS strips the PIXEL Frame out).
            CALL COF_WCSAX( INDF, IWCS, NDIM, STATUS )

         END IF

*  Save the modified FrameSet in the NDF. Any duplicate copies of the AXIS,
*  FRACTION, PIXEL and GRID Frames are removed by the NDF library.
         CALL NDF_PTWCS( IWCS, INDF, STATUS )
         CALL AST_ANNUL( IWCS, STATUS )

*  Get the WCS component again. The NDF library will ensure that the
*  returned FrameSet has a single AXIS Frame which is consistent with the
*  NDF's AXIS structures.
         CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  If the Current Frame has no Domain, but is equivalent to the AXIS Frame
*  (i.e. connected by a UnitMap), then remove the Current Frame and
*  re-store the FrameSet in the NDF. This is done to avoid having a Frame
*  which is effectively an AXIS Frame left in the WCS component. The NDF
*  library will remove all Frames with the Domain AXIS, but when non-Native
*  encodings are used (eg FITS-IRAF etc) no Domain values are stored, and so
*  the  NDF library will not be able to recognise the Frame as an AXIS Frame.
         CALL COF_WCSUT( INDF, STATUS )

      END IF

      END
