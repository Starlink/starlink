      SUBROUTINE WCSFRAME( STATUS )
*+
*  Name:
*     WCSFRAME

*  Purpose:
*     Change the current co-ordinate Frame in the WCS component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application displays the current co-ordinate Frame associated
*     with an NDF and then allows the user to specify a new Frame. The
*     current co-ordinate Frame determines the co-ordinate system in
*     which positions within the NDF will be expressed when communicating
*     with the user.
*
*     Having selected a new current co-ordinate Frame, its attributes
*     (such the specific system it uses to represents points within its
*     Domain, its units, etc.) can be changed using KAPPA command
*     WCSATTRIB.

*  Usage:
*     wcsframe ndf frame epoch

*  ADAM Parameters:
*     EPOCH = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        parameter FRAME) for a celestial co-ordinate system, then an epoch
*        value is needed to qualify it. This is the epoch at which the
*        supplied sky positions were determined. It should be given as a
*        decimal years value, with or without decimal places  ("1996.8" for
*        example). Such values are interpreted as a Besselian epoch if less
*        than 1984.0 and as a Julian epoch otherwise.
*     FRAME = LITERAL (Read)
*        A string specifying the new co-ordinate Frame. If a null parameter
*        value is supplied, then the current Frame is left unchanged. The
*        suggested default is the Domain (or index if the Domain is not
*        set) of the current Frame. The string can be one of the following:
*
*        - A domain name such as SKY, SPECTRUM, AXIS, PIXEL, etc. The two
*        "pseudo-domains" WORLD and DATA may be supplied and will be
*        translated into PIXEL and AXIS respectively, so long as the WCS
*        component of the NDF does not contain Frames with these domains.
*
*        - An integer value giving the index of the required Frame within
*        the WCS component.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000) (see
*        section "Sky Co-ordinate Systems" in SUN/95). Using an SCS value
*        is equivalent to specifying "SKY" for this parameter and then setting
*        the System attribute (to "FK5", "Galactic", etc.) using KAPPA command
*        WCSATTRIB. The specific system used to describe positions in other
*        Domains (SPECTRUM, for instance) must be set using WCSATTRIB.
*
*     NDF = NDF (Read and Write)
*        The NDF data structure in which the current co-ordinate Frame is to
*        be modified.

*  Examples:
*     wcsframe m51 pixel
*        This chooses pixel co-ordinates for the current co-ordinate
*        Frame in the NDF m51.
*     wcsframe m51 sky
*        This chooses celestial co-ordinates for the current co-ordinate
*        Frame in the NDF m51 (if available). The specific celestial
*        co-ordinate system (FK5, Galactic, etc.) will depend on the contents
*        of the WCS component of the NDF, but may be changed by setting a
*        new value for the System attribute using the WCSATTRIB command.
*     wcsframe m51 spectrum
*        This chooses spectral co-ordinates for the current co-ordinate
*        Frame in the NDF m51 (if available). The specific spectral
*        co-ordinate system (wavelength, frequency, etc) will depend on the
*        contents of the WCS component of the NDF, but may be changed by
*        setting a new value for the System attribute using the WCSATTRIB
*        command.
*     wcsframe m51 equ(J2000) epoch=1998.2
*        This chooses equatorial (RA/DEC) co-ordinates referred to the
*        equinox at Julian epoch 2000.0 for the current co-ordinate
*        Frame in the NDF m51. The positions were determined at the
*        Julian epoch 1998.2 (this is needed to correct positions for
*        the fictitious proper motions which may be introduced when
*        converting between different celestial co-ordinate systems).
*     wcsframe m51 2
*        This chooses the second co-ordinate Frame in the WCS component
*        of the NDF.
*     wcsframe m51 data
*        This chooses a co-ordinate Frame with domain DATA if one exists,
*        or the AXIS co-ordinate Frame otherwise.
*     wcsframe m51 world
*        This chooses a co-ordinate Frame with domain WORLD if one exists,
*        or the PIXEL co-ordinate Frame otherwise.

*  Notes:
*     -  The current co-ordinate Frame in the supplied NDF is not displayed
*     if a value is assigned to parameter FRAME on the command line.
*     -  This routine may add a new co-ordinate Frame into the WCS component
*     of the NDF.
*     -  The NDFTRACE command can be used to examine the co-ordinate
*     Frames in the WCS component of an NDF.

*  Related Applications:
*     KAPPA: NDFTRACE, WCSREMOVE, WCSCOPY, WCSATTRIB

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 1998-1999 Central Laboratory of the Research
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1998 (DSB):
*        Original version.
*     25-AUG-1999 (DSB):
*        Add TOKEN arg in call to KPG1_ASFRM
*     16-MAR-2011 (DSB):
*        Change call to KPG1_DSFRM so that the displayed pixel scales
*        are the median values taken at a range of different positions in
*        the NDF, rather than just being the scales at the first pixel.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER DEF*50           ! Dynamic default value for FRAME
      DOUBLE PRECISION GLB( NDF__MXDIM ) ! Lower GRID bounds
      DOUBLE PRECISION GUB( NDF__MXDIM ) ! Upper GRID bounds
      INTEGER DIMS( NDF__MXDIM ) ! Length of each NDF pixel axis
      INTEGER I                  ! Axis index
      INTEGER INDF               ! NDF identifier
      INTEGER IWCS               ! AST pointer for WCS FrameSet
      INTEGER NDIM               ! Number of pixel axes in the NDF
      INTEGER STATE              ! Indicates state of FRAME parameter
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Obtain an identifier for the NDF.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Create an AST FrameSet from the WCS component of the NDF.
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  If no value was supplied for parameter FRAME on the command line,
*  we display the current co-ordinate Frame.
      CALL LPG_STATE( 'FRAME', STATE, STATUS )
      IF( STATE .NE. PAR__ACTIVE ) THEN

*  Get the dimensions of the NDF, and store the bounds of the NDF in grid
*  coords.
         CALL NDF_DIM( INDF, NDF__MXDIM, DIMS, NDIM, STATUS )
         DO I = 1, NDIM
            GLB( I ) = 0.5D0
            GUB( I ) = DIMS( I ) + 0.5D0
         END DO

*  Display the coordinate Frame description.
         CALL MSG_BLANK( STATUS )
         CALL NDF_MSG( 'NDF', INDF )
         CALL KPG1_DSFRM( IWCS, 'Current co-ordinate Frame in ^NDF:',
     :                    GLB, GUB, .TRUE., STATUS )
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Choose the dynamic default for the FRAME parameter. This is the Domain of
*  the current Frame if not blank, and the current Frame index otherwise.
      DEF = AST_GETC( IWCS, 'DOMAIN', STATUS )
      IF( CHR_LEN( DEF ) .EQ. 0 ) DEF = AST_GETC( IWCS, 'CURRENT',
     :                                            STATUS )

*  Set the dynamic default for the FRAME parameter.
      CALL PAR_DEF0C( 'FRAME', DEF, STATUS )

*  Set the new Current Frame using parameter FRAME. If "WORLD" co-ordinates
*  are requested, use PIXEL. If "DATA" co-ordinates are requested, use
*  "AXIS".
      CALL NDF_MSG( 'NDF', INDF )
      CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IWCS, 'PIXEL', 'AXIS', .TRUE.,
     :                 '^NDF', STATUS )

*  Save a copy of the modified FrameSet in the NDF's WCS component.
      CALL NDF_PTWCS( IWCS, INDF, STATUS )

 999  CONTINUE

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WCSFRAME_ERR', 'WCSFRAME: Failed to set '//
     :                 'the current co-ordinate Frame in an NDF.',
     :                 STATUS )
      END IF

      END
