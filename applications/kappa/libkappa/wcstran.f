      SUBROUTINE WCSTRAN( STATUS )
*+
*  Name:
*     WCSTRAN

*  Purpose:
*     Transform a position from one NDF co-ordinate Frame to another.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSTRAN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application transforms a position from one NDF co-ordinate Frame
*     to another. The input and output Frames may be chosen freely from the
*     Frames available in the WCS component of the supplied NDF. The
*     transformed position is formatted for display and written to the screen
*     and also to an output parameter.

*  Usage:
*     wcstran ndf posin framein [frameout]

*  ADAM Parameters:
*     EPOCHIN = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        Parameter FRAMEIN) for a celestial co-ordinate system, then an epoch
*        value is needed to qualify it. This is the epoch at which the
*        supplied sky position was determined. It should be given as a
*        decimal years value, with or without decimal places  ("1996.8" for
*        example). Such values are interpreted as a Besselian epoch if less
*        than 1984.0 and as a Julian epoch otherwise.
*     EPOCHOUT = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        Parameter FRAMEOUT) for a celestial co-ordinate system, then an epoch
*        value is needed to qualify it. This is the epoch at which the
*        transformed sky position is required. It should be given as a
*        decimal years value, with or without decimal places  ("1996.8" for
*        example). Such values are interpreted as a Besselian epoch if less
*        than 1984.0 and as a Julian epoch otherwise.
*     FRAMEIN = LITERAL (Read)
*        A string specifying the co-ordinate Frame in which the input
*        position is supplied (see Parameter POSIN). If a null parameter
*        value is supplied, then the current Frame in the NDF is used. The
*        string can be one of the following:
*
*        - A domain name such as SKY, AXIS, PIXEL, etc. The two
*        "pseudo-domains" WORLD and DATA may be supplied and will be
*        translated into PIXEL and AXIS respectively, so long as the WCS
*        component of the NDF does not contain Frames with these domains.
*
*        - An integer value giving the index of the required Frame within
*        the WCS component.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000) (see
*        section "Sky Co-ordinate Systems" in SUN/95).
*
*     FRAMEOUT = LITERAL (Read)
*        A string specifying the co-ordinate Frame in which the transformed
*        position is required. If a null parameter value is supplied, then
*        the current Frame in the NDF is used. The string can be one of the
*        following:
*
*        - A domain name such as SKY, AXIS, PIXEL, etc. The two
*        "pseudo-domains" WORLD and DATA may be supplied and will be
*        translated into PIXEL and AXIS respectively, so long as the WCS
*        component of the NDF does not contain Frames with these domains.
*
*        - An integer value giving the index of the required Frame within
*        the WCS component.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000) (see
*        section "Sky Co-ordinate Systems" in SUN/95). [!]
*
*     NDF = NDF (Read and Write)
*        The NDF data structure containing the required co-ordinate Frames.
*     POSIN = LITERAL (Read)
*        The co-ordinates of the position to be transformed, in the
*        co-ordinate Frame specified by Parameter FRAMEIN (supplying
*        a colon ":" will display details of the required co-ordinate Frame).
*        The position should be supplied as a list of formatted axis values
*        separated by spaces or commas.
*     POSOUT = LITERAL (Write)
*        The formatted co-ordinates of the transformed position, in the
*        co-ordinate Frame specified by Parameter FRAMEOUT. The position
*        will be stored as a list of formatted axis values separated by
*        spaces or commas.
*     SKYDEG = _INTEGER (Read)
*        If greater than zero, the values for any celestial longitude or
*        latitude axes are formatted as decimal degrees, irrespective of
*        the Format attributes in the NDF WCS component. The supplied
*        integer value indicates the number of decimal places required.
*        If the SKYDEG value is less than or equal to zero, the formats
*        specified by the Format attributes in the WCS component are
*        honoured. [0]

*  Examples:
*     wcstran m51 "100.1 21.5" pixel
*        This transforms the pixel position "100.1 21.5" into the current
*        co-ordinate Frame of the NDF m51. The results are displayed on
*        the screen and written to the output Parameter POSOUT.
*     wcstran m51 "1:00:00 -12:30" equ(B1950) pixel
*        This transforms the RA/DEC position "1:00:00 -12:30" (referred
*        to the J2000 equinox) into pixel co-ordinates within the NDF m51.
*        The results are written to the output Parameter POSOUT.
*     wcstran m51 "1:00:00 -12:30" equ(B1950) equ(j2000)
*        This is like the previous example except that the position is
*        transformed into RA/DEC referred to the B1950 equinox, instead of
*        pixel co-ordinates.

*  Notes:
*     -  The transformed position is not written to the screen when the
*     message filter environment variable MSG_FILTER is set to QUIET.
*     The creation of the output Parameter POSOUT is unaffected
*     by MSG_FILTER.

*  Related Applications:
*     KAPPA: LISTMAKE, LISTSHOW, WCSFRAME, NDFTRACE, WCSATTRIB

*  Copyright:
*     Copyright (C) 1998-1999 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2009 Science and Technology Facilities Council.
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
*     4-NOV-1998 (DSB):
*        Original version.
*     25-AUG-1999 (DSB):
*        Add TOKEN arg in call to KPG1_ASFRM
*     3-SEP-1999 (DSB):
*        Added NULL argument to KPG1_GTPOS call.
*     2009 July 24 (MJC):
*        Remove QUIET parameter and use the current reporting level
*        instead (set by the global MSG_FILTER environment variable).
*     5-AUG-2009 (DSB):
*        Added SKYDEG parameter.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'MSG_PAR'          ! Message-system constants

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER ATTR*15          ! Attribute setting or name
      CHARACTER DOM*15           ! Axis Domain value
      CHARACTER TEXT*128         ! Formatted text
      DOUBLE PRECISION GRID( NDF__MXDIM )  ! GRID Frame position
      DOUBLE PRECISION POSIN( NDF__MXDIM ) ! Input position
      DOUBLE PRECISION POSOUT( NDF__MXDIM )! Output position
      INTEGER FRMIN              ! Pointer to requested input Frame
      INTEGER FRMOUT             ! Pointer to requested output Frame
      INTEGER I                  ! Loop index
      INTEGER IAT                ! No. of characters in the TEXT variable
      INTEGER ICURR              ! Index of original NDF current Frame
      INTEGER INDF               ! NDF identifier
      INTEGER IWCS               ! AST pointer for WCS FrameSet
      INTEGER NGRID              ! No. of GRID axes
      INTEGER NIN                ! No. of input axes
      INTEGER NOUT               ! No. of output axes
      INTEGER SKYDEG             ! Value of SKYDEG parameter
      LOGICAL REPORT             ! Report results to screen?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Obtain an identifier for the NDF.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  Create an AST FrameSet from the WCS component of the NDF.
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Get the number of Base Frame (GRID) axes.
      NGRID = AST_GETI( IWCS, 'NIN', STATUS )

*  Save the index of the current Frame.
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Set the current Frame to be the required input Frame specified by
*  Parameter FRAMEIN. If "WORLD" co-ordinates are requested, use PIXEL.
*  If "DATA" co-ordinates are requested, use "AXIS".
      CALL NDF_MSG( 'NDF', INDF )
      CALL KPG1_ASFRM( 'FRAMEIN', 'EPOCHIN', IWCS, 'PIXEL', 'AXIS',
     :                 .TRUE., '^NDF', STATUS )

*  Get a pointer to the input Frame.
      FRMIN = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Get the number of input axes.
      NIN = AST_GETI( FRMIN, 'NAXES', STATUS )

*  Get the input position. Do not supply a dynamic default.
      POSIN( 1 ) = AST__BAD
      CALL KPG1_GTPOS( 'POSIN', FRMIN, .FALSE., POSIN, 0.0D0, STATUS )

*  Transform the position into GRID co-ordinates.
      CALL AST_TRANN( IWCS, 1, NIN, 1, POSIN, .FALSE., NGRID, 1, GRID,
     :                STATUS )

*  Re-instate the original current Frame.
      CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Set the current Frame to be the required output Frame specified by
*  Parameter FRAMEOUT. If "WORLD" co-ordinates are requested, use PIXEL.
*  If "DATA" co-ordinates are requested, use "AXIS".
      CALL NDF_MSG( 'NDF', INDF )
      CALL KPG1_ASFRM( 'FRAMEOUT', 'EPOCHOUT', IWCS, 'PIXEL', 'AXIS',
     :                 .TRUE., '^NDF', STATUS )

*  Get a pointer to the output Frame.
      FRMOUT = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Get the number of output axes.
      NOUT = AST_GETI( FRMOUT, 'NAXES', STATUS )

*  Transform the GRID position into the output Frame.
      CALL AST_TRANN( IWCS, 1, NGRID, 1, GRID, .TRUE., NOUT, 1, POSOUT,
     :                STATUS )

*  See if SKYFRAME axes are top be formatted as decimal degrees.
      CALL PAR_GET0I( 'SKYDEG', SKYDEG, STATUS )

*  If so, set search for any SKYFRAME axes and set appropriate Format
*  values for them.
      IF( SKYDEG .GT. 0 ) THEN
         DO I = 1, NOUT
            ATTR = 'Domain('
            IAT = 7
            CALL CHR_PUTI( I, ATTR, IAT )
            CALL CHR_APPND( ')', ATTR, IAT )
            DOM = AST_GETC( FRMOUT, ATTR, STATUS )
            IF( DOM .EQ. 'SKY' ) THEN
               ATTR( : 6 ) = 'Format'
               CALL CHR_APPND( '=d.', ATTR, IAT )
               CALL CHR_PUTI( SKYDEG, ATTR, IAT )
               CALL AST_SET( FRMOUT, ATTR, STATUS )
            END IF
         END DO
      END IF

*  See if we are to report the results, i.e at NORMAL or higher priority.
      REPORT = MSG_FLEVOK( MSG__NORM, STATUS )

      IF ( REPORT ) THEN

*  If so, format the input position including axis symbols.
         TEXT = ' '
         IAT =  0

         CALL KPG1_ASPTP( FRMIN, NIN, POSIN, .TRUE., '  ', TEXT, IAT,
     :                    STATUS )

*  Add "  -->  " to the displayed text.
         CALL CHR_APPND( '  -->', TEXT, IAT )
         IAT = IAT + 2

*  Format the transformed position, including axis symbols.
         CALL KPG1_ASPTP( FRMOUT, NOUT, POSOUT, .TRUE., '  ', TEXT, IAT,
     :                    STATUS )

*  Display the text, between blank lines.
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETC( 'TEXT', TEXT )
         CALL MSG_OUT( 'WCSTRAN_MSG', '  ^TEXT', STATUS )
         CALL MSG_BLANK( STATUS )

      END IF

*  Format the transformed position again, this time without axis symbols.
      TEXT = ' '
      IAT = 0
      CALL KPG1_ASPTP( FRMOUT, NOUT, POSOUT, .FALSE., '  ', TEXT, IAT,
     :                    STATUS )

*  Write this text to the output parameter.
      CALL PAR_PUT0C( 'POSOUT', TEXT( : IAT ), STATUS )

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WCSTRAN_ERR', 'WCSTRAN: Failed to transform '//
     :                 'a position between two NDF co-ordinate Frames.',
     :                 STATUS )
      END IF

      END
