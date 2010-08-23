      SUBROUTINE KPG1_ASFGT( PDOM, PDIM, PEP, FRM, NAX, STATUS )
*+
*  Name:
*     KPG1_ASFGT

*  Purpose:
*     Creates a new Frame specified through the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASFGT( PDOM, PDIM, PEP, FRM, NAX, STATUS )

*  Description:
*     This routine creates a new AST Frame with properties specified by
*     the given environment parameter. The parameter value is interpreted
*     as an HDS path containing a WCS FrameSet in the form created by
*     KPG1_WWRT. If this is succesful, the current Frame of the FrameSet
*     is returned. Otherwise, an attempt is made to interpret the parameter
*     value as an NDF name. If the NDF is opened succesfully, its current
*     WCS Frame is returned. If this fails, and the parameter value ends
*     with ".FIT", am attempt is made to interpret the parameter value as
*     the name of a FITS file. If successful, the primary WCS Frame from
*     the primary HDU headers is returned. If the above attempt fails,
*     an attempt is made to interpret the parameter value as the name of a
*     text file containing either an AST Frame dump, or a set of FITS
*     headers.
*
*     If all the above fails, and the parameter value looks like an IRAS90
*     "Sky Co-ordinate System" (SCS) specification, then a SkyFrame is
*     returned with the properties specified by the SCS. Otherwise, a
*     simple Frame is returned with Domain set to the parameter value,
*     the number of axes in the Frame being specified by another environment
*     parameter.

*  Arguments:
*     PDOM = CHARACTER * ( * ) (Given)
*        Name of parameter to use to get Frame Domain.
*     PDIM = CHARACTER * ( * ) (Given)
*        Name of parameter to use to get number of Frame axes. Only
*        accessed if the value obtained for PDOM is not an IRAS90 SCS.
*     PEP = CHARACTER * ( * ) (Given)
*        Name of parameter to use to get the epoch of observation. Only
*        accessed if the value obtained for PDOM is an IRAS90 SCS.
*     FRM = INTEGER (Returned)
*        An AST pointer to the returned Frame. Returned equal to
*        AST__NULL if an error occurs.
*     NAX = INTEGER (Returned)
*        The number of axes in the returned Frame. Returned equal to zero
*        if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 1999, 2001, 2002 Central Laboratory of the Research Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-SEP-1998 (DSB):
*        Original version.
*     16-DEC-1998 (DSB):
*        Added BASEPIC as a special case Domain.
*     15-OCT-1999 (DSB):
*        Added CURPIC as a special case Domain.
*     4-DEC-2001 (DSB):
*        Added NDC as a special case Domain.
*     13-AUG-2002 (DSB):
*        Added CURNDC as a special case Domain.
*     8-JUN-2009 (DSB):
*        Extend options for interpreting the parameter value by calling
*        KPG1_GTOBJ.
*     4-AUG-2009 (DSB):
*        Add FRACTION Frame.
*     12-OCT-2009 (DSB):
*        Access the parameter first as a literal string, before accessing
*        it (via KPG1_GTOBJ) as an NDF, etc.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      CHARACTER PDOM*(*)
      CHARACTER PDIM*(*)
      CHARACTER PEP*(*)

*  Arguments Returned:
      INTEGER FRM
      INTEGER NAX

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL KPG1_ASSIR         ! Is string an IRAS90 SCS?
      INTEGER CHR_LEN            ! Used length of a string
      EXTERNAL AST_ISAFRAME      ! Passed to KPG1_GTOBJ

*  Local Variables:
      CHARACTER DOM*30           ! Co-ordinate Frame specification
      CHARACTER TEXT*60          ! Attribute value or name
      INTEGER I                  ! Axis count
      INTEGER IAT                ! No. of characters in a string
      INTEGER NEW                ! Pointer to new AST Object
*.

*  Initialise
      FRM = AST__NULL
      NAX = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the string describing the required co-ordinate Frame. Access it
*  using PAR_GET0C now before we call KPG1_GTOBJ. This forces the
*  parameter system to treat the user-supplied string as a literal string.
*  If KPG1_GTOBJ accesses the parameter first, then the parametet system
*  seems to treat it like an HDS reference for ever more, thus reporting
*  an error if it is later accessed using PAR_GET0C.
      CALL PAR_GET0C( PDOM, DOM, STATUS )

*  Attempt to get a Frame from the parameter value as an HDS path, an NDF
*  name, or a text file.
      CALL KPG1_GTOBJ( PDOM, 'Frame', AST_ISAFRAME, FRM, STATUS )

*  If successful, get the number of axes.
      IF( STATUS .EQ. SAI__OK ) THEN
         NAX = AST_GETI( FRM, 'NAXES', STATUS )

*  If a FrameSet was supplied, return the current Frame.
         IF( AST_ISAFRAMESET( FRM ) ) THEN
            NEW = AST_GETFRAME( FRM, AST__CURRENT, STATUS )
            CALL AST_ANNUL( FRM, STATUS )
            FRM = NEW
         END IF

*  If not successful, annul the error and interpret the parameter value as a
*  Domain name or IRAS90 SCS.
      ELSE
         CALL ERR_ANNUL( STATUS )

*  Convert to upper case, and remove blanks.
         CALL CHR_UCASE( DOM )
         CALL CHR_RMBLK( DOM )

*  Create a default SkyFrame.
         FRM = AST_SKYFRAME( ' ', STATUS )
         NAX = 2

*  Is the supplied string "SKY" or an IRAS90 SCS? If so, the SkyFrame
*  properties are changed to match the SCS. If not, we need to return a
*  simple Frame instead of a SkyFrame.
         IF( DOM .NE. 'SKY' .AND. .NOT. KPG1_ASSIR( FRM, DOM, PEP,
     :                                             STATUS ) ) THEN

*  Annul the SkyFrame.
            CALL AST_ANNUL( FRM, STATUS )

*  Get the number of axes for the Frame.
            IF( DOM .EQ. 'GRAPHICS' .OR. DOM .EQ. 'BASEPIC' .OR.
     :          DOM .EQ. 'CURPIC' .OR. DOM .EQ. 'NDC' .OR.
     :          DOM .EQ. 'CURNDC' ) THEN
               NAX = 2
            ELSE
               CALL PAR_GDR0I( PDIM, 2, 1, NDF__MXDIM, .FALSE., NAX,
     :                         STATUS )
            END IF

*  Create a Frame with this many axes.
            FRM = AST_FRAME( NAX, ' ', STATUS )

*  Give it the supplied Domain.
            CALL AST_SETC( FRM, 'DOMAIN', DOM( : CHR_LEN( DOM ) ),
     :                     STATUS )

*  If the DOMAIN was one of the standard Domains, set up Title, Symbols,
*  units, labels, etc.

*  Pixel co-ordinates...
            IF( DOM .EQ. 'PIXEL' ) THEN

*  Do not set a title since the title produced by the NDF library includes
*  the pixel origin which we do not know here. Setting a title would mean
*  that the Frame would not match another PIXEL Frame (because the other
*  Frames Title would be set to a different value).

*  For each axis, set up a label, symbol and unit value.
               DO I = 1, NAX
                  IAT = 0
                  CALL CHR_PUTI( I, TEXT, IAT )
                  CALL AST_SETC( FRM, 'Label(' // TEXT( : IAT ) // ')',
     :                        'Pixel co-ordinate ' // TEXT( : IAT ),
     :                        STATUS )
                  CALL AST_SETC( FRM, 'Symbol(' // TEXT( : IAT ) // ')',
     :                        'p' // TEXT( : IAT ), STATUS )
                  CALL AST_SETC( FRM, 'Unit(' // TEXT( : IAT ) // ')',
     :                        'pixel', STATUS )
               END DO

*  Normalised pixel co-ordinates...
            ELSE IF( DOM .EQ. 'FRACTION' ) THEN

*  Do not set a title since the title produced by the NDF library includes
*  the pixel origin which we do not know here.

*  For each axis, set up a label, symbol and unit value.
               DO I = 1, NAX
                  IAT = 0
                  CALL CHR_PUTI( I, TEXT, IAT )
                  CALL AST_SETC( FRM, 'Label(' // TEXT( : IAT ) // ')',
     :                        'Normalised pixel co-ordinate ' //
     :                        TEXT( : IAT ), STATUS )
                  CALL AST_SETC( FRM, 'Symbol(' // TEXT( : IAT ) // ')',
     :                        'f' // TEXT( : IAT ), STATUS )
                  CALL AST_SETC( FRM, 'Unit(' // TEXT( : IAT ) // ')',
     :                        ' ', STATUS )
               END DO

*  Data grid co-ordinates...
            ELSE IF( DOM .EQ. 'GRID' ) THEN

*  Store a string holding the co-ordinates at the centre of the first
*  pixel.
               TEXT = '('
               IAT = 1
               DO I = 1, NAX
                  IF ( I .GT. 1 ) CALL CHR_PUTC( ',', TEXT, IAT )
                  CALL CHR_APPND( '1', TEXT, IAT )
               END DO
               CALL CHR_APPND( ')', TEXT, IAT )

*  Set up a suitable Frame title.
               IF ( NAX .EQ. 1 ) THEN
                  CALL AST_SETC( FRM, 'Title', 'Data grid index; '//
     :                           'FIRST pixel at '//TEXT( : IAT ),
     :                           STATUS )
               ELSE
                  CALL AST_SETC( FRM, 'Title', 'Data grid indices; '//
     :                           'first pixel at '//TEXT( : IAT ),
     :                           STATUS )
               END IF

*  For each axis, set up a label, symbol and unit value.
               DO I = 1, NAX
                  IAT = 0
                  CALL CHR_PUTI( I, TEXT, IAT )
                  CALL AST_SETC( FRM, 'Label(' // TEXT( : IAT ) // ')',
     :                           'Data grid index ' // TEXT( : IAT ),
     :                           STATUS )
                  CALL AST_SETC( FRM, 'Symbol(' // TEXT( : IAT ) // ')',
     :                           'g' // TEXT( : IAT ), STATUS )
                  CALL AST_SETC( FRM, 'Unit(' // TEXT( : IAT ) // ')',
     :                           'pixel', STATUS )
               END DO

*  Graphical co-ordinates (symbols and labels are not set since these
*  are not set either by KPG1_GDGET or AST_PLOT)...
            ELSE IF( DOM .EQ. 'GRAPHICS' ) THEN
               CALL AST_SETC( FRM, 'Title', 'Graphical co-ordinates',
     :                        STATUS )
               CALL AST_SETC( FRM, 'Unit(1)', 'mm', STATUS )
               CALL AST_SETC( FRM, 'Unit(2)', 'mm', STATUS )

*  AGI BASE picture world co-ordinates.
            ELSE IF( DOM .EQ. 'BASEPIC' ) THEN
               CALL AST_SETC( FRM, 'Title', 'Normalised world '//
     :                        'co-ordinates in the AGI BASE picture.',
     :                        STATUS )

*  PGPLOT normalized device co-ords
            ELSE IF( DOM .EQ. 'NDC' ) THEN
               CALL AST_SETC( FRM, 'Title', 'Normalised device '//
     :                        'co-ordinates.', STATUS )

*  AGI picture equal-scale normalized co-ordinates.
            ELSE IF( DOM .EQ. 'CURPIC' ) THEN
               CALL AST_SETC( FRM, 'Title', 'Normalised world '//
     :                        'co-ordinates within an AGI picture.',
     :                        STATUS )

*  AGI picture unequal-scale normalized co-ordinates.
            ELSE IF( DOM .EQ. 'CURNDC' ) THEN
               CALL AST_SETC( FRM, 'Title', 'Normalised world '//
     :                        'co-ordinates within an AGI picture.',
     :                        STATUS )

            END IF

         END IF

      END IF

*  If an error has occurred, annul the returned pointer, and return zero
*  axes, and goive a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( FRM, STATUS )
         NAX = 0
         CALL MSG_SETC( 'P', PDOM )
         CALL ERR_REP( 'KPG1_ASFGT_ERR', 'Failed to create a new '//
     :                 'co-ordinate Frame using parameter %^P.',
     :                 STATUS )
      END IF

      END
