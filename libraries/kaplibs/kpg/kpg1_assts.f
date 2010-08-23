      SUBROUTINE KPG1_ASSTS( SETTNG, REPORT, OVER, IPLOT, BADAT,
     :                       STATUS )
*+
*  Name:
*     KPG1_ASSTS

*  Purpose:
*     Applies an attribute setting to a Plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSTS( SETTNG, REPORT, OVER, IPLOT, BADAT, STATUS )

*  Description:
*     This routine applies the supplied attribute setting to the supplied
*     Plot. The attribute setting should be of the form "name=value" where
*     "name" is an AST attribute name or a synonym for an AST attribute
*     name established by a call to KPG1_ASPSY, and "value" is the value
*     to assign to the attribute. If the attribute name contains either COLOR
*     or COLOUR then the value string is checked to see if it is the name
*     of a colour, and if so, the corresponding colour index is used
*     instead. The resulting setting, after translation of synonyms and
*     colour names is applied to the supplied Plot. If the Plot already
*     has a set value for the specified attribute, then the behaviour depends
*     on OVER; if OVER is .TRUE. then the new attribute value over-writes
*     the value already in the Plot; if OVER is .FALSE. then the supplied
*     setting is ignored.
*
*  Arguments:
*     SETTING = CHARACTER * ( * ) (Given)
*        The attribute setting string.
*     REPORT = LOGICAL (Given)
*        Should an error be reported if the attribute setting string
*        is not legal?
*     OVER = LOGICAL (Given)
*        Over-write existing attribute values in the Plot?
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot to be modified.
*     BADAT = LOGICAL (Returned)
*        Was the setting string invalid? If so, an appropriate error
*        message will have been reported (unless REPORT is .FALSE.).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Colour attribute values may be supplied in any form recognised
*     by KPG1_PGCOL (e.g. colour name, MIN, MAX, integer index), and the
*     nearest colour in the current KAPPA pallette is used.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1998 (DSB):
*        Original version.
*     16-DEC-2005: (DSB):
*        Added DrawDSB.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'AST_ERR'          ! AST error constants

*  Arguments Given:
      CHARACTER SETTNG*(*)
      LOGICAL REPORT
      LOGICAL OVER
      INTEGER IPLOT

*  Arguments Returned:
      LOGICAL BADAT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL CHR_SIMLR          ! Strings equal apart from case?

*  Local Variables:
      CHARACTER NAME*(GRP__SZNAM)  ! Attribute name
      CHARACTER VALUE*(GRP__SZNAM) ! Attribute value
      INTEGER ISTAT                ! CHR status value
      INTEGER IVAL                 ! Integer value read from string
*.

*  Initialise.
      BADAT = .FALSE.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Return if the setting is blank.
      IF( SETTNG .NE. ' ' ) THEN

*  Extract the attribute name and value, replacing any synonyms
*  or any colour names.
         CALL KPG1_ASSTY( SETTNG, NAME, VALUE, STATUS )

*  First check for KAPPA "pseudo-attributes". These are implememted by KAPPA
*  rather than AST...

*  TEXTBACKCOLOUR: specifies the colour index for the background when text
*  strings are drawn. A negative value results in the background being
*  clear.
         IF( CHR_SIMLR( NAME, 'TEXTBACKCOLOUR' ) ) THEN

*  Allow the string CL(EAR) to indicate a clear background. Set the text
*  background colour index used by the grf_kappa.c module to -1.
            IF( CHR_SIMLR( VALUE( : 2 ), 'CL' ) ) THEN
               CALL GRF_SETTBG( -1 )

*  For any other value attempt to extract a colour index. Colour names
*  will have been converted to colour indices by KPG1_ASSTY.
            ELSE
               ISTAT = STATUS
               CALL CHR_CTOI( VALUE, IVAL, ISTAT )

*  If a valid integer value was supplied, set it as the colour index.
               IF( ISTAT .EQ. SAI__OK ) THEN
                  CALL GRF_SETTBG( IVAL )
               END IF

            END IF

*  DrawDSB: specifies whether the unselected sideband of a DSBSpecFrame
*  should be labelled on the unsused axis by applications such as LINPLOT.
         ELSE IF( CHR_SIMLR( NAME, 'DRAWDSB' ) ) THEN

*  Zero means no, any other integer means yes.
            ISTAT = STATUS
            CALL CHR_CTOI( VALUE, IVAL, ISTAT )
            IF( ISTAT .EQ. SAI__OK ) THEN
               CALL KPG1_SETASTDSB( IVAL .NE. 0 )
            END IF

*  FileInTitle: specifies whether the NDF path should be included in the
*  Title, formatted on a second line using AST graphical escape sequences.
         ELSE IF( CHR_SIMLR( NAME, 'FileInTitle' ) ) THEN

*  Zero means no, any other integer means yes.
            ISTAT = STATUS
            CALL CHR_CTOI( VALUE, IVAL, ISTAT )
            IF( ISTAT .EQ. SAI__OK ) THEN
               CALL KPG1_SETASTFIT( IVAL .NE. 0 )
            END IF

*  If the attribute is not a KAPPA pseudo-attribute, assume it is a
*  genuine AST attribute.
         ELSE

*  Set the attribute in the Plot. If required, check that the attribute
*  is not already set in the Plot.
            IF( OVER .OR. .NOT. AST_TEST( IPLOT, NAME, STATUS ) ) THEN
               CALL AST_SETC( IPLOT, NAME( : CHR_LEN( NAME ) ),
     :                        VALUE( : CHR_LEN( VALUE ) ), STATUS )
            END IF

         END IF

*  If AST_SETC or AST_TESTC returned an error indicating that the setting
*  string was invalid, return a flag to indicate this.
         BADAT = ( STATUS .EQ. AST__BADAT .OR.
     :             STATUS .EQ. AST__ATSER .OR.
     :             STATUS .EQ. AST__ATTIN .OR.
     :             STATUS .EQ. AST__AXIIN .OR.
     :             STATUS .EQ. AST__OPT .OR.
     :             STATUS .EQ. AST__NOWRT )

*  If no report is wanted, annul "bad attribute" error.
         IF( .NOT. REPORT .AND. BADAT ) CALL ERR_ANNUL( STATUS )

      END IF

      END
