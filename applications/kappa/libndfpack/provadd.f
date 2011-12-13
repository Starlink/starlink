      SUBROUTINE PROVADD( STATUS )
*+
*  Name:
*     PROVADD

*  Purpose:
*     Stores provenance information in an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PROVADD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application modifies the provenance information stored in an
*     NDF. It records a second specified NDF as a direct parent of the
*     first NDF. If an NDF has more than one direct parent then this
*     application should be run multiple times, once for each parent.

*  Usage:
*     provadd ndf parent creator isroot moretext

*  ADAM Parameters:
*     CREATOR = LITERAL (Read)
*        A text identifier for the software that created the main NDF
*        (usually the name of the calling application).  The format of
*        the identifier is arbitrary, but the form "PACKAGE:COMMAND" is
*        recommended.  If a null (!) value is supplied, no creator
*        information is stored.  [!]
*     ISROOT = _LOGICAL (Read)
*        If TRUE, then the NDF given by parameter "PARENT" will be
*        treated as a root NDF.  That is, any provenance information
*        within PARENT describing its own parents is ignored.  If FALSE,
*        then any provenance information within PARENT is copied into
*        the main NDF.  PARENT is then only a root NDF only if it
*        contains no provenance information.  [FALSE]
*     MORE = UNIV (Read)
*        This is only accessed if a null value is supplied for parameter
*        MORETEXT.  If supplied, it should be an HDS object containing
*        arbitrary additional information about the parent NDF, and how
*        it was used in the creation of the main NDF.  This information
*        is stored with the provenance in the main NDF.  If a null (!)
*        value is supplied no additional information is stored.  [!]
*     MORETEXT = GROUP (Read)
*        A group of "keyword=value" strings that give additional
*        information about the parent NDF, and how it was used in the
*        creation of the main NDF.  If supplied, this information is
*        stored with the provenance in the main NDF.  If a null (!)
*        value is supplied, then the MORE parameter will be used to
*        obtain this extra information instead of MORETEXT.
*
*        The supplied value should be either a comma-separated list of
*        strings, or the name of a text file preceded by an up-arrow
*        character "^", containing one or more comma-separated list of
*        strings.  Each string is either a "keyword=value" setting, or
*        the name of a text file preceded by an up-arrow character "^".
*        Such text files should contain further comma-separated lists
*        which will be read and interpreted in the same manner (any
*        blank lines or lines beginning with "#" are ignored).  Within a
*        text file, newlines can be used as delimiters as well as
*        commas.
*
*        Each individual setting should be of the form:
*
*           <keyword>=<value>
*
*        where <keyword> is either a simple name, or a dot-delimited
*        hierarchy of names (e.g. "camera.settings.exp=1.0").  The
*        <value> string should not contain any commas.  [!]
*     NDF = NDF (Read and Write)
*        The NDF which is to be modified.
*     PARENT = NDF (Read)
*        An NDF that is to be recorded as a direct parent of the NDF
*        given by parameter "NDF".

*  Examples:
*     provadd m51_ff ff
*        Records the fact that NDF "ff" was used in the creation of NDF
*        "m51_ff".

*  Notes:
*     Provenance information is stored in an NDF extension called
*     PROVENANCE, and is propagated automatically by all KAPPA
*     applications.

*  Related Applications:
*     KAPPA: PROVMOD, PROVSHOW, HISCOM.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (JAC,UCLan)
*     {enter_new_authors_here}

*  History:
*     24-JAN-2008 (DSB):
*        Original version.
*     29-APR-2008 (DSB):
*        Added parameter MORETEXT.
*     25-JUN-2009 (DSB):
*        Updated to use new provenance API.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS public constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDG_PAR'          ! NDG constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CREATR*255       ! Name of creator
      CHARACTER MORE*(DAT__SZLOC) ! Locator for MORE structure
      INTEGER INDF1              ! Identifier for NDF being modified
      INTEGER INDF2              ! Identifier for parent NDF
      INTEGER IPROV              ! Identifier for provenance info
      LOGICAL CNCLMR             ! Cancel the MORE parameter on exit?
      LOGICAL ISROOT             ! Is the parent an orphan?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the NDF to be modified.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF1, STATUS )

*  Obtain the parent NDF.
      CALL LPG_ASSOC( 'PARENT', 'READ', INDF2, STATUS )

*  ABort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get additional information first as a set of text strings
*  using the MORETEXT parameter.
      CALL KPG1_GTMOR( 'MORETEXT', MORE, STATUS )

*  If a null value was supplied for MORETEXT, annul the error and
*  attempt to get the additional information directly as an HDS object
*  using parameter MORE.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

         MORE = DAT__NOLOC
         CALL DAT_ASSOC( 'MORE', 'READ', MORE, STATUS )

         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MORE = DAT__NOLOC
         ELSE
            CNCLMR = .TRUE.
         END IF

      ELSE
         CNCLMR = .FALSE.
      END IF

*  Obtain the creator string.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET0C( 'CREATOR', CREATR, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CREATR = ' '
         END IF
      END IF

*  Obtain the root flag.
      CALL PAR_GET0L( 'ISROOT', ISROOT, STATUS )

*  Read provenance information from the child NDF.
      CALL NDG_READPROV( INDF1, CREATR, IPROV, STATUS )

*  Read provenance information from the parent NDF and add it into the
*  structure holding provenance information read from the child NDF.
      CALL NDG_PUTPROV( IPROV, INDF2, MORE, AST__NULL, ISROOT,
     :                  STATUS )

*  Store the modified provenance information in the child NDF.
      CALL NDG_WRITEPROV( IPROV, INDF1, .FALSE., STATUS )

*  Arrive here if an error occurrs.
 999  CONTINUE

*  Free the Provenance information.
      CALL NDG_FREEPROV( IPROV, STATUS )

*  Free the MORE structure locator.
      IF( MORE .NE. DAT__NOLOC ) THEN
         IF( CNCLMR ) THEN
            CALL DAT_CANCL( 'MORE', STATUS )
         ELSE
            CALL DAT_ANNUL( MORE, STATUS )
         END IF
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PROVADD_ERR', 'PROVADD: Error adding '//
     :                 'provenance information to an NDF.', STATUS )
      END IF

      END
