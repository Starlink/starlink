      SUBROUTINE REMQUAL( STATUS )
*+
*  Name:
*     REMQUAL

*  Purpose:
*     Remove specified quality definitions from an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL REMQUAL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine removes selected quality name definitions from an NDF
*     (see task SETQUAL) and optionally clears the corresponding bit
*     in the Quality array of the supplied NDF.  All quality names
*     information may be removed by specifying a quality name of "ANY".
*
*     An error will be reported if an attempt is made to remove a quality
*     name that has been flagged as "read-only" (e.g. using the READONLY
*     parameter of the SETQUAL application).

*  Usage:
*     remqual ndf qnames

*  ADAM Parameters:
*     CLEAR = _LOGICAL (Update)
*        If TRUE, the bits in the NDF's Quality array that correspond to
*        the removed quality names will be cleared. If FALSE, no change
*        will be made to the Quality array. [FALSE]
*     NDF = NDF (Update)
*        The NDF to be modified.
*     QNAMES = LITERAL (Read)
*        A group of up to 10 quality names to be removed from the input
*        NDF. The group may be supplied as a comma separated list, or
*        within a text file (in which case the name of the text file should
*        be given, preceeded by a "^" character.) If more than 10 names are
*        supplied, only the first 10 are used. If any of the supplied
*        quality names are not defined in the NDF, then warning
*        messages are given but the application continues to remove any
*        other specified quality names. If the string ANY is specified,
*        then all defined quality names are removed. If no defined
*        quality names remain, the structure used to store quality name
*        information is deleted. This feature can be used to get rid of
*        corrupted quality name information.

*  Examples:
*     remqual "m51*" any
*        This example will remove all defined quality names from all
*        NDFs with names starting with the string "m51".

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2002 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     Copyright (C) 2021 East Asian Observatory.
*     All Rights Reserved.

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
*     28-OCT-1991 (DSB):
*        Original version.
*     17-JAN-2002 (DSB):
*        Brought into KAPPA.
*     15-FEB-2008 (DSB):
*        Initialise NNAMES.
*     11-MAR-2021 (DSB):
*        Added parameter CLEAR.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants
      INCLUDE 'IRQ_ERR'          ! IRQ error values
      INCLUDE 'PAR_ERR'          ! PAR error values

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXQNM             ! Max. no. of quality names which can
                                 ! be deleted.
      PARAMETER ( MAXQNM = 10 )

*  Local Variables:
      CHARACTER LOCS(5)*(DAT__SZLOC)! Locators to the quality name information
      CHARACTER QNAME*(IRQ__SZQNM)! Qaulity name to be removed
      CHARACTER XNAME*(DAT__SZNAM)! NDF enstension holding quality names
      INTEGER ADDED              ! No. of names added to group
      INTEGER IGRP               ! Group identifier
      INTEGER INAME              ! Name count
      INTEGER NDFIN              ! Identifier for the input NDF
      INTEGER NNAMES             ! No. of quality names to be removed
      LOGICAL CLEAR              ! Clear bits in the NDFs Quality array?
      LOGICAL FLAG               ! Group expression terminated by a minus sign?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the input NDF.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDFIN, STATUS )

*  See if bits are to be cleared in the NDFs Quality array.
      CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

*  Create a group to hold the quality names which are to be removed.
      CALL GRP_NEW( 'QUALITY NAMES', IGRP, STATUS )

*  Ensure that the group is case insensitive.
      CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the quality names to be removed, storing them in the group
*  just created. Loop until the group expression obtained from the
*  environment does not terminate with a minus sign.
      NNAMES = 0
      FLAG = .TRUE.
      DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )

*  Unless the maximum no. of quality names have already been supplied,
*  get more quality names.
         IF( NNAMES .LT. MAXQNM ) THEN
            CALL GRP_GROUP( 'QNAMES', GRP__NOID, IGRP, NNAMES, ADDED,
     :                       FLAG, STATUS )

*  If the maximum no. of quality names have already been supplied, warn
*  the user.
         ELSE
            FLAG = .FALSE.
            CALL MSG_SETI( 'MX', MAXQNM )
            CALL MSG_OUT( 'REMQUAL_MSG1', 'WARNING: Only ^MX quality '//
     :                    'names can be removed in a single run',
     :                    STATUS )

*  If too many quality names have been supplied, delete the surplus
*  names from the group.
            IF( NNAMES .GT. MAXQNM ) THEN
               CALL MSG_SETI( 'MX', MAXQNM )
               CALL MSG_OUT( 'REMQUAL_MSG2','WARNING: Only the first '//
     :                       '^MX supplied quality names will be used',
     :                       STATUS )
               CALL GRP_SETSZ( IGRP, MAXQNM, STATUS )
            END IF

         END IF

      END DO

*  The user may have indicated the end of the group by giving a null
*  value for the parameter. This would normally cause the application to
*  abort, so annul the error in order to prevent this from happening.
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  If there are no quality names to be removed, abort.
      IF( NNAMES .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'REMQUAL_ERR1', 'REMQUAL: No quality names '//
     :                 'specified', STATUS )
      END IF

*  Abort if an error has occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to locate any existing quality name information in the input
*  NDF. If such information is found, LOC is returned holding a set of
*  5 HDS locators which identify the NDF and the various components of
*  the quality information. XNAME is returned holding the name of the
*  NDF extension in which the information was found. If no quality name
*  information is found, then an error is reported.
      CALL IRQ_FIND( NDFIN, LOCS, XNAME, STATUS )

*  Annul any error, and indicate no names should be deleted.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL IRQ_RLSE( LOCS, STATUS )
         CALL ERR_ANNUL( STATUS )
         LOCS( 1 ) = ' '
         NNAMES = 0
      END IF

*  Loop round to remove each specified quality name.
      DO INAME = 1, NNAMES

*  Get the next quality name.
         CALL GRP_GET( IGRP, INAME, 1, QNAME, STATUS )

*  If required, clear the corresponding bit in the Quality array and then
*  remove the quality name.
         IF( STATUS .EQ. SAI__OK ) THEN
            IF( CLEAR ) CALL IRQ_RESQ( LOCS, QNAME, STATUS )
            CALL IRQ_REMQN( LOCS, QNAME, STATUS )

*  If the quality name was not found, annul the error and give a warning
*  message.
            IF( STATUS .EQ. IRQ__NOQNM ) THEN
               CALL ERR_ANNUL( STATUS )
               CALL MSG_SETC( 'QN', QNAME )
               CALL MSG_OUT( 'REMQUAL_MSG4', '  Quality name "^QN" '//
     :                       'not found', STATUS )

*  Otherwise, add the quality name to the list of quality names
*  successfully removed.
            ELSE
               IF( QNAME .NE. 'ANY' ) THEN
                  CALL MSG_SETC( 'QN', QNAME )
                  CALL MSG_OUT( 'REMQUAL_MSG5', '  Quality name '//
     :                          '"^QN" removed', STATUS )

*  Give a better message if all quality names have been removed.
               ELSE
                  CALL MSG_OUT( 'REMQUAL_MSG6', '  All quality '//
     :                          'names removed', STATUS )
               END IF

            END IF

         END IF

      END DO

*  If any quality information was found in the NDF, note the number of
*  remaining quality names, and release the quality name information.
      IF( LOCS( 1 ) .NE. ' ' ) THEN
         CALL IRQ_NUMQN( LOCS, NNAMES, STATUS )
         CALL IRQ_RLSE( LOCS, STATUS )

*  If no quality names are left, delete the NDF extension.
         IF( NNAMES .EQ. 0 ) CALL NDF_XDEL( NDFIN, XNAME, STATUS )

      END IF

*  Delete the group which holds the quality names.
 999  CONTINUE
      CALL GRP_DELET( IGRP, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'REMQUAL_ERR2', 'REMQUAL: Unable to remove '//
     :                 'quality names from a set of NDFs.', STATUS )
      END IF

      END
