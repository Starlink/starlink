      SUBROUTINE KPG1_WGNDF( PARAM, IGRP0, MAXSIZ, MINSIZ, TEXT, IGRP,
     :                       SIZE, STATUS )
*+
*  Name:
*     KPG1_WGNDF

*  Purpose:
*     Get a group of output NDF names.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WGNDF( PARAM, IGRP0, MAXSIZ, MINSIZ, TEXT, IGRP, SIZE,
*                      STATUS )

*  Description:
*     The supplied parameter is used to get a GRP group expression (see
*     SUN/150 ) holding a list of NDFs which are to be created by the
*     calling application (the syntax of the group expression is
*     defined by the current default GRP control characters).
*     Modification elements within the group expression are based on
*     the group identified by IGRP0. If the group expression is flagged,
*     then the current parameter value is cancelled, the string
*     supplied in TEXT is displayed (if it is not blank) and another
*     group expression is obtained. The NDFs specified by the second
*     group expression are added to the group holding the NDFs
*     specified by the first group expression. The group continues to
*     be expanded in this way until a group expression is obtained
*     which is not flagged, or a null value is given, or the limit on
*     the number of NDFs (MAXSIZ) is reached.  If the final group
*     contains more than MAXSIZ NDFs, then all but the first MAXSIZ
*     NDFs are removed from the group. The user is warned if this
*     happens. If MAXSIZ is supplied with the value zero no limit is
*     imposed on the number of NDFs within the group.  If the final
*     group contains less than MINSIZ NDFs then the user is told to
*     supply more, and is re-prompted for further NDF names. All
*     messages issued by this routine have a priority level of
*     MSG__NORM.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter (of type LITERAL).
*     IGRP0 = INTEGER (Given)
*        The NDG identifier for a group containing a set of NDF names
*        to be used as the basis for any modification elements contained
*        within the group expressions. If this is supplied equal to
*        GRP__NOID then modification elements are left un-expanded.
*     MAXSIZ = INTEGER (Given)
*        The maximum number of NDFs which can be allowed in the
*        returned group. If zero is supplied, no limit is imposed.
*     MINSIZ = INTEGER (Given)
*        The minimum number of NDFs which can be allowed in the
*        returned group. If zero is supplied, then the returned group
*        may contain no NDFs.
*     TEXT = CHARACTER * ( * ) (Given)
*        The text to display between issuing prompts for successive
*        group expressions. If blank then no text is displayed.
*     IGRP = INTEGER (Returned)
*        The NDG identifier for the returned group holding all the
*        specified NDFs.
*     SIZE = INTEGER (Returned)
*        The number of files in the output group. Returned equal to 1 if
*        STATUS is not equal to SAI__OK.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
*     15-SEP-1993 (DSB):
*        Original version, copied from IRAS90 routine IRM_WRNDF.FOR
*     6-OCT-1998 (DSB):
*        Copied from POLPACK routine WRNDF.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER IGRP0
      INTEGER MAXSIZ
      INTEGER MINSIZ
      CHARACTER TEXT*(*)

*  Arguments Returned:
      INTEGER IGRP
      INTEGER SIZE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL FLAG               ! True if the supplied group expression
                                 ! was terminated by a minus sign.
      INTEGER NEWSIZ             ! The new group size.

*.

*  Ensure size is returned equal to 1 if an erroro condition exists.
      SIZE = 1

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the argument MAXSIZ and MINSIZ are OK.
      IF( MAXSIZ .LT. MINSIZ .AND. MAXSIZ .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MAX', MAXSIZ )
         CALL MSG_SETI( 'MIN', MINSIZ )
         CALL ERR_REP( 'KPG1_WGNDF_ERR1', 'KPG1_WGNDF: Maximum '//
     :                 'allowed number of NDFs (^MAX) is less '//
     :                 'than the minimum (^MIN) - programming error.',
     :                 STATUS )
         GO TO 999
      END IF

*  The names of the NDFs are stored in a GRP group. The NDG package is
*  used to access the actual NDFs using the names stored in this group.
*  The identifier for this group (IGRP) is initially set to an invalid
*  value (GRP__NOID) in order to cause a new group to be created by
*  routine NDG_CREAT.
      IGRP = GRP__NOID

*  The user specifies the NDFs by means of one or more "group
*  expressions". The user can terminate a group expression with the
*  "flag character" (usually a minus sign) if more NDFs remain to be
*  specified. This is known as "flagging" the group expression. If this
*  is done a further call to NDG_CREAT is made to obtain a further set
*  of NDF names, which are appended to the list of names already given.
      FLAG = .TRUE.
      SIZE = 0
      DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )

*  Get another group expression and expand it into a list of NDF names,
*  storing the names in the GRP group identified by IGRP. On the first
*  pass through this loop, IGRP is invalid and so a new group is created
*  and IGRP is returned holding a valid group identifier.  On successive
*  passes, new names are appended to the group created on the first
*  pass.
         CALL NDG_CREAT( PARAM, IGRP0, IGRP, NEWSIZ, FLAG, STATUS )

*  If no error occured, save the new group size.
         IF( STATUS .EQ. SAI__OK ) SIZE = NEWSIZ

*  If the current group size is larger than the maximum allowed group
*  size, warn the user that only the first MAXSIZ files will be used,
*  delete the trailing names from the group, and cancel the re-prompt
*  flag.
         IF( SIZE .GT. MAXSIZ .AND. MAXSIZ .GT. 0 ) THEN
            CALL MSG_BLANKIF( MSG__NORM, STATUS )

            IF( MAXSIZ .GT. 1 ) THEN
               CALL MSG_SETI( 'MAX', MAXSIZ )
               CALL MSG_OUTIF( MSG__NORM, 'KPG1_WGNDF_MSG1',
     :       'WARNING: Only the first ^MAX NDFs names can be used',
     :                          STATUS )

            ELSE
               CALL MSG_OUTIF( MSG__NORM, 'KPG1_WGNDF_MSG2',
     :                'WARNING: Only the first NDF name can be used',
     :                          STATUS )
            END IF

            CALL MSG_BLANKIF( MSG__NORM, STATUS )

            CALL GRP_SETSZ( IGRP, MAXSIZ, STATUS )
            SIZE = MAXSIZ
            FLAG = .FALSE.

*  If the group expression was flagged, and the maximum no. of NDFs has
*  not been reached, prepare to get another group expression by
*  cancelling the parameter value and telling the user what to do.
         ELSE IF( FLAG ) THEN
            IF( SIZE .LT. MAXSIZ .OR. MAXSIZ .LE. 0 ) THEN

               CALL PAR_CANCL( PARAM, STATUS )
               IF( TEXT .NE. ' ' ) CALL MSG_OUTIF( MSG__NORM,
     :                                             'KPG1_WGNDF_MSG3',
     :                                             TEXT, STATUS )

*  If the limit on the number of NDFs has been reached, warn the user
*  that no more NDFs can be specified, and cancel the re-prompt flag.
            ELSE

               CALL MSG_BLANKIF( MSG__NORM, STATUS )
               CALL MSG_OUTIF( MSG__NORM, 'KPG1_WGNDF_MSG4',
     :             'WARNING: No more NDF names allowed', STATUS )
               CALL MSG_BLANKIF( MSG__NORM, STATUS )
               FLAG = .FALSE.

            END IF

         END IF

*  If the group is complete...
         IF( .NOT. FLAG ) THEN

*  Check that the minimum no. of NDFS has been supplied. If not, ask the
*  user for more NDFs, and set FLAG to cause another prompt to be
*  issued.
            IF( SIZE .LT. MINSIZ ) THEN

               IF( SIZE .GT. 0 ) THEN
                  CALL MSG_SETI( 'I', MINSIZ - SIZE )
                  CALL MSG_OUTIF( MSG__NORM, 'KPG1_WGNDF_MSG5',
     :              'WARNING: At least ^I more NDF(s) must be supplied',
     :                            STATUS )
               ELSE
                  CALL MSG_SETI( 'I', MINSIZ )
                  CALL MSG_OUTIF( MSG__NORM, 'KPG1_WGNDF_MSG6',
     :              'WARNING: At least ^I NDF(s) must be supplied',
     :                            STATUS )
               END IF

               IF( TEXT .NE. ' ' ) CALL MSG_OUTIF( MSG__NORM,
     :                                             'KPG1_WGNDF_MSG7',
     :                                             TEXT, STATUS )
               CALL PAR_CANCL( PARAM, STATUS )
               FLAG = .TRUE.

            END IF

         END IF

      END DO

*  Arrive here if an error occurs.
 999  CONTINUE

*  If a null value was supplied, then annul the error if the group
*  is allowed to be empty.
      IF( STATUS .EQ. PAR__NULL .AND. MINSIZ .LE. 0 ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error has occured, return SIZE with the value 1 to avoid
*  potential access violations if SIZE is used as the dimension of a
*  passed array.
      IF( STATUS .NE. SAI__OK ) SIZE = 1

      END
