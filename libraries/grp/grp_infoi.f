      SUBROUTINE GRP_INFOI( IGRP, INDEX, ITEM, VALUE, STATUS )
*+
*  Name:
*     GRP_INFOI

*  Purpose:
*     Retrieve an item of integer information about a name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_INFOI( IGRP, INDEX, ITEM, VALUE, STATUS )

*  Description:
*     This routine returns an item of integer information about a
*     single name from a group. The item can be any one of those
*     described under argument ITEM.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP group identifier.
*     INDEX = INTEGER (Given)
*        An index within the group specified by IGRP.  If the supplied
*        value is outside the bounds of the group, then a "null" value
*        is returned for VALUE as described below, and an error is
*        reported.
*     ITEM = CHARACTER * ( * ) (Given)
*        The name of an item of information. This can be any of the
*        following (abbreviations are not allowed):
*
*        MODGRP  -  If the name was specified by means of a
*        modification element, then the the GRP identifier of the group
*        used as a basis for the modification element is returned in
*        VALUE. If the name was not specified by a modification
*        element, then GRP__NOID is returned.  If INDEX is outside the
*        bounds of the group, then a value of GRP__NOID is returned.
*
*        MODIND  -  If the name was specified by means of a
*        modification element, then the index of the original name
*        (upon which the returned name was based) is returned in VALUE.
*        This is an index into the group identified by MODGRP. If
*        MODGRP is returned equal to GRP__NOID, then MODIND will be
*        zero.
*
*        DEPTH  -  The number of levels of indirection at which the
*        name was specified is returned in VALUE. Names given
*        explicitly within a group expression have a DEPTH value of
*        zero. Names given explicitly within a DEPTH zero indirection
*        element have DEPTH 1. Names given explicitly within a DEPTH 1
*        indirection element, have DEPTH 2, etc. If INDEX is out of
*        bounds, then zero is returned.
*
*     VALUE = INTEGER (Returned)
*        The requested item of information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'GRP_ERR'          ! GRP error values.

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER IGRP
      INTEGER INDEX
      CHARACTER ITEM*(*)

*  Arguments Returned:
      INTEGER VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Constants:
      INTEGER SZITEM             ! Max. length of an item name.
      PARAMETER ( SZITEM = 6 )

*  Local Variables:
      INTEGER DEPTH              ! No. of levels of indirection at
                                 ! which the name was specified.
      INTEGER IFILE              ! The index within the groups FILES
                                 ! array at which the indirection file
                                 ! name is stored.
      CHARACTER LITEM*(SZITEM)   ! Local copy of ITEM.
      INTEGER MODGRP             ! Identifier of group used as basis
                                 ! for modification element.
      INTEGER MODIND             ! Index of name within MODGRP from
                                 ! which the requested name was derived.
      CHARACTER NAME*(GRP__SZNAM)! The name.
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.

*.

*  Set up null values.
      DEPTH = 0
      MODGRP = GRP__NOID
      MODIND = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If the index is outside the bounds of the group, report an error.
      IF( INDEX .LE. 0 .OR. INDEX .GT. CMN_GSIZE( SLOT ) ) THEN
         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'I', INDEX )
         CALL MSG_SETI( 'S', CMN_GSIZE( SLOT ) )
         CALL ERR_REP( 'GRP_INFOI_ERR1',
     :               'GRP_INFOI: Group index (^I) out of bounds [1,^S]',
     :                 STATUS )

*  Otherwise get the information from the group.
      ELSE
         CALL GRP1_GTELM( SLOT, INDEX, NAME, DEPTH, IFILE, MODGRP,
     :                    MODIND, STATUS )

      END IF

*  If all has gone OK, store the relevant item.
      IF( STATUS .EQ. SAI__OK ) THEN

         LITEM = ITEM
         CALL CHR_UCASE( LITEM )

         IF( LITEM( : 6 ) .EQ. 'MODGRP' ) THEN
            VALUE = MODGRP

         ELSE IF( LITEM( : 6 ) .EQ. 'MODIND' ) THEN
            VALUE = MODIND

         ELSE IF( LITEM( : 5 ) .EQ. 'DEPTH' ) THEN
            VALUE = DEPTH

         ELSE
            STATUS = GRP__BADIT
            CALL MSG_SETC( 'ITEM', ITEM )
            CALL ERR_REP( 'GRP_INFOI_ERR2',
     :'GRP_INFOI: Unknown item of information requested - ^ITEM.',
     :                       STATUS )
         END IF

      END IF

*  If an error occurred, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'IT', ITEM )
         CALL ERR_REP( 'GRP_INFOI_ERR3',
     :      'GRP_INFOI: Unable to get the "^IT" attribute of a group '//
     :      'name.' , STATUS )
      END IF

      END
