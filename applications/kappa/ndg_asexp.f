      SUBROUTINE NDG_ASEXP( GRPEXP,  IGRP1, IGRP2, SIZE, FLAG, STATUS )
*+
*  Name:
*     NDG_ASEXP

*  Purpose:
*     Store names of existing NDFs supplied as a group expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_ASEXP( GRPEXP, IGRP1, IGRP2, SIZE, FLAG, STATUS )

*  Description:
*     The supplied group expression is parsed (using the facilities of 
*     the GRP routine GRP_GROUP, see SUN/150) to produce a list of 
*     explicit NDF names which are appended to the end of the group 
*     identified by IGRP2. A new group is created if the IGRP2 is 
*     equal to GRP__NOID.  Any names containing wildcards are expanded 
*     into a list of NDF names.
*
*     If any of the NDFs in the group identified by IGRP2 cannot be 
*     accessed, an error is reported and STATUS is returned equal to 
*     NDG__NOFIL. If this happens strings holding the name of each 
*     bad NDF are appended to the group identified by IGRP1 (so long 
*     as IGRP1 is not equal to GRP__NOID).

*  Arguments:
*     GRPEXP = CHARACTER * ( * ) (Given)
*        The group expression specifying the NDF names to be stored 
*        in the group.
*     IGRP1 = INTEGER (Given)
*        The identifier of a group to which the names of any 
*        inaccessable NDFs will be appended. The group should already
*        have been created by a call to GRP_NEW, and should be deleted
*        when no longer needed by a call to GRP_DELET. If IGRP1 is 
*        supplied equal to symbolic constant GRP__NOID, then no 
*        information is stored describing the bad NDFs. 
*     IGRP2 = INTEGER (Given and Returned)
*        The identifier of the group in which the NDF names are to be
*        stored.
*     SIZE = INTEGER (Returned)
*        The total number of NDF names in the returned group.
*     FLAG = LOGICAL (Returned)
*        If the group expression was terminated by the GRP "flag
*        character", then FLAG is returned .TRUE. Otherwise it is
*        returned .FALSE. Returned .FALSE. if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine checks that the specified files exist, and also 
*     checks that any .sdf files contain legal native NDFs. However, it 
*     does not attempt to check the legality of foreign data format files.
*     -  If an error occurs, the group is returned unaltered. If no
*     group was supplied, an empty group is returned.
*     -  Routine NDG_ASSOC can be used if the NDF names are to be 
*     obtained through the environment rather than being supplied
*     as an argument.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1992 (DSB):
*        Original version.
*     29-AUG-1997 (DSB):
*        Modified to work with automatic NDF data conversion.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDG_CONST'        ! NDG constants.
      INCLUDE 'NDG_ERR'          ! NDG error constants.
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Arguments Given:
      CHARACTER GRPEXP*(*)
      INTEGER   IGRP1

*  Arguments Given and Returned:
      INTEGER   IGRP2
      
*  Arguments Returned:
      INTEGER   SIZE
      LOGICAL   FLAG

*  Status:
      INTEGER   STATUS             ! Global status

*  Local Variables:
      INTEGER   ADDED              ! No. of names added to group.
      INTEGER   SIZE0              ! The size of the group on entry to
                                   ! this routine.
      CHARACTER TYPE*(GRP__SZTYP)  ! The group type string

*.

*  Ensure a .FALSE. value for FLAG is returned if an error has already 
*  occured.
      FLAG = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the supplied value of IGRP2 is GRP__NOID, create a new group to
*  hold the names of the NDFs. Set the group case insensitive if the
*  host file system is case insensitive.
      IF( IGRP2 .EQ. GRP__NOID ) THEN
         TYPE = ' '
         TYPE = 'A list of existing data sets'
         CALL GRP_NEW( TYPE, IGRP2, STATUS )
         SIZE0 = 0

*  If a group identifier was supplied, get the original size of the
*  group.
      ELSE
         CALL GRP_GRPSZ( IGRP2, SIZE0, STATUS )

      END IF

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( NDG__UCASE ) CALL GRP_SETCS( IGRP2, .FALSE., STATUS )

*  Append the names to the end of the group.
      CALL GRP_GRPEX( GRPEXP, GRP__NOID, IGRP2, SIZE, ADDED, FLAG,
     :                STATUS )

*  Extend the the group of NDFs by expanding any names which contained
*  wild cards. A check is made that the files exist, but not that they can 
*  be read as NDFs. NDG1_NDFCH gives messages identifying any unusable files. 
*  The final group consists of a set of file specifications which can be
*  opened by the NDF_ library. Only the group entries which have been added 
*  by this routine are checked.
      CALL NDG1_NDFCH( IGRP2, SIZE0 + 1, IGRP1, STATUS )

*  Update the SIZE argument to take account of the new group
*  members produced as a result of the expansion of any wild cards. This
*  needs to happen even if an error has been reported, so do it in a new
*  error reporting context.
      CALL ERR_BEGIN( STATUS )
      CALL GRP_GRPSZ( IGRP2, SIZE, STATUS )
      CALL ERR_END( STATUS )

*  If an error has been reported (other than "some NDFs not accessible")
*  set the group back to its original size.
      IF( STATUS .NE. SAI__OK .AND. STATUS .NE. NDG__NOFIL ) THEN
         CALL ERR_BEGIN( STATUS )

         SIZE = SIZE0
         CALL GRP_SETSZ( IGRP2, SIZE0, STATUS )

         CALL ERR_END( STATUS )
      END IF

*  If an error occured give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', GRPEXP )
         CALL ERR_REP( 'NDG_ASEXP_ERR2',
     :     'NDG_ASEXP: Error obtaining a group of existing data sets '//
     :     'using group expression "^P"', STATUS )
      END IF

      END
