      SUBROUTINE NDG_NDFPR( INDF1, CLIST, IGRP, INDEX, INDF2, STATUS )
*+
*  Name:
*     NDG_NDFPR

*  Purpose:
*     Propagate NDF information to create a new NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_NDFPR( INDF1, CLIST, IGRP, INDEX, INDF2, STATUS )

*  Description:
*     The routine creates a new NDF data structure specified at a given
*     index within the given group, and returns an identifier for it.
*     The shape, data type, etc. of this new NDF are based on a
*     existing "template" NDF, and the values of components of this
*     template may be selectively propagated to initialise the new data
*     structure. It is equivalent to NDF_PROP.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for an existing NDF (or NDF section) to act as a
*        template.
*     CLIST = CHARACTER * ( * ) (Given)
*        A comma-separated list of the NDF components which are to be
*        propagated to the new data structure. By default, the HISTORY,
*        LABEL and TITLE components and all extensions are propagated.
*        See below for further details.
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group holding the names of NDFs.
*     INDEX = INTEGER (Given)
*        The index within the group at which the name of the NDF to be
*        accessed is stored.
*     INDF2 = INTEGER (Returned)
*        Identifier for the new NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF2 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The NDF__NOID
*     constant is defined in the include file NDF_PAR.

*  Component Propagation:
*     -  The template components whose values are to be propagated to
*     initialise the new data structure are specified via the CLIST
*     argument. Thus CLIST='DATA,QUALITY' would cause the new NDF to
*     inherit its DATA and QUALITY values (if available) from the
*     template structure, in addition to those propagated by default.
*     Component propagation may be suppressed by supplying a component
*     name with the prefix 'NO'. Thus CLIST='DATA,NOHISTORY' would
*     propagate the DATA component, but suppress propagation of
*     HISTORY. If component names appear more than once in the CLIST
*     value, then the last occurrence takes precedence.
*     -  Propagation of specific NDF extensions may be suppressed by
*     using 'NOEXTENSION()' as one of the items in the CLIST argument;
*     a list of the extensions to be suppressed should appear between
*     the parentheses. Thus CLIST='AXIS,NOEXTENSION(IRAS,ASTERIX)'
*     would propagate the AXIS component, but suppress propagation of
*     the IRAS and ASTERIX extensions (if present). Propagation of
*     suppressed extensions may be re-enabled by specifying
*     'EXTENSION()' in a similar manner at a later point in the CLIST
*     value.
*     -  Component names in the CLIST argument may be abbreviated to 3
*     characters, but extension names must appear in full.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1992 (DSB):
*        Original version.
*     29-AUG-1997 (DSB):
*        Modified to use automatic NDF data conversion.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDG_CONST'        ! NDG constants

*  Arguments Given:
      INTEGER INDF1
      CHARACTER CLIST*(*)
      INTEGER IGRP
      INTEGER INDEX

*  Arguments Returned:
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME*(GRP__SZNAM)! NDF file name (without file type).
      INTEGER PLACE              ! NDF placeholder.
*.

*  Set an initial value for the INDF argument.
      INDF2 = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( NDG__UCASE ) CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  Get the required name.
      CALL GRP_GET( IGRP, INDEX, 1, NAME, STATUS )

*  If the name could not be obtained, set the name blank and abort.     
      IF ( STATUS .NE. SAI__OK ) THEN
         NAME = ' '
         GO TO 999
      END IF

*  Create the NDF place holder.
      CALL NDF_OPEN( DAT__ROOT, NAME, 'WRITE', 'NEW', INDF2, PLACE, 
     :               STATUS )

* Copy the required components.
      CALL NDF_SCOPY( INDF1, CLIST, PLACE, INDF2, STATUS )

*  If an error occured, add context information.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN

         IF( NAME .NE. ' ' ) THEN
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'NDG_NDFPR_ERR1',
     :         'NDG_NDFPR: Unable to get an NDF identifier for "^NAME"',
     :                     STATUS )

         ELSE
            CALL ERR_REP( 'NDG_NDFPR_ERR2',
     :   'NDG_NDFPR: Unable to get an NDF identifier for a created'//
     :   'data set.', STATUS )

         END IF

      END IF

      END
