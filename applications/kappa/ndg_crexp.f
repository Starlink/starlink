      SUBROUTINE NDG_CREXP( GRPEXP, IGRP0, IGRP, SIZE, FLAG, STATUS )
*+
*  Name:
*     NDG_CREXP

*  Purpose:
*     Store the names of a specified group of NDF to be created.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_CREXP( GRPEXP, IGRP0, IGRP, SIZE, FLAG, STATUS )

*  Description:
*     The supplied group expression is parsed (using the facilities of 
*     the GRP routine GRP_GROUP, see SUN/150) to produce a list of 
*     explicit NDF names. No check is made to see if these NDFs exist 
*     or not, and any wild-cards in the NDF names are ignored. The names
*     are appended to the group identified by IGRP. If IGRP has the 
*     value GRP__NOID on entry, then a new group is created and IGRP is 
*     returned holding the new group identifier.
*
*     If IGRP0 holds a valid group identifier on entry, then the group
*     identified by IGRP0 is used as the basis for any modification
*     element contained in the supplied group expression. If IGRP0 holds
*     an invalid identifier (such as GRP__NOID) on entry then 
*     modification elements are included literally in the output group.
*
*     The asterisk (or equivalent) within a modification element is
*     replaced by file base-name taken from the corresponding input file 
*     name. If no directory path is supplied in the modification
*     element, then the path from the corresponding input file is used.
*     If no file type is supplied in the modification element, then 
*     the first file type listed in the current value of the
*     NDF_FORMATS_OUT environment variable (see SSN/20) is used. If this
*     is "*" then the file type is copied from the corresponding input
*     file. 

*  Arguments:
*     GRPEXP = CHARACTER*(*) (Given)
*        The group expression specifying the NDF names to be stored in 
*        the group.
*     IGRP0 = INTEGER (Given)
*        The GRP identifier for the group to be used as the basis for
*        any modification elements. 
*     IGRP = INTEGER (Given and Returned)
*        The GRP identifier for the group to which the supplied NDF
*        names are to be appended. 
*     SIZE = INTEGER (Returned)
*        The total number of NDF names in the returned group.
*     FLAG = LOGICAL (Returned)
*        If the group expression was terminated by the GRP "flag"
*        character, then FLAG is returned .TRUE. Otherwise it is
*        returned .FALSE. Retuned .FALSE. if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
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
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDG_CONST'        ! NDG constants.
      INCLUDE 'PSX_ERR'          ! PSX error constants

*  Arguments Given:
      CHARACTER GRPEXP*(*)
      INTEGER   IGRP0

*  Arguments Given and Returned:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER SIZE
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  Externals:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER COMP*(GRP__SZNAM)  ! HDS component path from supplied spec
      CHARACTER COMP0*(GRP__SZNAM) ! HDS component path from basis spec
      CHARACTER DEFTYP*(GRP__SZNAM)! Default file type from NDF_FORMATS_OUT
      CHARACTER FDIRN*(GRP__SZNAM) ! Directory path from supplied spec.
      CHARACTER FDIRN0*(GRP__SZNAM)! Directory path from basis spec.
      CHARACTER FMTOUT*(NDG__SZFMT)! List of output NDF formats
      CHARACTER FNAME*(GRP__SZNAM) ! Base name from supplied spec
      CHARACTER FNAME0*(GRP__SZNAM)! Base name from basis spec
      CHARACTER FSPEC*(GRP__SZNAM) ! Total file specification 
      CHARACTER FSPEC0*(GRP__SZNAM)! Total file spec from basis group 
      CHARACTER FTYPE*(GRP__SZNAM) ! File type from supplied spec
      CHARACTER FTYPE0*(GRP__SZNAM)! File type from basis spec
      CHARACTER NAME*(GRP__SZNAM)  ! Current name.
      CHARACTER SLICE*(GRP__SZNAM) ! NDF slice from supplied spec
      CHARACTER SLICE0*(GRP__SZNAM)! NDF slice from basis spec
      CHARACTER TYPE*(GRP__SZTYP)  ! Group type string.
      INTEGER ADDED              ! No. of names added to the group.
      INTEGER CL                 ! Index of 1st closing parenthesis
      INTEGER COMMA              ! Index of 1st comma in NDF_FORMATS_OUT
      INTEGER FORM               ! The form of the file spec.
      INTEGER I                  ! Loop count.
      INTEGER IAT                ! Index of last non-blank character.
      INTEGER MODIND             ! Index of basis spec
      INTEGER MODSIZ             ! Size of basis group.
      INTEGER OP                 ! Index of 1st opening parenthesis
      INTEGER SIZE0              ! Size of group on entry.
      INTEGER TMPGRP             ! Group holding NDF names only.
      LOGICAL IN                 ! Does IGRP identify a valid group?
      LOGICAL INGRP              ! Does IGRP0 identify a valid group?
*.

*  Ensure that FLAG is returned .FALSE if an error has already occured.
      FLAG = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the current value of environment variable NDF_FORMATS_OUT.
*  Annul the error and use a blank value if it is not defined.
      CALL PSX_GETENV( 'NDF_FORMATS_OUT', FMTOUT, STATUS )
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         FMTOUT = ' '
      ELSE
         CALL CHR_RMBLK( FMTOUT )
      END IF

*  If it is not defined, use a default value of "." (i.e. use native NDF
*  format).
      IF( FMTOUT .EQ. ' ' ) FMTOUT = '.'

*  Get the file type from the first entry in NDF_FORMATS_OUT (stored within
*  opening and closing parenthesise before the first comma). If the first
*  entry is "*", then store "*" (meaning "use the input file type"), otherwise 
*  if the first entry is "." or does not contain a file type, store "."
*  (meaning "use native NDF format").
      COMMA = INDEX( FMTOUT, ',' )
      IF( COMMA .EQ. 0 ) COMMA = CHR_LEN( FMTOUT ) + 1

      IF( FMTOUT( : COMMA - 1 ) .EQ. '.' ) THEN
         DEFTYP = '.'

      ELSE IF( FMTOUT( : COMMA - 1 ) .EQ. '*' ) THEN
         DEFTYP = '*'

      ELSE
         OP = INDEX( FMTOUT( : COMMA - 1 ), '(' )
         CL = INDEX( FMTOUT( : COMMA - 1 ), ')' )
         IF( OP .NE. 0 .AND. CL .NE. 0 .AND. CL - OP .GT. 1 ) THEN
            DEFTYP = FMTOUT( OP + 1 : CL - 1 )
         ELSE
            DEFTYP = '.'
         END IF

      END IF

*  If the supplied value of IGRP is GRP__NOID, create a new group to
*  hold the names of the NDFs.
      IF( IGRP .EQ. GRP__NOID ) THEN
         TYPE = ' '
         TYPE = 'A list of data sets'
         CALL GRP_NEW( TYPE, IGRP, STATUS )
         SIZE0 = 0
         IN = .FALSE.

*  If a group identifier was supplied, store the current size.
      ELSE
         CALL GRP_GRPSZ( IGRP, SIZE0, STATUS )
         IN = .TRUE.

      END IF

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( NDG__UCASE ) CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  See if IGRP0 is a valid GRP identifier.
      CALL GRP_VALID( IGRP0, INGRP, STATUS )

*  If the identifier is valid, set the group to be case insensitive if
*  the host file system is case insensitive, and get the group size.
      IF( INGRP ) THEN
         IF( NDG__UCASE ) CALL GRP_SETCS( IGRP0, .FALSE., STATUS )
         CALL GRP_GRPSZ( IGRP0, MODSIZ, STATUS )

*  Create a new temporary group containing just the file name field
*  from the total file specifications given in the group identified by
*  IGRP0.
         CALL GRP_NEW( ' ', TMPGRP, STATUS )
         IF( NDG__UCASE ) CALL GRP_SETCS( TMPGRP, .FALSE., STATUS )

         DO I = 1, MODSIZ
            CALL GRP_GET( IGRP0, I, 1, FSPEC0, STATUS )

*  Extact the file basename from the total file specification, and store 
*  it in the temporary group.
            CALL NDG1_HSPEC( FSPEC0, FMTOUT, .FALSE., FDIRN0, FNAME0, 
     :                       FTYPE0, COMP0, SLICE0, FORM, STATUS )
            CALL GRP_PUT( TMPGRP, 1, FNAME0, 0, STATUS )

         END DO

      END IF

*  Call GRP_GRPEX to append NDF names specified using the supplied 
*  group expresson, to the group. 
      CALL GRP_GRPEX( GRPEXP, TMPGRP, IGRP, SIZE, ADDED, FLAG, STATUS )

*  Go through each new name in the group, filling in any fields in the
*  full file specification which were not provided.
      DO I = SIZE0 + 1, SIZE

*  Get the next new name added to the output group.
         CALL GRP_GET( IGRP, I, 1, NAME, STATUS )

*  Split the new name up into directory, file basename, file type, 
*  HDS component, and NDF slice strings.
         CALL NDG1_HSPEC( NAME, FMTOUT, .FALSE., FDIRN, FNAME, FTYPE, 
     :                    COMP, SLICE, FORM, STATUS )

*  Get the index of the name within the basis group from which the new
*  name was derived. If the new name was not specified by a
*  modification element the index will be returned equal to zero.
         CALL GRP_INFOI( IGRP, I, 'MODIND', MODIND, STATUS )

*  If the name was specified by a modification element...
         IF( MODIND .NE. 0 ) THEN

*  ...get the file specification used as the basis for the current name
*  and split it up into the same set of parts as before.
            CALL GRP_GET( IGRP0, MODIND, 1, FSPEC0, STATUS )
            CALL NDG1_HSPEC( FSPEC0, FMTOUT, .FALSE., FDIRN0, FNAME0, 
     :                       FTYPE0, COMP0, SLICE0, FORM, STATUS )

*  If no file type was given, use the input file type if the first entry
*  in NDF_FORMATS_OUT is "*"; leave the file type blank if the first entry
*  in NDF_FORMATS_OUT is "."; and otherwise use the file type specified in
*  parenthesise in the first entry in NDF_FORMATS_OUT.
            IF( FTYPE .EQ. ' ' ) THEN
               IF( DEFTYP .EQ. '*' ) THEN
                  FTYPE = FTYPE0
               ELSE IF( DEFTYP .NE. '.' ) THEN
                  FTYPE = DEFTYP
               END IF
            END IF

*  The directory path is inherited from the basis element if it was 
*  not supplied.
            IF( FDIRN .EQ. ' ' ) FDIRN = FDIRN0

         END IF

*  Construct a file specification from the required elements. The NDF
*  slice specification is set blank.
         CALL NDG1_MSPEC( FMTOUT, FDIRN, FNAME, FTYPE, COMP, ' ', 
     :                    .FALSE., FSPEC, FORM, STATUS )

*  Replace the supplied name with the expanded file specification.
         CALL GRP_PUT( IGRP, 1, FSPEC, I, STATUS )

*  Abort if an error has occured.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Delete the temporary group.
 999  CONTINUE
      IF( INGRP ) CALL GRP_DELET( TMPGRP, STATUS )

*  If an error occured, reset the group back to its original size if a 
*  group was supplied, or delete the group if no group was supplied.
*  Ensure FLAG is returned .FALSE.
      IF( STATUS .NE. SAI__OK ) THEN
         FLAG = .FALSE.
         CALL ERR_BEGIN( STATUS )

         IF( IN ) THEN
            CALL GRP_SETSZ( IGRP, SIZE0, STATUS )
         ELSE
            CALL GRP_DELET( IGRP, STATUS )
         END IF

         CALL ERR_END( STATUS )

*  Give a context message.
         CALL MSG_SETC( 'P', GRPEXP )
         CALL ERR_REP( 'NDG_CREXP_ERR2',
     :          'NDG_CREXP: Error obtaining a group of data sets '//
     :          'using group expression "^P"', STATUS )

      END IF

      END
