      SUBROUTINE NDG1_CREXP( GRPEXP, IGRP0, IGRP, SIZE, FLAG, STATUS )
*+
*  Name:
*     NDG1_CREXP

*  Purpose:
*     Store the names of a specified group of NDF to be created.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_CREXP( GRPEXP, IGRP0, IGRP, SIZE, FLAG, STATUS )

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
*     22-FEB-1999 (DSB):
*        If no HDS component path is supplied, inherit it from the
*        basis element.
*     25-MAY-1999 (DSB):
*        Modified to allow the basis group to be created directly by GRP,
*        as well as by NDF (i.e. cater for possibility that no slave groups 
*        exist).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants.
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

*  Local Constants:
      INTEGER MXTYP              ! Max. number of foreign data formats
      PARAMETER ( MXTYP = 50 )
      INTEGER SZTYP              ! Max. length of a foreign file type
      PARAMETER ( SZTYP = 15 )

*  Local Variables:
      CHARACTER ALTTYP*20          ! Second choice file type from NDF_FORMATS_OUT
      CHARACTER BN1*50             ! Supplied file base name
      CHARACTER DEFTYP*20          ! First choice file type from NDF_FORMATS_OUT
      CHARACTER DIR*(GRP__SZNAM)   ! Directory path 
      CHARACTER DIR1*(GRP__SZFNM)  ! Supplied directory path 
      CHARACTER FMTOUT*(NDG__SZFMT)! List of output NDF formats
      CHARACTER LOC*(DAT__SZLOC)   ! Locator to top-level HDS object
      CHARACTER NAM*(GRP__SZNAM)   ! Basis name
      CHARACTER NAME*(GRP__SZNAM)  ! Current name
      CHARACTER PATH*(GRP__SZNAM)  ! HDS component path 
      CHARACTER SEC1*50            ! Supplied NDF section (ignored)
      CHARACTER SPEC*(GRP__SZNAM)  ! Basis container file spec
      CHARACTER SUF1*100           ! Supplied file suffix
      CHARACTER TYP*(GRP__SZNAM)   ! File type 
      CHARACTER TYPS( MXTYP )*(SZTYP)! Known foreign file types
      INTEGER ADDED              ! No. of names added to the group
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Index of last non-blank character
      INTEGER IAT2               ! Index of last non-blank character
      INTEGER IGRPB              ! Group of file base names
      INTEGER IGRPD              ! Group of directories
      INTEGER IGRPF              ! Group of initialised HDS files
      INTEGER IGRPH              ! Group of HDS paths
      INTEGER IGRPT              ! Group of file types
      INTEGER IND                ! Index of matching group element
      INTEGER MODIND             ! Index of basis spec
      INTEGER NTYP               ! No. of known foreign data formats
      INTEGER SIZE0              ! Size of group on entry.
      LOGICAL IN                 ! Does IGRP identify a valid group?
      LOGICAL INGRP              ! Does IGRP0 identify a valid group?
*.

*  Ensure that FLAG is returned .FALSE if an error has already occured.
      FLAG = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new error context.
      CALL ERR_MARK

*  Get the current value of environment variable NDF_FORMATS_OUT.
*  Annul the error and indicate native NDFs should be created if it is 
*  not defined.
      CALL PSX_GETENV( 'NDF_FORMATS_OUT', FMTOUT, STATUS )
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         DEFTYP = '.'
         ALTTYP = '.'

*  If NDF_FORMATS_OUT is defined, extract the file types into an array.
      ELSE
         CALL CHR_RMBLK( FMTOUT )
         CALL NDG1_GTYPS( MXTYP, FMTOUT, NTYP, TYPS, STATUS )

*  Get the first choice file type. This will be used as the file type if
*  the user does not supply a file type.
         IF( NTYP .GE. 1 ) THEN
            DEFTYP = TYPS( 1 )
         ELSE
            DEFTYP = '.'
         END IF

*  If the first choice file type is "*" (meaning "use the format of the
*  corresponding input file"), we will be stuck if the name is specified
*  explicitly instead of by a modification element (because we won't have a
*  corresponding input file). In this case, we use the second choice file
*  type.
         IF( NTYP .GE. 2 ) THEN
            ALTTYP = TYPS( 2 )
         ELSE
            ALTTYP = '.'
         END IF

      END IF

*  If the supplied value of IGRP is GRP__NOID, create a new group to
*  hold the names of the NDFs.
      IF( IGRP .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'A list of data sets', IGRP, STATUS )
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

*  If the identifier is valid, get identifiers for the associated groups 
*  holding the individual fields.
      IF( INGRP ) THEN
         CALL GRP_OWN( IGRP0, IGRPD, STATUS )
         IF( IGRPD .NE. GRP__NOID ) THEN 
            CALL GRP_OWN( IGRPD, IGRPB, STATUS )
            CALL GRP_OWN( IGRPB, IGRPT, STATUS )
            CALL GRP_OWN( IGRPT, IGRPH, STATUS )
         ELSE
            IGRPB = IGRP0
            IGRPT = GRP__NOID
            IGRPH = GRP__NOID
         END IF
      END IF

*  Call GRP_GRPEX to append NDF names specified using the supplied 
*  group expresson, to the group. Any modification elements are based on
*  the group holding the file base names.
      CALL GRP_GRPEX( GRPEXP, IGRPB, IGRP, SIZE, ADDED, FLAG, STATUS )

*  Create a group to hold the names of HDS container files which have
*  been initialised to hold NDFs within them.
      CALL GRP_NEW( 'Initialised HDS files', IGRPF, STATUS )

*  Go through each new name in the group.
      DO I = SIZE0 + 1, SIZE

*  Get the next new name added to the output group.
         CALL GRP_GET( IGRP, I, 1, NAME, STATUS )

*  Split this up into directory, basename, suffix and NDF section (not
*  used).
         CALL NDG1_FPARS( NAME, DIR1, BN1, SUF1, SEC1, STATUS )

*  Get the index of the name within the basis group from which the new
*  name was derived. If the new name was not specified by a
*  modification element the index will be returned equal to zero.
         CALL GRP_INFOI( IGRP, I, 'MODIND', MODIND, STATUS )

*  If the name was specified by a modification element, we need to fill in
*  any fields which have not been supplied, using the fields associated
*  with the basis group as defaults.
         IF( MODIND .NE. 0 ) THEN

*  If there is no directory specification in the supplied string, prefix
*  it with the directory spec from the basis group.
            IF( DIR1 .EQ. ' ' .AND. IGRPD .NE. GRP__NOID ) THEN
               CALL GRP_GET( IGRPD, MODIND, 1, DIR, STATUS )
               IF( DIR .NE. ' ' ) THEN
                  IAT = CHR_LEN( DIR )
                  IAT2 = IAT
                  CALL CHR_APPND( NAME, DIR, IAT2 )
                  NAME = DIR
               END IF
            END IF

*  If any text was supplied after the base name, leave it as it is. If no
*  text was supplied after the base name, we may need to append an HDS
*  path or a file type.
            IF( SUF1 .EQ. ' ' .AND. SEC1 .EQ. ' ' ) THEN

*  No file type was specified, so we need to choose one now on the basis
*  of the NDF_FORMATS_OUT default file type, and the file type of the
*  basis element. If the default file type is "." use ".sdf".
               IF( DEFTYP .EQ. '.' ) THEN
                  TYP = NDG__NDFTP

*  If the default file type is "*" use the file type from the basis
*  element.
               ELSE IF( DEFTYP .EQ. '*' ) THEN
                  IF( IGRPT .NE. GRP__NOID ) THEN
                     CALL GRP_GET( IGRPT, MODIND, 1, TYP, STATUS )
                  ELSE
                     TYP = ALTTYP
                     IF( TYP .EQ. '.' ) TYP = NDG__NDFTP
                  END IF

*  If an explicit default file is available, use it.
               ELSE
                  TYP = DEFTYP
               END IF

*  If the file type is ".sdf"...
               IF( TYP .EQ. NDG__NDFTP ) THEN

*  See if the HDS structure within the output file has already been
*  initialised. If it has, the output container file spec (without the
*  .sdf file type) will be somewhere in the IGRPF group. IND will be
*  returned as zero if the structure has not been initialised.
                  CALL GRP_INDEX( NAME, IGRPF, 1, IND, STATUS )

*  See if any HDS path was included in the basis NDF spec.
                  IF( IGRPH .NE. GRP__NOID ) THEN
                     CALL GRP_GET( IGRPH, MODIND, 1, PATH, STATUS )
                  ELSE
                     PATH = ' '
                  END IF

                  IF( PATH .NE. ' ' ) THEN

*  We get here if the output is a native NDF for which no HDS component
*  path was explicitly given, but which has inherited a non-blank component 
*  path from the basis NDF. The NDF library will only create an NDF within
*  an HDS container file if all the parent components already exist in
*  the container file. In the particular case of an output NDF specified
*  by a modification element, we create the required parent structures
*  now by copying the basis container file to the output container file.
*  If the structure of the output file has already been initialised, 
*  do not initialise it again.
                     IF( IND .EQ. 0 ) THEN

*  Get the directory, base name and file type for the basis NDF.
                        CALL GRP_GET( IGRPB, MODIND, 1, NAM, STATUS )
                        IF( IGRPD .NE. GRP__NOID ) THEN
                           CALL GRP_GET( IGRPD, MODIND, 1, DIR, STATUS )
                           CALL GRP_GET( IGRPT, MODIND, 1, TYP, STATUS )
                        ELSE
                           DIR = ' '
                           TYP = ' '   
                        END IF

*  If the basis file is a .sdf file, form the full spec for the container 
*  file.
                        IF( TYP .EQ. '.sdf' ) THEN
                           SPEC = ' '
                           IAT2 = 0
                           CALL CHR_APPND( DIR, SPEC, IAT2 )
                           CALL CHR_APPND( NAM, SPEC, IAT2 )
                           CALL CHR_APPND( TYP, SPEC, IAT2 )

*  Open it.
                           CALL HDS_OPEN( SPEC( : IAT2 ), 'READ', LOC, 
     :                                    STATUS ) 

*  Create the output container file, and copy the contents of basis container 
*  file to it. The file base name is used as the HDS top-level object name.
                           CALL HDS_COPY( LOC, NAME, NAME( IAT + 1 : ), 
     :                                    STATUS )

*  Close the input container file.
                           CALL DAT_ANNUL( LOC, STATUS )

*  Open the output container file.
                           CALL HDS_OPEN( NAME, 'UPDATE', LOC, STATUS ) 

*  Now go through the output container file, deleting all existing NDFs.
*  This needs to be done sine the NDF library will not create an NDF if
*  there is an existing NDF with the same HDS path.
                           CALL NDG1_NDFDL( LOC, STATUS )

*  Close the output container file.
                           CALL DAT_ANNUL( LOC, STATUS )

*  Put the spec of this output file into the group of files which have
*  had their structure initialised.
                           CALL GRP_PUT( IGRPF, 1, NAME, 0, STATUS )

                        END IF

                     END IF

*  Append the HDS path from the basis element to the output NDF spec.
                     IAT = CHR_LEN( NAME )
                     CALL CHR_APPND( PATH, NAME, IAT )

                  END IF

*  For any non-HDS file, just append the file type.
               ELSE
                  IAT = CHR_LEN( NAME )
                  CALL CHR_APPND( TYP, NAME, IAT )
               END IF
            END IF

*  If the name was not specified by a modification element, we use the
*  string as given, except that we append a file type (if possible) if
*  the supplied string did not include a file type.
         ELSE

*  If any text followed the file base name, assume the file type has been
*  specified. Otherwise we append one now.
            IF( SUF1 .EQ. ' ' .AND. SEC1 .EQ. ' ' ) THEN
               IAT = CHR_LEN( NAME )

*  Choose the file type, and append to the supplied name.
               IF( DEFTYP .EQ. '*' ) THEN
                  CALL CHR_APPND( ALTTYP, NAME, IAT )
               ELSE IF( DEFTYP .NE. '.' ) THEN
                  CALL CHR_APPND( DEFTYP, NAME, IAT )
               END IF               

            END IF

         END IF

*  Replace the supplied string with the expanded string.
         CALL GRP_PUT( IGRP, 1, NAME, I, STATUS )

      END DO

*  Delete the group holding initialised HDS files.
      CALL GRP_DELET( IGRPF, STATUS )

*  If an error occured, reset the group back to its original size if a 
*  group was supplied, or delete the group if no group was supplied.
*  Ensure FLAG is returned .FALSE.
      IF( STATUS .NE. SAI__OK ) THEN
         FLAG = .FALSE.
         CALL ERR_BEGIN( STATUS )

         IF( IN ) THEN
            CALL NDG_SETSZ( IGRP, SIZE0, STATUS )
         ELSE
            CALL GRP_DELET( IGRP, STATUS )
         END IF

         CALL ERR_END( STATUS )

*  Give a context message.
         CALL MSG_SETC( 'P', GRPEXP )
         CALL ERR_REP( 'NDG_CREXP_ERR1','Error obtaining a group of '//
     :                 'NDFs using group expression ''^P''.', STATUS )

      END IF

*  Release the current error context.
      CALL ERR_RLSE

      END
