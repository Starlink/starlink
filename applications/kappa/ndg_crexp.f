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
*
*     If possible, explicit file types are included in all the elements
*     of the returned group. If no file type is suppllied by the user,
*     then a file type will be obtained from the NDF_FORMATS_OUT value,
*     or from the basis name (if a modification element was supplied).
*     This is done because the name may be passed out to a script (eg
*     POLPACK:POLKA) which may change the value of NDF_FORMATS_OUT before
*     using the NDF name.

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

*  Local Variables:
      CHARACTER DEFTYP*(GRP__SZNAM)! Default file type from NDF_FORMATS_OUT
      CHARACTER DIR*(GRP__SZNAM)   ! Directory path 
      CHARACTER FMTOUT*(NDG__SZFMT)! List of output NDF formats
      CHARACTER LOC*(DAT__SZLOC)   ! Locator to top-level HDS object
      CHARACTER NAME*(GRP__SZNAM)  ! Current name
      CHARACTER NAM*(GRP__SZNAM)   ! Basis name
      CHARACTER PATH*(GRP__SZNAM)  ! HDS component path 
      CHARACTER SPEC*(GRP__SZNAM)  ! Basis container file spec
      CHARACTER TYP*(GRP__SZNAM)   ! File type 
      INTEGER ADDED              ! No. of names added to the group
      INTEGER CL                 ! Index of 1st closing parenthesis
      INTEGER COMMA              ! Index of 1st comma in NDF_FORMATS_OUT
      INTEGER DOT                ! Index of first "."
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
      INTEGER OP                 ! Index of 1st opening parenthesis
      INTEGER PAR                ! Index of first "("
      INTEGER SIZE0              ! Size of group on entry.
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
      IF( FMTOUT .EQ. ' ' ) THEN
         DEFTYP = '.'

*  Otherwise, 
      ELSE

*  Get the file type from the first entry in NDF_FORMATS_OUT (stored within
*  opening and closing parenthesise before the first comma). If the first
*  entry is "*", then store "*" (meaning "use the input file type"), otherwise 
*  if the first entry is "." or does not contain a file type, store "."
*  (meaning "use native NDF format").
         COMMA = INDEX( FMTOUT, ',' )
         IF( COMMA .EQ. 1 ) THEN
            DEFTYP = '.'
         ELSE
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

*  Get the index of the name within the basis group from which the new
*  name was derived. If the new name was not specified by a
*  modification element the index will be returned equal to zero.
         CALL GRP_INFOI( IGRP, I, 'MODIND', MODIND, STATUS )

*  If the name was specified by a modification element, we need to fill in
*  any fields which have not been supplied, using the fields associated
*  with the basis group as defaults.
         IF( MODIND .NE. 0 ) THEN

*  Get the last "/" in the supplied string, marking the end of the
*  directory path.
            CALL NDG1_LASTO( NAME, '/', IAT, STATUS )

*  If there is no directory specification in the supplied string, prefix
*  it with the directory spec from the basis group.
            IF( IAT .EQ. 0 .AND. IGRPD .NE. GRP__NOID ) THEN
               CALL GRP_GET( IGRPD, MODIND, 1, DIR, STATUS )
               IF( DIR .NE. ' ' ) THEN
                  IAT = CHR_LEN( DIR )
                  IAT2 = IAT
                  CALL CHR_APPND( NAME, DIR, IAT2 )
                  NAME = DIR
               END IF
            END IF

*  Get the first "." or "(" following the directory path. This marks the 
*  start of a file type or an HDS path.
            DOT = INDEX( NAME( IAT + 1 : ), '.' )
            PAR = INDEX( NAME( IAT + 1 : ), '(' )

*  If any text was supplied after the base name, leave it as it is. If no
*  text was supplied after the base name, we may need to append an HDS
*  path or a file type.
            IF( DOT .EQ. 0 .AND. PAR .EQ. 0 ) THEN

*  No file type was specified, so we need to choose one now on the basis
*  of the NDF_FORMATS_OUT default file type, and the file type of the
*  basis element. If the default file type is "." use ".sdf".
               IF( DEFTYP .EQ. '.' ) THEN
                  TYP = '.sdf'

*  If the default file type is "*" use the file type from the basis
*  element.
               ELSE IF( DEFTYP .EQ. '*' .AND. 
     :                  IGRPT .NE. GRP__NOID ) THEN
                  CALL GRP_GET( IGRPT, MODIND, 1, TYP, STATUS )

*  If an explicit default file is available, use it.
               ELSE
                  TYP = DEFTYP
               END IF

*  If the file type is ".sdf"...
               IF( TYP .EQ. '.sdf' ) THEN

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

*  Get the last "/" in the supplied string, marking the end of the
*  directory path.
            CALL NDG1_LASTO( NAME, '/', IAT, STATUS )

*  Get the first "." or "(" following the directory path. This marks the 
*  start of a file type or an HDS path.
            DOT = INDEX( NAME( IAT + 1 : ), '.' )
            PAR = INDEX( NAME( IAT + 1 : ), '(' )

*  If any text followed the file base name, assume the file type has been
*  specified. Otherwise we append one now.
            IF( PAR .EQ. 0 .AND. DOT .EQ. 0 ) THEN

*  We can only append a file type if NDF_FORMATS_OUT gives us an explicit
*  default file type.
               IF( DEFTYP .NE. '.' .AND. DEFTYP .NE. '*' .AND.
     :             DEFTYP .NE. ' ' ) THEN               

                  IAT = CHR_LEN( NAME )
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
