      SUBROUTINE CTG1_EXPAN( TEMPLT, IGRP, NFMT, FMT, FOUND, STATUS )
*+
*  Name:
*     CTG1_EXPAN

*  Purpose:
*     Expands a wild card template.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG1_EXPAN( TEMPLT, IGRP, NFMT, FMT, FOUND, STATUS )

*  Description:
*     The supplied wild-card template is expanded into a list of file 
*     names, which are appended to the supplied group. If the supplied 
*     template matches files with the same directory/basename but with 
*     different file types, then only the file with the highest priority 
*     file type is returned (i.e. the file type which is closest to 
*     the start of CAT_FORMATS_IN).

*  Arguments:
*     TEMPLT = CHARACTER * ( * ) (Given)
*        The wild card template.
*     IGRP = INTEGER (Given)
*        An identifier for the group to which the expanded names should
*        be appended. On exit, this group is at the end of a chain of 
*        groups connected by a GRP "owner-slave" relationship. The other
*        groups is the chain hold individual fields  from the full catalogue
*        specification. The full chain (starting from the head) is as
*        follows:
*
*                 1) FITS extension specification
*                 2) File types
*                 3) Base file names
*                 4) Directory paths
*                 5) Full catalogue specification (this is the returned group - IGRP)
*
*        The other groups in the chain are deleted automatically by GRP
*        when the returned group (IGRP) is deleted.
*     NFMT = INTEGER (Given)
*        The number of catalogue file types stored in FMT. May be zero.
*     FMT( NFMT ) = CHARACTER * ( * ) (Given)
*        An array of known catalogue file types. 
*     FOUND = LOGICAL (Returned)
*        Returned .TRUE. if one or more files were found matching the
*        supplied template. Returned .FALSE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'CTG_CONST'        ! CTG private constants.

*  Arguments Given:
      CHARACTER TEMPLT*(*)
      INTEGER IGRP
      INTEGER NFMT
      CHARACTER FMT( * )*(*)
      
*  Arguments Returned:
      LOGICAL FOUND

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER CHR_LEN
      LOGICAL CTG1_MATCH

*  Local Variables:
      CHARACTER BN*50              ! File basename
      CHARACTER DIR*(GRP__SZNAM)   ! Directory field
      CHARACTER EXT*10             ! FITS extension specifier
      CHARACTER FTEMP*(GRP__SZNAM) ! File template
      CHARACTER NAM*(GRP__SZNAM)   ! File base name field
      CHARACTER REST*64            ! The FITS extension specifier
      CHARACTER SEARCH*1024        ! The total search string
      CHARACTER SPEC*(GRP__SZNAM)  ! The file spec of the matching file
      CHARACTER SUF*20             ! File suffix
      CHARACTER TYP*(GRP__SZNAM)   ! File type field
      INTEGER CI                 ! CAT identifier
      INTEGER F                  ! Index of first non-blank character
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of a string
      INTEGER IFMT               ! Index of current file type
      INTEGER IGRP2              ! Group of potential file matches
      INTEGER IGRP3              ! Group holding remaining text
      INTEGER IGRPB              ! Group holding base name fields
      INTEGER IGRPD              ! Group holding directory fields
      INTEGER IGRPH              ! Group holding FITS extension fields
      INTEGER IGRPT              ! Group holding file type fields
      INTEGER L                  ! Index of last non-blank character
      INTEGER NMATCH             ! No. of matching file types
      INTEGER SIZE0              ! Size of original group
      INTEGER SLEN               ! Length of total search string
      LOGICAL PURGE              ! Purge duplicate file names?
*.

*  Initialise the returned flag to indicate that no matching files have
*  yet been found.
      FOUND = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the original size of the supplied group.
      CALL GRP_GRPSZ( IGRP, SIZE0, STATUS )

*  Ensure that supplemental groups are associated with the returned group
*  holding extra information. These groups are deleted automatically when
*  the returned group is deleted.
*  ----------------------------------------------------------------------

*  The returned group is owned by a group holding the directory
*  specifications for each file. Check to see if the group exists. If
*  not, create it and establish it as the owner of the returned group.
      CALL GRP_OWN( IGRP, IGRPD, STATUS )
      IF( IGRPD .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'Directory', IGRPD, STATUS )
         CALL GRP_SOWN( IGRP, IGRPD, STATUS )
      END IF

*  The directories group is owned by a group holding the file base names.
      CALL GRP_OWN( IGRPD, IGRPB, STATUS )
      IF( IGRPB .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'Base name', IGRPB, STATUS )
         CALL GRP_SOWN( IGRPD, IGRPB, STATUS )
      END IF

*  The base names group is owned by a group holding the file types.
      CALL GRP_OWN( IGRPB, IGRPT, STATUS )
      IF( IGRPT .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'File type', IGRPT, STATUS )
         CALL GRP_SOWN( IGRPB, IGRPT, STATUS )
      END IF

*  The file types group is owned by a group holding the FITS extension 
*  specifications.
      CALL GRP_OWN( IGRPT, IGRPH, STATUS )
      IF( IGRPH .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'FITS extensions', IGRPH, STATUS )
         CALL GRP_SOWN( IGRPT, IGRPH, STATUS )
      END IF

*  Create a temporary group to hold potential file matches.
      CALL GRP_NEW( 'FILE SPECS ', IGRP2, STATUS )

*  Create another temporary group to hold the remainder of the supplied
*  template for each entry in IGRP2.
      CALL GRP_NEW( 'REST ', IGRP3, STATUS )

*  Find the first and last non-blank characters in the template.
      CALL CHR_FANDL( TEMPLT, F, L )

*  If the first and last characters are backwards quotes, just search for
*  files matching the template as supplied. This allows for shell command 
*  substitution to be included in the supplied string.
      IF( TEMPLT( F : F ) .EQ. '`' .AND.
     :    TEMPLT( L : L ) .EQ. '`' ) THEN
         CALL CTG1_APPEN( IGRP2, IGRP3, TEMPLT, ' ', STATUS )

*  Otherwise, split the template into directory, basename, suffix and
*  FITS extension. 
      ELSE
        CALL CTG1_FPARS( TEMPLT, DIR, BN, SUF, EXT, STATUS )

*  If no suffix was given, use ".*" so that we pick up files with any of 
*  the types included in the list of known catalogue formats.
         IF( SUF .EQ. ' ' ) SUF = '.*'

*  Initialise the total file search string.
         SEARCH = ' '
         SLEN = 0

*  Now loop round each known file type.
         DO IFMT = 1, NFMT

*  Does the suffix supplied in the template match this known catalogue file
*  type?
            IF( CTG1_MATCH( SUF, FMT( IFMT ), STATUS ) ) THEN

*  If it does, append a wildcard template to the total file search string
*  which will match files with the given base name and the current
*  catalogue file type. Append a trailing space by incrementing SLEN.
               FTEMP = ' '
               IAT = 0
               CALL CHR_APPND( DIR, FTEMP, IAT )
               CALL CHR_APPND( BN, FTEMP, IAT )
               CALL CHR_APPND( FMT( IFMT ), FTEMP, IAT )
         
               IF( IAT .GT. 0 ) THEN
                  CALL CHR_APPND( FTEMP( : IAT ), SEARCH, SLEN )
                  SLEN = SLEN + 1
               END IF
   
            END IF

         END DO

*  Get a list of all matching files.
         IF( SLEN .GT. 0 ) THEN
            CALL CTG1_APPEN( IGRP2, IGRP3, SEARCH( : SLEN ), EXT, 
     :                       STATUS )
         END IF

      END IF

*  Get the number of potentially matching files.
      CALL GRP_GRPSZ( IGRP2, NMATCH, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Begin a new error context.
      CALL ERR_BEGIN( STATUS )

*  Check each one.
      DO I = 1, NMATCH

*  Get the full matching file spec, and the remaining text for this
*  potential match.
         CALL GRP_GET( IGRP2, I, 1, SPEC, STATUS )
         CALL GRP_GET( IGRP3, I, 1, REST, STATUS )

*  Append the FITS extension string (if any) to the file specification.
         IAT = CHR_LEN( SPEC )
         CALL CHR_APPND( REST, SPEC, IAT )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to open the file as an existing catalogue.
         CALL CAT_TOPEN( SPEC, 'OLD', 'READ', CI, STATUS )

*  If succesfull,
         IF( STATUS .EQ. SAI__OK ) THEN

*  Store the full catalogue spec, and indicate we have found a matching
*  file.
            CALL GRP_PUT( IGRP, 1, SPEC, 0, STATUS )
            FOUND = .TRUE.

*  Split this catalogue spec into directory, basename, suffix, and extension.
            CALL CTG1_FPARS( SPEC, DIR, NAM, TYP, EXT, STATUS )

*  Store individual fields in the supplemental groups.
            CALL GRP_PUT( IGRPD, 1, DIR, 0, STATUS )
            CALL GRP_PUT( IGRPB, 1, NAM, 0, STATUS )
            CALL GRP_PUT( IGRPT, 1, TYP, 0, STATUS )
            CALL GRP_PUT( IGRPH, 1, EXT, 0, STATUS )

         END IF

*  Release the catalogue identifier.
         CALL ERR_BEGIN( STATUS )
         CALL CAT_TRLSE( CI, STATUS ) 
         CALL ERR_END( STATUS )

*  Annul any error which has occurred so that any remaining names can be
*  checked.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

      END DO

*  End the error context started before the loop.
      CALL ERR_END( STATUS )         

*  Purge the returned groups of matching files (i.e. file with the same
*  directory and basename but differing file types).
      IF( PURGE ) CALL CTG1_SORT( IGRP, IGRPD, IGRPB, IGRPT, IGRPH, 
     :                            SIZE0 + 1, NFMT, FMT, STATUS )

*  Delete the temporary groups.
 999  CONTINUE

      CALL GRP_DELET( IGRP2, STATUS )
      CALL GRP_DELET( IGRP3, STATUS )

      END
