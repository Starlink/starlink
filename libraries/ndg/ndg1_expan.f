      SUBROUTINE NDG1_EXPAN( TEMPLT, IGRP, NFMT, FMT, FOUND, STATUS )
*+
*  Name:
*     NDG1_EXPAN

*  Purpose:
*     Expands a wild card template.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_EXPAN( TEMPLT, IGRP, NFMT, FMT, FOUND, STATUS )

*  Description:
*     The supplied wild-card template is expanded into a list of file 
*     names, which are appended to the supplied group. 
*
*     The ambiguity between HDS component paths and file types is solved
*     here by returning only the HDS file in the returned group. For
*     instance, the template "datafile.fit" will match either a FITS file 
*     called "datafile.fit" or an HDS file called "datafile.sdf" containing 
*     a ".fit" component. If the current directory contains both of these
*     files, then only "datafile.sdf" will be added to the returned group. 
*
*     Each combination of directory and basename is included only once in
*     the returned group. If the supplied template matches files with the
*     same directory/basename but with different file types, then only
*     the file with the highest priority file type is returned (i.e. 
*     the .sdf file if available, or the file type which is closest to 
*     the start of NDF_FORMATS_IN otherwise).

*  Arguments:
*     TEMPLT = CHARACTER * ( * ) (Given)
*        The wild card template.
*     IGRP = INTEGER (Given)
*        An identifier for the group to which the expanded names should
*        be appended. On exit, this group is at the end of a chain of 
*        groups connected by a GRP "owner-slave" relationship. The other
*        groups is the chain hold individual fields  from the full NDF
*        specification. The full chain (starting from the head) is as
*        follows:
*
*                 1) NDF slice specifications
*                 2) HDS paths
*                 3) File types
*                 4) Base file names
*                 5) Directory paths
*                 6) Full NDF specification (this is the returned group - IGRP)
*
*        The other groups in the chain are deleted automatically by GRP
*        when the returned group (IGRP) is deleted.
*     NFMT = INTEGER (Given)
*        The number of foreign file types stored in FMT. May be zero.
*     FMT( NFMT ) = CHARACTER * ( * ) (Given)
*        An array of known foreign file types. Not accessed if NFMT is
*        zero.
*     FOUND = LOGICAL (Returned)
*        Returned .TRUE. if one or more files were found matching the
*        supplied template. Returned .FALSE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-DEC-1997 (DSB):
*        Original version.
*     15-FEB-1999 (DSB):
*        Modified to include paths to all NDFs contained within a given
*        HDS container file.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'NDG_CONST'        ! NDG private constants.

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
      LOGICAL NDG1_MATCH

*  Local Variables:
      CHARACTER BN*50              ! File basename
      CHARACTER DIR*(GRP__SZNAM)   ! Directory field
      CHARACTER FTEMP*(GRP__SZNAM) ! File template
      CHARACTER LOC*(DAT__SZLOC)   ! Locator for top-level object
      CHARACTER LOC2*(DAT__SZLOC)  ! Locator for component object
      CHARACTER NAM*(GRP__SZNAM)   ! File base name field
      CHARACTER PATH*(GRP__SZNAM)  ! HDS path field
      CHARACTER REST*(GRP__SZNAM)  ! The remaining text after the file spec
      CHARACTER SEC*50             ! File NDF/HDS section
      CHARACTER SEARCH*1024        ! The total search string
      CHARACTER SLICE*(GRP__SZNAM) ! The NDF slice spec
      CHARACTER SPEC*(GRP__SZNAM)  ! The file spec of the matching file
      CHARACTER STORED*(GRP__SZNAM)! Text to store in the supplied group
      CHARACTER SUF*50             ! File suffix
      CHARACTER TYP*(GRP__SZNAM)   ! File type field
      INTEGER DOT                ! Index of next "."
      INTEGER F                  ! Index of first non-blank character
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of a string
      INTEGER IFMT               ! Index of current file type
      INTEGER IGRP2              ! Group of potential file matches
      INTEGER IGRP3              ! Group holding remaining text
      INTEGER IGRPB              ! Group holding base name fields
      INTEGER IGRPD              ! Group holding directory fields
      INTEGER IGRPH              ! Group holding HDS path fields
      INTEGER IGRPS              ! Group holding NDF slice fields
      INTEGER IGRPT              ! Group holding file type fields
      INTEGER L                  ! Index of last non-blank character
      INTEGER NC                 ! No. of characters in string
      INTEGER NMATCH             ! No. of matching file types
      INTEGER PAR                ! Index of next "("
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

*  The file types group is owned by a group holding the HDS paths.
      CALL GRP_OWN( IGRPT, IGRPH, STATUS )
      IF( IGRPH .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'HDS path', IGRPH, STATUS )
         CALL GRP_SOWN( IGRPT, IGRPH, STATUS )
      END IF

*  The HDS paths group is owned by a group holding the NDF slices.
      CALL GRP_OWN( IGRPH, IGRPS, STATUS )
      IF( IGRPS .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'NDF slice', IGRPS, STATUS )
         CALL GRP_SOWN( IGRPH, IGRPS, STATUS )
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
         CALL NDG1_APPEN( IGRP2, IGRP3, TEMPLT, ' ', STATUS )

*  Indicate that duplicate file names should bnot be purged.
         PURGE = .FALSE.

*  Otherwise, split the template into directory, basename, suffix and
*  section.
      ELSE
        CALL NDG1_FPARS( TEMPLT, DIR, BN, SUF, SEC, STATUS )

*  First of all look for any ".sdf" files with the given directory path
*  and file basename. Ignore the file suffix since "fred.fit" could refer
*  to component ".fit" within file fred.sdf. Store matching file specs
*  in IGRP2, and "the rest" (i.e. file suffix - so long as it is not a
*  simple wild-card ".*" - and section) in IGRP3.
         REST = ' '
         IAT = 0
         IF( SUF .NE. '.*') CALL CHR_APPND( SUF, REST, IAT )
         CALL CHR_APPND( SEC, REST, IAT )
   
         FTEMP = ' '
         IAT = 0
         CALL CHR_APPND( DIR, FTEMP, IAT )
         CALL CHR_APPND( BN, FTEMP, IAT )
         CALL CHR_APPND( NDG__NDFTP, FTEMP, IAT )
   
         IF( IAT .GT. 0 ) THEN
            CALL NDG1_APPEN( IGRP2, IGRP3, FTEMP( : IAT ), REST, 
     :                       STATUS )
         END IF

*  From now on, if no suffix was given, use ".*" so that we pick up files 
*  with any of the types included in NDF_FORMATS_IN. But indicate that
*  duplicate files with different file types should be purged.
         IF( SUF .EQ. ' ' ) THEN
            SUF = '.*'
            PURGE = .TRUE.
         ELSE
            PURGE = .FALSE.
         END IF

*  Initialise the total file search string.
         SEARCH = ' '
         SLEN = 0

*  Now loop round each file type in NDF_FORMATS_IN (if any).
         DO IFMT = 1, NFMT

*  Does the suffix supplied in the template match this known foreign file
*  type?
            IF( NDG1_MATCH( SUF, FMT( IFMT ), STATUS ) ) THEN

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
               ENDIF

            END IF

         END DO

*  Get a list of all matching files, appending them to IGRP2.
         IF( SLEN .GT. 0 ) THEN
            CALL NDG1_APPEN( IGRP2, IGRP3, SEARCH( : SLEN ), SEC, 
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

*  Split the file spec into directory, basename, suffix and section.
         CALL NDG1_FPARS( SPEC, DIR, NAM, TYP, SEC, STATUS )

*  Attempt to open the file as an HDS container file.
         CALL HDS_OPEN( SPEC, 'READ', LOC, STATUS )

*  If succesfull,
         IF( STATUS .EQ. SAI__OK ) THEN

*  Assume for the moment that the entire remaining text is an HDS path,
*  and there is no NDF slice.
            PATH = REST
            SLICE = ' '

*  Attempt to obtain a locator for the object using the remaining text 
*  from the supplied template as an HDS path.         
            CALL NDG1_HFIND( LOC, REST, 'READ', LOC2, STATUS )

*  If this failed, it may be because the REST text ended with an NDF slice
*  specification. If the last non-blank chartacter is a ")", chop of the
*  trailing text in parentheses (the slice spec), and try again.
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )

               IF( REST .NE. ' ' ) THEN
                  NC = CHR_LEN( REST )
                  IF( REST( NC : NC ) .EQ. ')' ) THEN
                     CALL NDG1_LASTO( REST( : NC ), '(', IAT, STATUS )
                     IF( STATUS .EQ. SAI__OK .AND. IAT .GT. 0 ) THEN

                        SLICE = REST( IAT : )

                        IF( IAT .EQ. 1 ) THEN
                           PATH = ' '
                        ELSE 
                           PATH = REST( : IAT - 1 )
                        END IF

                        CALL NDG1_HFIND( LOC, PATH, 'READ', LOC2, 
     :                                   STATUS )

                        IF( STATUS .NE. SAI__OK ) THEN
                           CALL ERR_ANNUL( STATUS )
                        END IF

                     END IF
                  END IF
               END IF
            END IF

*  If we found an object.
            IF( LOC2 .NE. DAT__NOLOC ) THEN

*  See if it contains any NDFs. If so, the details to the NDFs are stored
*  in the returned groups.
               CALL NDG1_SDFEX( IGRP, IGRPD, IGRPB, IGRPT, IGRPH, IGRPS, 
     :                          LOC2, DIR, NAM, TYP, SLICE, FOUND, 
     :                          STATUS )

*  Annul the component locator.
               CALL DAT_ANNUL( LOC2, STATUS )
            END IF

*  Annul the top-level locator.
            CALL DAT_ANNUL( LOC, STATUS )

*  If the file could not be opened as an HDS container file, annul the
*  error.
         ELSE     
            CALL ERR_ANNUL( STATUS )

*  Initialise the stored string to be the full file spec.
            STORED = SPEC
            IAT = CHR_LEN( SPEC )

*  The file spec cannot be used if it is a .sdf file.
            IF( TYP .NE. NDG__NDFTP ) THEN

*  Non-HDS files cannot have a component path. Any remaining text must
*  be an NDF slice if the file is usable.
               PATH = ' '
               SLICE = REST

*  The file spec can be used if the REST text is blank.
               IF( REST .EQ. ' ' ) THEN
                  CALL GRP_PUT( IGRP, 1, STORED( : IAT ), 0, STATUS )
                  FOUND = .TRUE.

*  Store individual fields in the supplemental groups.
                  CALL GRP_PUT( IGRPD, 1, DIR, 0, STATUS )
                  CALL GRP_PUT( IGRPB, 1, NAM, 0, STATUS )
                  CALL GRP_PUT( IGRPT, 1, TYP, 0, STATUS )
                  CALL GRP_PUT( IGRPH, 1, PATH, 0, STATUS )
                  CALL GRP_PUT( IGRPS, 1, SLICE, 0, STATUS )

*  If the REST text is not blank, the file can only be used if REST looks
*  like it may be an NDF slice specification (i.e. if it starts with "(",
*  ends with ")" and has no other parentheses in it).
               ELSE
                  CALL CHR_FANDL( REST, F, L )
                  IF( REST( F : F ) .EQ. '(' .AND. 
     :                REST( L : L ) .EQ. ')' ) THEN
                     IF( F + 1 .LE. L - 1 ) THEN
                        IF( INDEX( REST( F + 1 : L - 1 ), ')' ) .EQ. 0
     :                      .AND.
     :                      INDEX( REST( F + 1 : L - 1 ), '(' ) .EQ. 0
     :                    ) THEN

*  REST looks like an NDF slice spec, so append it to the end of the 
*  file spec and store it in the returned group.
                           CALL CHR_APPND( REST, STORED, IAT )
                           CALL GRP_PUT( IGRP, 1, STORED( : IAT ), 0, 
     :                                   STATUS )
                           FOUND = .TRUE.

*  Store individual fields in the supplemental groups.
                           CALL GRP_PUT( IGRPD, 1, DIR, 0, STATUS )
                           CALL GRP_PUT( IGRPB, 1, NAM, 0, STATUS )
                           CALL GRP_PUT( IGRPT, 1, TYP, 0, STATUS )
                           CALL GRP_PUT( IGRPH, 1, PATH, 0, STATUS )
                           CALL GRP_PUT( IGRPS, 1, SLICE, 0, STATUS )

                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  Annul any error which has occurred so that any remaining names can be
*  checked.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

      END DO

*  End the error context started before the loop.
      CALL ERR_END( STATUS )         

*  Purge the returned groups of matching files (i.e. file with the same
*  directory and basename but differing file types).
      IF( PURGE ) CALL NDG1_SORT( IGRP, IGRPD, IGRPB, IGRPT, IGRPH, 
     :                            IGRPS, SIZE0 + 1, NFMT, FMT, STATUS )

*  Delete the temporary groups.
 999  CONTINUE

      CALL GRP_DELET( IGRP2, STATUS )
      CALL GRP_DELET( IGRP3, STATUS )

      END
