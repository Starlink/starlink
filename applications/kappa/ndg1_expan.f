      SUBROUTINE NDG1_EXPAN( TEMPLT, IGRP, FMTS, FOUND, STATUS )
*+
*  Name:
*     NDG1_EXPAN

*  Purpose:
*     Expands a wild card template.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_EXPAN( TEMPLT, IGRP, FMTS, FOUND, STATUS )

*  Description:
*     The supplied wild-card template is expanded into a list of file 
*     names, which are appended to the supplied group. Only files with 
*     types included in the environment variable NDF_FORMATS_IN are 
*     included, and priority is given to files with types earlier in 
*     the list.
*
*     The ambiguity between HDS component paths and file types is solved
*     here by including all matching files in the returned group. For
*     instance, the template "datafile.fit" will match either a FITS file 
*     called "datafile.fit" or an HDS file called "datafile.sdf" containing 
*     a ".fit" component. If the current directory contains both of these
*     files, then they will both be added to the returned group. This
*     effect is achieved by searching for matching files several times,
*     once for each dot-delimited field in the template. In the above
*     example, the first search is for "datafile". This will return the
*     hds file "native.sdf". A check is then made to see if this file
*     contains a ".fit" component. If it does it is added to the returned 
*     group. A second file search is then performed looking for files
*     matching "datafile.fit". This would return the FITS file which
*     would also be added to the returned group. 
*
*     A second example could be a template of "datafile.sdf.gz". Three file
*     searches are made; "datafile", "datafile.sdf" and "datafile.sdf.gz".
*     Files found in the first two searches are only accepted if they are
*     HDS files containing suitable components (i.e. ".sdf.gz" and ".gz"
*     respectively).

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
*     FMTS = CHARACTER * ( * ) (Given)
*        The value of the environment variable NDF_FORMATS_IN.
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

*  Bugs:
*     - Will not work on VMS since assumptions are made about Unix file 
*     names.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'DAT_PAR'          ! HDS constants.

*  Arguments Given:
      CHARACTER TEMPLT*(*)
      INTEGER IGRP
      CHARACTER FMTS*(*)
      
*  Arguments Returned:
      LOGICAL FOUND

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER DIR*(GRP__SZNAM)   ! Directory field
      CHARACTER LOC*(DAT__SZLOC)   ! Locator for top-level object
      CHARACTER LOC2*(DAT__SZLOC)  ! Locator for component object
      CHARACTER NAM*(GRP__SZNAM)   ! File base name field
      CHARACTER PATH*(GRP__SZNAM)  ! HDS path field
      CHARACTER REST*(GRP__SZNAM)  ! The remaining text after the file spec
      CHARACTER SLICE*(GRP__SZNAM) ! The NDF slice spec
      CHARACTER SPEC*(GRP__SZNAM)  ! The file spec of the matching file
      CHARACTER STORED*(GRP__SZNAM)! Text to store in the supplied group
      CHARACTER TYP*(GRP__SZNAM)   ! File type field
      INTEGER BNEND              ! Index of end marker
      INTEGER DOT                ! Index of next "."
      INTEGER F                  ! Index of first non-blank character
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of a string
      INTEGER IGRP2              ! Group of potential file matches
      INTEGER IGRP3              ! Group holding remaining text
      INTEGER IGRPB              ! Group holding base name fields
      INTEGER IGRPD              ! Group holding directory fields
      INTEGER IGRPH              ! Group holding HDS path fields
      INTEGER IGRPS              ! Group holding NDF slice fields
      INTEGER IGRPT              ! Group holding file type fields
      INTEGER L                  ! Index of last non-blank character
      INTEGER LTEM               ! Length of supplied template
      INTEGER NC                 ! No. of characters in string
      INTEGER NMATCH             ! No. of matching file types
      INTEGER PAR                ! Index of next "("
*.

*  Initialise the returned flag to indicate that no matching files have
*  yet been found.
      FOUND = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

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

*  Now expand the supplied template, storing the individual fields
*  in these supplemental groups, and the full NDF spec in the returned
*  group.
*  -------------------------------------------------------------------

*  Save the used length of the template.
      LTEM = CHR_LEN( TEMPLT ) 

*  Create a temporary group to hold potential file matches.
      CALL GRP_NEW( 'FILE SPECS ', IGRP2, STATUS )

*  Create another temporary group to hold the remainder of the supplied
*  template for each entry in IGRP2.
      CALL GRP_NEW( 'REST ', IGRP3, STATUS )

*  Find the end of the directory path.
      CALL NDG1_LASTO( TEMPLT, '/', IAT, STATUS )
      IAT = IAT + 1

*  Loop round searching for files which match succesively longer sections
*  of the supplied template. Each pass through the loop includes another
*  dot-delimited field from the supplied template.
      DO WHILE( IAT .GT. 0 .AND. STATUS .EQ. SAI__OK ) 

*  Find the next ".".
         DOT = INDEX( TEMPLT( IAT : ), '.' )

*  Find the next "(" (a potential NDF slice or HDS cell specification).
         PAR = INDEX( TEMPLT( IAT : ), '(' )

*  The end of the current file name is marked by the earlier of the two.
         IF( DOT .EQ. 0 ) THEN
            BNEND = PAR
         ELSE IF( PAR .EQ. 0 ) THEN
            BNEND = DOT
         ELSE
            BNEND = MIN( PAR, DOT )
         END IF

*  If no end marker was found use the whole string. 
         IF( BNEND .EQ. 0 ) THEN
            BNEND = LTEM + 1

*  Otherwise, correct for the start of the string.
         ELSE
            BNEND = BNEND + IAT - 1
         END IF

*  Save the rest of the supplied template string (including the end
*  marker) in REST.
         IF( BNEND .LE. LTEM ) THEN
            REST = TEMPLT( BNEND : )
         ELSE
            REST = ' '
         END IF

*  Search for files matching the current section of the file template and add 
*  them to the end of the group of potential file matches.  Add the contents 
*  of REST to the other group so that each matching file name has a
*  corresponding REST value.
         IF( BNEND .GT. 1 ) THEN
            CALL NDG1_APPEN( IGRP2, IGRP3, TEMPLT( : BNEND - 1 ), REST,
     :                       STATUS )
         END IF

*  If we have passed the end of string, leave the loop.
         IF( BNEND .GT. LTEM ) THEN
            IAT = 0

*  Otherwise, if the end marker was a dot, set IAT so that the search for the 
*  next end marker will begin at the next character.
         ELSE
            IF( TEMPLT( BNEND : BNEND ) .EQ. '.' ) THEN
               IAT = BNEND + 1            

*  If the end marker was a "(", find the next ")" and start searching
*  after the following ".".
            ELSE
               PAR = INDEX( TEMPLT( BNEND : ), ')' )
               IF( PAR .EQ. 0 ) THEN
                  IAT = 0
               ELSE
                  PAR = PAR + BNEND - 1
                  DOT = INDEX( TEMPLT( PAR : ), '.' )
                  IF( DOT .EQ. 0 ) THEN
                     IAT = 0
                  ELSE
                     IAT = DOT + PAR 
                  END IF
               END IF         
            END IF

*  Leave the loop if the template has been finished.
            IF( IAT .GT. LTEM ) IAT = 0

         END IF

      END DO

*  Get the number of potentially matching files.
      CALL GRP_GRPSZ( IGRP2, NMATCH, STATUS )

*  Check each one.
      DO I = 1, NMATCH

*  Get the full matching file spec, and the remaining text for this
*  potential match.
         CALL GRP_GET( IGRP2, I, 1, SPEC, STATUS )
         CALL GRP_GET( IGRP3, I, 1, REST, STATUS )

*  Get the directory path (everything from the start up to and including
*  the last "/").
         CALL NDG1_LASTO( SPEC, '/', IAT, STATUS )
         IF( IAT .EQ. 0 ) THEN
            DIR = ' '
         ELSE
            DIR = SPEC( : IAT )
         END IF

*  Get the base name (everything between the last "/" and the next ".")
*  and the file type (everything following the first ".").
         IAT = IAT + 1
         DOT = INDEX( SPEC( IAT : ), '.' )
         IF( DOT .EQ. 0 ) THEN
            NAM  = SPEC( IAT : )
            TYP = ' '
         ELSE IF( DOT .EQ. 1 ) THEN
            NAM = ' '
            TYP = SPEC( IAT : )
         ELSE
            NAM = SPEC( IAT : DOT + IAT - 2 )
            TYP = SPEC( DOT + IAT - 1: )
         END IF

*  Attempt to open the file as an HDS container file.
         CALL ERR_BEGIN( STATUS )
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
            IF( SPEC( IAT - 3 : IAT ) .NE. '.sdf' ) THEN

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

         CALL ERR_END( STATUS )         

      END DO

*  Delete the temporary groups.
      CALL GRP_DELET( IGRP2, STATUS )
      CALL GRP_DELET( IGRP3, STATUS )

      END
