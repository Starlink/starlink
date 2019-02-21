      SUBROUTINE NDG1_EXPAN( TEMPLT, VERB, IGRP, NFMT, FMT, FOUND,
     :                       STATUS )
*+
*  Name:
*     NDG1_EXPAN

*  Purpose:
*     Expands a wild card template.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_EXPAN( TEMPLT, VERB, IGRP, NFMT, FMT, FOUND, STATUS )

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
*     VERB = LOGICAL (Given)
*        If TRUE then errors which occur whilst accessing supplied NDFs
*        are flushed so that the user can see them before re-prompting for
*        a new NDF ("verbose" mode). Otherwise, they are annulled and
*        a general "Cannot access file xyz" message is displayed before
*        re-prompting.
*     IGRP = INTEGER (Given)
*        An identifier for the group to which the expanded names should
*        be appended. On exit, this group is at the end of a chain of
*        groups connected by a GRP "owner-slave" relationship. The other
*        groups is the chain hold individual fields  from the full NDF
*        specification. The full chain (starting from the head) is as
*        follows:
*
*                 1) NDF slice specifications
*                 2) Data paths (HDS path or foreign extension specifier)
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

*  Copyright:
*     Copyright (C) 1999 CLRC
*     Copyright (C) 2005, 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2009,2011 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     PWD: Peter W. Draper (STARLINK, Durham University)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-DEC-1997 (DSB):
*        Original version.
*     15-FEB-1999 (DSB):
*        Modified to include paths to all NDFs contained within a given
*        HDS container file.
*     30-NOV-1999 (DSB):
*        Check that nothing is written beyond the end of SEARCH string.
*     10-APR-2000 (DSB):
*        Added argument VERB.
*     18-JUL-2000 (DSB):
*        Added support for foreign extension specifiers.
*     28-FEB-2001 (DSB):
*        Assume trailing "[.]" strings are glob patterns, unless they
*        result in no matching files, in which case assume they are
*        foreign extension specifiers.
*     11-MAR-2004 (PWD):
*        Increased the size of all local character strings to
*        GRP__SZNAM. Previously some (notably BN) where 50 which was
*        truncating file names.
*     23-DEC-2005 (TIMJ):
*        Use HDS_FIND rather than NDG1_HFIND
*     27-DEC-2005 (TIMJ):
*        Let HDS_FIND deal with ".sdf" in the filename and handle the
*        logic fallout from that.
*     28-DEC-2005 (TIMJ):
*        Revamp the HDS/NDF/.sdf logic to put more reliance on HDS_FIND
*        rather than attempting to deal with special cases here.
*     6-MAR-2006 (DSB):
*        Escape any spaces in the supplied template before using
*        ONE_FIND_FILE.
*     7-MAR-2006 (DSB):
*        Turn off interpretation of shell metacharacters within HDS_FIND.
*     27-APR-2006 (DSB):
*        Do not escape spaces within NDF section strings.
*     26-JUN-2009 (TIMJ):
*        RPOS should be zero when using CHR_APPND otherwise you get a leading space.
*     14-NOV-2011 (DSB):
*        Allow foreign files to have dots in the file basename.
*     21-FEB-2019 (DSB):
*        Ensure LOC is always initialised, to avoid valgrind warnings.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'NDG_CONST'        ! NDG private constants.
      INCLUDE 'NDG_ERR'          ! NDG error constants.
      INCLUDE 'DAT_ERR'          ! HDS error constants

*  Arguments Given:
      CHARACTER TEMPLT*(*)
      LOGICAL VERB
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
      LOGICAL CHR_SIMLR

*  Local Constants:
      INTEGER MXSRCH             ! Max length of search list string
      PARAMETER ( MXSRCH = 2048 )
      INTEGER MAXDOT             ! Max no. of dots in file basename
      PARAMETER ( MAXDOT = 100 )

*  Local Variables:
      CHARACTER BN*(GRP__SZNAM)    ! File basename
      CHARACTER BNM*(GRP__SZNAM)   ! Modified file basename
      CHARACTER DIR*(GRP__SZNAM)   ! Directory field
      CHARACTER FTEMP*(GRP__SZNAM) ! File template
      CHARACTER FXS*(GRP__SZNAM)    ! Foreign extension specifier
      CHARACTER HDSPATH*(2048)     ! Path to HDS component including file and directory
      CHARACTER LOC*(DAT__SZLOC)   ! Locator for top-level object
      CHARACTER NAM*(GRP__SZNAM)   ! File base name field
      CHARACTER PATH*(GRP__SZNAM)  ! Data path
      CHARACTER REST*(GRP__SZNAM)  ! The remaining text after the file spec
      CHARACTER SEARCH*(MXSRCH)    ! The total search string
      CHARACTER SEC*(GRP__SZNAM)   ! File NDF/HDS section
      CHARACTER SLICE*(GRP__SZNAM) ! The NDF slice spec
      CHARACTER SPEC*(GRP__SZNAM)  ! The file spec of the matching file
      CHARACTER STORED*(GRP__SZNAM)! Text to store in the supplied group
      CHARACTER SUF*(GRP__SZNAM)   ! File suffix
      CHARACTER SUFF*(GRP__SZNAM)  ! Modified file suffix
      CHARACTER TMPLT2*(GRP__SZNAM)! File basename
      CHARACTER TYP*(GRP__SZNAM)   ! File type field
      INTEGER F                  ! Index of first non-blank character
      INTEGER FSTDOT( 0:MAXDOT ) ! Index of 1st IGRP2 entry with NDOT dots in basename
      INTEGER G2SIZ              ! Size of IGRP2 group
      INTEGER G2SIZ0             ! Original size of IGRP2 group
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of a string
      INTEGER IFMT               ! Index of current file type
      INTEGER IGRP2              ! Group of potential file matches
      INTEGER IGRP3              ! Group holding remaining text
      INTEGER IGRPB              ! Group holding base name fields
      INTEGER IGRPD              ! Group holding directory fields
      INTEGER IGRPH              ! Group holding data path fields
      INTEGER IGRPS              ! Group holding NDF slice fields
      INTEGER IGRPT              ! Group holding file type fields
      INTEGER J                  ! Loop count
      INTEGER L                  ! Index of last non-blank character
      INTEGER LEND               ! Index of last character before section
      INTEGER NC                 ! No. of characters in string
      INTEGER NDOT               ! No. of dots expected in basename
      INTEGER NMATCH             ! No. of matching file types
      INTEGER RPOS               ! Offset position in REST() for .sdf
      INTEGER SHELL              ! Origonal value of HDS SHELL tuning param
      INTEGER SIZE0              ! Size of original group
      INTEGER SLEN               ! Length of total search string
      LOGICAL MORE               ! Loop again?
      LOGICAL OK                 ! Was NDF slice OK?
      LOGICAL PURGE              ! Purge duplicate file names?
      LOGICAL QUOTE              ! Do we quote the HDS_FIND string?
      LOGICAL USEFXS             ! Use a "[.]" string as a for. ext. spec?
*.

*  Initialise the returned flag to indicate that no matching files have
*  yet been found.
      FOUND = .FALSE.

*  Placate the compiler
      PURGE = .FALSE.

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

*  The file types group is owned by a group holding the data paths.
      CALL GRP_OWN( IGRPT, IGRPH, STATUS )
      IF( IGRPH .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'Data path', IGRPH, STATUS )
         CALL GRP_SOWN( IGRPT, IGRPH, STATUS )
      END IF

*  The data paths group is owned by a group holding the NDF slices.
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

*  Indicate that duplicate file names should not be purged.
         PURGE = .FALSE.

*  Otherwise, we escape any embedded spaces in the template so that the
*  shell script used by one_find_file will interpret the spaces as part
*  of the file path. We do not escape spaces within the NDF section
*  identifier (if any) since this would confuse the NDF library. So we
*  stop checking for spaces when the first "(" or "." character is
*  encountered.
      ELSE
         J = INDEX( TEMPLT, '.' ) - 1
         LEND = INDEX( TEMPLT, '(' ) - 1
         IF( J .NE. -1 ) THEN
            IF( LEND .NE. -1 ) THEN
               LEND = MIN( LEND, J )
            ELSE
               LEND = J
            END IF
         END IF
         IF( LEND .EQ. -1 ) LEND = L + 1

         TMPLT2 = ' '
         J = 1
         DO I = F, L
            IF( TEMPLT( I : I ) .EQ. ' ' .AND. I .LT. LEND ) THEN
               TMPLT2( J : J ) = NDG__BKSLH
               J = J + 1
            END IF
            TMPLT2( J : J ) = TEMPLT( I : I )
            J = J + 1
         END DO

*  Split the template into directory, basename, suffix and section. On
*  the first pass the suffix is assumed to start at the first dot, but
*  the basename may contain a dot. So another pass is made in which the
*  suffix is assumed to start at the second dot (if any). This loop
*  continues, using one extra dot on each pass, until we exceed the
*  number of dots in the template. Only loop more than once if we have
*  some foreign formats defined.
         NDOT = 0
         SUF = '.'
         DO WHILE( SUF .NE. ' ' .AND. NDOT .LE. MAXDOT .AND.
     :             ( NDOT .EQ. 0 .OR. NFMT .GT. 0 ) .AND.
     :             STATUS .EQ. SAI__OK )
            CALL NDG1_FPARS( TMPLT2 ( : J ), NDOT, DIR, BN, SUF, SEC,
     :                       STATUS )

*  Store the index within IGRP2 of the first matching file spec that
*  contains "NDOT" dots in the basename.
            CALL GRP_GRPSZ( IGRP2, G2SIZ, STATUS )
            FSTDOT( NDOT ) = G2SIZ + 1

*  Increment the number of dots expected in the file basename in
*  preparation for the next pass round this loop.
            NDOT = NDOT + 1

*  Take copies of the file base name and suffix so that the originals
*  are not changed by the following code.
            BNM = BN
            SUFF = SUF

*  First, we look for any ".sdf" files with the given directory path
*  and file basename, ignoring the file suffix since "fred.fit" could refer
*  to component ".fit" within file fred.sdf. Any "[..]" string at the
*  start of the suffix is interpreted as a glob pattern, and is
*  transferred to the end of the basename.
            CALL NDG1_FORXT( SUFF, F, L, STATUS )
            IF( F .LE. L .AND. F .EQ. 1 ) THEN
               IAT = CHR_LEN( BNM )
               CALL CHR_APPND( SUFF( F : L ), BNM, IAT )
               SUFF( F : L ) = ' '
               CALL CHR_RMBLK( SUFF )
            END IF

*  Store matching file specs in IGRP2, and "the rest" (i.e. file suffix
*  - so long as it is not a simple wild-card ".*" - and section) in IGRP3.
            REST = ' '
            IAT = 0
            IF( SUFF .NE. '.*') CALL CHR_APPND( SUFF, REST, IAT )
            CALL CHR_APPND( SEC, REST, IAT )

            FTEMP = ' '
            IAT = 0
            CALL CHR_APPND( DIR, FTEMP, IAT )
            CALL CHR_APPND( BNM, FTEMP, IAT )
            CALL CHR_APPND( NDG__NDFTP, FTEMP, IAT )

            IF( IAT .GT. 0 ) THEN
               CALL NDG1_APPEN( IGRP2, IGRP3, FTEMP( : IAT ), REST,
     :                          STATUS )
            END IF

*  On the first pass through this "DO WHILE" loop, any trailing "[..]"
*  string in the suffix is treated as a glob pattern. If this assumption
*  results in no files being found, we make a second pass through this
*  loop in whoch the trailing "[..]" string is interpreted as a foreign
*  extension specifier.
            USEFXS = .TRUE.
            MORE = .TRUE.
            DO WHILE( MORE )
               USEFXS = .NOT. USEFXS
               IF( USEFXS ) MORE = .FALSE.

*  Take copies of the file base name and suffix so that the originals
*  are not changed by the following code.
               BNM = BN
               SUFF = SUF

*  Indicate we have not yet found a foreign extension specifier.
               FXS = ' '

*  See if there is a "[...]" string in the suffix. If not, pass on
*  without modifying the file base name or suffix.
               CALL NDG1_FORXT( SUF, F, L, STATUS )
               IF( F .LE. L ) THEN

*  If we are recognizing foreign extension specifiers, see if there is
*  a foreign extension specifier included in the suffix (a trailing
*  string enclosed in square brackets). If so, save it and remove it
*  from the suffix.
                  IF( USEFXS ) THEN
                     FXS = SUF( F : L )
                     SUFF( F : L ) = ' '
                     CALL CHR_RMBLK( SUFF )

*  If we are not recognizing foreign extension specifiers, any "[..]"
*  string in the suffix is assumed to be a glob pattern matching string.
*  If the "[..]" string is at the start of the suffix then it really
*  belongs at the end of the file base name. Transfer it.
                  ELSE IF( F .EQ. 1 ) THEN
                     IAT = CHR_LEN( BNM )
                     CALL CHR_APPND( SUF( F : L ), BNM, IAT )
                     SUFF( F : L ) = ' '
                     CALL CHR_RMBLK( SUFF )

                  END IF

               END IF

*  Construct the total trailing string following the file type.
               REST = ' '
               IAT = 0
               CALL CHR_APPND( FXS, REST, IAT )
               CALL CHR_APPND( SEC, REST, IAT )

*  From now on, if no suffix was given, use ".*" so that we pick up files
*  with any of the types included in NDF_FORMATS_IN. But indicate that
*  duplicate files with different file types should be purged.
               IF( SUFF .EQ. ' ' ) THEN
                  SUFF = '.*'
                  PURGE = .TRUE.

*  If a suffix was given, remove any foreign extension specifier (any
*  trailing string enclosed in matching square brackets).
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
                  IF( NDG1_MATCH( SUFF, FMT( IFMT ), STATUS ) ) THEN

*  If it does, append a wildcard template to the total file search string
*  which will match files with the given base name and the current
*  file type. Append a trailing space by incrementing SLEN.
                     FTEMP = ' '
                     IAT = 0
                     CALL CHR_APPND( DIR, FTEMP, IAT )
                     CALL CHR_APPND( BNM, FTEMP, IAT )
                     CALL CHR_APPND( FMT( IFMT ), FTEMP, IAT )

                     IF( IAT .GT. 0 .AND. MXSRCH - SLEN .GT. IAT ) THEN
                        CALL CHR_APPND( FTEMP( : IAT ), SEARCH, SLEN )
                        SLEN = SLEN + 1
                     ENDIF

                  END IF

               END DO

*  Get a list of all matching files, appending them to IGRP2. If the
*  the group increases in size, we have found some matching files. So
*  leave the "DO WHILE" loop.
               IF( SLEN .GT. 0 ) THEN
                  CALL GRP_GRPSZ( IGRP2, G2SIZ0, STATUS )

                  CALL NDG1_APPEN( IGRP2, IGRP3, SEARCH( : SLEN ), REST,
     :                             STATUS )

                  CALL GRP_GRPSZ( IGRP2, G2SIZ, STATUS )
                  IF( G2SIZ .GT. G2SIZ0 ) MORE =.FALSE.

               END IF

            END DO

         END DO

      END IF

*  Get the number of potentially matching files.
      CALL GRP_GRPSZ( IGRP2, NMATCH, STATUS )

*  Report an error if no matching files were found in verbose mode.
      IF( VERB .AND. NMATCH .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = NDG__NOFIL
         CALL MSG_SETC( 'T', TEMPLT )
         CALL ERR_REP( 'NDG1_EXPAN_ERR1', 'No files found matching '//
     :                 '''^T''.', STATUS )
      END IF

*  Get the original value of the HDS "SHELL" tuning parameter.
      CALL HDS_GTUNE( 'SHELL', SHELL, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Begin a new error context.
      CALL ERR_BEGIN( STATUS )

*  Set the HDS "SHELL" tuning parameters to stop HDS trying to interpret
*  shell metacharacters within the filename. We can do this because any
*  shell metacharacters will already have been interpreted within
*  NDG1_APPEN. The benefit of setting SHELL thus is that HDS mis-interprets
*  spaces within file names, resulting in HDS_FIND reporting an error if
*  the file spec ontains any spaces.
      CALL HDS_TUNE( 'SHELL', -1, STATUS )

*  Put a sentinel value at the end of the FSTDOT array.
      FSTDOT( NDOT ) = NMATCH + 1

*  The first matches have zero dots in the basename.
      NDOT = 0

*  Check each one.
      DO I = 1, NMATCH

*  Get the full matching file spec, and the remaining text for this
*  potential match.
         CALL GRP_GET( IGRP2, I, 1, SPEC, STATUS )
         CALL GRP_GET( IGRP3, I, 1, REST, STATUS )

*  Get the number of dots in the basename.
         DO WHILE( I .EQ. FSTDOT( NDOT + 1 ) )
            NDOT = NDOT + 1
         END DO

*  Split the file spec into directory, basename, suffix and section.
         CALL NDG1_FPARS( SPEC, NDOT, DIR, NAM, TYP, SEC, STATUS )

*  Let HDS do all the heavy lifting. So put "SPEC" and "REST" together
*  and let HDS_FIND look for the path. If the TYP indicates that this is not
*  a standard ".sdf" we quote the file path, else we do not include TYP so that
*  HDS can deal with the fact that it will be present in REST.
         RPOS = 0
         HDSPATH = ' '
         QUOTE = .FALSE.
         IF (TYP .NE. DAT__FLEXT) QUOTE = .TRUE.
         IF (QUOTE) CALL CHR_APPND( '"', HDSPATH, RPOS )
         CALL CHR_APPND( DIR, HDSPATH, RPOS )
         CALL CHR_APPND( NAM, HDSPATH, RPOS )
         IF (QUOTE) THEN
            CALL CHR_APPND( TYP, HDSPATH, RPOS )
            CALL CHR_APPND( '"', HDSPATH, RPOS )
         END IF

*  Append REST, with the slice to begin with
         CALL CHR_APPND( REST, HDSPATH, RPOS )

*  Now find the locator assuming HDS file (but only if status is good at this point)
         SLICE = ' '
         LOC = DAT__NOLOC
         CALL HDS_FIND( DAT__ROOT, HDSPATH, 'READ', LOC, STATUS )

*  If this worked we look for NDFs...
*  If this failed with DAT__SUBIN then it may be because of the slice specification
*  Note that HDS_FIND will only work with a slice into an HDS component but
*  the NDF section will apply to the components *within* the specified component.

         IF (STATUS .EQ. SAI__OK .OR. STATUS .EQ. DAT__SUBIN ) THEN

*  Deal with SUBIN case
            IF (STATUS .EQ. DAT__SUBIN) THEN

*  Look for a slice if path ends in ')' and try again
               CALL ERR_ANNUL( STATUS )

               IF( REST .NE. ' ' ) THEN
                  NC = CHR_LEN( HDSPATH )
                  IF( HDSPATH( NC : NC ) .EQ. ')' ) THEN
                     CALL CHR_LASTO( HDSPATH( : NC ), '(', IAT )
                     IF( STATUS .EQ. SAI__OK .AND. IAT .GT. 0 ) THEN

                        SLICE = HDSPATH( IAT : )
                        CALL HDS_FIND( DAT__ROOT, HDSPATH( : IAT - 1 ),
     :                       'READ', LOC, STATUS )

                        IF (STATUS .NE. SAI__OK) THEN
                           CALL ERR_ANNUL( STATUS )
                        END IF

                     END IF
                  END IF
               END IF

            END IF

*  If we found an object.
            IF( LOC .NE. DAT__NOLOC ) THEN

*  See if it contains any NDFs. If so, the details to the NDFs are stored
*  in the returned groups.
               CALL NDG1_SDFEX( IGRP, .FALSE., IGRPD, IGRPB, IGRPT,
     :                          IGRPH, IGRPS, LOC, DIR, NAM, TYP, SLICE,
     :                          FOUND, STATUS )


*  Annul the locator.
               CALL DAT_ANNUL( LOC, STATUS )
            END IF

*  If the file could not be opened as an HDS container file, annul the
*  error.
         ELSE
            IF( CHR_SIMLR(TYP, DAT__FLEXT) .AND. VERB ) THEN
               CALL ERR_FLUSH( STATUS )
            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF

*  Initialise the stored string to be the full file spec.
            STORED = SPEC
            IAT = CHR_LEN( SPEC )

*  The file spec cannot be used if it is a .sdf file.
            IF( TYP .NE. NDG__NDFTP ) THEN

*  Split the remaining text up into a data path (i.e. a foreign extension
*  specifier) and NDF section. If a FXS was given, append it to the end
*  of the returned NDF spec.
               F = INDEX( REST, '[' )
               L = INDEX( REST, ']' )
               IF( F .GT. 0 .AND. L .GT. F ) THEN
                  PATH = REST( F : L )
                  SLICE = REST( L + 1 : )
                  CALL CHR_APPND( PATH, STORED, IAT )
               ELSE
                  PATH = ' '
                  SLICE = REST
               END IF

*  If an NDF slice specification was given, check that it starts with "(",
*  ends with ")" and has no other parentheses in it. If a good slice is
*  found, append it to the end of the returned NDF spec.
               OK = .TRUE.
               IF( SLICE .NE. ' ' ) THEN
                  OK = .FALSE.
                  CALL CHR_FANDL( SLICE, F, L )
                  IF( SLICE( F : F ) .EQ. '(' .AND.
     :                SLICE( L : L ) .EQ. ')' ) THEN
                     IF( F + 1 .LE. L - 1 ) THEN
                        IF( INDEX( SLICE( F + 1 : L - 1 ), ')' ) .EQ. 0
     :                      .AND.
     :                      INDEX( SLICE( F + 1 : L - 1 ), '(' ) .EQ. 0
     :                    ) THEN
                             OK = .TRUE.
                             CALL CHR_APPND( SLICE( F : L ), STORED,
     :                                       IAT )
                        END IF
                     END IF
                  END IF
               END IF

*  If any slice was OK, store the NDF spec in the returned group.
               IF( OK ) THEN
                  CALL GRP_PUT( IGRP, 1, STORED( : IAT ), 0, STATUS )
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

*  Annul or flush any error which has occurred so that any remaining names
*  can be checked.
         IF( STATUS .NE. SAI__OK ) THEN
            IF( VERB ) THEN
               CALL ERR_FLUSH( STATUS )
            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF
         END IF

      END DO

*  End the error context started before the loop.
      CALL ERR_END( STATUS )

*  Return to the old HDS SHELL tuning setting.
      CALL ERR_BEGIN( STATUS )
      CALL HDS_TUNE( 'SHELL', SHELL, STATUS )
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
