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
*     The supplied template can include an HDS component path and may use
*     the "quoted file name" syntax recognised by the NDF_ library (but
*     these can only be used when specifying native NDFs).

*  Arguments:
*     TEMPLT = CHARACTER * ( * ) (Given)
*        The wild card template.
*     IGRP = INTEGER (Given)
*        An identifier for the group to which the expanded names should
*        be appended.
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
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'NDG_CONST'        ! NDG private constants.
      INCLUDE 'NDG_ERR'          ! NDG error constants.

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
      INTEGER NDG1_WILD
      INTEGER NDG1_EWILD

*  Local Variables:
      CHARACTER COMP*(GRP__SZNAM)  ! HDS component path from template
      CHARACTER COMP0*(GRP__SZNAM) ! HDS component path from matching file
      CHARACTER FDIRN*(GRP__SZFNM) ! The file directory path 
      CHARACTER FILE*(GRP__SZFNM)  ! The file spec of the matching file
      CHARACTER FNAME*(GRP__SZFNM) ! The file name 
      CHARACTER FTMPLT*(GRP__SZFNM)! The file name template
      CHARACTER FTYPE*(GRP__SZNAM) ! The file "type" (or HDS component path)
      CHARACTER SLICE*(GRP__SZNAM) ! NDF slice specification from template
      CHARACTER SLICE0*(GRP__SZNAM)! NDF slice specification from matching file
      CHARACTER STORED*(GRP__SZNAM)! Text to store in the supplied group
      CHARACTER TEXT*(GRP__SZNAM)  ! Text to append to matching file names
      INTEGER FORM               ! The form of the file spec
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of a string
      INTEGER ICONTX             ! Context for ndg1_wild
      INTEGER INDF               ! NDF identifier for matching native NDF
      INTEGER ISTAT              ! Local status value
      INTEGER PLACE              ! Dummy argument
      INTEGER SDF                ! Index of ".sdf" 
*.

*  Initialise the returned flag to indicate that no matching files have
*  yet been found.
      FOUND = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Split the supplied template up into its component parts; directory
*  path file base name, file type, HDS component path and NDF slice
*  specification. The form of the file spec is also determined. See
*  NDG1_HSPEC for a description of these forms.
      CALL NDG1_HSPEC( TEMPLT, FMTS, .TRUE., FDIRN, FNAME, FTYPE, 
     :                 COMP, SLICE, FORM, STATUS )

*  If the template did not include a file type, then we use ".sdf" if the
*  template included any HDS-specific syntax, and ".*" otherwise.
      IF( FTYPE .EQ. ' ' ) THEN
         IF( FORM .EQ. 1 ) THEN
            FTYPE = '.*'
         ELSE
            FTYPE = NDG__NDFTP
         END IF
      END IF

*  Get the file specification by concatenating the directory path, file
*  base name and file type.
      FTMPLT = FDIRN
      IAT = CHR_LEN( FDIRN )
      CALL CHR_APPND( FNAME, FTMPLT, IAT )
      CALL CHR_APPND( FTYPE, FTMPLT, IAT )

*  Return without further action if the template is blank.
      IF( FTMPLT .NE. ' ' ) THEN

*  Initialise the context value used by NDG1_WILD so that a new file
*  searching context will be started.
         ICONTX = 0

*  Loop round looking for matching files.
         ISTAT = NDG__OK
         DO WHILE( ISTAT .EQ. NDG__OK .AND. STATUS .EQ. SAI__OK ) 

*  Attempt to find the next matching file.
            FILE = ' '
            ISTAT = NDG1_WILD( FTMPLT, FILE, ICONTX )

*  If another file was found which matches the name...
            IF( ISTAT .EQ. NDG__OK ) THEN

*  If the file name ends with ".sdf" remove it.
               SDF = INDEX( FILE, NDG__NDFTP )
               IF( SDF .NE. 0 ) THEN
                  IF( FILE( SDF : ) .EQ. NDG__NDFTP ) THEN
                     FILE( SDF : ) = ' '
                  END IF
               END IF

*  Split the file name up into its component parts.
               CALL NDG1_HSPEC( FILE, FMTS, .FALSE., FDIRN, FNAME, 
     :                          FTYPE, COMP0, SLICE0, FORM, STATUS )

*  Construct a new file spec using the directory, nase-name and file type
*  from the matching file, together with the HDS component path and NDF
*  slice specification supplied in the template.
               CALL NDG1_MSPEC( FMTS, FDIRN, FNAME, FTYPE, COMP, SLICE, 
     :                          .FALSE., STORED, FORM, STATUS )

*  Before storing the string, we check that it can be accessed as an NDF.
*  This is only done for native (.sdf) files only because an expensive format 
*  conversion operation may be required to check foreign data files.
               IF( FORM .NE. 1 ) THEN

*  Abort if an error has occurred.
                  IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to open the NDF using the string which will be stored as its
*  name.
                  CALL NDF_OPEN( DAT__ROOT, STORED, 'READ', 'OLD', INDF, 
     :                           PLACE, STATUS )

*  If this failed, annul the error and set the stored string blank.
*  Otherwise, annul the NDF identifier.
                  IF( STATUS .NE. SAI__OK ) THEN                  
                     CALL ERR_ANNUL( STATUS )
                     STORED = ' '
                  ELSE
                     CALL NDF_ANNUL( INDF, STATUS )
                  END IF

               END IF

*  If OK, append the text to the supplied group, and set the returned flag 
*  to indicate that at least one file has been found.
               IF( STORED .NE. ' ' ) THEN
                  CALL GRP_PUT( IGRP, 1, STORED, 0, STATUS )
                  FOUND = .TRUE.
               END IF

*  If a system error was detected by NDG1_WILD, report it.
            ELSE IF ( ISTAT .EQ. NDG__WPER ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'NDG1_EXPAN_ERR1', 'NDG1_EXPAN: Error'//
     :                       ' getting pipe from forked process', 
     :                       STATUS )
      
            ELSE IF ( ISTAT .EQ. NDG__WMER ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'NDG1_EXPAN_ERR2', 'NDG1_EXPAN: '//
     :                       'Cannot allocate memory', STATUS )
      
            ELSE IF ( ISTAT .EQ. NDG__WSER ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'NDG1_EXPAN_ERR3', 'NDG1_EXPAN: '//
     :                       'Failed to locate the irg_wild script',
     :                       STATUS )
            END IF

         END DO

*  End the search context.
 999     CONTINUE

         ISTAT = NDG1_EWILD( ICONTX )

      END IF

      END
