      SUBROUTINE NDFPACK_MON( STATUS )
*+
*  Name:
*     NDFPACK_MON

*  Purpose:
*     Top-level NDFPACK subroutine for A-task monolith on UNIX.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDFPACK_MON( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This is the top-level A-task monolith subroutine for the NDFPACK
*     suite of A-tasks.  Each NDFPACK command is an alias to a softlink
*     that points to this monolith.  The chosen command is obtained
*     from the ADAM routine TASK_GET_NAME.  The command may be specified
*     from the shell or ICL.  Given the command, the requested A-task
*     is called after a successful matching of the input string with a
*     valid task name.  If there is no match, an error report is made.

*  Implementation Deficiencies:
*     The input string has to be forced to upper-case.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 August 19 (MJC):
*        Original version.
*     1997 May 31 (MJC):
*        Added FITSMOD. V1.1.
*     7-OCT-1998 (DSB):
*        Added WCSADD, WCSATTRIB, WCSCOPY, WCSFRAME, WCSSHOW, WCSREMOVE,
*        CHAIN, RESHAPE for V0.13.
*     13-MAY-1999 (DSB):
*        Changed history application name to incorporate the current version
*        of KAPPA.
*     30-AUG-1999 (DSB):
*        Added multiple invocation of applications using NDG looping.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE              ! no implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'          ! SSE global definitions

*  Status:
      INTEGER  STATUS

*  External References:
      EXTERNAL RESHAPE            ! To distinguish KAPPA RESHAPE from Fortran
                                  ! intrinsic function of the same name.
      LOGICAL NDG_AGAIN           ! Invoke the application again?

*  Local Variables:
      CHARACTER NAME * ( 15 )     ! Task name from the command
      LOGICAL VERB                ! Run in verbose mode?

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the command from the environment.  This returns uppercase
*  names.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Identify and execute the task.
*  ==============================
*
*  Define the current application name for history. The package version
*  number gets substituted in here when the KAPPA release source tar file 
*  is contructed.
      CALL NDF_HAPPN( NAME // ' (KAPPA PKG_VERS)', STATUS )

*  See if we are running in verbose mode.
      CALL KPG1_VERB( VERB, STATUS )

*  Initialise the common blocks used to control multiple invokation of
*  applications to process lists of NDFs.
      CALL NDG_START( VERB, STATUS )

*  Loop round invoking the task for each set of NDFs specified by the user.
      DO WHILE( NDG_AGAIN( STATUS ) )

*  Check the string against valid A-task names---if matched then call
*  the relevant A-task

*  Expands spaced axes in an NDF into the primitive form.
         IF ( NAME .EQ. 'AXCONV' ) THEN
            CALL AXCONV ( STATUS )

*  Sets a new label value for an axis within an NDF data structure.
         ELSE IF ( NAME .EQ. 'AXLABEL' ) THEN
            CALL AXLABEL ( STATUS )

*  Sets a new units value for an axis within an NDF data structure.
         ELSE IF ( NAME .EQ. 'AXUNITS' ) THEN
            CALL AXUNITS ( STATUS )

*  Concatenates a series of vectorized NDFs.
         ELSE IF ( NAME .EQ. 'CHAIN' ) THEN
            CALL CHAIN ( STATUS )

*  Erases an HDS object.
         ELSE IF ( NAME .EQ. 'ERASE' ) THEN
            CALL ERASE ( STATUS )

*  Reads a FITS disk file composed of simple, group or table files.
         ELSE IF ( NAME .EQ. 'FITSDIN' ) THEN
            CALL FITSDIN ( STATUS )

*  Exports NDF-extension information into an NDF FITS extension.
         ELSE IF ( NAME .EQ. 'FITSEXP' ) THEN
            CALL FITSEXP ( STATUS )

*  Imports FITS information into an NDF extension.
         ELSE IF ( NAME .EQ. 'FITSIMP' ) THEN
            CALL FITSIMP ( STATUS )

*  Reads a FITS tape composed of simple, group or table files.
         ELSE IF ( NAME .EQ. 'FITSIN' ) THEN
            CALL FITSIN ( STATUS )

*  Lists the FITS extension of an NDF.
         ELSE IF ( NAME .EQ. 'FITSLIST' ) THEN
            CALL FITSLIST ( STATUS )

*  Edits an NDF FITS extension via a text file or parameters.
         ELSE IF ( NAME .EQ. 'FITSMOD' ) THEN
            CALL FITSMOD ( STATUS )

*  Creates an NDF FITS extension from a text file.
         ELSE IF ( NAME .EQ. 'FITSTEXT' ) THEN
            CALL FITSTEXT ( STATUS )

*  Sets the NDF history update mode.
         ELSE IF ( NAME .EQ. 'HISCOM' ) THEN
            CALL HISCOM ( STATUS )

*  Lists NDF history records.
         ELSE IF ( NAME .EQ. 'HISLIST' ) THEN
            CALL HISLIST ( STATUS )

*  Sets the NDF history update mode.
         ELSE IF ( NAME .EQ. 'HISSET' ) THEN
            CALL HISSET ( STATUS )

*  Converts an HDS object to  machine data representation.
         ELSE IF ( NAME .EQ. 'NATIVE' ) THEN
            CALL NATIVE ( STATUS )

*  Copies an NDF (or NDF section) to a new location.
         ELSE IF ( NAME .EQ. 'NDFCOPY' ) THEN
            CALL NDFCOPY ( STATUS )

*  Displays the attributes of an NDF data structure.
         ELSE IF ( NAME .EQ. 'NDFTRACE' ) THEN
            CALL NDFTRACE ( STATUS )

*  Reshapes an NDF, treating its arrays as vectors.
         ELSE IF ( NAME .EQ. 'RESHAPE' ) THEN
            CALL RESHAPE ( STATUS )

*  Sets values for an axis array component within an NDF data
*  structure.
         ELSE IF ( NAME .EQ. 'SETAXIS' ) THEN
            CALL SETAXIS ( STATUS )

*  Sets new bad-pixel flag values for an NDF.
         ELSE IF ( NAME .EQ. 'SETBAD' ) THEN
            CALL SETBAD ( STATUS )

*  Sets a new value for the quality bad-bits mask of an NDF.
         ELSE IF ( NAME .EQ. 'SETBB' ) THEN
            CALL SETBB ( STATUS )

*  Sets new bounds for an NDF.
         ELSE IF ( NAME .EQ. 'SETBOUND' ) THEN
            CALL SETBOUND ( STATUS )

*  Manipulates the contents of a specified NDF extension.
         ELSE IF ( NAME .EQ. 'SETEXT' ) THEN
            CALL SETEXT ( STATUS )

*  Sets a new value for the label component of an NDF data structure.
         ELSE IF ( NAME .EQ. 'SETLABEL' ) THEN
            CALL SETLABEL ( STATUS )

*  Sets a new value for one or all of an NDF's axis-normalisation
*  flags.
         ELSE IF ( NAME .EQ. 'SETNORM' ) THEN
            CALL SETNORM ( STATUS )

*  Sets a new pixel origin for an NDF.
         ELSE IF ( NAME .EQ. 'SETORIGIN' ) THEN
            CALL SETORIGIN ( STATUS )

*  Makes an IRAS astrometry extension.
         ELSE IF ( NAME .EQ. 'SETSKY' ) THEN
            CALL SETSKY ( STATUS )

*  Sets a new value for the title component of an NDF data structure.
         ELSE IF ( NAME .EQ. 'SETTITLE' ) THEN
            CALL SETTITLE ( STATUS )

*  Sets a new numeric type for the data and variance components of an
*  NDF.
         ELSE IF ( NAME .EQ. 'SETTYPE' ) THEN
            CALL SETTYPE ( STATUS )

*  Sets a new value for the units component of an NDF data structure.
         ELSE IF ( NAME .EQ. 'SETUNITS' ) THEN
            CALL SETUNITS ( STATUS )

*  Sets new values for the variance component of an NDF data structure.
         ELSE IF ( NAME .EQ. 'SETVAR' ) THEN
            CALL SETVAR ( STATUS )

*  Add a Frame into the WCS component of an NDF.
         ELSE IF ( NAME .EQ. 'WCSADD' ) THEN
            CALL WCSADD ( STATUS )

*  Manage the attributes of the WCS component of an NDF.
         ELSE IF ( NAME .EQ. 'WCSATTRIB' ) THEN
            CALL WCSATTRIB ( STATUS )

*  Copy the WCS component from one NDF to another.
         ELSE IF ( NAME .EQ. 'WCSCOPY' ) THEN
            CALL WCSCOPY ( STATUS )

*  Chaneg the current co-ordinate Frame in the WCS component of an NDF.
         ELSE IF ( NAME .EQ. 'WCSFRAME' ) THEN
            CALL WCSFRAME ( STATUS )

*  Remove Frames from the WCS component of an NDF.
         ELSE IF ( NAME .EQ. 'WCSREMOVE' ) THEN
            CALL WCSREMOVE ( STATUS )

*  Display a WCS structure as an AST dump.
         ELSE IF ( NAME .EQ. 'WCSSHOW' ) THEN
            CALL WCSSHOW ( STATUS )

         ELSE

*  No such option exists.
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'CMD', NAME )
            CALL ERR_REP( 'NDFPACK_MON_NOCOM',
     :     'NDFPACK: No such option ^CMD.', STATUS )

         END IF

      END DO

*  End and return.
      END
