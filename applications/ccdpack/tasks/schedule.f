      SUBROUTINE SCHEDULE( STATUS )
*+
*  Name:
*     SCHEDULE

*  Purpose:
*     Schedules an automated CCDPACK reduction.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SCHEDULE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine accepts a list of input NDFs and uses the information
*     in their CCDPACK extensions to schedule a reduction. The schedule
*     is produced as a command script which may be executed immediately
*     or retained for execution using the standard CCDPACK facilities
*     (CCDFORK).
*
*     The reduction schedule produced covers the following stages of
*     data reduction (this is order).
*
*        1) production of a master bias
*        2) removal of the bias contribution
*        3) production of a master dark
*        4) removal of dark count contribution
*        5) production of a master pre-flash
*        6) removal of pre-flash contribution
*        5) production of master flatfields (one for each filter type)
*        6) correction of data for flatfield response
*
*     The stages which are preformed for each NDF depend on the type of
*     NDF (TARGET, FLAT, BIAS, DARK etc.) and any processing which has
*     already taken place. For instance if calibration masters of any
*     type already exist then they will be used in preference to the
*     production of any new masters. If all the TARGET frames have
*     already been flatfielded then no further processing will be
*     performed, if no BIAS frames of any type exist then debiassing
*     will be performed using bias strip interpolation or by
*     subtracting a single constant etc. Reductions which have failed
*     (due to a lack of resources) can be "picked up" and restarted
*     from the position at which they failed (by a re-invocation of
*     this routine). Facilities for controlling the use of disk space
*     are also available.
*
*     Before you can use this routine you must make sure that all the
*     necessary information is entered into the NDF extensions. You can
*     do this using the routines IMPORT or CCDSETUP and PRESENT or any
*     combination of these which give the desired effect.

*  Usage:
*     schedule in script stype debias=? execute=? interp=? spacesave=?

*  ADAM Parameters:
*     DARKEXT = LITERAL (Read)
*       The extension which added to the names of any NDFs processed by
*       CALCOR when performing dark count correction. This makes the
*       parameter
*
*          OUT=*"darkext"
*
*       form the names of the NDFs output from CALCOR.
*       [-dk]
*     DEBIAS = _INTEGER (Read)
*       The form of debiassing that should be used. This is an integer
*       which represents one of the following:
*          1 = produce a master and offset to bias strips (master bias
*              is zeroed)
*          2 = produce a master and do not offset to strips (in this
*              case the master bias is not zeroed)
*          3 = use interpolation between bias strip(s)
*          4 = subtract a constant as bias.
*
*       Using the information about the frame types which are available
*       and the presence or not of bias strips etc. a list of the
*       possible debiassing options is shown, before this parameter is
*       accessed. Any of the above methods can be selected regardless
*       of this advice, but the reduction may then fail unless action is
*       taken (such as adapting the output script).
*
*       If the interpolation option is selected then the method is
*       determined by the INTERP parameter.
*     DEBIASEXT = LITERAL (Read)
*       The extension which added to the names of any NDFs processed by
*       DEBIAS. This makes the parameter
*
*          OUT=*"debiasext"
*
*       form the names of the NDFs output from DEBIAS.
*       [-db]
*     EXECUTE = _LOGICAL (Read)
*       Whether to execute the output command script immediately or not.
*       If the option to execute is chosen then a background
*       process is started which performs the actual execution.
*       Do not execute the procedure using this method if your system
*       supports a queuing system which should be used instead (if you
*       expect the reduction to take some time). This option does not
*       work for ICL scripts at this time.
*       [FALSE]
*     EXELOGFILE = LITERAL (Read)
*       If the reduction is started immediately then the output will be
*       redirected to this file.
*       [SCHEDULE.LOG]
*     FLASHEXT = LITERAL (Read)
*       The extension which added to the names of any NDFs processed by
*       CALCOR when performing pre-flash correction. This makes the
*       parameter
*
*          OUT=*"flashext"
*
*       form the names of the NDFs output from CALCOR.
*       [-dk]
*     FLATEXT = LITERAL (Read)
*       The extension which added to the names of any NDFs processed by
*       FLATCOR. This makes the parameter
*
*          OUT=*"flatext"
*
*       form the names of the NDFs output from FLATCOR.
*       [-flt]
*     IN = LITERAL (Read)
*       A list of the names of the NDFs which contain the data to be
*       reduced. All NDFs must already have the correct "frame type"
*       information (extension item FTYPE) entered into their CCDPACK
*       extensions. Together with any other relevant information (such
*       as filter type, position of the bias strips, useful area etc.,
*       see IMPORT and/or PRESENT).
*
*       The NDF names should be separated by commas and may include
*       wildcards.
*     INTERP = _INTEGER (Read)
*       If the interpolation method is chosen using the DEBIAS parameter
*       then this parameter controls how the interpolation should be
*       performed. The possible returns are:
*
*          1 = fit a constant for each row/column
*          2 = fit a single value for whole NDF
*          3 = fit a line to each row/column
*          4 = fit a plane to whole NDF
*
*       The possible options given the input information about the
*       presence of bias strips are shown before the value of this
*       parameter is accessed.
*     IRFLATS = _LOGICAL (Read)
*       This parameter allows input frames of type TARGET to be also
*       used as flatfields. This is designed for use when no real
*       flatfields exist. IR data is often calibrated in this way, and
*       less commonly optical data. In both these cases it is asummed
*       that the objects are moved on the sky sufficiently, between
*       exposures, so that taking the median of a stack of frames
*       results in the rejection of any object data (leaving the
*       equivalent of a map of a blank piece of sky).
*
*       TARGET frames will only be used to create flatfields, if no
*       flatfields (of the correct colour) are present in the input list.
*       [FALSE]
*     LOGFILE = FILENAME (Read)
*       Name of the CCDPACK logfile.  If a null (!) value is given for
*       this parameter then no logfile will be written, regardless of
*       the value of the LOGTO parameter.
*
*       If the logging system has been initialised using CCDSETUP,
*       then the value specified there will be used. Otherwise, the
*       default is "CCDPACK.LOG".
*       [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*       Every CCDPACK application has the ability to log its output
*       for future reference as well as for display on the terminal.
*       This parameter controls this process, and may be set to any
*       unique abbreviation of the following:
*          -  TERMINAL  -- Send output to the terminal only
*          -  LOGFILE   -- Send output to the logfile only (see the
*                          LOGFILE parameter)
*          -  BOTH      -- Send output to both the terminal and the
*                          logfile
*          -  NEITHER   -- Produce no output at all
*       If the logging system has been initialised using CCDSETUP
*       then the value specified there will be used. Otherwise, the
*       default is "BOTH".
*       [BOTH]
*     MASTERBIAS = LITERAL (Read)
*       The name which will be given to a master bias NDF if one is
*       created.
*       [MASTER_BIAS]
*     MASTERDARK = LITERAL (Read)
*       The name which will be given to a master dark NDF if one is
*       created.
*       [MASTER_DARK]
*     MASTERFLASH = LITERAL (Read)
*       The name which will be given to a master flash NDF if one is
*       created.
*       [MASTER_FLASH]
*     MASTERFLAT = LITERAL (Read)
*       The prefix of the name which will be given to any master flat
*       NDFs which are created. The filter name will be appended to
*       this.
*       [MASTER_FLAT]
*     SCRIPT = LITERAL (Read)
*       The name of the output file which will contain the CCDPACK
*       commands which need to be executed to perform the reduction. The
*       nature of this script is controlled by the STYPE parameter. The
*       default name is dynamically set to be SCHEDULE with a type set
*       by the choice of STYPE. The extension of the script name should
*       always be the same as STYPE.
*       [schedule."stype"]
*     SPACESAVE = LITERAL (Read)
*       This parameter controls if any disk space management should be
*       used or not. It can take one of the values, "NONE", "SOME" or
*       "LOTS".
*
*          "NONE" indicates that no NDFs should be deleted.
*          "SOME" indicates that all intermediate NDFs should be
*                 deleted. This occurs after they are processed.
*          "LOTS" indicates that all processed NDFs should be deleted.
*                 In this case all intermediary NDFs and the original
*                 NDFs are deleted when processed.
*
*       Intermediary NDFs are deleted by the CCDPACK applications when
*       they are finished processing then. So for instance in the case
*       of FLATCOR each NDF is deleted in turn, so the additional disk
*       space required is one NDF. Using "SOME" preserves the original
*       NDFs. Calibration masters are never deleted.
*       [NONE]
*     STYPE = LITERAL (Read)
*       The type of CCDPACK command procedure to be produced. This
*       should be one of "CSH" or "ICL". Once a type has been
*       chosen the output script (parameter SCRIPT) can only be
*       executed using the selected interpreter. Note that if you
*       choose ICL then the resultant script cannot be executed
*       immediately, you must activate this yourself.
*       [CSH]

*  Examples:
*     schedule '*' ccdreduce csh debias=1
*        This example processes all the NDFs in the current directory
*        producing a script file called ccdreduce.csh which is suitable
*        for executing from the C-shell. The debiassing method chosen is
*        to use a zeroed master bias which is offset to the bias strip
*        data level.
*     schedule '*' ccdreduce csh debias=1 execute=true
*        As above except that the script ccdreduce.csh is forked into a
*        background process and executed. The output from this job will
*        be found in the file schedule.log.
*     schedule '*' tryinterp debias=3 interp=3
*        In this example the debiassing is performed using interpolation
*        between the bias strips.
*     schedule spacesave=lots
*        In this example the command script will be written so that all
*        intermediary NDFs (those produced by the various applications)
*        and the original raw NDFs, will be deleted as and when they are
*        processed.
*     schedule 'data*' irflats debias=4
*        In this example the frames 'data*' are scheduled for reduction.
*        The debiassing method is subtraction of a constant (this should
*        be set by PRESENT) and a flatfield is produced by median stacking
*        all the data frames.

*  Copyright:
*     Copyright (C) 1993-1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 2000 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-NOV-1993 (PDRAPER):
*        Original version.
*     1-FEB-1994 (PDRAPER):
*        Added debiassing methods.
*     19-JUL-1995 (PDRAPER):
*        Removed AIF_ calls.
*     1-SEP-1995 (PDRAPER):
*        Now closes script before attempting to execute it.
*     4-SEP-1995 (PDRAPER):
*        Removed DCL compatibility and introduced ICL.
*     6-OCT-1995 (PDRAPER):
*        Updated fof CCDPACK version 2.0.
*     13-NOV-1995 (PDRAPER):
*        Added ability to use targets as possible flatfields to better
*        support IR data reductions.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters
      INCLUDE 'FIO_PAR'          ! FIO constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 40 ) DEBEXT ! Extension applied to NDF names after debiassing
      CHARACTER * ( 40 ) DRKEXT ! Extension applied to NDF names after dark correction
      CHARACTER * ( 40 ) FLSEXT ! Extension applied to NDF names after pre-flash correction
      CHARACTER * ( 40 ) FLTEXT ! Extension applied to NDF names after flatfielding
      CHARACTER * ( 3 ) STYPE    ! Script type
      CHARACTER * ( 4 ) SAVER    ! Disk space saving option
      CHARACTER * ( 80 ) BIAS    ! Name of master bias
      CHARACTER * ( 80 ) DARK    ! Name of master bias
      CHARACTER * ( 80 ) FLASH   ! Name of master bias
      CHARACTER * ( 80 ) FLAT    ! Name of master bias
      CHARACTER * ( CCD1__NMLEN ) FTYPES( 2, CCD1__MXINS ) ! NDF frame and filter types.
      CHARACTER * ( CCD1__NMLEN) FILNMS( CCD1__MXINS ) ! FILTER names
      CHARACTER * ( FIO__SZFNM ) ORIG( CCD1__MXINS ) ! Names of original data files.
      DOUBLE PRECISION DRKTIM( CCD1__MXINS ) ! NDF dark times
      DOUBLE PRECISION FLSTIM( CCD1__MXINS ) ! NDF dark times
      INTEGER DEBTYP             ! The type of debiassing to be performed
      INTEGER GIDIN              ! Input NDF GRP group identifier
      INTEGER GIDOWN             ! GRP identifier for owning group
      INTEGER INTERP             ! Interpolation method
      INTEGER NBOUND             ! Minimum number of bounds values in NDFs
      INTEGER NFILS              ! Number of FILTER types
      INTEGER NNDF               ! Number of input NDFs
      INTEGER PTEMP1( CCD1__MXINS ) ! Workspace
      LOGICAL PTEMP2( CCD1__MXINS ) ! Workspace
      INTEGER SCRFD              ! Script file descriptor
      LOGICAL DARKCR( CCD1__MXINS ) ! Dark corrected status of input NDFs
      LOGICAL DEBICR( CCD1__MXINS ) ! Debiassing status of input NDFs
      LOGICAL DEBOPT( 4 )        ! Possible debiassing options
      LOGICAL DODARK             ! Do some dark correction
      LOGICAL DODEBI             ! Do some debiassing
      LOGICAL DOFLAS             ! Do some flash correction
      LOGICAL DOFLAT( CCD1__MXINS ) ! Do some flatfielding
      LOGICAL EXECUT             ! Whether to execute command script now or not
      LOGICAL FLASCR( CCD1__MXINS ) ! Flash corrected status of input NDFs
      LOGICAL FLATCR( CCD1__MXINS ) ! Flatfield corrected status of input NDFs
      LOGICAL HAVONE             ! Have at least one value
      LOGICAL HVBIAS             ! Have a master bias already
      LOGICAL HVDARK             ! Have a master dark already
      LOGICAL HVFLAS             ! Have a master flash already
      LOGICAL HVFLAT( CCD1__MXINS ) ! Have FLATs for this filter
      LOGICAL HVZERO             ! NDFs have a single bias level
      LOGICAL INTER( 4 )         ! Possible interpolation options
      LOGICAL IRFLAT             ! IR data using TARGETS as flats
      LOGICAL MKBIAS             ! Need a MASTER_BIAS
      LOGICAL MKDARK             ! Need a MASTER_DARK
      LOGICAL MKFLAS             ! Need a MASTER_FLASH
      LOGICAL MKFLAT( CCD1__MXINS ) ! Need for MASTER_FLATs
      LOGICAL SCROPN             ! Script file open
      LOGICAL VALID( CCD1__MXINS ) ! Valid input NDF mask
      LOGICAL ZEROCK             ! Whether to check ZEROED options
      LOGICAL ZEROED             ! Whether or not a MASTER_BIAS is zeroed
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up CCDPACK.
      CALL CCD1_START( 'SCHEDULE', STATUS )

*  Set up global NDF context.
      CALL NDF_BEGIN

*  Access the input name list.
      CALL CCD1_NDFGR( 'IN', GIDIN, NNDF, STATUS )

*  Because of the way in which this group is manipulated by the rest of
*  this task, it ought to be treated as a simple GRP group, without the
*  supplemental information it has acquired from NDG_ASSOC as part of
*  an NDG group.  We thus cut loose all the supplemental information
*  leaving an unowned group containing only full file specifications.
      CALL GRP_OWN( GIDIN, GIDOWN, STATUS )
      CALL GRP_SOWN( GIDIN, GRP__NOID, STATUS )
      CALL CCD1_GRDEL( GIDOWN, STATUS )

*  Create a list of the NDF frame and filter types. The presence of
*  extension items which are concerned with the type of DEBIASsing are
*  also checked as are the processing state of the NDFs.
      CALL CCD1_FTYPL( GIDIN, NNDF, FTYPES, VALID, ORIG, NBOUND,
     :                 HVZERO, DEBICR, DARKCR, DRKTIM, FLASCR, FLSTIM,
     :                 FLATCR, ZEROED, ZEROCK, STATUS )

*  Look for NDFs which shouldn't be reduced (already flatfielded) and
*  repeats of NDFs (i.e. the name of NDFs which correspond to earlier
*  stages of reduction) and remove these from the lists.
      CALL CCD1_RESTA( FTYPES, NNDF, GIDIN, ORIG, DEBICR, DARKCR,
     :                 FLASCR, FLATCR, VALID, STATUS )


*  Offer the chance to use TARGETS as flatfields. This is a useful option
*  for IRDATA users.
      CALL PAR_GET0L( 'IRFLATS', IRFLAT, STATUS )
      IF ( IRFLAT ) THEN
         CALL CCD1_MSG( ' ',
     :'  Warning - targets may be used as substitute flatfields',
     :   STATUS )
      END IF

*  Take a first look at the processing schedule and decide what we might
*  attempt.
      CALL CCD1_SCHED( FTYPES, NNDF, DEBICR, DARKCR, DRKTIM, FLASCR,
     :                 FLSTIM, FLATCR, IRFLAT, VALID, PTEMP1, MKBIAS,
     :                 HVBIAS, DODEBI, MKDARK, HVDARK, DODARK, MKFLAS,
     :                 HVFLAS, DOFLAS, MKFLAT, HVFLAT, DOFLAT, FILNMS,
     :                 NFILS, STATUS )

*  Find out what type of script to write.
      CALL PAR_CHOIC( 'STYPE', 'csh', 'csh,icl', .FALSE., STYPE,
     :                STATUS )
      CALL CHR_LCASE( STYPE )

*  Get the name of a file to write the script into.
      ORIG( 1 ) = 'schedule.'//STYPE
      CALL PAR_DEF0C( 'SCRIPT', ORIG( 1 ), STATUS )
      CALL CCD1_ASFIO( 'SCRIPT', 'WRITE', 'LIST', 0, SCRFD, SCROPN,
     :                 STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get the type of bias subtraction we're going to do. This may depend
*  on what information is available in the NDF extensions.
*  DEBTYP
*  The debiassing method one of
*    1 = producing master and offsetting to strips
*    2 = producing master and not offsetting to strips
*        (either none exist or not using)
*    3 = using interpolation (no biases but need strips)
*    4 = direct subtraction of constant
      DEBOPT( 1 ) = .FALSE.
      DEBOPT( 2 ) = .FALSE.
      DEBOPT( 3 ) = .FALSE.
      DEBOPT( 4 ) = .FALSE.
      IF ( MKBIAS .OR .HVBIAS ) THEN
         DEBOPT( 2 ) = .TRUE.
         IF ( NBOUND .GT. 0 ) DEBOPT( 1 ) = .TRUE.
      END IF
      IF ( NBOUND .GT. 0 ) DEBOPT( 3 ) = .TRUE.
      IF ( HVZERO ) DEBOPT( 4 ) = .TRUE.

*  See if these make sense with the state of a master bias (if one
*  exists already).
      IF ( HVBIAS ) THEN
         IF ( ZEROCK ) THEN
            IF ( ZEROED ) THEN

*  Master is zeroed, we should offset.
               DEBOPT( 2 ) = .FALSE.
            ELSE

*  Master is not zeroed we should not offset.
               DEBOPT( 1 ) = .FALSE.
            END IF
         END IF
      END IF

*  Write message to user about the debiassing options they now have.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ',
     :'  Debiassing options which seem to be available:', STATUS )
      HAVONE = .FALSE.
      IF ( DEBOPT( 1 ) ) THEN
         CALL CCD1_MSG( ' ',
     :'    1 - zeroed master bias, offsetting to bias strips', STATUS )
         HAVONE = .TRUE.
      END IF
      IF ( DEBOPT( 2 ) ) THEN
         CALL CCD1_MSG( ' ',
     :'    2 - unzeroed master bias', STATUS )
         HAVONE = .TRUE.
      END IF
      IF ( DEBOPT( 3 ) ) THEN
         CALL CCD1_MSG( ' ',
     :'    3 - interpolate using bias strips', STATUS )
         HAVONE = .TRUE.
      END IF
      IF ( DEBOPT( 4 ) ) THEN
         CALL CCD1_MSG( ' ',
     :'    4 - single constant', STATUS )
         HAVONE = .TRUE.
      END IF

*  If no options seem to be available issue a warning.
      IF ( .NOT. HAVONE ) THEN
         CALL CCD1_MSG( ' ', '    None', STATUS )
      END IF
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Get the preference from the user. Note this allows any value in the
*  range 1-4.
      CALL PAR_GDR0I( 'DEBIAS', 0, 1, 4, .FALSE., DEBTYP, STATUS )

*  If the DEBTYP is 3 then interpolation has been chosen.
      IF ( DEBTYP .EQ. 3 ) THEN

*  The interpolation methods which may be used are.
*    1 = constant for each line (SMODE=CONSTANT, FMODE=LINE)
*    2 = single value for whole NDF (SMODE=CONSTANT, FMODE=PLANE)
*    3 = linear fits for each line (SMODE=LINEAR, FMODE=LINE)
*    4 = planar fit (SMODE=LINEAR, FMODE=PLANE)
*  The value for this  argument should be consistent with the extension
*  information of the NDFs, i.e. only one bias strip means can only do
*  1 or 2.
         INTER( 1 ) = .FALSE.
         INTER( 2 ) = .FALSE.
         INTER( 3 ) = .FALSE.
         INTER( 4 ) = .FALSE.
         IF ( NBOUND .EQ. 2 ) THEN
            INTER( 1 ) = .TRUE.
            INTER( 2 ) = .TRUE.
         ELSE IF ( NBOUND .GT. 2 ) THEN
            INTER( 1 ) = .TRUE.
            INTER( 2 ) = .TRUE.
            INTER( 3 ) = .TRUE.
            INTER( 4 ) = .TRUE.
         END IF

*  Report the possible options to the user.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :'  Interpolation options which seem to be available:', STATUS )
         HAVONE = .FALSE.
         IF ( INTER( 1 ) ) THEN
         CALL CCD1_MSG( ' ',
     :'    1 - constant for each line', STATUS )
            HAVONE = .TRUE.
         END IF
         IF ( INTER( 2 ) ) THEN
            CALL CCD1_MSG( ' ',
     :'    2 - constant for whole NDF', STATUS )
            HAVONE = .TRUE.
         END IF
         IF ( INTER( 3 ) ) THEN
            CALL CCD1_MSG( ' ',
     :'    3 - linear fit for each line', STATUS )
            HAVONE = .TRUE.
         END IF
         IF ( INTER( 4 ) ) THEN
            CALL CCD1_MSG( ' ',
     :'    4 - plane for whole NDF', STATUS )
            HAVONE = .TRUE.
         END IF

*  If no options seem to be available issue a warning.
         IF ( .NOT. HAVONE ) THEN
            CALL CCD1_MSG( ' ', '    None', STATUS )
         END IF
         CALL CCD1_MSG( ' ', ' ', STATUS )

*  Get the preference from the user. Note this allows any value in the
*  range 1-4.
         CALL PAR_GDR0I( 'INTERP', 0, 1, 4, .FALSE., INTERP, STATUS )
      END IF

*  Get the extension modifications to the NDF names for each processing
*  section.
      CALL PAR_GET0C( 'DEBIASEXT', DEBEXT, STATUS )
      CALL PAR_GET0C( 'DARKEXT', DRKEXT, STATUS )
      CALL PAR_GET0C( 'FLASHEXT', FLSEXT, STATUS )
      CALL PAR_GET0C( 'FLATEXT', FLTEXT, STATUS )

*  Get the names of the various masters.
      IF ( MKBIAS ) CALL PAR_GET0C( 'MASTERBIAS', BIAS, STATUS )
      IF ( MKDARK ) CALL PAR_GET0C( 'MASTERDARK', DARK, STATUS )
      IF ( MKFLAS ) CALL PAR_GET0C( 'MASTERFLASH', FLASH, STATUS )
      IF ( NFILS .GT. 0 ) CALL PAR_GET0C( 'MASTERFLAT', FLAT, STATUS )

*  Final question before producing script.
      CALL PAR_CHOIC( 'SPACESAVE', 'NONE', 'NONE,SOME,LOTS', .TRUE.,
     :                 SAVER, STATUS )

*  Now write the reduction procedure.
      CALL CCD1_AUTO( STYPE, SCRFD, GIDIN, FTYPES, NNDF, VALID, MKBIAS,
     :                BIAS, HVBIAS, DEBICR, DODEBI, DEBTYP, INTERP,
     :                MKDARK, DARK, HVDARK, DARKCR, DODARK, DRKTIM,
     :                MKFLAS, FLASH, HVFLAS, DOFLAS, FLASCR, FLSTIM,
     :                MKFLAT, FLAT, HVFLAT, DOFLAT, FILNMS, NFILS,
     :                DEBEXT, DRKEXT, FLSEXT, FLTEXT, SAVER, IRFLAT,
     :                PTEMP1, PTEMP2, STATUS )


*  Get the name of the output file.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL FIO_FNAME( SCRFD, ORIG( 1 ), STATUS )

*  Decide whether to execute it now or not (not available for ICL, this
*  requires that output is attached to tty).
      IF ( STYPE .NE. 'ICL' ) THEN
         CALL PAR_GET0L( 'EXECUTE', EXECUT, STATUS )
      ELSE
         EXECUT = .FALSE.
      END IF

*  Close the script file to flush the contents.
      IF ( SCROPN ) CALL FIO_CLOSE( SCRFD, STATUS )
      SCROPN = .FALSE.

*  And execute the command procedure if asked.
      CALL MSG_SETC( 'SCRIPT', ORIG( 1 ) )
      IF ( EXECUT .AND. STATUS .EQ. SAI__OK ) THEN
         CALL  CCD1_MSG( ' ',
     :'  Executing commands in file: ^SCRIPT', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL PAR_GET0C( 'EXELOGFILE', ORIG(2), STATUS )
         CALL CCD1_DOCMD( STYPE, ORIG( 1 ), ORIG( 2 ), STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL MSG_SETC( 'LOGFILE', ORIG( 2 ) )
         CALL CCD1_MSG( ' ',
     :'  Output will be written to file: ^LOGFILE ', STATUS )
      ELSE

*  Tell user name of script.
         CALL CCD1_MSG( ' ',
     :'  CCDPACK commands written to file: ^SCRIPT', STATUS )
      END IF
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Exit with error label.
 99   CONTINUE

*  Close the script file.
      IF ( SCROPN ) CALL FIO_CLOSE( SCRFD, STATUS )

*  Release group resources.
      CALL CCD1_GRDEL( GIDIN, STATUS )

*  Close NDF.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SCHEDULE_ERR',
     :   'SCHEDULE: Error scheduling CCDPACK automated reduction.',
     :   STATUS )
      END IF

*  Close CCDPACK.
      CALL CCD1_END( STATUS )

      END
* $Id$
