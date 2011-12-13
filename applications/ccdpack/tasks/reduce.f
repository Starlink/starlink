      SUBROUTINE REDUCE( PID, STATUS )
*+
*  Name:
*     REDUCE

*  Purpose:
*     Automatic CCD data reduction facility (command-line version).

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL REDUCE( STATUS )

*  Arguments:
*     PID = CHARACTER * ( * ) (Given)
*        A string to append to any task names used in this routine.
*        (this should be setup to allow attachment to existing monoliths).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine provides a command-line interface to the automated
*     reduction facilities of CCDPACK.
*
*     It guides you though the selection of the appropriate route for
*     performing a reduction. Possible routes are using an import
*     control table to interpret FITS headers, choosing from a list of
*     known detector setups or just supplying all the necessary
*     information.
*
*     Using FITS headers is only possible if your data contains the
*     correct information. If a table is not listed for your
*     telescope/detector combination then you will need to create one.
*     The contents of import tables are described in the help for the
*     program IMPORT. Unless you (and perhaps your colleagues) are
*     going to reduce large amounts of data from an unknown telescope
*     then you should use the normal setup and data organization
*     techniques.
*
*     If you do not choose a detector setup file or have none you will
*     need to organize your data into different frame types (bias, flat,
*     target etc.), so either use a naming scheme that allows you to
*     distinguish between them using wildcard patterns or create lists
*     of the names in files.
*
*     If you cannot select from any of the known detectors then the
*     most crucial information that you require is a knowledge of
*     where the bias strips are and the useful CCD area (if these are
*     appropriate for the type of data you're reducing). If you are
*     sitting at an X display then the CCD geometry can be determined
*     from within reduce. Otherwise you will need to determine these
*     before running reduce.

*  Usage:
*     REDUCE

*  ADAM Parameters:
*     CHOICE = _CHAR (Read)
*        The operation that you want to use on the selected file.
*        Either "view", "select" or "continue". Continue means
*        you do not want to selected a file.
*        ["view"]
*     CLEAR = _LOGICAL (Read)
*        Whether or not to clear any existing CCD global parameters.
*        [TRUE]
*     INDEX = INTEGER (Read)
*        The index of the known detector file that you want to view
*        or select.
*     KNOWN = _LOGICAL (Read)
*        Whether or not you want to select or view the list of known
*        detectors.
*        [FALSE]
*     MANUAL = _LOGICAL (Read)
*        Whether the types of the input data will be assigned manually
*        (by running PRESENT) or not.
*        [TRUE]
*     RESTORE = _LOGICAL (Read)
*        Whether or not a "restoration" file will be used to reset
*        the CCDPACK global parameters, or not.
*        [FALSE]
*     RESTOREFILE = _CHAR (Read)
*        The name of an existing "restoration" file. This musy have been
*        created by a previous run of the CCDSETUP command.
*     SETGEOM = _LOGICAL (Read)
*        Whether or not a graphical method will be used to determine
*        the geometries of the CCD (i.e. the position of the useful
*        region of the CCD and the bias strips).
*        [TRUE]
*     TABLE = _CHAR (Read)
*        The name of an import control table (see the IMPORT command for
*        details about this) to be used to import information from the
*        FITS extension of an NDF.

*  Notes:
*     Unknown detectors.
*        If you do develop an import table or restoration (setup) file
*        for a telescope/detector pass these on to the maintainer of
*        this package, together with a description. They will be
*        distributed in future releases for the benefit of others.

*  Copyright:
*     Copyright (C) 1997, 2001 Central Laboratory of the Research
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JUN-1997 (PDRAPER):
*        Original version.
*     26-MAR-2001 (MBT):
*        Added references to USESET parameter.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MSG_PAR'         ! Message system constants
      INCLUDE 'PSX_ERR'         ! Posix library
      INCLUDE 'PRM_PAR'         ! Primitive data constants

*  Arguments Given:
      CHARACTER * ( * ) PID     ! Parent process ID

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
C      EXTERNAL SLV_LOADW        ! Load an ADAM task
C      INTEGER SLV_LOADW
      EXTERNAL PRESENT          ! Avoid confusion with PRESENT intrinsic

*  Local Constants:
      INTEGER TIMOUT
      PARAMETER ( TIMOUT = 30 ) ! Timeout when loading tasks

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of string

*  Local Variables:
      CHARACTER * ( 1024 ) LINE ! Output message line (long)
      CHARACTER * ( 132 ) CMD   ! Command string
      CHARACTER * ( 2 ) DIRECT  ! Readout direction
      CHARACTER * ( 30 ) CCDRES ! Message system name for ccdpack_res
      CHARACTER * ( 5 * VAL__SZI ) BOUNDS ! Bounds of CCD bias strips(x,x,x,x form)
      CHARACTER * ( 5 * VAL__SZI ) EXTENT ! Extent of CCD (x,y,x,y form)
      CHARACTER * ( MSG__SZMSG ) FILNAM ! Name of restoration file/table
      INTEGER CCDRID            ! Id for CCDPACK_RES monolith
      INTEGER FD                ! File identifier
      LOGICAL CLEAR             ! Clear existing global parameters
      LOGICAL HAVRES            ! Have a restoration file
      LOGICAL HAVTAB            ! Have an import table
      LOGICAL MANUAL            ! File types are defined manually
      LOGICAL OK                ! OK to loop
      LOGICAL RESTOR            ! Want to use a restoration file
*.

*==============================================================================
*  NOTE SLV ROUTINES NOT USED, parameters control not good enought yet.
*  Cannot mix and match direct calls as parameter system is not updated.
*  All SLV stuff is commented out and should be used when SLV is developed.
*==============================================================================

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No tasks have been attached.
      CCDRID = 0

*  Startup any monoliths that we need to use:
*  - CCDPACK_RES
C      CALL PSX_GETENV( 'CCDPACK_DIR', CMD, STATUS )
C      CMD = CMD( :CHR_LEN( CMD ) )//'/ccdpack_res'
C      CCDRES = 'ccdpack_res'//PID
C      CCDRID = SLV_LOADW( CCDRES, CMD, .FALSE., TIMOUT, STATUS )
C      IF ( STATUS .NE. SAI__OK ) THEN
C         STATUS = SAI__ERROR
C         CALL ERR_REP( 'FAILED',
C     :   'Sorry cannot proceed. Failed to load monolith '//
C     :   '$CCDPACK_DIR/ccdpack_res', STATUS )
C         GO TO 99
C      END IF

*  Start the application and introduce ourselves.
      CALL CCD1_START( 'REDUCE', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ',
     :     '  An automated reduction aid.', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Write the introduction.
      LINE =
     :'Before you can perform an "automated" reduction you '//
     :'need to identify and type your data. It may also be necessary '//
     :'to supply some information about the CCD "characteristics".'
      CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
      CALL MSG_BLANK( STATUS )
      LINE =
     :'For some CCD/telescope combinations all this information '//
     :'(including the type of data) can be determined and all you '//
     :'may need to do is identify the detector/telescope combination '//
     :'you have used and the names of your data files. For others '//
     :'some information about the CCD characteristics has been '//
     :'determined. In this case all you need to do is organize '//
     :'your data into the different "frame types".'
      CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
      CALL MSG_BLANK( STATUS )
      LINE =
     :'If no information is available about your CCD then you will '//
     :'need to determine how the data is to be processed (in '//
     :'particular how the debiassing will be done) and you will '//
     :'then need to determine certain geometric properties such as '//
     :'positions of the bias strips, what the useful area of the CCD '//
     :'is etc. If you are unsure about these then consult the '//
     :'documentation supplied by the observatories, talk to local '//
     :'experts and, as a last resort, read SUN/139.'
      CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
      CALL MSG_BLANK( STATUS )

*  Offer to display known detectors.
      CALL PAR_GET0L( 'KNOWN', OK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  And select one if appropriate.
      HAVTAB = .FALSE.
      HAVRES = .FALSE.
      IF ( OK ) CALL CCD1_SDECT( FILNAM, HAVRES, HAVTAB, STATUS )

*  If HAVRES and HAVTAB are .FALSE. after this section then no choice has been
*  made about how to proceed. So we must ask.
      MANUAL = .FALSE.
      IF ( .NOT. HAVRES .AND. .NOT. HAVTAB ) THEN
         CALL MSG_BLANK( STATUS )
         LINE =
     :'Since you have chosen not to, or have been unable to, select '//
     :'from the list of known detectors, you now need to proceed '//
     :'either by organizing your data into their different frame '//
     :'types "by hand" or by supplying an import control table.'
         CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
         CALL MSG_BLANK( STATUS )
         LINE =
     :'Import control tables tell how the information already in '//
     :'your data (supplied as FITS values by the observatories) can '//
     :'be used to determine the frame types and the CCD '//
     :'characteristics. More information about these can be found in '//
     :'SUN/139. It is unlikely that one of these is available unless '//
     :'you have created it yourself or have obtained one from a '//
     :'colleague.'
         CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
         CALL MSG_BLANK( STATUS )

*  Offer the choice to use manual selection of the frame types.
         CALL PAR_GET0L( 'MANUAL', MANUAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         IF ( MANUAL ) THEN

*  No FITS information so need to run CCDSETUP. Offer the chance
*  to use a restoration file.
            CALL MSG_BLANK( STATUS )
            LINE =
     :'Do you want to select a file for restoring the setup from '//
     :'a previous reduction?'
            CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
            CALL MSG_BLANK( STATUS )
            LINE =
     :'Restoration files are created by the CCDSETUP program. These '//
     :'contain a description of the state of this program and can be '//
     :'used to return it to this state. CCDSETUP sets the CCD '//
     :'characteristics (where the bias strips are, the useful CCD '//
     :'area etc.) and some more general preferences (such as whether '//
     :'to generate data errors).'
            CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
            CALL MSG_BLANK( STATUS )
            LINE =
     :'It is unlikely that you have a restoration file unless you '//
     :'have created one by a previous run of CCDSETUP (this can be '//
     :'done from this procedure), or have been given one.'
            CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
            CALL MSG_BLANK( STATUS )
            CALL PAR_GET0L( 'RESTORE', RESTOR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            IF ( RESTOR .AND. STATUS .EQ. SAI__OK ) THEN
               CALL CCD1_ASFIO( 'RESTOREFILE', 'READ', 'LIST', 0, FD,
     :                          HAVRES, STATUS )
               IF ( .NOT. HAVRES ) THEN
                  IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
                  CALL ERR_REP( 'NOREST',
     :                        '   No restoration file selected',
     :                        STATUS )
                  GO TO 99
               END IF
               CALL FIO_FNAME( FD, FILNAM, STATUS )
               CALL FIO_CLOSE( FD, STATUS )
            END IF
         ELSE

*  We want to use an import table.
            CALL CCD1_ASFIO( 'IMPORTTABLE', 'READ', 'LIST', 0, FD,
     :                       HAVTAB, STATUS )
            IF ( .NOT. HAVTAB ) THEN
               IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
               CALL ERR_REP( 'NOTAB', '   No table selected', STATUS )
               GO TO 99
            END IF
            CALL FIO_FNAME( FD, FILNAM, STATUS )
            CALL FIO_CLOSE( FD, STATUS )
         END IF
      END IF

*  Ok now perform the required setup.
      IF ( HAVTAB ) THEN
         CALL MSG_BLANK( STATUS )
         LINE =
     :'Clearing any existing irrelevant CCDPACK global parameters.'
         CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
         CALL MSG_BLANK( STATUS )
         CALL CCD1_SETPA( 'BYNAME', 'TRUE', STATUS )
         CALL CCD1_SETPA( 'NAMES',
     :        'extent,direction,bounds,adc,rnoise,deferred,saturation',
     :        STATUS )
         CALL CCDCLEAR( STATUS )
C         CMD = 'byname=true names=''extent,direction,bounds,adc,'//
C     :         'rnoise,deferred,saturation'''
C         CALL SLV_OBEYW( CCDRES, 'ccdclear', CMD, ' ', STATUS )
         LINE =
     :'Configure the general CCDPACK options.'
         CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
         CALL MSG_BLANK( STATUS )
         LINE =
     :'If you do not have any preferences then accept the defaults. '//
     :'If you do not have a data mask then make sure that you '//
     :'respond with an "!" (this is the null parameter).'
         CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
         CALL MSG_BLANK( STATUS )
         LINE =
     :'If you do not understand the significance of a parameter then '//
     :'use a "?" to get help.'
         CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
         CALL MSG_BLANK( STATUS )

*  Prepare to run CCDSETUP.
         CALL CCD1_SETPA( 'EXTENT', '!', STATUS )
         CALL CCD1_SETPA( 'DIRECTION', '!', STATUS )
         CALL CCD1_SETPA( 'BOUNDS', '!', STATUS )
         CALL CCD1_SETPA( 'ADC', '!', STATUS )
         CALL CCD1_SETPA( 'RNOISE', '!', STATUS )
         CALL CCD1_SETPA( 'DEFERRED', '!', STATUS )
         CALL CCD1_SETPA( 'SATURATION', '!', STATUS )
         CALL CCD1_SETPA( 'NDFNAMES', '!', STATUS )
         CALL CCD1_SETPA( 'USESET', '!', STATUS )
         CALL CCD1_SETPA( 'RESTORE', 'FALSE', STATUS )
         CALL CCD1_SETPA( 'SAVE', 'FALSE', STATUS )
         CALL CCDSETUP( STATUS )
C         CMD = ' extent=! direction=! bounds=! adc=! rnoise=! '//
C     : 'deferred=! saturation=! ndfnames=! restore=false save=false'
C         CALL SLV_OBEYW( CCDRES, 'ccdsetup', CMD,
C     :'MASK<MASK,PRESERVE<PRESERVE,GENVAR<GENVAR,NDFNAMES<NDFNAMES,'//
C     :'USESET<USESET,SETSAT<SETSAT', STATUS )
      ELSE

*  No table available so need to use a setup file or get all values from
*  user.
         IF ( HAVRES ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_SETC( 'FILE', FILNAM )
            LINE =
     :'Restoring the values of the CCDPACK global parameters using '//
     :'file ^FILE'
            CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
            CALL MSG_BLANK( STATUS )

*  Run CCDSETUP directly (no parameter exchange for none strings yet).
*  Note RESTORE and RESTOREFILE are shared and will be set already!
            CALL CCD1_SETPA( 'NDFNAMES', '!', STATUS )
            CALL CCD1_SETPA( 'USESET', '!', STATUS )
            CALL CCDSETUP( STATUS )
C            CMD =
C     :'restore=true ndfnames=! reset accept restorefile='//FILNAM
C            CALL SLV_OBEYW( CCDRES, 'ccdsetup', CMD,
C     :'ADC<ADC,BOUNDS<BOUNDS,RNOISE<RNOISE,MASK<MASK,'//
C     :'DIRECTION<DIRECTION,DEFERRED<DEFERRED,EXTENT<EXTENT,'//
C     :'PRESERVE<PRESERVE,GENVAR<GENVAR,NDFNAMES<NDFNAMES,'//
C     :'SATURATE<SATURATE, SATURATION<SATURATION,SETSAT<SETSAT', STATUS )
         ELSE
            LINE =
     :'Do you wish to clear any existing global parameters (these '//
     :'might be set from a previous reduction and may well be '//
     :'irrelevant). If you are not sure then clear them.'
            CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            IF ( CLEAR ) THEN
               CALL MSG_BLANK( STATUS )
               LINE =
     :'Clearing any existing CCDPACK global parameters.'
               CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
               CALL MSG_BLANK( STATUS )
            END IF
            CALL CCD1_SETPA( 'BYNAME', 'FALSE', STATUS )
            CALL CCDCLEAR( STATUS )
C            CMD = 'byname=false accept'
C            CALL SLV_OBEYW( CCDRES, 'ccdclear', CMD, ' ', STATUS )

*  Offer advice about this section and the chance to determine the CCD
*  geometries.
            CALL MSG_BLANK( STATUS )
            LINE =
     :'It is vital that you set all parameters necessary for the '//
     :'type of reduction that is required. For instance if you '//
     :'intend to debias your frames using specifically taken bias '//
     :'frames and you want to fine tune the offset using the bias '//
     :'strips then you must set the BOUNDS and DIRECTION parameters. '//
     :'You must also set these if you want to debias by '//
     :'interpolation (or extrapolation) of the bias strips.  If you '//
     :'have bias strips then you should set the EXTENT parameter to '//
     :'exclude these from the final data.'
            CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
            CALL MSG_BLANK( STATUS )
            LINE =
     :'If you want to generate errors for your data then you must '//
     :'set the parameters ADC, RNOISE and GENVAR.'
            CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
            CALL MSG_BLANK( STATUS )

*  Offer a change to determine the CCD geometries now if an X display
*  is available.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            CALL PSX_GETENV( 'DISPLAY', CMD, STATUS )
            IF ( STATUS .EQ. PSX__NOENV ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE
               CALL MSG_BLANK( STATUS )
               LINE =
     :'Since you have an X display available (DISPLAY variable set) '//
     :'you can determine any of the required CCD geometries '//
     :'interactively. To do this you must first choose a suitable '//
     :'image for display.'
               CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
               CALL MSG_BLANK( STATUS )
               CALL PAR_GET0L( 'SETGEOM', OK, STATUS )
               EXTENT = ' '
               DIRECT = ' '
               BOUNDS = ' '
               IF ( OK ) CALL CCD1_GGEOM(EXTENT, DIRECT, BOUNDS, STATUS)
            END IF
            CALL MSG_BLANK( STATUS )
            LINE =
     :'Now set the CCDPACK global parameters.'
            CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
            CALL MSG_BLANK( STATUS )
            LINE =
     :'If you are unsure about a parameter accept the default '//
     :'or use a "?" to get help.'
            CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
            CALL MSG_BLANK( STATUS )

*  Run CCDSETUP directly (no parameter exchange for none strings yet).
*  Note RESTORE and RESTOREFILE are shared and will be set already!
            CALL CCD1_SETPA( 'NDFNAMES', '!', STATUS )
            CALL CCD1_SETPA( 'USESET', '!', STATUS )
            CALL CCD1_SETPA( 'RESTORE', 'FALSE', STATUS )
            IF ( EXTENT .NE. ' ' ) THEN
               CALL CCD1_SETPA( 'EXTENT', EXTENT, STATUS )
            END IF
            IF ( DIRECT .NE. ' ' ) THEN
               CALL CCD1_SETPA( 'DIRECTION', DIRECT, STATUS )
            END IF
            IF ( BOUNDS .NE. ' ' ) THEN
               CALL CCD1_SETPA( 'BOUNDS', BOUNDS, STATUS )
            END IF
            CALL CCDSETUP( STATUS )
C            CMD = 'ndfnames=! restore=false reset '
C            IF ( EXTENT .NE. ' ' ) THEN
C               CMD = CMD (: CHR_LEN( CMD ) ) // 'extent='//EXTENT
C            END IF
C            IF ( DIRECT .NE. ' ' ) THEN
C               CMD = CMD (: CHR_LEN( CMD ) ) // 'direction='//DIRECT
C            END IF
C            IF ( BOUNDS .NE. ' ' ) THEN
C               CMD = CMD (: CHR_LEN( CMD ) ) // 'bounds='//BOUNDS
C            END IF
C            CALL SLV_OBEYW( CCDRES, 'ccdsetup', CMD,
C     :'ADC<ADC,RNOISE<RNOISE,MASK<MASK,DEFERRED<DEFERRED,'//
C     :'PRESERVE<PRESERVE,GENVAR<GENVAR,SATURATE<SATURATE,'//
C     :'SATURATION<SATURATION,SETSAT<SETSAT', STATUS )
         END IF
      END IF

*  Import the NDFs.
      IF ( HAVTAB ) THEN

*  All NDFs given in response to one prompt. FITS stuff from headers.
         CALL MSG_BLANK( STATUS )
         LINE =
     :'Give the names of all the NDFs to be processed.'
         CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
         CALL MSG_BLANK( STATUS )
         CALL CCD1_SETPA( 'NAMELIST', 'REDUCE.NDFS', STATUS )
         CALL CCD1_SETPA( 'TABLE', FILNAM, STATUS )
         CALL IMPORT( STATUS )
C         CMD = 'namelist=REDUCE.NDFS reset table='//FILNAM
C         CALL SLV_OBEYW( CCDRES, 'import', CMD, 'IN<IN', STATUS )
      ELSE
         CALL MSG_BLANK( STATUS )
         LINE =
     :'Organization of data frames.'
         CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
         CALL MSG_BLANK( STATUS )
         LINE =
     :'Your data frames now need to be identified with the correct '//
     :'frame types. This stage also records the information that '//
     :'you gave previously (about the CCD characteristics) in the '//
     :'data. If your data have different filter types then you will '//
     :'also need to indicate this here (if they all have the same '//
     :'filter then just give NONE as the filter type). It may well '//
     :'be worth inspecting the output to ensure that the correct '//
     :'information is being stored with your data.'
         CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
         CALL MSG_BLANK( STATUS )

         CALL MSG_OUT( ' ',
     :'   Some of the frame types that are used are:', STATUS )

         CALL MSG_OUT( ' ',
     :'     o target -- these are the astronomy data', STATUS )
         CALL MSG_OUT( ' ',
     :'     o flat   -- flatfield frames', STATUS )
         CALL MSG_OUT( ' ',
     :'     o bias   -- CCD bias frames.', STATUS )
         CALL CCD1_SETPA( 'SIMPLE', 'FALSE', STATUS )
         CALL CCD1_SETPA( 'MODIFY', 'TRUE', STATUS )
         CALL CCD1_SETPA( 'NAMELIST', 'REDUCE.NDFS', STATUS )
         CALL PRESENT( STATUS )
C         CMD = 'simple=false modify=true namelist=REDUCE.NDFS reset'
C         CALL SLV_OBEYW( CCDRES, 'present', CMD,
C     :        'BIAS<BIAS,TARGET<TARGET,FLAT<FLAT,DARK<DARK,FLASH<FLASH',
C     :                   STATUS )
      END IF

*  And finally perform the reduction.
      CALL MSG_BLANK( STATUS )
      LINE =
     :'Schedule a reduction.'
      CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
      CALL MSG_BLANK( STATUS )
      LINE =
     :'Your data frames will now be inspected to see how they should '//
     :'be reduced. You will also be given the opportunity to specify '//
     :'the sort of debiassing that you want to do (from a list of '//
     :'possible options given the data available). Whether you want '//
     :'to reduce the amount of disk space used by removing any '//
     :'intermediary files (you can also remove the original data '//
     :'using the options "LOTS", but you shouldn''t use this unless '//
     :'absolutely sure).'
      CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
      CALL MSG_BLANK( STATUS )
      LINE =
     :'Finally a reduction will be performed by a background process.'
      CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
      CALL MSG_BLANK( STATUS )
      CALL CCD1_SETPA( 'IN', '^REDUCE.NDFS', STATUS )
      CALL CCD1_SETPA( 'EXECUTE', 'TRUE', STATUS )
      CALL CCD1_SETPA( 'SCRIPT', 'reduce.csh', STATUS )
      CALL CCD1_SETPA( 'STYPE', 'csh', STATUS )
      CALL CCD1_SETPA( 'EXELOGFILE', 'reduce.log', STATUS )
      CALL CCD1_SETPA( 'MASTERBIAS', 'MASTER_BIAS', STATUS )
      CALL CCD1_SETPA( 'MASTERDARK', 'MASTER_DARK', STATUS )
      CALL CCD1_SETPA( 'MASTERFLASH', 'MASTER_FLASH', STATUS )
      CALL CCD1_SETPA( 'MASTERFLAT', 'MASTER_FLAT', STATUS )
      CALL SCHEDULE( STATUS )
C      CMD = 'in=''^REDUCE.NDFS'' execute=true script=reduce.csh '//
C     :      'stype=csh exelogfile=reduce.log'
C      CALL SLV_OBEYW( CCDRES, 'schedule', CMD,
C     :     'DEBIAS<DEBIAS,INTERP<INTERP,SAVESPACE<SAVESPACE', STATUS )

*  Exit in error label. Kill monolith if allowed.
 99   CONTINUE
C      CALL SLV_KILLW( CCDRID, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'REDUCE_ERR','REDUCE: error reducing CCD data.',
     :     STATUS )
      END IF

*  Close logging system.
      CALL CCD1_END( STATUS )

      END
