      SUBROUTINE CCDSHOW( STATUS )
*+
*  Name:
*     CCDSHOW

*  Purpose:
*     Displays the value of the CCDPACK global parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CCDSHOW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine shows the current value of any CCDPACK global
*     parameters.  It can also be used to save the current setup
*     to a file for restoration by CCDSETUP.

*  Usage:
*     ccdshow

*  ADAM Parameters:
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter, then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP,
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP,
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     SAVE = _LOGICAL (Read)
*        Whether or not to save the values of the program parameters
*        to a "restoration" file, which can later be used by CCDSETUP
*        to restore the current values of the global parameters.
*        If TRUE then you'll need to specify the name of the file using
*        the SAVEFILE parameter.
*        [FALSE]
*     SAVEFILE = FILENAME (Read)
*        This parameter is only used if the SAVE parameters is TRUE.
*        It allows you to give the name of the restoration file to be used
*        when saving the program parameters.
*        [CCDPACK_SETUP.DAT]
*     USESET = _LOGICAL (Read)
*        This parameter determines whether values keyed by Set Index
*        are to be displayed.  If CCDSETUP has been used to set up
*        different global parameter values for different members of
*        each Set, and this parameter is true, CCDSHOW will display
*        the parameter values specific to each Set Index value
*        as well as the current unkeyed value.
*        [FALSE]

*  Examples:
*     ccdshow
*        This displays the current values of all the CCDPACK global
*        parameters to the screen.
*     ccdshow save savefile=params.save
*        As well as displaying the global parameter values to the
*        screen, this will also write them to a restoration file
*        called 'params.save'.  This file can be used at a later date
*        to restore the current global parameter setup using CCDSETUP.

*  Behaviour of Parameters:
*     The parameters LOGTO, LOGFILE and USESET have global values. These
*     global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     using the CCDSETUP command.

*  Copyright:
*     Copyright (C) 1991-1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-AUG-1991 (PDRAPER):
*        Original version.
*     7-APR-1992 (PDRAPER):
*        Changed to use parameter system to get values rather than
*        directly accessing the global file.
*     20-JUL-1993 (PDRAPER):
*        Added LOG, LOGFILE and NDFNAMES parameters.
*     28-JAN-1994 (PDRAPER):
*        Added saturation extensions.
*     23-MAR-2001 (MBT):
*        Changed access method for calibration NDFs to go via SUBPAR -
*        previous method did not work with NDG.
*     10-MAY-2001 (MBT):
*        Modified to access Set Index-keyed parameter values.  The code
*        is substantially rewritten.  In particular access to global
*        variables is now not done via the parameter system at all,
*        but using direct inspection of the GLOBAL ADAM parameter file
*        (via the CCD1_KPGT routine).  SAVE and SAVEFILE parameters
*        are also added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT constants
      INCLUDE 'PAR_ERR'          ! Parameter system constants
      INCLUDE 'MSG_PAR'          ! Message system constants
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'CCD1_PAR'         ! CCDPACK private constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string

*  Local Variables:
      CHARACTER * ( 8 ) LOGTO    ! Where information is logged to
      CHARACTER BIAS * ( FIO__SZFNM ) ! Name of master bias file.
      CHARACTER CAL * ( FIO__SZFNM ) ! Name of master cal file.
      CHARACTER FLAT * ( FIO__SZFNM ) ! Name of master flat file.
      CHARACTER KLOCS( CCD1__MXSI ) * ( DAT__SZLOC ) ! Locators for keyed vals
      CHARACTER KSTRS( CCD1__MXSI ) * ( 80 ) ! String representing keyed vals
      CHARACTER MSKNAM * ( FIO__SZFNM ) ! Name of mask file.
      CHARACTER RFILE * ( FIO__SZFNM ) ! Name of restoration file
      CHARACTER ULOC * ( DAT__SZLOC ) ! Locator for unkeyed parameter value
      CHARACTER USTR * ( 80 )    ! String representation of unkeyed parameter
      DOUBLE PRECISION ADC       ! ADC factor
      DOUBLE PRECISION DEFER     ! Deferred charge value
      DOUBLE PRECISION RNOISE    ! Readout noise
      DOUBLE PRECISION SATVAL    ! Saturation value
      INTEGER BOUNDS( 4 )        ! Bias strip bounds
      INTEGER EXTENT( 4 )        ! Useful CCD area
      INTEGER FDS                ! Restoration file descriptor
      INTEGER I                  ! Dummy
      INTEGER IAT                ! Position in string
      INTEGER KEYS( CCD1__MXSI ) ! Set Index keys for keyed parameters
      INTEGER LENG               ! String length
      INTEGER MKEY               ! Maximum number of keyed values to be used
      INTEGER NBOUND             ! Number of bounds
      INTEGER NK                 ! Number of keys found
      LOGICAL GENVAR             ! Whether variances are generated
      LOGICAL GOTADC             ! Flags showing which values have been
      LOGICAL GOTBDS             ! obtained
      LOGICAL GOTBIA             ! " "
      LOGICAL GOTCAL             ! " "
      LOGICAL GOTDEF             ! " "
      LOGICAL GOTDIR             ! " "
      LOGICAL GOTEXT             ! " "
      LOGICAL GOTFLA             ! " "
      LOGICAL GOTGEN             ! " "
      LOGICAL GOTLG2             ! " "
      LOGICAL GOTLGN             ! " "
      LOGICAL GOTMSK             ! " "
      LOGICAL GOTNAM             ! " "
      LOGICAL GOTNOI             ! " "
      LOGICAL GOTONE             ! True when have at least one value
      LOGICAL GOTPRE             ! " "
      LOGICAL GOTSAT             ! " "
      LOGICAL GOTSET             ! " "
      LOGICAL GOTSPR             ! " "
      LOGICAL GOTSVL             ! " "
      LOGICAL NDFS               ! INLIST prompts are NDFs
      LOGICAL PRESER             ! Whether to preserve data types.
      LOGICAL QUNKEY             ! Do we have an unkeyed version?
      LOGICAL SAVE               ! Save restoration file?
      LOGICAL SATUR              ! Look for saturated pixels
      LOGICAL SETSAT             ! Set saturated pixels to saturation value
      LOGICAL SOPEN              ! Is restoration file open?
      LOGICAL THERE              ! Is HDS component present?
      LOGICAL USESET             ! Whether to use available Set header info
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No files are open.
      SOPEN = .FALSE.

*  Startup CCDPACK logging system.
      CALL CCD1_START( 'CCDSHOW', STATUS )

*  Make sure that the LOGINT is greater than 0 (no output -
*  bit pointless)
      CALL CCD1_SILEV( 1, 3, I, STATUS )

*  Set GOTONE.
      GOTONE = .FALSE.

*  See if we are using Sets.
      CALL PAR_GET0L( 'USESET', USESET, STATUS )
      IF ( USESET ) THEN
         MKEY = CCD1__MXSI
      ELSE
         MKEY = 0
      END IF

*  Write a banner.
      CALL CCD1_MSG( ' ', '  Listing of the current '//
     :              'CCDPACK global parameters:',
     :              STATUS )
      IF ( USESET ) THEN
         CALL CCD1_MSG( ' ',
     :'  Set Index-keyed values will be shown where available', STATUS )
      END IF
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Access the values through GLOBAL associations one-by-one

*  ADC conversion factor.
      CALL CCD1_KPGT( 'ADC', MKEY, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTADC = QUNKEY .OR. NK .GT. 0
      IF ( GOTADC ) THEN
         GOTONE = .TRUE.
         IF ( QUNKEY ) THEN
            CALL DAT_GET0D( ULOC, ADC, STATUS )
            CALL DAT_ANNUL( ULOC, STATUS )
            CALL CHR_RTOC( REAL( ADC ), USTR, LENG )
         END IF
         IF ( NK .GT. 0 ) THEN
            DO I = 1, NK
               CALL DAT_GET0D( KLOCS( I ), ADC, STATUS )
               CALL DAT_ANNUL( KLOCS( I ), STATUS )
               CALL CHR_RTOC( REAL( ADC ), KSTRS( I ), LENG )
            END DO
         END IF
         CALL CCD1_KPOP( 'Global ADC factor',
     :                   QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
      END IF

*  Extent of useful CCD area.
      CALL CCD1_KPGT( 'EXTENT', MKEY, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTEXT = QUNKEY .OR. NK .GT. 0
      IF ( GOTEXT ) THEN
         GOTONE = .TRUE.
         IF ( QUNKEY ) THEN
            CALL DAT_GET1I( ULOC, 4, EXTENT, NBOUND, STATUS )
            CALL DAT_ANNUL( ULOC, STATUS )
            CALL MSG_SETI( 'B1', EXTENT( 1 ) )
            CALL MSG_SETI( 'B2', EXTENT( 2 ) )
            CALL MSG_SETI( 'B3', EXTENT( 3 ) )
            CALL MSG_SETI( 'B4', EXTENT( 4 ) )
            CALL MSG_LOAD( ' ', '(^B1:^B2,^B3:^B4)', USTR, LENG,
     :                     STATUS )
         END IF
         IF ( NK .GT. 0 ) THEN
            DO I = 1, NK
               CALL DAT_GET1I( KLOCS( I ), 4, EXTENT, NBOUND, STATUS )
               CALL DAT_ANNUL( KLOCS( I ), STATUS )
               CALL MSG_SETI( 'B1', EXTENT( 1 ) )
               CALL MSG_SETI( 'B2', EXTENT( 2 ) )
               CALL MSG_SETI( 'B3', EXTENT( 3 ) )
               CALL MSG_SETI( 'B4', EXTENT( 4 ) )
               CALL MSG_LOAD( ' ', '(^B1:^B2,^B3:^B4)', KSTRS( I ),
     :                        LENG, STATUS )
            END DO
         END IF
         CALL CCD1_KPOP( 'Global output NDF extent '//
     :                   '(xmin:xmax,ymin:ymax)',
     :                   QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
      END IF

*  Readout noise estimate (may be none).
      CALL CCD1_KPGT( 'RNOISE', MKEY, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTNOI = QUNKEY .OR. NK .GT. 0
      IF ( GOTNOI ) THEN
         GOTONE = .TRUE.
         IF ( QUNKEY ) THEN
            CALL DAT_GET0D( ULOC, RNOISE, STATUS )
            CALL DAT_ANNUL( ULOC, STATUS )
            CALL CHR_RTOC( REAL( RNOISE ), USTR, LENG )
         END IF
         IF ( NK .GT. 0 ) THEN
            DO I = 1, NK
               CALL DAT_GET0D( KLOCS( I ), RNOISE, STATUS )
               CALL DAT_ANNUL( KLOCS( I ), STATUS )
               CALL CHR_RTOC( REAL( RNOISE ), KSTRS( I ), LENG )
               CALL MSG_SETR( 'RNOISE', REAL( RNOISE ) )
            END DO
         END IF
         CALL CCD1_KPOP( 'Global readout noise (ADUs)',
     :                   QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
      END IF

*  Bounds of the bias strips. May default to none.
      CALL CCD1_KPGT( 'BOUNDS', MKEY, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTBDS = QUNKEY .OR. NK .GT. 0
      IF ( GOTBDS ) THEN
         GOTONE = .TRUE.
         IF ( QUNKEY ) THEN
            CALL DAT_GET1I( ULOC, 4, BOUNDS, NBOUND, STATUS )
            CALL DAT_ANNUL( ULOC, STATUS )
            USTR = '('
            IAT = 1
            CALL CHR_PUTI( BOUNDS( 1 ), USTR, IAT )
            IF ( NBOUND .GE. 2 ) THEN
               CALL CHR_PUTC( ':', USTR, IAT )
               CALL CHR_PUTI( BOUNDS( 2 ), USTR, IAT )
               IF ( NBOUND .GE. 3 ) THEN
                  CALL CHR_PUTC( ',', USTR, IAT )
                  CALL CHR_PUTI( BOUNDS( 3 ), USTR, IAT )
                  IF ( NBOUND .GE. 4 ) THEN
                     CALL CHR_PUTC( ':', USTR, IAT )
                     CALL CHR_PUTI( BOUNDS( 4 ), USTR, IAT )
                  END IF
               END IF
            END IF
            CALL CHR_PUTC( ')', USTR, IAT )
         END IF
         IF ( NK .GT. 0 ) THEN
            DO I = 1, NK
               CALL DAT_GET1I( KLOCS( I ), 4, BOUNDS, NBOUND, STATUS )
               CALL DAT_ANNUL( KLOCS( I ), STATUS )
               KSTRS( I ) = '('
               IAT = 1
               CALL CHR_PUTI( BOUNDS( 1 ), KSTRS( I ), IAT )
               IF ( NBOUND .GE. 2 ) THEN
                  CALL CHR_PUTC( ':', KSTRS( I ), IAT )
                  CALL CHR_PUTI( BOUNDS( 2 ), KSTRS( I ), IAT )
                  IF ( NBOUND .GE. 3 ) THEN
                     CALL CHR_PUTC( ',', KSTRS( I ), IAT )
                     CALL CHR_PUTI( BOUNDS( 3 ), KSTRS( I ), IAT )
                     IF ( NBOUND .GE. 4 ) THEN
                        CALL CHR_PUTC( ':', KSTRS( I ), IAT )
                        CALL CHR_PUTI( BOUNDS( 4 ), KSTRS( I ), IAT )
                     END IF
                  END IF
               END IF
               CALL CHR_PUTC( ')', KSTRS( I ), IAT )
            END DO
         END IF
         CALL CCD1_KPOP( 'Global bias strip bounds',
     :                   QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
      END IF

*  The readout direction.
      CALL CCD1_KPGT( 'DIRECTION', MKEY, QUNKEY, ULOC, NK, KEYS,
     :                KLOCS, STATUS )
      GOTDIR = QUNKEY .OR. NK .GT. 0
      IF ( GOTDIR ) THEN
         GOTONE = .TRUE.
         IF ( QUNKEY ) THEN
            CALL DAT_GET0C( ULOC, USTR, STATUS )
            CALL DAT_ANNUL( ULOC, STATUS )
            CALL CHR_UCASE( USTR )
         END IF
         IF ( NK .GT. 0 ) THEN
            DO I = 1, NK
               CALL DAT_GET0C( KLOCS( I ), KSTRS( I ), STATUS )
               CALL DAT_ANNUL( KLOCS( I ), STATUS )
               CALL CHR_UCASE( KSTRS( I ) )
            END DO
         END IF
         CALL CCD1_KPOP( 'Global readout direction',
     :                   QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
      END IF

*  Deferred charge.
      CALL CCD1_KPGT( 'DEFERRED', MKEY, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTDEF = QUNKEY .OR. NK .GT. 0
      IF ( GOTDEF ) THEN
         GOTONE = .TRUE.
         IF ( QUNKEY ) THEN
            CALL DAT_GET0D( ULOC, DEFER, STATUS )
            CALL DAT_ANNUL( ULOC, STATUS )
            CALL CHR_RTOC( REAL( DEFER ), USTR, LENG )
         END IF
         IF ( NK .GT. 0 ) THEN
            DO I = 1, NK
               CALL DAT_GET0D( KLOCS( I ), DEFER, STATUS )
               CALL DAT_ANNUL( KLOCS( I ), STATUS )
               CALL CHR_RTOC( REAL( DEFER ), KSTRS( I ), LENG )
            END DO
         END IF
         CALL CCD1_KPOP( 'Global deferred charge value (ADUs)',
     :                   QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
      END IF

*  Mask file.
      CALL CCD1_KPGT( 'MASK', MKEY, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTMSK = QUNKEY .OR. NK .GT. 0
      IF ( GOTMSK ) THEN
         GOTONE = .TRUE.
         IF ( QUNKEY ) THEN
            CALL DAT_GET0C( ULOC, MSKNAM, STATUS )
            CALL DAT_ANNUL( ULOC, STATUS )
            USTR = MSKNAM
         END IF
         IF ( NK .GT. 0 ) THEN
            DO I = 1, NK
               CALL DAT_GET0C( KLOCS( I ), MSKNAM, STATUS )
               CALL DAT_ANNUL( KLOCS( I ), STATUS )
               KSTRS( I ) = MSKNAM
            END DO
         END IF
         CALL CCD1_KPOP( 'Global data mask',
     :                   QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
      END IF

*  Find out the users preferences for saturated pixel processing.
      CALL CCD1_KPGT( 'SATURATE', 0, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTSAT = QUNKEY
      IF ( GOTSAT ) THEN
         GOTONE = .TRUE.
         CALL DAT_GET0L( ULOC, SATUR, STATUS )
         IF ( .NOT. SATUR ) THEN
            GOTSVL = .FALSE.
            GOTSPR = .FALSE.
            CALL CCD1_MSG( ' ', '  Not looking for saturated pixels',
     :                     STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  Looking for saturated pixels',
     :                     STATUS )

*  Need a saturation value and the method to use when applying it.
            CALL CCD1_KPGT( 'SATURATION', MKEY, QUNKEY, ULOC, NK,
     :                      KEYS, KLOCS, STATUS )
            GOTSVL = QUNKEY .OR. NK .GT. 0
            IF ( GOTSVL ) THEN
               IF ( QUNKEY ) THEN
                  CALL DAT_GET0D( ULOC, SATVAL, STATUS )
                  CALL DAT_ANNUL( ULOC, STATUS )
                  CALL CHR_RTOC( REAL( SATVAL ), USTR, LENG )
               END IF
               IF ( NK .GT. 0 ) THEN
                  DO I = 1, NK
                     CALL DAT_GET0D( KLOCS( I ), SATVAL, STATUS )
                     CALL DAT_ANNUL( KLOCS( I ), STATUS )
                     CALL CHR_RTOC( REAL( SATVAL ), KSTRS( I ), LENG )
                  END DO
               END IF
               CALL CCD1_KPOP( 'Global saturation value',
     :                         QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
            END IF

*  Preference for saturated pixel flagging.
            CALL CCD1_KPGT( 'SETSAT', 0, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                      STATUS )
            GOTSPR = QUNKEY
            IF ( GOTSPR ) THEN
               CALL DAT_GET0L( ULOC, SETSAT, STATUS )
               CALL DAT_ANNUL( ULOC, STATUS )
               IF ( SETSAT ) THEN
                  CALL CCD1_MSG( ' ',
     :'  Saturated pixels will be set to saturation value', STATUS )
               ELSE
                  CALL CCD1_MSG( ' ',
     :'  Saturated pixels will not be set to saturation value', STATUS )
               END IF
            END IF
         END IF
      END IF

*  Global master bias frame.
      CALL CCD1_KPGT( 'BIAS', MKEY, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTBIA = QUNKEY .OR. NK .GT. 0
      IF ( GOTBIA ) THEN
         GOTONE = .TRUE.
         IF ( QUNKEY ) THEN
            CALL DAT_THERE( ULOC, 'NAMEPTR', THERE, STATUS )
            IF ( THERE ) THEN
               CALL CMP_GET0C( ULOC, 'NAMEPTR', BIAS, STATUS )
            ELSE
               CALL DAT_GET0C( ULOC, BIAS, STATUS )
            END IF
            CALL DAT_ANNUL( ULOC, STATUS )
            USTR = BIAS
         END IF
         IF ( NK .GT. 0 ) THEN
            DO I = 1, NK
               CALL DAT_THERE( KLOCS( I ), 'NAMEPTR', THERE, STATUS )
               IF ( THERE ) THEN
                  CALL CMP_GET0C( KLOCS( I ), 'NAMEPTR', BIAS, STATUS )
               ELSE
                  CALL DAT_GET0C( KLOCS( I ), BIAS, STATUS )
               END IF
               CALL DAT_ANNUL( KLOCS( I ), STATUS )
               KSTRS( I ) = BIAS
            END DO
         END IF
         CALL CCD1_KPOP( 'Global master bias',
     :                   QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
      END IF

*  Global master flatfield.
      CALL CCD1_KPGT( 'FLAT', MKEY, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTFLA = QUNKEY .OR. NK .GT. 0
      IF ( GOTFLA ) THEN
         GOTONE = .TRUE.
         IF ( QUNKEY ) THEN
            CALL DAT_THERE( ULOC, 'NAMEPTR', THERE, STATUS )
            IF ( THERE ) THEN
               CALL CMP_GET0C( ULOC, 'NAMEPTR', FLAT, STATUS )
            ELSE
               CALL DAT_GET0C( ULOC, FLAT, STATUS )
            END IF
            CALL DAT_ANNUL( ULOC, STATUS )
            USTR = FLAT
         END IF
         IF ( NK .GT. 0 ) THEN
            DO I = 1, NK
               CALL DAT_THERE( KLOCS( I ), 'NAMEPTR', THERE, STATUS )
               IF ( THERE ) THEN
                  CALL CMP_GET0C( KLOCS( I ), 'NAMEPTR', FLAT, STATUS )
               ELSE
                  CALL DAT_GET0C( KLOCS( I ), FLAT, STATUS )
               END IF
               CALL DAT_ANNUL( KLOCS( I ), STATUS )
               KSTRS( I ) = FLAT
            END DO
         END IF
         CALL CCD1_KPOP( 'Global master flatfield',
     :                   QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
      END IF

*  Global master calibration frame.
      CALL CCD1_KPGT( 'CAL', MKEY, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTCAL = QUNKEY .OR. NK .GT. 0
      IF ( GOTCAL ) THEN
         GOTONE = .TRUE.
         IF ( QUNKEY ) THEN
            CALL DAT_THERE( ULOC, 'NAMEPTR', THERE, STATUS )
            IF ( THERE ) THEN
               CALL CMP_GET0C( ULOC, 'NAMEPTR', CAL, STATUS )
            ELSE
               CALL DAT_GET0C( ULOC, CAL, STATUS )
            END IF
            CALL DAT_ANNUL( ULOC, STATUS )
            USTR = CAL
         END IF
         IF ( NK .GT. 0 ) THEN
            DO I = 1, NK
               CALL DAT_THERE( KLOCS( I ), 'NAMEPTR', THERE, STATUS )
               IF ( THERE ) THEN
                  CALL CMP_GET0C( KLOCS( I ), 'NAMEPTR', CAL, STATUS )
               ELSE
                  CALL DAT_GET0C( KLOCS( I ), CAL, STATUS )
               END IF
               CALL DAT_ANNUL( KLOCS( I ), STATUS )
               KSTRS( I ) = CAL
            END DO
         END IF
         CALL CCD1_KPOP( 'Global (general) calibration NDF',
     :                   QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
      END IF

*  Do we want to preserve data types through out the processing ?
      CALL CCD1_KPGT( 'PRESERVE', 0, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTPRE = QUNKEY
      IF ( GOTPRE ) THEN
         GOTONE = .TRUE.
         CALL DAT_GET0L( ULOC, PRESER, STATUS )
         CALL DAT_ANNUL( ULOC, STATUS )
         IF ( PRESER ) THEN
            CALL CCD1_MSG( ' ', '  Data types will be preserved',
     :                     STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  Data types will not be preserved',
     :                     STATUS )
         END IF
      END IF

*  Do we want to generate variances ?
      CALL CCD1_KPGT( 'GENVAR', 0, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTGEN = QUNKEY
      IF ( GOTGEN ) THEN
         GOTONE = .TRUE.
         CALL DAT_GET0L( ULOC, GENVAR, STATUS )
         CALL DAT_ANNUL( ULOC, STATUS )
         IF ( GENVAR ) THEN
            CALL CCD1_MSG( ' ', '  Variances will be generated',
     :                     STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  Variances will not be generated',
     :                     STATUS )
         END IF
      END IF

*  Are reponses to INLIST prompts NDF names or position list names?
      CALL CCD1_KPGT( 'NDFNAMES', 0, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTNAM = QUNKEY
      IF ( GOTNAM ) THEN
         GOTONE = .TRUE.
         CALL DAT_GET0L( ULOC, NDFS, STATUS )
         CALL DAT_ANNUL( ULOC, STATUS )
         IF ( NDFS ) THEN
            CALL CCD1_MSG( ' ',
     :'  Position lists will be associated with NDFs', STATUS )
         ELSE
            CALL CCD1_MSG( ' ',
     :'  Position lists will be accessed directly', STATUS )
         END IF
      END IF

*  Will Set header information be used?
      CALL CCD1_KPGT( 'USESET', 0, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTSET = QUNKEY
      IF ( GOTSET ) THEN
         GOTONE = .TRUE.
         CALL DAT_GET0L( ULOC, USESET, STATUS )
         CALL DAT_ANNUL( ULOC, STATUS )
         IF ( USESET ) THEN
            CALL CCD1_MSG( ' ',
     :'  CCDPACK Set header information will be used if available',
     :                     STATUS )
         ELSE
            CALL CCD1_MSG( ' ',
     :'  CCDPACK Set header information will be ignored', STATUS )
         END IF
      END IF

*  Where is the logfile information coming from?
      CALL CCD1_KPGT( 'LOGTO', 0, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                STATUS )
      GOTLG2 = QUNKEY
      IF ( GOTLG2 ) THEN
         GOTONE = .TRUE.
         CALL DAT_GET0C( ULOC, LOGTO, STATUS )
         CALL DAT_ANNUL( ULOC, STATUS )
         CALL CHR_UCASE( LOGTO )
         IF ( LOGTO .EQ. 'BOTH' ) THEN
            CALL CCD1_MSG( ' ',
     :'  Log information will be written to log file and terminal',
     :                     STATUS )
         ELSE IF ( LOGTO .EQ. 'TERMINAL' ) THEN
            CALL CCD1_MSG( ' ',
     :'  Log information will be written to terminal only', STATUS )
         ELSE IF ( LOGTO .EQ. 'LOGFILE' ) THEN
            CALL CCD1_MSG( ' ',
     :'  Log information will be written to log file only', STATUS )
         ELSE IF ( LOGTO .EQ. 'NEITHER' ) THEN
            CALL CCD1_MSG( ' ',
     :'  Log information will be not be written', STATUS )
         END IF
      END IF

*  What is the name of the log file?
      IF ( LOGTO .EQ. 'LOGFILE' .OR. LOGTO .EQ. 'BOTH' ) THEN
         CALL CCD1_KPGT( 'LOGFILE', 0, QUNKEY, ULOC, NK, KEYS, KLOCS,
     :                   STATUS )
         GOTLGN = QUNKEY
         IF ( GOTLGN ) THEN
            GOTONE = .TRUE.
            CALL DAT_GET0C( ULOC, USTR, STATUS )
            CALL DAT_ANNUL( ULOC, STATUS )
            CALL CCD1_KPOP( 'Name of log file',
     :                      QUNKEY, USTR, NK, KEYS, KSTRS, STATUS )
         END IF
      ELSE
         GOTLGN = .FALSE.
      END IF

*  If there were no significant values, mention this.
      IF ( .NOT. GOTONE ) THEN
         IF ( GOTLGN .OR. GOTLG2 ) CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  No current settings exist for any'//
     :                      ' significant CCDPACK global parameters -',
     :                  STATUS )
         CALL CCD1_MSG( ' ', '  use CCDSETUP to set any required'//
     :                       ' values ', STATUS )
       END IF

*  Find out if the user wants to save the setup for future restorations.
      CALL PAR_GET0L( 'SAVE', SAVE, STATUS )
      IF ( SAVE ) THEN

*  Yes he does get a file name.
         CALL CCD1_ASFIO( 'SAVEFILE', 'WRITE', 'LIST', 0, FDS, SOPEN,
     :                    STATUS )

*  And write out the current values.
         CALL CCD1_SAV( FDS, STATUS )

*  Where the set up is saved to.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL FIO_FNAME( FDS, RFILE, STATUS )
         CALL MSG_SETC( 'FNAME', RFILE )
         CALL CCD1_MSG( ' ', '  Setup stored in file: ^FNAME', STATUS )
      END IF

*  Close down.
 99   CONTINUE

*  Close any restoration files.
      IF ( SOPEN ) CALL FIO_CLOSE( FDS, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'CCDSHOW_ERR',
     :   'CCDSHOW: Error showing global parameters...',
     :   STATUS )
      END IF

*  Terminate CCDPACK logging
      CALL CCD1_END( STATUS )

      END
* $Id$
