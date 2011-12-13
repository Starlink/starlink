      SUBROUTINE CCD1_SAV( FD, STATUS )

*+
*  Name:
*     CCD1_SAV

*  Purpose:
*     Saves the current CCD setup.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*         CALL CCD1_SAV( FD, BUFFER, STATUS )

*  Description:
*     The routine writes out the CCDPACK global parameters as set up
*     by CCDSETUP. The values are written to a formatted file which
*     can be used to restore them.

*  Arguments:
*     FD = INTEGER (Given)
*        FIO file descriptor pointing to file which will contain the
*        environment setup.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JUL-2001 (MBT):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS standard constants
      INCLUDE 'CCD1_PAR'         ! Private CCDPACK constants

*  Status:
      INTEGER STATUS             ! Global status

*  Arguments Given:
      INTEGER FD

*  Local constants:
      INTEGER COMLEN             ! Maximum length of parameter comment text
      PARAMETER ( COMLEN = 40 )
      INTEGER MAXEL              ! Maximum number of elements in a vector global
      PARAMETER ( MAXEL = 4 )
      INTEGER MAXDIM             ! Maximum number of dimensions in a global
      PARAMETER ( MAXDIM = 1 )
      INTEGER NPARAM             ! Number of globals
      PARAMETER ( NPARAM = 16 )

*  Local variables:
      CHARACTER * ( CCD1__BLEN ) BUFFER ! Line buffer
      CHARACTER * ( CCD1__BLEN ) VALSTR ! String representation of param value
      CHARACTER * ( CCD1__BLEN ) VALARR( MAXEL ) ! Text of array param elements
      CHARACTER * ( DAT__SZNAM ) PNAMES( NPARAM ) ! Global parameter names
      CHARACTER * ( DAT__SZLOC ) ULOC ! Locator for unkeyed value
      CHARACTER * ( DAT__SZLOC ) KLOCS( CCD1__MXSI ) ! Locators for keyed vals
      CHARACTER * ( COMLEN ) COMENT( NPARAM ) ! Text descript of global params
      INTEGER DIMS( MAXEL )      ! Actual dimensions of parameter
      INTEGER IAT                ! Length of/position in string
      INTEGER IE                 ! Element loop index
      INTEGER IK                 ! Key loop index
      INTEGER IP                 ! Parameter loop index
      INTEGER KEYS( CCD1__MXSI ) ! Set Index values for keyed parameters
      INTEGER LENG               ! Length of string
      INTEGER NDIM               ! Number of dimensions of parameter
      INTEGER NEL                ! Number of elements in array parameter
      INTEGER NK                 ! Number of keyed values found
      INTEGER NTICKS             ! Number of time ticks
      LOGICAL QUNKEY             ! Does the unkeyed value exist?

*  Local Data:
      DATA PNAMES /
     :   'ADC',
     :   'RNOISE',
     :   'EXTENT',
     :   'BOUNDS',
     :   'DIRECTION',
     :   'DEFERRED',
     :   'MASK',
     :   'SATURATE',
     :   'SATURATION',
     :   'SETSAT',
     :   'PRESERVE',
     :   'GENVAR',
     :   'NDFNAMES',
     :   'USESET',
     :   'LOGTO',
     :   'LOGFILE'
     :   /
      DATA COMENT /
     :   'Electrons/ADU',
     :   'Nominal readout noise in ADUs',
     :   'Extent of useful CCD area',
     :   'Bounds of bias strips',
     :   'Readout direction',
     :   'Deferred charge in ADUs',
     :   'Defect mask',
     :   'Look for saturated pixels',
     :   'Saturation value',
     :   'Set saturated pixels',
     :   'Preserve data types',
     :   'Generate data variances',
     :   'Position lists associated with NDFs',
     :   'Use Set header information',
     :   'Log file information to',
     :   'Name of logfile'
     :   /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write a header into the output file.
*  ===================================

*  Add the title part to the file.
      CALL FIO_WRITE( FD, '#', STATUS )
      BUFFER = ' '
      CALL MSG_LOAD( ' ', '#   CCDPACK - Restoration file',
     :               BUFFER, IAT, STATUS )
      CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )
      CALL FIO_WRITE( FD, '#', STATUS )

*  Get the username from PSX.
      BUFFER = ' '
      CALL PSX_CUSERID( BUFFER, STATUS )
      CALL MSG_SETC( 'USER', BUFFER )

*  Get the date from PSX convert this to a string.
      CALL PSX_TIME( NTICKS, STATUS )
      BUFFER = ' '
      CALL PSX_CTIME( NTICKS, BUFFER, STATUS )
      CALL MSG_SETC( 'DATE', BUFFER )

*  Construct user time and date string
      CALL MSG_LOAD( ' ', '#   Written by ^USER on ^DATE.', BUFFER,
     :               IAT, STATUS )
      CALL FIO_WRITE( FD, BUFFER( : IAT ), STATUS )
      CALL FIO_WRITE( FD, '#', STATUS )

*  Write out the value of each parameter which has been set.
*  ========================================================

*  Loop over all known parameters.
      DO IP = 1, NPARAM

*  Get locators for the keyed and unkeyed values for the parameter from
*  the globals file.
         CALL CCD1_KPGT( PNAMES( IP ), CCD1__MXSI, QUNKEY, ULOC, NK,
     :                   KEYS, KLOCS, STATUS )

*  If there are keyed values, output them first.
         IF ( NK .GT. 0 ) THEN
            DO IK = 1, NK

*  See if the locator refers to a scalar or vector value.
               CALL DAT_SHAPE( KLOCS( IK ), MAXDIM, DIMS, NDIM, STATUS )

*  Construct a string representing its value accordingly.
               VALSTR = ' '
               IF ( NDIM .EQ. 0 ) THEN
                  CALL DAT_GET0C( KLOCS( IK ), VALSTR, STATUS )
               ELSE
                  CALL DAT_GET1C( KLOCS( IK ), MAXEL, VALARR, NEL,
     :                            STATUS )
                  IAT = 0
                  DO IE = 1, NEL
                     CALL CHR_APPND( VALARR( IE ), VALSTR, IAT )
                     IF ( IE .LT. NEL )
     :                  CALL CHR_APPND( ', ', VALSTR, IAT )
                  END DO
               END IF

*  Annul the locator which we no longer need.
               CALL DAT_ANNUL( KLOCS( IK ), STATUS )

*  Construct a line giving the keyed name, value and commment.
               CALL MSG_SETI( 'IKEY', IK )
               CALL MSG_SETC( 'PNAME', PNAMES( IP ) )
               CALL MSG_SETC( 'VALUE', VALSTR )
               CALL MSG_SETC( 'COMMENT', COMENT( IP ) )
               CALL MSG_LOAD( ' ',
     :         '^IKEY,^PNAME = ^VALUE  ! ^COMMENT (Set Index ^IKEY)',
     :                        BUFFER, LENG, STATUS )

*  Write the line to the output file.
               CALL FIO_WRITE( FD, BUFFER( 1:LENG ), STATUS )
            END DO
         END IF

*  If there is an unkeyed value of the parameter, output that as well.
         IF ( QUNKEY ) THEN

*  See if the locator refers to a scalar or vector value.
            CALL DAT_SHAPE( ULOC, MAXDIM, DIMS, NDIM, STATUS )

*  Construct a string representing its value accordingly.
            VALSTR = ' '
            IF ( NDIM .EQ. 0 ) THEN
               CALL DAT_GET0C( ULOC, VALSTR, STATUS )
            ELSE
               CALL DAT_GET1C( ULOC, MAXEL, VALARR, NEL, STATUS )
               IAT = 0
               DO IE = 1, NEL
                  CALL CHR_APPND( VALARR( IE ), VALSTR, IAT )
                  IF ( IE .LT. NEL ) CALL CHR_APPND( ', ', VALSTR, IAT )
               END DO
            END IF

*  Annul the locator which we no longer need.
            CALL DAT_ANNUL( ULOC, STATUS )

*  Construct a line giving the parameter name, value and comment.
            CALL MSG_SETC( 'PNAME', PNAMES( IP ) )
            CALL MSG_SETC( 'VALUE', VALSTR )
            CALL MSG_SETC( 'COMMENT', COMENT( IP ) )
            CALL MSG_LOAD( ' ', '^PNAME = ^VALUE  ! ^COMMENT',
     :                     BUFFER, LENG, STATUS )

*  Write the name to the output file.
            CALL FIO_WRITE( FD, BUFFER( 1:LENG ), STATUS )
         END IF
      END DO

      END
* $Id$
