      SUBROUTINE AGP1_TRANS( PARAM, USPEC, PSPEC, AGINAM, STATUS )
*+
*  Name:
*     AGP1_TRANS

*  Purpose:
*     Translate a user-supplied device specification to a PGPLOT
*     device specification and an AGI workstation name.

*  Invocation:
*     CALL AGP1_TRANS( PARAM, USPEC, PSPEC, AGINAM, STATUS )

*  Description:
*     This routine converts a user-supplied graphics device specification
*     into a native PGPLOT equivalent, and an AGI worksation name.
*
*     The user supplied specification can either use the GNS or
*     native PGPLOT syntax:
*
*        GNS: The specification takes the form "gtype[;gfile]", where
*        gtype is a GNS device type and gfile is the GNS name of an output
*        file or specific device.
*
*        Native PGPLOT: The specification takes the form "[pfile]/ptype",
*        where ptype is a PGPLOT device type and pfile is the PGPLOT name
*        of an output file or specific device.
*
*     In both case the device type or file name may be an environment
*     variable, in which case the translation will be used.
*
*     The returned PGPLOT specification will always be of the "native
*     PGPLOT" form described above.
*
*     The AGI workstation name is of the form "AGI_xxx_y" where xxx and y
*     are integers. These are chosen to be the same as the values used by
*     the GNS GKS interface, so that PGPLOT applications can find and use
*     AGI pictures created by GKS applications.
*
*     The conversions are driven by a static device description table
*     which is defined in the text file "pgnames.txt". Fortran source
*     files are generated from this file at build-time, using the
*     make_agp_files script.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter used to access the device. May be
*        blank. Only used in error reports.
*     USPEC = CHARACTER * ( * ) (Given)
*        The user-supplied device specification. If a question mark is
*        given, a list of available devices is given, and an error is
*        reported.
*     PSPEC = CHARACTER * ( * ) (Returned)
*        The equivalent native PGPLOT device specification.
*     AGINAM = CHARACTER * ( * ) (Returned)
*        The equivalent AGI workstation name.
*     STATUS = INTEGER (Given and Returned)
*        The global status

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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)

*  History:
*     31-OCT-2001 (DSB):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'AGP_CONST'

*  Global Variables:
      INCLUDE 'AGP_COM'

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER USPEC*(*)

*  Arguments Returned:
      CHARACTER PSPEC*(*)
      CHARACTER AGINAM*(*)

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AGP1_INIT          ! Initializes AGP common blocks

*  Local Variables:
      CHARACTER GFILE*(AGP__SZUSP)! GNS file name
      CHARACTER GTYPE*(AGP__SZGTY)! GNS device type
      CHARACTER LSPEC*(AGP__SZUSP)! Local copy of user-supplied device spec
      CHARACTER PFILE*(AGP__SZUSP)! PGPLOT file name
      CHARACTER PTYPE*(AGP__SZPTY)! PGPLOT device type
      CHARACTER TRANS*(AGP__SZUSP)! Value of environment variable
      INTEGER F                   ! Index of first non-blank character
      INTEGER IAT                 ! Current length of string
      INTEGER ISEP                ! Index of file-type separator
      INTEGER ITYPE               ! Index of device type in pgnames.txt
      INTEGER L                   ! Index of last non-blank character
      LOGICAL GNS                 ! Was a ";" found?
      LOGICAL OK                  ! Was a translation found?
      LOGICAL PGP                 ! Was a "/" found?
*.

*  Initialize.
      PSPEC = ' '
      AGINAM = ' '

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Take a local copy of the supplied device specification, removing any
*  white space.
      IF( USPEC .NE. ' ' ) THEN
         CALL CHR_FANDL( USPEC, F, L )
         LSPEC = USPEC( F : L )
         CALL CHR_RMBLK( LSPEC )
      ELSE
         STATUS = SAI__ERROR
         IF( PARAM .NE. ' ' ) THEN
            CALL MSG_SETC( 'P', PARAM )
            CALL ERR_REP( 'AGP1_TRANS_ERR1', 'Blank graphics device '//
     :                    'specified for parameter ^P.', STATUS )
         ELSE
            CALL ERR_REP( 'AGP1_TRANS_ERR2', 'Blank graphics device '//
     :                    'specified.', STATUS )
         END IF
         GO TO 999
      END IF

*  If the user spec is just a question mark, display a list of devices
*  and then report an error to force a re-prompt.
      IF( LSPEC .EQ. '?' ) THEN
         CALL MSG_OUT( 'AGP1_TRANS_MSG1', 'Available graphics devices:',
     :                 STATUS )
         CALL AGP_GDLST( STATUS )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'AGP1_TRANS_ERR3', 'Please supply a new device'//
     :                 ' name', STATUS )
         GO TO 999
      END IF

*  If the supplied device spec contains a "/" it is a PGPLOT spec.
      ISEP = INDEX( LSPEC, '/' )
      IF( ISEP .NE. 0 ) THEN
         PGP = .TRUE.
         GNS = .FALSE.

*  Otherwise, if the supplied device spec contains a ";" it is a GNS spec.
      ELSE
         PGP = .FALSE.
         ISEP = INDEX( LSPEC, ';' )
         GNS =  ( ISEP .NE. 0 )
      END IF

*  If the supplied device spec contains neither a ";" or a "/" try
*  translating it as a logical name.
      IF( .NOT.( PGP .OR. GNS ) ) THEN
         CALL AGP1_ENVGT( LSPEC, TRANS, OK, STATUS )

*  If the environment variable exists, replace the supplied spec with the
*  translation, and look for "/" and ";" characters again.
         IF( OK ) THEN
            LSPEC = TRANS

            ISEP = INDEX( LSPEC, '/' )
            IF( ISEP .NE. 0 ) THEN
               PGP = .TRUE.
            ELSE
               ISEP = INDEX( LSPEC, ';' )
               GNS =  ( ISEP .NE. 0 )
            END IF

         END IF

      END IF

*  If the device spec included a "/", a PGPLOT name has been supplied...
      IF( PGP ) THEN

*  Split the spec into type and file name.
         PTYPE = LSPEC( ISEP + 1: )
         IF( PTYPE .EQ. ' ' .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'U', USPEC )
            IF( PARAM .NE. ' ' ) THEN
               CALL MSG_SETC( 'P', PARAM )
               CALL ERR_REP( 'AGP1_TRANS_ERR4', 'Blank graphics type '//
     :                       'specified for parameter ^P (''^U'').',
     :                       STATUS )
            ELSE
               CALL ERR_REP( 'AGP1_TRANS_ERR5', 'Blank graphics type '//
     :                       'specified by ''^U''.', STATUS )
            END IF
            GO TO 999
         END IF

         IF( ISEP .GT. 1 ) THEN
            PFILE = LSPEC( : ISEP - 1 )
         ELSE
            PFILE = ' '
         END IF

*  Attempt to translate them as environment variables.
         CALL AGP1_ENVGT( PTYPE, TRANS, OK, STATUS )
         IF( OK ) PTYPE = TRANS
         CALL AGP1_ENVGT( PFILE, TRANS, OK, STATUS )
         IF( OK ) PFILE = TRANS

*  Find the index of the PGPLOT device type in the common arrays.
         CALL AGP1_FNDTY( PTYPE, PFILE, .FALSE., ITYPE, STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN

*  Get the full case-correct PGPLOT device type.
            PTYPE = AGP_PTY( ITYPE )

*  Return the corresponding AGI workstation name.
            AGINAM = AGP_ANM( ITYPE )

*  If the user spec did not include a file name, use any file name in the
*  pgnames.txt file.
            IF( PFILE .EQ. ' ' ) PFILE = AGP_PFN( ITYPE )
         END IF

*  If the device spec did not included a "/", we assume a GNS name has been
*  supplied...
      ELSE

*  Extract the GNS device type, and file name.
         IF( GNS ) THEN
            IF( ISEP .EQ. 1 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'U', USPEC )
               IF( PARAM .NE. ' ' ) THEN
                  CALL MSG_SETC( 'P', PARAM )
                  CALL ERR_REP( 'AGP1_TRANS_ERR6', 'Blank graphics '//
     :                          'type specified for parameter ^P '//
     :                          '(''^U'').', STATUS )
               ELSE
                  CALL ERR_REP( 'AGP1_TRANS_ERR7', 'Blank graphics '//
     :                          'type specified by ''^U''.', STATUS )
               END IF
               GO TO 999
            END IF

            GTYPE = LSPEC( : ISEP - 1 )
            GFILE = LSPEC( ISEP + 1 : )

*  If no ";" character was included, assume the supplied string is the
*  device type, and use a blank file name.
         ELSE
            GTYPE = LSPEC
            GFILE = ' '
         END IF

*  Attempt to translate them as environment variables.
         CALL AGP1_ENVGT( GTYPE, TRANS, OK, STATUS )
         IF( OK ) GTYPE = TRANS
         CALL AGP1_ENVGT( GFILE, TRANS, OK, STATUS )
         IF( OK ) GFILE = TRANS

*  Find the index of the GNS device type in the common arrays.
         CALL AGP1_FNDTY( GTYPE, ' ', .TRUE., ITYPE, STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN

*  Get the PGPLOT device type, default file name and AGI workstation
*  name for this GNS device type.
            PTYPE = AGP_PTY( ITYPE )
            PFILE = AGP_PFN( ITYPE )
            AGINAM = AGP_ANM( ITYPE )

*  If a GNS file name was included in the supplied file spec, use it in
*  preference to the default PGPLOT file name.
            IF( GFILE .NE. ' ' ) PFILE = GFILE
         END IF

      END IF

*  Return the total PGPLOT spec.
      IF( STATUS .EQ. SAI__OK ) THEN
         IAT = 0
         PSPEC = ' '
         CALL CHR_APPND( PFILE, PSPEC, IAT )
         CALL CHR_APPND( '/', PSPEC, IAT )
         CALL CHR_APPND( PTYPE, PSPEC, IAT )
      END IF

 999  CONTINUE

      END
