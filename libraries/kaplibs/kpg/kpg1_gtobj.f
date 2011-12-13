      SUBROUTINE KPG1_GTOBJ( PARAM, CLASS, ISA, IAST, STATUS )
*+
*  Name:
*     KPG1_GTOBJ

*  Purpose:
*     Gets an AST Object using an environment parameter.

*  Description:
*     Gets an AST Object from an NDF, FITS file, HDS path or text file
*     using an environment parameter.
*
*     First, attempt to interpret the parameter value as an HDS path. The
*     HDS object must have a type of WCS, must be scalar, and must contain
*     a single one-dimensional array component with name DATA and type _CHAR.
*     This is the scheme used for HDS structures created by KPG1_WWRT.
*
*     If the above attempt fails, attempt to interpret the parameter
*     value as an NDF name. If the NDF is opened succesfully, its WCS
*     FrameSet is returned.
*
*     If the above attempt fails, and the parameter value ends with
*     ".FIT", attempt to interpret the parameter value as the name of a
*     FITS file. Open the FITS file and attempt to obtained an AST
*     FrameSet from the primary HDU headers.
*
*     If the above attempt fails, attempt to interpret the parameter
*     value as the name of a text file containing either an AST object
*     dump, or a set of FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTOBJ( PARAM, CLASS, ISA, IAST, STATUS )

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     CLASS = CHARACTER * ( * ) (Given)
*        The required class. Used in error reports (see ISA). If Objects
*        of more than 1 class can be used, this should be supplied blank, and
*        the calling routine should verify that the Object is usable.
*     ISA = EXTERNAL (Given)
*        A suitable AST "ISA.." function which returns .TRUE. if an Object
*        is of a suitable class. This is ignored if CLASS is blank.
*        Otherwise, an error is reported if th supplied Object is not of the
*        required class.
*     IAST = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - If the group contains the AST dump of a Channel (of any class),
*     then the Object returned via IAST will be the Channel itself. The
*     exception to this is that if the "Begin " line at the start of
*     the dump ends with the string "(Read)", then the returned IAST
*     Object will be the Object read from the Channel, rather than the
*     Channel itself. For instance, if the group contains the AST dump of
*     a FitsChan, and the first line of the dump is "Begin FitsChan(Read)",
*     then the returned IAST object will be the Object read from the
*     FitsChan, rather than the FitsChan itself. This facility is only
*     available for top level objects (e.g. FitsChans contained within
*     FitsChans cannot be read in this way).

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     6-JAN-2005 (DSB):
*        Allow CLASS and ISA to specify particular subclasses of Frame.
*     31-MAY-2006 (DSB):
*        Move from ATL to KAPLIBS because of the NDF dependency.
*     23-APR-2009 (DSB):
*        Take acount of foreign format conversion by the NDF library.
*     5-JUN-2009 (DSB):
*        Allow AST objects to be read from an HDS path.
*     8-JUN-2009 (DSB):
*        Avoid use of NDF_EXIST since it gives the appearance of a
*        locator leak (KAPPA monolith locator checsk do not, and cannot,
*        filter out the locators stored in the parameter system by
*        NDF_EXIST and NDF_ASSOC).
*     9-JUN-2009 (DSB):
*        Improve error reporting.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER CLASS*(*)
      LOGICAL ISA

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ISA

*  Local Variables:
      CHARACTER LOC1*(DAT__SZLOC)
      CHARACTER LOC2*(DAT__SZLOC)
      CHARACTER NAME*(DAT__SZNAM)
      CHARACTER PARVAL*512
      INTEGER DOCVT
      INTEGER IAST2
      INTEGER IGRP
      INTEGER INDF
      INTEGER IPAR
      LOGICAL OK
*.

*  Initialise.
      IAST = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a string form the user. Use subpar to avoid problem caused by
*  interpretion of the text within thr parameter system.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, PARVAL, STATUS )

*  Check we got some text successfully.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  First of all, attempt to get an object assuming the user has supplied
*  an HDS path. Do not use DAT_ASSOC since it required the parameter to
*  be declared as type UNIV in the IFL file. Instead, get the value of
*  the parameter using SUBPAR to avoid interpretation of the string by
*  the parameter system.
      CALL HDS_FIND( DAT__ROOT, PARVAL, 'Read', LOC1, STATUS )
      CALL DAT_NAME( LOC1, NAME, STATUS )
      CALL DAT_PAREN( LOC1, LOC2, STATUS )

      CALL KPG1_WREAD( LOC2, NAME, IAST, STATUS )

      CALL DAT_ANNUL( LOC2, STATUS )
      CALL DAT_ANNUL( LOC1, STATUS )

      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( IAST, STATUS )
         IF( STATUS .NE. PAR__NULL .AND. STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF

      ELSE IF( IAST .NE. AST__NULL ) THEN
         CALL DAT_MSG( 'OBJ', LOC1 )
         CALL ATL_NOTIF( '   AST data read from HDS object ''^OBJ''.',
     :                   STATUS )
      END IF


*  If this failed, attempt to access the parameter as an NDF, without
*  foreign format conversion (in case the file has a known foreign format
*  file type but does not actualy contain an NDF). First switch off format
*  conversion, then access the NDF then switch format conversion back on
*  again if required. Note, use NDF_FIND rather than NDF_EXIST since
*  NDF_EXIST causes HDS locators for the NDF to be stored in the
*  parameter system until the parameter is cancelled. This gives the
*  (erroneous) appearance of an HDS locator leak to packages such as
*  KAPPA that perform checks for HDS locators leaks.
      IF( IAST .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         CALL NDF_GTUNE( 'DOCVT', DOCVT, STATUS )
         CALL NDF_TUNE( 0, 'DOCVT', STATUS )

         IF( STATUS .EQ. SAI__OK ) THEN
            CALL NDF_FIND( DAT__ROOT, PARVAL, INDF, STATUS )
            IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         END IF

         CALL NDF_TUNE( DOCVT, 'DOCVT', STATUS )

*  If succesful, get the WCS FrameSet from it.
         IF( INDF .NE. NDF__NOID ) THEN
            CALL KPG1_GTWCS( INDF, IAST, STATUS )

*  Tell the user where the object came from.
            IF( IAST .NE. AST__NULL ) THEN
               CALL NDF_MSG( 'NDF', INDF )
               CALL ATL_NOTIF( '   AST data read from NDF ''^NDF''.',
     :                         STATUS )
            END IF

*  Annul the NDF identifer.
            CALL NDF_ANNUL( INDF, STATUS )

         END IF
      END IF

*  If it was not a native HDS NDF or HDS object ...
      IF( IAST .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN

*  Obtain a GRP group containing text from which an Object is to be read.
         CALL ATL_GTGRP( PARAM, IGRP, STATUS )

*  Abort if requested.
         IF( STATUS .EQ. PAR__NULL .OR.
     :       STATUS .EQ. PAR__ABORT ) GO TO 999

*  Tried to read an object from the group.
         CALL ATL_RDGRP( IGRP, IAST, STATUS )

*  Delete the group.
         CALL GRP_DELET( IGRP, STATUS )

*  If it was not in a format readable by ATL, annull the error, and try to
*  access it as a foreign format NDF.
         IF( STATUS .NE. SAI__OK .AND. DOCVT .NE. 0 ) THEN
            CALL ERR_ANNUL( STATUS )

            CALL NDF_FIND( DAT__ROOT, PARVAL, INDF, STATUS )
            IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

            IF( INDF .NE. NDF__NOID ) THEN
               CALL KPG1_GTWCS( INDF, IAST, STATUS )

*  Tell the user where the object came from.
               IF( IAST .NE. AST__NULL ) THEN
                  CALL NDF_MSG( 'NDF', INDF )
                  CALL ATL_NOTIF( '   AST data read from NDF ''^NDF''.',
     :                             STATUS )
               END IF

*  Annul the NDF identifer.
               CALL NDF_ANNUL( INDF, STATUS )
            END IF
         END IF
      END IF

*  Check the Object class if CLASS is not blank.
      IF( CLASS .NE. ' ' .AND. STATUS .EQ. SAI__OK .AND.
     :    IAST .NE. AST__NULL ) THEN

*  See if the object is of the required class.
         OK = ISA( IAST, STATUS )

*  If not, and if the object is a FrameSet, see if the current Frame is
*  of the required class. If so return a pointer to the current Frame.
         IF( AST_ISAFRAMESET( IAST, STATUS ) ) THEN
            IF( .NOT. OK ) THEN
               IAST2 = AST_GETFRAME( IAST, AST__CURRENT, STATUS )
               IF( ISA( IAST2, STATUS ) ) THEN
                  OK = .TRUE.
                  CALL AST_ANNUL( IAST, STATUS )
                  IAST = IAST2
               ELSE
                  CALL AST_ANNUL( IAST2, STATUS )
               END IF
            END IF

*  If not, see if the base to current Mapping is of the required class. If
*  so return a pointer to the base to current Mapping.
            IF( .NOT. OK ) THEN
               IAST2 = AST_GETMAPPING( IAST, AST__BASE, AST__CURRENT,
     :                                STATUS )
               IF( ISA( IAST2, STATUS ) ) THEN
                  OK = .TRUE.
                  CALL AST_ANNUL( IAST, STATUS )
                  IAST = IAST2
               ELSE
                  CALL AST_ANNUL( IAST2, STATUS )
               END IF
            END IF

         END IF

*  Report an error if we could not find an object of the correct class.
         IF( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN
            IF( AST_ISAFRAMESET( IAST, STATUS ) ) THEN
               IAST2 = AST_GETFRAME( IAST, AST__CURRENT, STATUS )
               CALL MSG_SETC( 'C', AST_GETC( IAST2, 'CLASS', STATUS ) )
               CALL MSG_SETC( 'P', PARAM )
               CALL MSG_SETC( 'L', CLASS )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPG1_GTOBJ_ERR1', '$^P contains a '//
     :                       'FrameSet representing a ^C, but a '//
     :                       '^L is required.', STATUS )
               CALL AST_ANNUL( IAST2, STATUS )
            ELSE
               CALL MSG_SETC( 'C', AST_GETC( IAST, 'CLASS', STATUS ) )
               CALL MSG_SETC( 'P', PARAM )
               CALL MSG_SETC( 'L', CLASS )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPG1_GTOBJ_ERR2', '$^P contains a ^C, '//
     :                       'but a ^L is required.', STATUS )
            END IF
         END IF

      END IF

*  Report an error if no object was read.
      IF( IAST .EQ. AST__NULL ) THEN
         IF( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL MSG_SETC( 'P', PARAM )
         CALL MSG_SETC( 'V', PARVAL )
         CALL MSG_SETC( 'L', CLASS )

         IF( INDEX( PARVAL( 1 : 1 ), 'AEIOU' ) .NE. 0 ) THEN
            CALL MSG_SETC( 'A', 'a' )
         ELSE
            CALL MSG_SETC( 'A', 'an' )
         END IF

         CALL ERR_REP( 'KPG1_GTOBJ_ERR3', 'Failed to obtain ^A '//
     :                 '^L from ''^V'' (parameter ''^P'').', STATUS )
      END IF

*  Annul the object if an error occurred.
 999  IF( STATUS .NE. SAI__OK ) CALL AST_ANNUL( IAST, STATUS )

      END
