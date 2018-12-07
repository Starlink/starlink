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
*     FrameSet is returned. If a FrameSet is required, its current Frame
*     is returned if a Frame is required. Its base->current Mapping is
*     returned if a Mapping is required. A Box covering the pixel grid
*     and re-mapped into the current WCS Frame is returned if a Regin is
*     required.
*
*     If the above attempt fails, and the parameter value ends with
*     ".FIT", attempt to interpret the parameter value as the name of a
*     FITS file. Open the FITS file and attempt to obtained an AST
*     FrameSet from the primary HDU headers, or a MOC from a binary table
*     extension.
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
*     2-OCT-2012 (DSB):
*        Ensure any error generated by AST is displayed to the user.
*     12-DEC-2012 (DSB):
*        Ensure any error not generated by AST are annulled, to enable
*        subsequent data formats to be checked.
*     16-JUL-2013 (DSB):
*        If a Region is requested, and an NDF is supplied by the user,
*        return a Box that covers the rectangular pixel grid, and which
*        is mapped into the current Frame of the NDF's WCS FrameSet.
*     21-FEB-2014 (DSB):
*        If a Region is requested, and an NDF is supplied by the user
*        that has an OUTLINE extension, read the region form the outline
*        (see kpgPutOutline).
*     7-DEC-2018 (DSB):
*        Add support for reading FrameSets and Mocs from FITS files.
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
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'CNF_PAR'          ! CNF constants
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
      INTEGER KPG1_GETOUTLINE
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER CARD*80
      CHARACTER LOC1*(DAT__SZLOC)
      CHARACTER LOC2*(DAT__SZLOC)
      CHARACTER NAME*(DAT__SZNAM)
      CHARACTER PARVAL*512
      CHARACTER TFORM*10
      DOUBLE PRECISION DLBND( NDF__MXDIM )
      DOUBLE PRECISION DUBND( NDF__MXDIM )
      INTEGER DIMS( NDF__MXDIM )
      INTEGER DOCVT
      INTEGER FC
      INTEGER FSTAT
      INTEGER FUNIT
      INTEGER HDUTYP
      INTEGER I
      INTEGER IAST3
      INTEGER IAST2
      INTEGER ICARD
      INTEGER IFRM
      INTEGER IGRP
      INTEGER INDF
      INTEGER IP
      INTEGER IPAR
      INTEGER JUNK
      INTEGER MOCORD
      INTEGER MOCLEN
      INTEGER NBYTE
      INTEGER NCARD
      INTEGER NCOL
      INTEGER NDIM
      INTEGER OUTLINE
      LOGICAL AGAIN
      LOGICAL ANYF
      LOGICAL GOTXTN
      LOGICAL MORE
      LOGICAL OK

*.

*  Initialise.
      IAST = AST__NULL
      IGRP = GRP__NOID
      NDIM = 0
      OUTLINE = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a string form the user. Use subpar to avoid problem caused by
*  interpretion of the text within the parameter system.
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

      OK = ( STATUS .EQ. SAI__OK )

      CALL KPG1_WREAD( LOC2, NAME, IAST, STATUS )

      CALL DAT_ANNUL( LOC2, STATUS )
      CALL DAT_ANNUL( LOC1, STATUS )

      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( IAST, STATUS )
         IF( .NOT. OK .AND. STATUS .NE. PAR__NULL .AND.
     :                      STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE IF( OK .AND. STATUS .EQ. SAI__ERROR ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF

      ELSE IF( IAST .NE. AST__NULL ) THEN
         CALL DAT_MSG( 'OBJ', LOC1 )
         CALL ATL_NOTIF( '   AST data read from HDS object ''^OBJ''.',
     :                   STATUS )
      END IF


*  If the HDS access failed, attempt to access the parameter as an NDF, without
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

*  Also get any outline region.
            OUTLINE = KPG1_GETOUTLINE( INDF, STATUS )

*  Also get the dimensions of the NDF in case the caller wants a Region.
            CALL NDF_DIM( INDF, NDF__MXDIM, DIMS, NDIM, STATUS )

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

*  If it was not a native HDS NDF or HDS object, see if it is a FITS file.
      IF( IAST .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN

*  Create a FitsChan to hold header cards.
         CALL AST_BEGIN( STATUS )
         FC = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )

*  Set a flag indicating if a specific extension is specified in PARVAL
*  (i.e. the last character is "]").
         GOTXTN = ( PARVAL( CHR_LEN( PARVAL ): ) .EQ. ']' )

*  Find a free logical-unit.
         FSTAT = 0
         CALL FTGIOU( FUNIT, FSTAT )

*  Attempt to open the file with read access, moving to the extension
*  specified in PARVAL (the primary HDU if no extension is included in
*  PARVAL). Get the type of the opened HDU.
         CALL FTNOPN( FUNIT, PARVAL, 0, FSTAT )
         CALL FTGHDT( FUNIT, HDUTYP, FSTAT )

*  If successful, check each required HDU in turn. Negative status values
*  are reserved for non-fatal warnings.
         MORE = .TRUE.
         DO WHILE( MORE .AND. FSTAT .LE. 0 )

*  Empty the FitsChan then copy all header cards from the HDU into
*  the FitsChan.
            CALL AST_EMPTYFITS( FC, STATUS )
            CALL FTGHSP( FUNIT, NCARD, JUNK, FSTAT )
            DO ICARD = 1, NCARD
               CALL FTGREC( FUNIT, ICARD, CARD, FSTAT )
               CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )
            END DO

*  Read AST objects from the FitsChan until one is found of the required
*  class, or no more can be read.
            CALL AST_CLEAR( FC, 'Card', STATUS )
            AGAIN  = .TRUE.
            DO WHILE( AGAIN )
               IAST = AST_READ( FC, STATUS )
               IF( IAST .EQ. AST__NULL ) THEN
                  AGAIN = .FALSE.
               ELSE IF( ISA( IAST, STATUS ) ) THEN
                  AGAIN = .FALSE.
               ELSE
                  CALL AST_ANNUL( IAST, STATUS )
               END IF
            END DO

*  If no suitable object has yet been found, and this HDU is a binary table,
*  we may be able to read a Region (Moc) from it.
            IF( IAST .EQ. AST__NULL .AND. HDUTYP .EQ. 2 ) THEN

*  Create an empty Moc, and use the supplied ISA function to check that
*  Mocs are acceptable. Annull the Moc if not.
               IAST = AST_MOC( ' ', STATUS )
               IF( .NOT. ISA( IAST, STATUS ) ) THEN
                  CALL AST_ANNUL( IAST, STATUS )

*  If Mocs are acceptable, get the number of bytes per integer value
*  from the TFORM1 header.
               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  NBYTE = 0
                  IF( AST_GETFITSS( FC, 'TFORM1', TFORM,
     :                              STATUS )  ) THEN
                     IF( TFORM .EQ. '1J' ) THEN
                        NBYTE = 4
                     ELSE IF( TFORM .EQ. '1K' ) THEN
                        NBYTE = 8
                     END IF
                  END IF

*  Get the order and length of the MOC and check there is only one column
*  in the table.
                  IF( AST_GETFITSI( FC, 'MOCORDER', MOCORD,
     :                              STATUS ) .AND.
     :                AST_GETFITSI( FC, 'NAXIS2', MOCLEN, STATUS ) .AND.
     :                AST_GETFITSI( FC, 'TFIELDS', NCOL, STATUS ) .AND.
     :                NCOL .EQ. 1 .AND. NBYTE .GT. 0 .AND.
     :                STATUS .EQ. SAI__OK ) THEN

*  Allocate memory then copy the column data from the FITS file into it.
                     CALL PSX_CALLOC( NBYTE*MOCLEN, '_BYTE', IP,
     :                                STATUS )
                     IF( NBYTE .EQ. 4 ) THEN
                        CALL FTGCVJ( FUNIT, 1, 1, 1, MOCLEN, 0,
     :                               %VAL( CNF_PVAL( IP ) ), ANYF,
     :                               FSTAT )
                     ELSE
                        CALL FTGCVK( FUNIT, 1, 1, 1, MOCLEN, 0,
     :                               %VAL( CNF_PVAL( IP ) ), ANYF,
     :                               FSTAT )
                     END IF

*  Add the column data into the Moc. Then free the memory.
                     CALL AST_ADDMOCDATA( IAST, AST__OR, .FALSE.,
     :                                    MOCORD, MOCLEN, NBYTE,
     :                                    %VAL( CNF_PVAL( IP ) ),
     :                                    STATUS )
                     CALL PSX_FREE( IP, STATUS )

*  Annul the Moc if any of the required keywords were not found or were
*  inappropriate for a Moc.
                  ELSE
                      CALL AST_ANNUL( IAST, STATUS )
                  END IF

*  If anything went wrong creating the Moc, annull the object and error.
                  IF( STATUS .NE. SAI__OK ) THEN
                     CALL AST_ANNUL( IAST, STATUS )
                     CALL ERR_ANNUL( STATUS )
                  END IF
               END IF
            END IF

*  If a specific HDU was included in PARVAL, we do not check any more HDUs.
            IF( GOTXTN ) MORE = .FALSE.

*  If we have not yet found a suitable object, move to the next HDU.
            IF( MORE ) CALL FTMRHD( FUNIT, 1, HDUTYP, FSTAT )
         END DO

*  Close the file and release the logical-unit.
         CALL FTCLOS( FUNIT, FSTAT )
         CALL FTFIOU( FUNIT, FSTAT )

*  If an AST object was found, export it from the current AST context so
*  that it is not annulled by the following call to AST_END.
         IF( IAST .NE. AST__NULL ) CALL AST_EXPORT( IAST, STATUS )

*  End the AST object context.
         CALL AST_END( STATUS )
      END IF

*  If it was not a FITS file, native HDS NDF or HDS object ...
      IF( IAST .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN

*  Obtain a GRP group containing text from which an Object is to be read.
         CALL ATL_GTGRP( PARAM, IGRP, STATUS )

*  Abort if requested.
         IF( STATUS .EQ. PAR__NULL .OR.
     :       STATUS .EQ. PAR__ABORT ) GO TO 999

*  Tried to read an object from the group.
         CALL ATL_RDGRP( IGRP, IAST, STATUS )

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

*  If it could not be read as a foreign NDF, re-read it as an AST dump.
*  This is so that we end up with a useful error message being displayed.
            ELSE IF( IGRP .NE. GRP__NOID ) THEN
               CALL ATL_RDGRP( IGRP, IAST, STATUS )
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

*  If not, and we have an outline region, see if the outline region
*  is of the required class.
            IF( .NOT. OK .AND. OUTLINE .NE. AST__NULL ) THEN
               IF( ISA( OUTLINE, STATUS ) ) THEN
                  OK = .TRUE.
                  IAST = AST_CLONE( OUTLINE, STATUS )
               END IF
            END IF

*  If not, and the FrameSet was read from an NDF, create a Region defined
*  in the base Frame of the FrameSet (GRID coords), re-map it into the
*  current Frame, and check if it is of the required class. If so return
*  a pointer to the Region.
            IF( .NOT. OK .AND. NDIM .GT. 0 ) THEN

               IFRM = AST_GETFRAME( IAST, AST__BASE, STATUS )

               DO I = 1, NDIM
                  DLBND( I ) = 0.5D0
                  DUBND( I ) = DIMS( I ) + 0.5D0
               END DO

               IAST2 = AST_BOX( IFRM, 1, DLBND, DUBND, AST__NULL, ' ',
     :                          STATUS )

               IAST3 = AST_MAPREGION( IAST2, IAST, IAST, STATUS )

               IF( ISA( IAST3, STATUS ) ) THEN
                  OK = .TRUE.
                  CALL AST_ANNUL( IAST, STATUS )
                  IAST = IAST3
               ELSE
                  CALL AST_ANNUL( IAST3, STATUS )
               END IF

               CALL AST_ANNUL( IAST2, STATUS )
               CALL AST_ANNUL( IFRM, STATUS )

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
         IF( CLASS .NE. ' ' ) THEN
            CALL MSG_SETC( 'L', CLASS )
         ELSE
            CALL MSG_SETC( 'L', 'Object' )
         END IF

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

*  Delete any groups.
      IF( IGRP .NE. GRP__NOID ) CALL GRP_DELET( IGRP, STATUS )

*  Annul any outline region.
      IF( OUTLINE .NE. AST__NULL ) CALL AST_ANNUL( OUTLINE, STATUS )

      END
