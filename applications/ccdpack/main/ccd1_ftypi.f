      SUBROUTINE CCD1_FTYPI( IDS, NNDF, REPORT, FTYPES, VALID, NVALID,
     :                       STATUS )
*+
*  Name:
*     CCD1_FTYPI

*  Purpose:
*     Creates a list of NDF FRAME and FILTER types.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_FTYPI( IDS, NNDF, REPORT, FTYPES, VALID, NVALID,
*                      STATUS )

*  Description:
*     The reads the extensions of all the NDFs whose identifiers are
*     given in IDS. The frame types and filters are then written into
*     a list FTYPES. If NDFs have no CCDPACK extension or frame type
*     then this is flagged in VALID. If REPORT is true then each
*     invalid NDF is reported and if all NDFs are invalid the
*     routine sets status issues an error report and exits.

*  Arguments:
*     IDS( NNDF ) = INTEGER (Given)
*        Identifier for all input NDFs.
*     NNDF = INTEGER (Given)
*        Number of entries in input group
*     REPORT = LOGICAL (Given)
*        If true then all NDFs which do not have a frame type are
*        reported. NDFs without a frame types are always flagged in
*        VALID.
*     FTYPES( 2, NNDF ) = CHARACTER * ( * ) (Returned)
*        Frame types of the input NDFs.
*     VALID( NNDF ) = LOGICAL (Returned)
*        If an input NDF does not meet the necessary requirements
*        it is flagged as invalid.
*     NVALID = INTEGER (Returned)
*        The number of valid NDFs returned in VALID.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     20-FEB-1992 (PDRAPER):
*        Original version.
*     28-SEP-1993 (PDRAPER):
*        Changed to use NDF identifiers and only optional make error
*        reports.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters

*  Arguments Given:
      INTEGER NNDF
      INTEGER IDS( NNDF )
      LOGICAL REPORT

*  Arguments Returned:
      CHARACTER * ( * ) FTYPES( 2, NNDF )
      LOGICAL VALID( NNDF )
      INTEGER NVALID

*  Status:
      INTEGER STATUS             ! Global status

*  External References
      LOGICAL CCD1_MATCH
      EXTERNAL CCD1_MATCH        ! Checks string against list of
                                 ! possible strings
*  Local Constants:
      INTEGER MAXTYP             ! Maximum number of frame types
      PARAMETER ( MAXTYP = 12 )

*  Local Variables:
      CHARACTER * ( CCD1__NMLEN ) FLIST( MAXTYP ) ! List of possible
                                                  ! frame types
      CHARACTER * ( CCD1__NMLEN ) FTYPE ! Frame type
      CHARACTER * ( DAT__SZLOC ) LOCEXT ! Locator to extension
      INTEGER I                  ! Loop variable
      LOGICAL OK                 ! Flag
      LOGICAL THERE              ! Flag

*  Local Data: Set lists of possible frame types.
      DATA FLIST / 'BIAS',
     :             'TARGET',
     :             'DARK',
     :             'FLASH',
     :             'FLAT',
     :             'TWILIGHT_SKY',
     :             'NIGHT_SKY',
     :             'DOME',
     :             'MASTER_FLAT',
     :             'MASTER_FLASH',
     :             'MASTER_DARK',
     :             'MASTER_BIAS' /


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set number of output NDFs.
      NVALID = NNDF

*  Look at all NDFs.
      DO 2 I = 1, NNDF

*  Set NDF validity to true
         VALID( I ) = .TRUE.

*  Look for the CCDPACK extension, if one is not found then create one.
         CALL NDF_XSTAT( IDS( I ), 'CCDPACK', THERE, STATUS )

*  If the extension does not exist then issue a warning and flag this
*  NDF as not valid if REPORT is TRUE.
         IF ( .NOT. THERE ) THEN
            IF ( REPORT ) THEN
               CALL NDF_MSG( 'NDF', IDS( I ) )
               CALL CCD1_MSG( ' ', ' Warning - NDF : ^NDF ', STATUS )
               CALL CCD1_MSG( ' ',
     :'  has no CCDPACK extension, it will not be processed', STATUS )
            END IF

*  Flag this NDF as not valid.
            VALID( I ) = .FALSE.
            NVALID = NVALID - 1
         ELSE

*  Extension ok, just get a locator to it.
            CALL NDF_XLOC( IDS( I ), 'CCDPACK', 'READ', LOCEXT, STATUS )

*  Look for the FTYPE extension item.
            CALL DAT_THERE( LOCEXT, 'FTYPE', THERE, STATUS )
            IF ( .NOT. THERE ) THEN

*  May be fatal for this NDF - issue a warning if asked.
               IF ( REPORT ) THEN
                  CALL NDF_MSG( 'NDF', IDS( I ) )
                  CALL CCD1_MSG( ' ', ' Warning - NDF : ^NDF', STATUS )
                  CALL CCD1_MSG( ' ',
     :'  has no frame type, it will not be processed', STATUS )

               END IF

*  Flag this NDF as not valid.
               VALID( I ) = .FALSE.
               NVALID = NVALID - 1
            ELSE

*  Finally ok extract the value and check it against the possible types.
               CALL CMP_GET0C( LOCEXT, 'FTYPE', FTYPE, STATUS )

*  Check the type against the list of those possible.
               OK = CCD1_MATCH( FTYPE, FLIST, MAXTYP, STATUS )
               IF ( .NOT. OK ) THEN

*  Again may be fatal for this NDF - issue a warning and flag it.
                  IF ( REPORT ) THEN
                     CALL NDF_MSG( 'NDF', IDS( I ) )
                     CALL CCD1_MSG( ' ', ' Warning - NDF : ^NDF',
     :                             STATUS )
                     CALL CCD1_MSG( ' ',
     :'  has an invalid frame type, it will not be processed', STATUS )
                  END IF

*  Flag this NDF as not valid.
                  VALID( I ) = .FALSE.
                  NVALID = NVALID - 1
               ELSE

*  Store the value.
                  FTYPES( 1, I ) = FTYPE
               END IF

*  Extract the FILTER, this can be any string so no checks are
*  performed.
               FTYPES( 2, I ) = ' '
               CALL DAT_THERE( LOCEXT, 'FILTER', THERE, STATUS )
               IF( THERE ) THEN
                  CALL CMP_GET0C( LOCEXT, 'FILTER', FTYPES( 2, I ),
     :                            STATUS )
               END IF
            END IF

*  Release locator to extension.
            CALL DAT_ANNUL( LOCEXT, STATUS )
         END IF
 2    CONTINUE

*  All done check that we have not excluded all NDFs. If REPORT is true
*  then this is a serious condition.
      IF ( REPORT ) THEN
         IF ( NVALID .LT. 1 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_FTYPL1',
     :   '  CCD1_FTYPI: All input NDFs rejected, no valid CCDPACK'//
     :   ' extensions', STATUS )
         END IF
      END IF
      END
* $Id$
