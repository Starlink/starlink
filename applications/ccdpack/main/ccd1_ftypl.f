      SUBROUTINE CCD1_FTYPL( GID, NNDF, FTYPES, VALID, ORIG, NBOUND,
     :                       HVZERO, DEBICR, DARKCR, DRKTIM, FLASCR,
     :                       FLSTIM, FLATCR, ZEROED, ZEROCK, STATUS )
*+
*  Name:
*     CCD1_FTYPL

*  Purpose:
*     Extracts information about NDFs from their CCDPACK extensions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CCD1_FTYPL( GID, NNDF, FTYPES, VALID, ORIG, NBOUND, HVZERO,
*                       DEBICR, DARKCR, DRKTIM, FLASCR, FLSTIM, FLATCR,
*                       ZEROED, ZEROCK, STATUS )

*  Description:
*     The routine access all the NDFs in the GRP group GID. It then
*     extracts all the frame types and filters and writes them to a
*     list pointed to by FTYPES. If NDFs have no CCDPACK extension or
*     frame type a warning is issued and this NDF is flagged in VALID.
*     Other information which is useful when forming a reduction
*     schedule is also returned, such as the processing status the
*     presence of bias strip bounds, the presence of bias value,
*     whether any master biases are zero etc. If all NDFs are removed
*     the routine sets status issues and error report and exits.

*  Arguments:
*     GID = INTEGER (Given)
*        GRP group identifier for all input NDFs.
*     NNDF = INTEGER (Given)
*        Number of entries in input group
*     FTYPES( 2, NNDF ) = CHARACTER * ( * ) (Returned)
*        Frame types of the input NDFs.
*     VALID( NNDF ) = LOGICAL (Returned)
*        If an input NDF does not meet the necessary requirements
*        it is flagged as invalid.
*     ORIG( NNDF ) = CHARACTER * ( * ) (Returned)
*        The names of the data files which correspond to the original
*        "raw" data. This name should be propagated through any
*        processing chains so that later NDFs can be identified as
*        part of a processing chain.
*     NBOUND = INTEGER (Returned)
*        The numbers of sub-components of the BOUNDS extension item in
*        the NDFs. This is the minimum number and will be zero if
*        none are found in any NDF.
*     HVZERO = LOGICAL (Returned)
*        Whether or not all the NDFs have the ZERO extension item
*        or not.
*     DEBICR( NNDF ) = LOGICAL (Returned)
*        Whether or not the NDF has been debiassed already.
*     DARKCR( NNDF ) = LOGICAL (Returned)
*        Whether or not the NDF has been dark corrected already.
*     DRKTIM( NNDF ) = DOUBLE PRECISION (Returned)
*        The dark exposure time of the NDF.
*     FLASCR( NNDF ) = LOGICAL (Returned)
*        Whether or not the NDF has been pre-flash corrected already.
*     FLSTIM( NNDF ) = DOUBLE PRECISION (Returned)
*        The pre-flash exposure time of the NDF.
*     FLATCR( NNDF ) = LOGICAL (Returned)
*        Whether or not the NDF has been flatfielded already.
*     ZEROED = LOGICAL (Returned)
*        If a MASTER_BIAS is located then this flag indicates whether
*        or not the bias master has been zeroed or not. This is here
*        for efficiency -- don't want to reopen the NDFs later.
*     ZEROCK = LOGICAL (Returned)
*        Whether or not the ZEROED extension item is present in
*        the master bias.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997, 2000 Central Laboratory of the Research
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
*     20-FEB-1992 (PDRAPER):
*        Original version.
*     31-JAN-1994 (PDRAPER):
*        Added checks for items related to the possible ways in which
*        the NDFs could be debiassed.
*     1-FEB-1994 (PDRAPER):
*        Added checks for zeroed master bias (put here for efficiency).
*     3-FEB-1994 (PDRAPER):
*        Added processing status options.
*     3-FEB-1994 (PDRAPER):
*        Added ORIG argument.
*     14-FEB-1994 (PDRAPER):
*        Added DRKTIM and FLSTIM.
*     13-NOV-1995 (PDRAPER):
*        Now check thats NBOUND and HVZERO do not operate on MASTERS
*        and BIASes
*     3-MAR-1997 (PDRAPER):
*        Removed LOC argument from IRG_NDFEX call. Added DAT_ANNUL
*        for LOCCMP that was dangling.
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
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters

*  Arguments Given:
      INTEGER GID
      INTEGER NNDF

*  Arguments Returned:
      CHARACTER * ( * ) FTYPES( 2, NNDF )
      LOGICAL VALID( NNDF )
      CHARACTER * ( * ) ORIG( NNDF )
      INTEGER NBOUND
      LOGICAL HVZERO
      LOGICAL DEBICR( NNDF )
      LOGICAL DARKCR( NNDF )
      DOUBLE PRECISION DRKTIM( NNDF )
      LOGICAL FLASCR( NNDF )
      DOUBLE PRECISION FLSTIM( NNDF )
      LOGICAL FLATCR( NNDF )
      LOGICAL ZEROED
      LOGICAL ZEROCK

*  Status:
      INTEGER STATUS             ! Global status

*  External References
      LOGICAL CCD1_MATCH
      EXTERNAL CCD1_MATCH        ! Checks string against list of
                                 ! possible strings
*  Local Constants:
      INTEGER MAXTYP             ! Maximum number of frame types
      PARAMETER ( MAXTYP = 9 )

*  Local Variables:
      CHARACTER * ( CCD1__NMLEN ) FLIST( MAXTYP ) ! List of possible frame types
      CHARACTER * ( CCD1__NMLEN ) FTYPE ! Frame type
      CHARACTER * ( DAT__SZLOC ) LOCCMP ! Locator to component
      CHARACTER * ( DAT__SZLOC ) LOCEXT ! HDS locator to extension
      INTEGER I                  ! Loop variable
      INTEGER NCOMP              ! Number of components to item
      INTEGER NDFID              ! NDF identifier
      INTEGER NOUT               ! Number of valid NDFs.
      LOGICAL FIRST              ! First occurence of MASTER_BIAS
      LOGICAL OK                 ! Flag
      LOGICAL THERE              ! Flag

*  Local Data: Set lists of possible frame types. First commented set
*  are the all the possible types, but only FLAT is supported from
*  amongst the flatfield types at present.
C      DATA FLIST / 'BIAS',
C     :             'TARGET',
C     :             'DARK',
C     :             'FLASH',
C     :             'FLAT',
C     :             'TWILIGHT_SKY',
C     :             'NIGHT_SKY',
C     :             'DOME',
C     :             'MASTER_FLAT',
C     :             'MASTER_FLASH',
C     :             'MASTER_DARK',
C     :             'MASTER_BIAS' /
      DATA FLIST / 'BIAS',
     :             'TARGET',
     :             'DARK',
     :             'FLASH',
     :             'FLAT',
     :             'MASTER_FLAT',
     :             'MASTER_FLASH',
     :             'MASTER_DARK',
     :             'MASTER_BIAS' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set number of output NDFs.
      NOUT = NNDF

*  Number of bounds in NDFs.
      NBOUND = 4

*  Have a bias level.
      HVZERO = .TRUE.

*  Is first MASTER_BIAS.
      FIRST = .TRUE.
      ZEROCK = .FALSE.
      ZEROED = .FALSE.

*  Look at all NDFs.
      DO 2 I = 1, NNDF

*  Set NDF validity to true
         VALID( I ) = .TRUE.

*  Get the NDF.
         CALL NDG_NDFAS( GID, I, 'READ', NDFID, STATUS )

*  Look for the CCDPACK extension.
         CALL NDF_XSTAT( NDFID, 'CCDPACK', THERE, STATUS )

*  If the extension does not exist then issue a warning and flag this
*  NDF as not valid.
         IF ( .NOT. THERE ) THEN
            CALL NDF_MSG( 'NDF', NDFID )
            CALL CCD1_MSG( ' ', ' Warning - NDF : ^NDF ', STATUS )
            CALL CCD1_MSG( ' ',
     :'  has no CCDPACK extension, it will not be processed', STATUS )

*  Flag this NDF as not valid.
            VALID( I ) = .FALSE.
            NOUT = NOUT - 1
         ELSE

*  Extension ok, just get a locator to it.
            CALL NDF_XLOC( NDFID, 'CCDPACK', 'READ', LOCEXT, STATUS )

*  Look for the frame type extension item.
            CALL DAT_THERE( LOCEXT, 'FTYPE', THERE, STATUS )
            IF ( .NOT. THERE ) THEN

*  Fatal for this NDF - issue a warning and flag it.
               CALL NDF_MSG( 'NDF', NDFID )
               CALL CCD1_MSG( ' ', ' Warning - NDF : ^NDF', STATUS )
               CALL CCD1_MSG( ' ',
     :'  has no frame type, it will not be processed', STATUS )

*  Flag this NDF as not valid.
               VALID( I ) = .FALSE.
               NOUT = NOUT - 1
            ELSE

*  Finally ok extract the value and check it against the possible types.
               CALL CMP_GET0C( LOCEXT, 'FTYPE', FTYPE, STATUS )

*  Check the type against the list of those possible.
               OK = CCD1_MATCH( FTYPE, FLIST, MAXTYP, STATUS )
               IF ( .NOT. OK ) THEN

*  Fatal for this NDF - issue a warning and flag it.
                  CALL NDF_MSG( 'NDF', NDFID )
                  CALL CCD1_MSG( ' ', ' Warning - NDF : ^NDF',
     :                          STATUS )
                  CALL MSG_SETC( 'FTYPE', FTYPE )
                  CALL CCD1_MSG( ' ', ' has a frame type (^FTYPE)'//
     :' which is not recognised by this application', STATUS )

*  Flag this NDF as not valid.
                  VALID( I ) = .FALSE.
                  NOUT = NOUT - 1
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

*  Look for a BOUNDS structure. Count the number of values.
               CALL DAT_THERE( LOCEXT, 'BOUNDS', OK, STATUS )
               IF ( OK ) THEN

*  How many?
                  CALL DAT_FIND( LOCEXT, 'BOUNDS', LOCCMP, STATUS )
                  CALL DAT_NCOMP( LOCCMP, NCOMP, STATUS )

*  Number of bounds is minimum of this and current value.
                  NBOUND = MIN( NBOUND, NCOMP )
                  CALL DAT_ANNUL( LOCCMP, STATUS )
               ELSE

*  No bounds if this frame is not a master or bias (all other types
*  should have this item).
                  IF ( FTYPE( 1: 6 ) .NE. 'MASTER' .AND.
     :                 FTYPE( 1: 4 ) .NE. 'BIAS') THEN
                     NBOUND = 0
                  END IF
               END IF

*  Look for ZERO value in NDF (not present in MASTERs and BIASes).
               CALL DAT_THERE( LOCEXT, 'ZERO', OK, STATUS )
               IF ( FTYPE( 1: 6 ) .NE. 'MASTER' .AND.
     :              FTYPE( 1: 4 ) .NE. 'BIAS') THEN
                  HVZERO = HVZERO .AND. OK
               END IF

*  Is the NDF a MASTER_BIAS? If so check for the presence of the ZEROED
*  extension item. (only check the first occurence).
               IF ( FIRST ) THEN
                  FIRST = .FALSE.
                  IF ( FTYPE( 1: 11 ) .EQ. 'MASTER_BIAS' ) THEN

*  Try to extract item.
                     CALL DAT_THERE( LOCEXT, 'ZEROED', ZEROCK, STATUS )
                     IF ( ZEROCK ) THEN
                        CALL CMP_GET0L( LOCEXT, 'ZEROED', ZEROED,
     :                                  STATUS )
                     END IF
                  END IF
               END IF

*  Find out about the various processing options.
               CALL DAT_THERE( LOCEXT, 'DEBIAS', DEBICR( I ), STATUS )
               CALL DAT_THERE( LOCEXT, 'DARKCOR', DARKCR( I ), STATUS )
               CALL DAT_THERE( LOCEXT, 'FLASHCOR', FLASCR( I ), STATUS )
               CALL DAT_THERE( LOCEXT, 'FLATCOR', FLATCR( I ), STATUS )

*  Get the dark and flash times.
               DRKTIM( I ) = 0.0D0
               FLSTIM( I ) = 0.0D0
               CALL DAT_THERE( LOCEXT, 'TIMES', OK, STATUS )
               IF ( OK ) THEN
                  CALL DAT_FIND( LOCEXT, 'TIMES', LOCCMP, STATUS )
                  CALL DAT_THERE( LOCCMP, 'DARK', OK, STATUS )
                  IF ( OK ) CALL CMP_GET0D( LOCCMP, 'DARK', DRKTIM( I ),
     :                                      STATUS )
                  CALL DAT_THERE( LOCCMP, 'FLASH', OK, STATUS )
                  IF ( OK ) CALL CMP_GET0D( LOCCMP, 'FLASH',
     :                                      FLSTIM( I ), STATUS )
                  CALL DAT_ANNUL( LOCCMP, STATUS )
               END IF

*  Get the name of the original data file used when processing (if one
*  exists).
               CALL DAT_THERE( LOCEXT, 'ORIGINAL', OK, STATUS )
               IF ( OK ) THEN
                  CALL CMP_GET0C( LOCEXT, 'ORIGINAL', ORIG( I ),
     :                            STATUS )
               ELSE
                  ORIG( I ) = ' '
               END IF

*  Close this NDF and release all locators.
               CALL DAT_ANNUL( LOCEXT, STATUS )
               CALL NDF_ANNUL( NDFID, STATUS )
            END IF
         END IF
 2    CONTINUE

*  All done check that we have not excluded all NDFs.
      IF ( NOUT .LT. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_FTYPL1',
     :   '  CCD1_FTYPL: All input NDFs rejected, no valid CCDPACK'//
     :   ' extensions', STATUS )
      END IF
      END
* $Id$
