      SUBROUTINE CCD1_SCHED( FTYPES, NNDF, DEBICR, DARKCR, DRKTIM,
     :                       FLASCR, FLSTIM, FLATCR, IRFLAT, VALID,
     :                       PTEMP, MKBIAS, HVBIAS, DODEBI, MKDARK,
     :                       HVDARK, DODARK, MKFLAS, HVFLAS, DOFLAS,
     :                       MKFLAT, HVFLAT, DOFLAT, FILNMS, NFILS,
     :                       STATUS )
*+
*  Name:
*     CCD1_SCHED

*  Purpose:
*     Sets the CCDPACK automated schedule.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_SCHED( FTYPES, NNDF, DEBICR, DARKCR, DRKTIM, FLASCR,
*                      FLSTIM, FLATCR, IRFLAT, VALID, PTEMP, MKBIAS,
*                      HVBIAS, DODEBI, MKDARK, HVDARK, DODARK, MKFLAS,
*                      HVFLAS, DOFLAS, MKFLAT, HVFLAT, DOFLAT, FILNMS,
*                      NFILS, IRFLAT, STATUS )

*  Description:
*     This routine uses the selected FRAME and FILTER types and
*     processing state of NDFs to determine what sort of reduction is
*     possible. The schedule is set by a series of logical flags and
*     pointers which describe what action is required and which frames
*     to use to perform each stage.

*  Arguments:
*     FTYPES( 2, NNDF ) = CHARACTER * ( * ) (Given)
*        List of the input data frame and filter types.
*     NNDF = INTEGER (Given)
*        Number of input frames.
*     DEBICR( NNDF ) = LOGICAL (Given)
*        Whether or not the corresponding NDFs have already been
*        debiassed.
*     DARKCR( NNDF ) = LOGICAL (Given)
*        Whether or not the corresponding NDFs have already been
*        dark corrected.
*     DRKTIM( NNDF ) = DOUBLE PRECISION (Given)
*        The dark exposure times of the NDFs. NDFs with a dark time
*        other than one are corrected, others are not.
*     FLASCR( NNDF ) = LOGICAL (Given)
*        Whether or not the corresponding NDFs have already been
*        pre-flash corrected.
*     FLSTIM( NNDF ) = DOUBLE PRECISION (Given)
*        The per-flash exposure times of the NDFs. NDFs with a dark
*        time other than one are corrected, others are not.
*     FLATCR( NNDF ) = LOGICAL (Given)
*        Whether or not the corresponding NDFs have already been
*        flatfield corrected.
*     IRFLAT = LOGICAL (Given)
*        Whether or not TARGET frames may be used to flatfield.
*        This is an IR option and will only be used if no flatfields
*        of an appropriate colour exist.
*     VALID( NNDF ) = LOGICAL (Given and Returned)
*        Mask of which entries in FTYPES are to be used. On exit any
*        frames of the same type as existing masters are also
*        invalidated.
*     PTEMP( NNDF ) = INTEGER (Given and Returned)
*        Workspace.
*     MKBIAS = LOGICAL (Returned)
*        Flag indicating whether a MASTER_BIAS frame should/can be
*        produced.
*     HVBIAS = LOGICAL (Returned)
*        Flag indicating that we already have a MASTER_BIAS.
*     DODEBI = LOGICAL (Returned)
*        Whether to do any debiassing or not.
*     MKDARK = LOGICAL (Returned)
*        Flag indicating whether a MASTER_FLAT frame should/can be
*        produced.
*     HVDARK = LOGICAL (Returned)
*        Flag indicating that we already have a MASTER_DARK.
*     DODARK = LOGICAL (Returned)
*        Whether to do any dark correction or not.
*     MKFLAS= LOGICAL (Returned)
*        Flag indicating whether a MASTER_FLASH frame should/can be
*        produced.
*     HVFLAS = LOGICAL (Returned)
*        Flag indicating that we already have a MASTER_FLASH.
*     DOFLAS = LOGICAL (Returned)
*        Whether to do any dark correction or not.
*     MKFLAT( NNDF ) = LOGICAL (Returned)
*        Flags which indicate whether a MASTER_FLAT should be produced
*        for the associated filter type (FILNMS).
*     HVFLAT( NNDF ) = LOGICAL (Returned)
*        Flags which indicate whether a MASTER_FLAT of the associated
*        filter type already exists.
*     DOFLAT( NNDF ) = LOGICAL (Returned)
*        Flags indicating whether there are frames which need
*        flatfielding using this filter type.
*     FILNMS( NNDF ) = CHARACTER * ( * ) (Returned)
*        The filter types of the MASTER_FLAT frames.
*     NFILS = INTEGER (Returned)
*        The number of entries returned in MKFLAT, HVFLAT and FILNMS.
*        I.e. the number of filters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     21-FEB-1992 (PDRAPER):
*        Original version.
*     31-JAN-1994 (PDRAPER):
*        Completed prologue!
*     31-JAN-1994 (PDRAPER):
*        Added arguments for information about existing masters.
*     1-FEB-1994 (PDRAPER):
*        Now masks out frame types which are already represented by
*        masters.
*     4-FEB-1994 (PDRAPER):
*        Added DODEBI to cope with situation of picking up old
*        reductions.
*     14-FEB-1994 (PDRAPER):
*        Added dark and flash exposure times.
*     10-NOV-1995 (PDRAPER):
*        Added IRFLAT for IR data reductions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NNDF
      CHARACTER * ( * ) FTYPES( 2, NNDF )
      LOGICAL DEBICR( NNDF )
      LOGICAL DARKCR( NNDF )
      DOUBLE PRECISION DRKTIM( NNDF )
      LOGICAL FLASCR( NNDF )
      DOUBLE PRECISION FLSTIM( NNDF )
      LOGICAL FLATCR( NNDF )
      LOGICAL IRFLAT

*  Arguments Given and Returned:
      LOGICAL VALID( NNDF )
      INTEGER PTEMP( NNDF )

*  Arguments Returned:
      LOGICAL MKBIAS
      LOGICAL HVBIAS
      LOGICAL DODEBI
      LOGICAL MKDARK
      LOGICAL HVDARK
      LOGICAL DODARK
      LOGICAL MKFLAS
      LOGICAL HVFLAS
      LOGICAL DOFLAS
      LOGICAL HVFLAT( NNDF )
      LOGICAL MKFLAT( NNDF )
      LOGICAL DOFLAT( NNDF )
      CHARACTER * ( * ) FILNMS( NNDF )
      INTEGER NFILS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER NFRMS              ! Number of frames
      INTEGER NMAST              ! Number of MASTER frames
      INTEGER NMATCH             ! Number of matches
      LOGICAL PROCED             ! Whether to proceed with reduction or not

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do we require a master bias? Not one already exists.
      CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'MASTER_BIAS', PTEMP,
     :                 NMAST, STATUS )
      CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'BIAS', PTEMP, NFRMS,
     :                 STATUS )
      HVBIAS = .FALSE.
      MKBIAS = .FALSE.
      IF ( NMAST .GE. 1 ) THEN

*  No we don't. We have one already.
         HVBIAS = .TRUE.
         IF ( NMAST .GT. 1 ) THEN

*  Have too many - issue WARNING.
            CALL CCD1_MSG( ' ', ' Warning - input NDF list '//
     :         'contains more than one MASTER_BIAS', STATUS )
         END IF

*  Blank out any existing BIAS frames (we do not want to use these
*  later).
         IF ( NFRMS .GT. 0 ) THEN
            DO 11 I = 1, NFRMS
               VALID( PTEMP( I ) ) = .FALSE.
 11         CONTINUE
         END IF
      ELSE

*  We need to make a master bias, have we got any frames?
         IF ( NFRMS .GE. 1 ) THEN

*  Can make one.
            MKBIAS = .TRUE.
         END IF
      END IF

*  Check for dark count frames. Look for master then look for dark
*  frames if necessary.
      MKDARK = .FALSE.
      HVDARK = .FALSE.
      CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'MASTER_DARK', PTEMP,
     :                 NMAST, STATUS )
      CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'DARK', PTEMP, NFRMS,
     :                 STATUS)
      IF ( NMAST .EQ. 0 ) THEN

*  No master, can we make one ?
         IF ( NFRMS .GE. 1 ) THEN

*  Can create a DARK calibration frame.
            MKDARK = .TRUE.
         END IF
      ELSE

*  Have a master already.
         HVDARK = .TRUE.
         IF ( NMAST .GT. 1 ) THEN

*  Have too many - issue WARNING.
            CALL CCD1_MSG( ' ', ' Warning - input NDF list contains'//
     :      ' more than one MASTER_DARK', STATUS )
         END IF

*  Blank out any existing DARK frames (we do not want to use these
*  later).
         IF ( NFRMS .GT. 0 ) THEN
            DO 12 I = 1, NFRMS
               VALID( PTEMP( I ) ) = .FALSE.
 12         CONTINUE
         END IF
      END IF

*  Check for flash count frames.
      MKFLAS = .FALSE.
      HVFLAS = .FALSE.
      CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'MASTER_FLASH', PTEMP,
     :                 NMAST, STATUS )
      CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'FLASH', PTEMP, NFRMS,
     :                 STATUS)
      IF ( NMAST .EQ. 0 ) THEN

*  No MASTER, can we make one ?
         IF ( NFRMS .GE. 1 ) THEN

*  Can create a FLASH calibration frame.
            MKFLAS = .TRUE.
         END IF
      ELSE

*  Have a master already.
         HVFLAS = .TRUE.
         IF ( NMAST .GT. 1 ) THEN

*  Have too many - issue WARNING.
            CALL CCD1_MSG( ' ', ' Warning - input NDF list contains'//
     :      ' more than one MASTER_FLASH', STATUS )
         END IF

*  Blank out any existing FLASH frames (we do not want to use these
*  later).
         IF ( NFRMS .GT. 0 ) THEN
            DO 13 I = 1, NFRMS
               VALID( PTEMP( I ) ) = .FALSE.
 13         CONTINUE
         END IF
      END IF

*  Extract list of flatfields --- note only FLAT is enabled at this
*  time.
*  Look for MASTER_calibration frames.
      CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'MASTER_FLAT', PTEMP,
     :                 NMAST, STATUS )

*  Check these lists for sense.
*  MASTERs take precedence over processing (least action principle).
*  Flat fields are most complex - look for all possible filter types.
      CALL CCD1_FLNMS( FTYPES, NNDF, VALID, HVFLAT, FILNMS, NFILS,
     :                 STATUS )

*  Initialise flags indicating the type of processing that is required
*  for each filter.
      DO 3 I = 1, NFILS
         DOFLAT( I ) = .FALSE.
         HVFLAT( I ) = .FALSE.
         MKFLAT( I ) = .FALSE.
 3    CONTINUE

*  Check MASTER_FLATs against filters looking for missing elements.
*  First reset HVFLAT workspace to FALSE. This is used to indicate which
*  MASTER_FLATs have a filter association with data, and which are still
*  required. The MKFLAT array indicates if a MASTER flat needs to be
*  made. If .NOT. HVFLAT .AND. .NOT. MKFLAT then we do not have
*  sufficient data to create a flatfield for the associated filter type.
      IF ( NFILS .GT. 0 ) THEN

*  Number of matched FILTERS and MASTERs, number of master flats which
*  are available/will be created.
         NMATCH = 0

*  Now look at MASTER_FLATs for FILTER correspondence.
         IF ( NMAST .GT. 0 ) THEN
            DO 1 I = 1, NMAST

*  Check this master against the possible filters.
               DO 2 J = 1, NFILS
                  IF ( FTYPES( 2, PTEMP( I ) ) .EQ. FILNMS( J ) ) THEN

*  This one matched flag it.
                     HVFLAT( J ) = .TRUE.
                     NMATCH = NMATCH + 1
                  END IF
 2             CONTINUE
 1          CONTINUE
         END IF

*  Mask out any FLATs which have the same filter specification as a
*  existing master.
         IF ( NMATCH .GT. 0 ) THEN
            DO 7 I = 1, NFILS
               IF ( HVFLAT( I ) ) THEN
                  CALL CCD1_LOCS3( FTYPES, 2, NNDF, 1, 2, VALID, 'FLAT',
     :                             FILNMS( I ), PTEMP, NFRMS, STATUS )
               END IF
               IF ( NFRMS .GT. 0 ) THEN
                  DO 14 J = 1, NFRMS
                     VALID( PTEMP( J ) ) = .FALSE.
 14               CONTINUE
               END IF
 7          CONTINUE
         END IF

*  Right now which flatfields need to be made, have we the associated
*  FLATs ?
         IF ( NMATCH .LT. NFILS .OR. NMATCH .EQ. 0 ) THEN

*  Look for HVFLAT requirement and check for FLATs with this filter
*  type.
            DO 6 I = 1, NFILS
               IF ( .NOT. HVFLAT( I ) ) THEN

*  No master flat for this one. Look for all FLATs.
                  CALL CCD1_LOCS3( FTYPES, 2, NNDF, 1, 2, VALID, 'FLAT',
     :                             FILNMS( I ), PTEMP, NFRMS, STATUS )

*  If no FLATs then and we're allowing TARGETs to be used as flatfields
*  then check for some with the right filter.
                  IF ( NFRMS .EQ. 0 .AND. IRFLAT ) THEN
                     CALL CCD1_LOCS3( FTYPES, 2, NNDF, 1, 2, VALID,
     :                                'TARGET', FILNMS( I ), PTEMP,
     :                                NFRMS, STATUS )
                  END IF

*  Now If number of frames is at least 1 for this filter type then
*  creating a MASTER is ok.
                  IF ( NFRMS .GE. 1 ) THEN

*  Can create a MASTER for this FILTER. Set MKFLAT and increment the
*  number of flatfields.
                     MKFLAT( I ) = .TRUE.

*  Create a list of pointers to these flatfields.
                  ELSE
                     MKFLAT( I ) = .FALSE.

*  And issue a warning that some data cannot be processed.
                     CALL MSG_SETC( 'FILTER', FILNMS( I ) )
                     CALL CCD1_MSG( ' ',
     :' Warning - a flatfield for filter ^FILTER cannot be made'//
     :' and no associated MASTER_FLAT exists; data with this filter'//
     :' type will not be flatfielded', STATUS )
                  END IF
               END IF
 6          CONTINUE
         END IF
      END IF

*  See if any form of debiassing is possible by looking at all
*  remaining valid frames and checking if they are not already
*  debiassed. Do not check masters and bias frames anyway. Any
*  frames which are normally used for masters which already exist
*  should be excluded by now so no explicit checks for these are
*  needed.
      DODEBI = .FALSE.
      DO 17 I = 1, NNDF
         IF ( VALID( I ) .AND. .NOT. DEBICR( I ) ) THEN
            IF ( FTYPES( 1, I )( 1 : 6 ) .NE. 'MASTER' ) THEN
               IF ( FTYPES( 1, I )( 1 : 4 ) .NE. 'BIAS' ) THEN
                  DODEBI = .TRUE.
               END IF
            END IF
         END IF
 17   CONTINUE

*  See if any form of dark correction is possible by looking at all
*  valid frames and checking if they are not already dark corrected.
*  Do not check masters, bias, dark frames and frames with dark exposure
*  times of zero.
      DODARK = .FALSE.
      IF ( HVDARK .OR. MKDARK ) THEN
         DO 15 I = 1, NNDF
            IF ( VALID( I ) .AND. .NOT. DARKCR( I ) ) THEN
               IF ( FTYPES( 1, I )( 1 : 6 ) .NE. 'MASTER' ) THEN
                  IF ( FTYPES( 1, I )( 1 : 4 ) .NE. 'BIAS' ) THEN
                     IF ( FTYPES( 1, I )( 1 : 4 ) .NE. 'DARK' ) THEN
                        IF ( DRKTIM( I ) .GT. 0.0D0 ) THEN
                           DODARK = .TRUE.
                        END IF
                     END IF
                  END IF
               END IF
            END IF
 15      CONTINUE
      END IF

*  See if any form of pre-flash correction is possible by looking at all
*  valid frames and checking if they are not already corrected.
*  Do not check masters, bias, dark, flash frames and frames with
*  pre-flash exposure times of zero.
      DOFLAS = .FALSE.
      IF ( HVFLAS .OR. MKFLAS ) THEN
         DO 18 I = 1, NNDF
            IF ( VALID( I ) .AND. .NOT. FLASCR( I ) ) THEN
               IF ( FTYPES( 1, I )( 1:6 ) .NE. 'MASTER' ) THEN
                  IF ( FTYPES( 1, I )( 1:4 ) .NE. 'BIAS' ) THEN
                     IF ( FTYPES( 1, I )( 1:4 ) .NE. 'DARK' ) THEN
                        IF ( FTYPES( 1, I )( 1:5 ) .NE. 'FLASH' ) THEN
                           IF ( FLSTIM( I ) .GT. 0.0D0 ) THEN
                              DOFLAS = .TRUE.
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END IF
 18      CONTINUE
      END IF

*  Are there any frames which require flatfielding? Only TARGETs may
*  be flatfielded.
      IF ( NFILS .GT. 0 ) THEN
         DO 19 J = 1, NFILS
            DOFLAT( J ) = .FALSE.
            IF ( MKFLAT( J ) .OR. HVFLAT( J ) ) THEN
               DO 16 I = 1, NNDF
                  IF ( VALID( I ) .AND. .NOT. FLATCR( I ) ) THEN
                     IF ( FTYPES( 1, I )( 1:6 ) .EQ. 'TARGET' ) THEN
                        IF ( FTYPES( 2, I ) .EQ. FILNMS( J ) ) THEN
                           DOFLAT( J )  = .TRUE.
                        END IF
                     END IF
                  END IF
 16            CONTINUE
            END IF
 19      CONTINUE
      END IF

*  Final check. If no processing of any description is possible then
*  set status issue an error message and exit.
      PROCED = MKBIAS .OR. DODEBI .OR. MKDARK .OR. DODARK .OR.
     :         MKFLAS .OR. DOFLAS
      IF ( .NOT. PROCED ) THEN
         IF ( NFILS .GT. 0 ) THEN
            DO 20 I = 1, NFILS
               IF ( MKFLAT( I ) .OR. DOFLAT( I ) ) THEN
                  PROCED = .TRUE.
               END IF
 20         CONTINUE
         END IF
      END IF

*  If no processing is required set status and exit.
      IF ( .NOT. PROCED .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_SCHEDNOGO', '  No reduction is necessary',
     :                 STATUS )
      END IF
 99   CONTINUE
      END
* $Id$
