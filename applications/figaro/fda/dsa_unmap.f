      SUBROUTINE DSA_UNMAP( MSLOT, STATUS )
*+
*  Name:
*     DSA_UNMAP

*  Purpose:
*     Unmap a previously mapped array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_UNMAP( MSLOT, STATUS )

*  Description:
*     This routine can be called to explicitly unmap an array
*     previously mapped by one of the DSA mapping routines. This is
*     usually not necessary, since DSA_CLOSE will take care of that for
*     all mapped arrays.
*
*     Contrary to earlier implementations, data and quality cannot be
*     unmapped separately. The second unmap call would result in an
*     error.

*  Arguments:
*     MSLOT = INTEGER (Given)
*        The map slot that was returned by the mapping routine to its
*        caller. This is a handle into the DSA global variables, which
*        in turn enable this routine to take the proper action.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine will try to function even if
*        entered with a bad status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     08 Jul 1987 (ks):
*        Original version.
*     20 Jul 1988 (ks):
*        Reflagging of unflagged data added, and type conversion now
*        done through CNV_FMTCON.
*     28 Jun 1989 (ks):
*        Check for variance/error processing added.
*     08 Sep 1989 (ks):
*        Control of flag value propagation added.
*     12 Mar 1990 (ks):
*        Support for variance arrays mapped as uncertainty arrays added.
*     27 Apr 1990 (ks):
*        Support for some types of SGP38 structured arrays added.
*        MAP_STRUCT changed to MAP_SCALED.
*     21 Aug 1992 (hme):
*        Replace CNV_ with DSA_FMTCON, which calls VEC_ routines.
*     31 Aug 1992 (ks):
*        Added use of DSA_WRFLUSH.
*     01 Sep 1992 (ks):
*        Usused variable declarations removed.
*     24 Feb 1993 (ks):
*        Arrays are now passed to DSA_FMTCON as arrays instead
*        of by pointer.
*     02 Jul 1993 (ks):
*        Now closes down any workspace used to hold flagged
*        data information.
*     26 Nov 1995 (hme):
*        FDA library.
*     18 Dec 1995 (hme):
*        Review unmapping. Will now always unmap quality and data together
*        and will always use the reference slot as well as the map slot.
*     02 Feb 1996 (hme):
*        Unmap axis centres (N-D or not).
*     19 Feb 1996 (hme):
*        Move the work into DSA1_UNMAP.
*        Translate between application-side status and Starlink status.
*     20 Feb 1996 (hme):
*        Unmap axis widths.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISTAT              ! Global status

*.

*  Begin error context and translate status.
      ISTAT = STATUS
      CALL ERR_MARK
      STATUS = SAI__OK

*  Call the work routine.
      CALL DSA1_UNMAP( MSLOT, STATUS )

*  Translate status and end error context.
      IF ( ISTAT .NE. 0 ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = ISTAT
      END IF
      CALL ERR_RLSE

*  Return.
      END



      SUBROUTINE DSA1_UNMAP( MSLOT, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! Reference slot
      INTEGER NELM               ! Array size
      CHARACTER * ( NDF__SZTYP ) NDFTY1 ! The type in NDF speak
      CHARACTER * ( NDF__SZTYP ) NDFTY2 ! The type in NDF speak

*.

*  Begin error context.
      CALL ERR_BEGIN( STATUS )

*  Check map slot number in range and used.
      IF ( MSLOT .LT. 1 .OR. MSLOT .GT. DSA__MAXMAP ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T006', MSLOT )
         CALL ERR_REP( 'FDA_E050', 'DSA1_UNMAP: Attempt to ' //
     :      'unmap invalid map slot number ^FDA_T006.', STATUS )
         GO TO 500
      END IF
      IF ( .NOT. DSA__MAPUSD(MSLOT) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T006', MSLOT )
         CALL ERR_REP( 'FDA_E051', 'DSA1_UNMAP: Attempt to ' //
     :      'unmap the unused map slot ^FDA_T006.', STATUS )
         GO TO 500
      END IF

*  Check that this is not a work space.
      IF ( DSA__MAPREF(MSLOT) .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T006', MSLOT )
         CALL ERR_REP( 'FDA_E052', 'DSA1_UNMAP: Attempt ' //
     :      'to release a work space as if it were data ' //
     :      '(slot ^FDA_T006).', STATUS )
         GO TO 500
      END IF


*  Action for NDF component (excluding axis).
*  ==========================================

      IF ( DSA__MAPAXI(MSLOT) .LE. 0 ) THEN

*     Unmap the nominated components of the nominated NDF.
         CALL NDF_UNMAP( DSA__MAPID1(MSLOT), DSA__MAPNAM(MSLOT),
     :      STATUS )

*     If this unmaps the data (and quality) component, then clean up the
*     reference slot accordingly. The array pointers must be reset. And if
*     the data/quality buffer NDF exists, it must now be annulled. In some
*     cases that is rather important, since it is possible that above we
*     unmapped only the data component, but that the quality component is
*     still hiding in the buffer NDF.
*     If we spot errors in this, we do not bail out, but clear the map
*     slot in any case.
         IF ( DSA__MAPNAM(MSLOT)(:4) .EQ. 'DATA' ) THEN
            SLOT = DSA__MAPREF(MSLOT)
            IF ( SLOT .GE. 1 .AND. SLOT .LE. DSA__MAXREF ) THEN
               IF ( DSA__REFUSD(SLOT) ) THEN
                  DSA__REFDPT(SLOT) = 0
                  DSA__REFQPT(SLOT) = 0
                  IF ( DSA__REFID2(SLOT) .NE. NDF__NOID )
     :               CALL NDF_ANNUL( DSA__REFID2(SLOT), STATUS )
               ELSE
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'FDA_T006', MSLOT )
                  CALL MSG_SETI( 'FDA_T015', SLOT )
                  CALL ERR_REP( 'FDA_E053', 'DSA1_UNMAP: The map ' //
     :               'slot ^FDA_T006 refers to an unused reference ' //
     :               'slot ^FDA_T015.', STATUS )
               END IF
            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'FDA_T006', MSLOT )
               CALL MSG_SETI( 'DSA_T015', SLOT )
               CALL ERR_REP( 'FDA_E054', 'DSA1_UNMAP: The map ' //
     :            'slot ^FDA_T006 refers to an invalid reference ' //
     :            'slot ^FDA_T015.', STATUS )
            END IF

*     If this unmaps the variance component, then clean up the
*     reference slot accordingly. If the variance buffer NDF exists
*     it must now be annulled.
*     If we spot errors in this, we do not bail out, but clear the map
*     slot in any case.
         ELSE IF ( DSA__MAPNAM(MSLOT)(:4) .EQ. 'VARIANCE' ) THEN
            SLOT = DSA__MAPREF(MSLOT)
            IF ( SLOT .GE. 1 .AND. SLOT .LE. DSA__MAXREF ) THEN
               IF ( DSA__REFUSD(SLOT) ) THEN
                  IF ( DSA__REFID3(SLOT) .NE. NDF__NOID )
     :               CALL NDF_ANNUL( DSA__REFID3(SLOT), STATUS )
               ELSE
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'FDA_T006', MSLOT )
                  CALL MSG_SETI( 'FDA_T015', SLOT )
                  CALL ERR_REP( 'FDA_E053', 'DSA1_UNMAP: The map ' //
     :               'slot ^FDA_T006 refers to an unused ' //
     :               'reference slot ^FDA_T015.', STATUS )
               END IF
            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'FDA_T006', MSLOT )
               CALL MSG_SETI( 'FDA_T015', SLOT )
               CALL ERR_REP( 'FDA_E054', 'DSA1_UNMAP: The map slot ' //
     :            '^FDA_T006 refers to an invalid reference slot ' //
     :            '^FDA_T015.', STATUS )
            END IF
         END IF


*  Action for an axis component (including an N-D axis).
*  =====================================================

      ELSE

*     If the component is 'N*' then we have an N-D centre or width
*     to unmap.
         IF ( DSA__MAPNAM(MSLOT)(:1) .EQ. 'N' ) THEN

*        If both locators valid, copy data from second to first pointer.
*        This is the back conversion in case of update access, and the
*        conversion in case of write access.
            IF ( DSA__MAPLO1(MSLOT) .NE. DAT__NOLOC .AND.
     :           DSA__MAPLO2(MSLOT) .NE. DAT__NOLOC ) THEN
               CALL DAT_TYPE( DSA__MAPLO1(MSLOT), NDFTY1, STATUS )
               CALL DAT_TYPE( DSA__MAPLO2(MSLOT), NDFTY2, STATUS )
               CALL DAT_SIZE( DSA__MAPLO2(MSLOT), NELM,   STATUS )
               CALL DSA1_CPDAT( NDFTY2, NDFTY1, NELM,
     :                          %VAL( CNF_PVAL(DSA__MAPPT2(MSLOT)) ),
     :                          %VAL( CNF_PVAL(DSA__MAPPT1(MSLOT)) ),
     :                          STATUS )
            END IF

*        Unmap and annull each valid locator.
            IF ( DSA__MAPLO2(MSLOT) .NE. DAT__NOLOC ) THEN
               CALL DAT_UNMAP( DSA__MAPLO2(MSLOT), STATUS )
               CALL DAT_ANNUL( DSA__MAPLO2(MSLOT), STATUS )
            END IF
            IF ( DSA__MAPLO1(MSLOT) .NE. DAT__NOLOC ) THEN
               CALL DAT_UNMAP( DSA__MAPLO1(MSLOT), STATUS )
               CALL DAT_ANNUL( DSA__MAPLO1(MSLOT), STATUS )
            END IF

*     Else (the component is a genuine axis component)
         ELSE

*        We unmap it by NDF identifier, axis number and component name.
            CALL NDF_AUNMP( DSA__MAPID1(MSLOT), DSA__MAPNAM(MSLOT),
     :         DSA__MAPAXI(MSLOT), STATUS )

         END IF

      END IF


*  Reset the map slot.
*  ===================

      DSA__MAPUSD(MSLOT) = .FALSE.
      DSA__MAPID1(MSLOT) = NDF__NOID
      DSA__MAPAXI(MSLOT) = 0
      DSA__MAPREF(MSLOT) = 0
      DSA__MAPPT1(MSLOT) = 0
      DSA__MAPPT2(MSLOT) = 0
      DSA__MAPNAM(MSLOT) = ' '
      DSA__MAPLO1(MSLOT) = DAT__NOLOC
      DSA__MAPLO2(MSLOT) = DAT__NOLOC

*  Return.
 500  CONTINUE
      CALL ERR_END( STATUS )
      END
