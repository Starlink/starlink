      SUBROUTINE DSA1_MDAT( SLOT, MODE, NDFTYP, ADDRESS, MSLOT, STATUS )
*+
*  Name:
*     DSA1_MDAT

*  Purpose:
*     Map the main data array in an NDF, cleaning bad values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_MDAT( SLOT, MODE, NDFTYP, ADDRESS, SLOT, STATUS )

*  Description:
*     This routine maps the main data array in an NDF, returning
*     the address of the dynamic memory array that may be used to
*     access it. This routine makes sure there are no bad values in
*     the mapped array. If necessary, bad-value information will be
*     moved to a separate quality array, which for update access may
*     become permanent.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The reference slot associated with the NDF.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode, 'READ', 'WRITE', or 'UPDATE'.
*     NDFTYP = CHARACTER * ( * ) (Given)
*        The data type in NDF speak to be used to map the array. E.g.
*        '_REAL' or '_DOUBLE'.
*     ADDRESS = INTEGER (Returned)
*        The memory address of the mapped array.
*     MSLOT = INTEGER (Returned)
*        The map slot, a handle that can be used to unmap this array
*        later.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25 Nov 1995 (hme):
*        Original version.
*     12 Dec 1995 (hme):
*        For write and update access set the bad pixel flag to false.
*     13 Dec 1995 (hme):
*        For created qualities set a bad bit mask of 255. This happens
*        for update access to contaminated data but so far without
*        quality. In that case the data are cleaned and the bad pixel
*        information is made permanent in a new quality component.
*        The tentative mapping of data at the beginning, which is in some
*        circumstances only to check for bad values, must be done with
*        read access. Otherwise the quality will be merged into data
*        at that time and permanently.
*        However, for write access the data may not yet exist. Since write
*        is a simple case anyway, omit the preliminary mapping.
*     14 Dec 1995 (hme):
*        Review again. The processing should not follow the DSA example,
*        but fewer, simpler, firmer principles.
*        Basically, first merge quality into bad values, then merge bad
*        values into quality, then clean the data. The principle involved
*        here is that of equivalence of bad values and quality.
*     18 Dec 1995 (hme):
*        In case of write access or update access to clean data,
*        delete quality.
*        Update use of common block to enable unmapping. Data and quality
*        are regarded as one entity and will be unmapped in a single call.
*     1996 July 12 (MJC):
*        Test for the existence of the data array in update mode.  DSA
*        is more relaxed and appears to allow update mode for a new
*        data array.  However, the NDF library gives an error.  So to
*        avoid changing numerous calls in applications the mode is
*        changed to write access in this case.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     2005 Aug 15 (TIMJ):
*        Use NUM__MAXUB rather than integer initialisation for BADBIT.
*        255 is out of range for a signed byte
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'PRM_PAR'          ! For NUM__MAXUB

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER SLOT
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) NDFTYP

*  Arguments Returned:
      INTEGER ADDRESS
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE BADBIT
      PARAMETER ( BADBIT = NUM__MAXUB ) ! Bad bit mask for new quality

*  Local Variables:
      LOGICAL DTHERE             ! Whether or not there is a data array
      CHARACTER * ( NDF__SZACC ) EMODE ! Effective access mode
      LOGICAL EXIST              ! Whether there are bad values
      INTEGER NELM               ! Mapped array size
      INTEGER PLACE              ! NDF placeholder
      INTEGER QPTR               ! Quality array pointer
      LOGICAL QTHERE             ! Whether or not a quality array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Preliminaries.
*  ==============

*  Find map slot, see if quality might be there for use.
      CALL DSA1_MSNEW( MSLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL NDF_STATE( DSA__REFID1(SLOT), 'QUALITY', QTHERE, STATUS )

*  Test whether the data array exists for update mode.  If it does not
*  exist, the effective mode is set to write otherwise this will fail.
*  This is to mimic the laissez-faire approach of DSA.   Make an upper
*  copy of the effective mode.
      CALL NDF_STATE( DSA__REFID1(SLOT), 'DATA', DTHERE, STATUS )
      EMODE = MODE
      CALL CHR_UCASE( EMODE )
      IF ( EMODE(:1) .EQ. 'U' .AND. .NOT. DTHERE ) EMODE = 'WRITE'


*  If not write access.
*  ====================

      IF ( EMODE(:1) .EQ. 'R' .OR. EMODE(:1) .EQ. 'U' ) THEN

*     Map data cautiously and see if bad values present.
         CALL NDF_MAP( DSA__REFID1(SLOT), 'DATA', NDFTYP, 'READ',
     :      ADDRESS, NELM, STATUS )
         CALL NDF_BAD( DSA__REFID1(SLOT), 'DATA', .TRUE.,
     :      EXIST, STATUS )


*     If read access to data with bad values.
*     =======================================

         IF ( EMODE(:1) .EQ. 'R' ) THEN
            IF ( EXIST ) THEN

*           Discard original data.
*           Get a temporary NDF to hold the data and quality.
*           Map data (merging quality in).
*           Clean the data.
*           Fill in reference slot and map slot.
               CALL NDF_UNMAP( DSA__REFID1(SLOT), 'DATA', STATUS )
               CALL NDF_TEMP( PLACE, STATUS )
               CALL NDF_SCOPY( DSA__REFID1(SLOT),
     :            'DATA,QUALITY,NOTITLE,NOLABEL,' //
     :            'NOHISTORY,NOEXTENSION()',
     :            PLACE, DSA__REFID2(SLOT), STATUS )
               CALL NDF_MAP( DSA__REFID2(SLOT), 'DATA', NDFTYP,
     :            'UPDATE', ADDRESS, NELM, STATUS )
               CALL DSA1_CLEAN( SLOT, NDFTYP, NELM,
     :            %VAL( CNF_PVAL(ADDRESS) ), STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DSA__REFDPT(SLOT) = ADDRESS
                  DSA__MAPUSD(MSLOT) = .TRUE.
                  DSA__MAPID1(MSLOT) = DSA__REFID2(SLOT)
                  DSA__MAPREF(MSLOT) = SLOT
                  DSA__MAPNAM(MSLOT) = 'DATA'
               END IF


*     Else if read access, but no bad values present.
*     ===============================================

            ELSE

*           Fill in reference slot and map slot.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DSA__REFDPT(SLOT) = ADDRESS
                  DSA__MAPUSD(MSLOT) = .TRUE.
                  DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
                  DSA__MAPREF(MSLOT) = SLOT
                  DSA__MAPNAM(MSLOT) = 'DATA'
               END IF

            END IF


*     Else if update access to data with bad values.
*     ==============================================

         ELSE IF ( EMODE(:1) .EQ. 'U' ) THEN
            IF ( EXIST ) THEN

*           Unmap the data again.
*           Map quality for update or write/zero.
*           Re-map data for update.
*           Merge quality into bad values.
*           Merge bad values into quality.
*           Clean the data.
*           Set bad pixel flag.
*           Fill in reference slot and map slot.
               CALL NDF_UNMAP( DSA__REFID1(SLOT), 'DATA', STATUS )
               IF ( QTHERE ) THEN
                  CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE',
     :               MODE, QPTR, NELM, STATUS )
               ELSE
                  CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE',
     :               'WRITE/ZERO', QPTR, NELM, STATUS )
                  CALL NDF_SBB( BADBIT, DSA__REFID1(SLOT), STATUS )
               END IF
               CALL NDF_MAP( DSA__REFID1(SLOT), 'DATA', NDFTYP, MODE,
     :            ADDRESS, NELM, STATUS )
               CALL DSA1_MRGFQ( NDFTYP, NELM, %VAL( CNF_PVAL(ADDRESS) ),
     :            %VAL( CNF_PVAL(QPTR) ), STATUS )
               CALL DSA1_CLEAN( SLOT, NDFTYP, NELM,
     :            %VAL( CNF_PVAL(ADDRESS) ), STATUS )
               CALL NDF_SBAD( .FALSE., DSA__REFID1(SLOT), 'DATA',
     :            STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DSA__REFDPT(SLOT) = ADDRESS
                  DSA__REFQPT(SLOT) = QPTR
                  DSA__MAPUSD(MSLOT) = .TRUE.
                  DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
                  DSA__MAPREF(MSLOT) = SLOT
                  DSA__MAPNAM(MSLOT) = 'DATA,QUALITY'
               END IF


*     Else if update access, but no bad values present.
*     =================================================

            ELSE

*           Unmap the data and re-map them with the proper access mode.
*           Delete quality in the meantime.
*           Set bad pixel flag.
*           Fill in reference slot and map slot.
               CALL NDF_UNMAP( DSA__REFID1(SLOT), 'DATA', STATUS )
               IF ( QTHERE )
     :            CALL NDF_RESET( DSA__REFID1(SLOT), 'QUALITY', STATUS )
               CALL NDF_MAP( DSA__REFID1(SLOT), 'DATA', NDFTYP, MODE,
     :            ADDRESS, NELM, STATUS )
               CALL NDF_SBAD( .FALSE., DSA__REFID1(SLOT), 'DATA',
     :            STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DSA__REFDPT(SLOT) = ADDRESS
                  DSA__MAPUSD(MSLOT) = .TRUE.
                  DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
                  DSA__MAPREF(MSLOT) = SLOT
                  DSA__MAPNAM(MSLOT) = 'DATA'
               END IF

            END IF

         END IF

*  Else (write access)
*  ===================

      ELSE

*     Map data with the proper access mode.  This might have been an
*     supplied update mode for a new data array, so use the effective
*     mode.
*     Delete quality.
*     Set bad pixel flag.
*     Fill in reference slot and map slot.
         CALL NDF_MAP( DSA__REFID1(SLOT), 'DATA', NDFTYP, EMODE,
     :      ADDRESS, NELM, STATUS )
         IF ( QTHERE )
     :      CALL NDF_RESET( DSA__REFID1(SLOT), 'QUALITY', STATUS )
         CALL NDF_SBAD( .FALSE., DSA__REFID1(SLOT), 'DATA', STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            DSA__REFDPT(SLOT) = ADDRESS
            DSA__MAPUSD(MSLOT) = .TRUE.
            DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
            DSA__MAPREF(MSLOT) = SLOT
            DSA__MAPNAM(MSLOT) = 'DATA'
         END IF

      END IF


*  Return.
*  =======

 500  CONTINUE
      END
