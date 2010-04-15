      SUBROUTINE DSA1_MDQ( SLOT, MODE, NDFTYP, ADDRESS, MSLOT, STATUS )
*+
*  Name:
*     DSA1_MDQ

*  Purpose:
*     Map an NDF's data and quality, cleaning bad values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_MDQ( SLOT, MODE, NDFTYP, ADDRESS, MSLOT, STATUS )

*  Description:
*     This routine maps the main data array in an NDF. This routine
*     makes sure there are no bad values in the mapped array.
*
*     At the same time (just before the data), the quality array is
*     mapped. Information about this is not returned, but buffered in
*     the DSA common block for a later call to DSA_MAP_QUALITY.

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
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26 Nov 1995 (hme):
*        Original version.
*     12 Dec 1995 (hme):
*        For write and update access set the bad pixel flag to false.
*     13 Dec 1995 (hme):
*        For new and zero-filled quality components set bad bit mask
*        of 255.
*        For write access, zero-fill quality.
*        No longer clean the data. Apparently DSA doesn't do it either.
*     14 Dec 1995 (hme):
*        Review again. The processing should not follow the DSA example,
*        but fewer, simpler, firmer principles.
*        Basically, first merge quality into bad values, then merge bad
*        values into quality, then clean the data. The principle involved
*        here is that of equivalence of bad values and quality.
*        Correct check for contamination. In this routine EXIST shows
*        only bad values in original data, since quality is already mapped.
*        The correct contamination indicator is therefore EXIST.OR.QTHERE.
*     18 Dec 1995 (hme):
*        Update use of common block to enable unmapping. Data and quality
*        are regarded as one entity and will be unmapped in a single call.
*     05 Feb 1996 (hme):
*        For write access looking for bad values may be fatal. Therefore
*        call NDF_BAD only if access is not write.
*     09 Feb 1996 (hme):
*        When using a D/Q buffer NDF, always use both from the
*        buffer none from the original. That way existence of the buffer
*        indicates that its data component should be searched by
*        DSA_SEEK_FLAGGED_VALUES.
*     1996 July 12 (MJC):
*        Test for the existence of the data array in update mode.  DSA
*        is more relaxed and appears to allow update mode for a new
*        data array.  However, the NDF library gives an error.  So to
*        avoid changing numerous calls in applications the mode is
*        changed to write access in this case.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     2005 Aug 15 (TIMJ):
*        Use NUM__MAXUB for BADBIT initialiser. More portable.
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

*  Find free map slot.
      CALL DSA1_MSNEW( MSLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Test whether the data array exists for update mode.  If it does not
*  exist, the effective mode is set to write otherwise this will fail.
*  This is to mimic the laissez-faire approach of DSA.   Make an upper
*  copy of the effective mode.
      CALL NDF_STATE( DSA__REFID1(SLOT), 'DATA', DTHERE, STATUS )
      EMODE = MODE
      CALL CHR_UCASE( EMODE )
      IF ( EMODE(:1) .EQ. 'U' .AND. .NOT. DTHERE ) EMODE = 'WRITE'

*  Find out if quality pre-exists.
*  Initially map quality if there, then data (using the effective mode).
*  Then find out if data are contaminated.
      CALL NDF_STATE( DSA__REFID1(SLOT), 'QUALITY', QTHERE, STATUS )
      IF ( QTHERE )
     :   CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE', MODE,
     :      QPTR, NELM, STATUS )
      CALL NDF_MAP( DSA__REFID1(SLOT), 'DATA', NDFTYP, EMODE,
     :   ADDRESS, NELM, STATUS )
      IF ( EMODE(:1) .NE. 'W' )
     :   CALL NDF_BAD( DSA__REFID1(SLOT), 'DATA', .TRUE., EXIST,
     :   STATUS )


*  If read access to data with bad values.
*  =======================================

      IF ( EMODE(:1) .EQ. 'R' ) THEN
         IF ( EXIST .OR. QTHERE ) THEN

*        Release original data and quality.
*        Get a temporary NDF to hold the data and quality.
*        Copy the data and quality, access the copies for update.
*        Merge quality into bad values.
*        Merge bad values into quality.
*        Clean the data.
*        Fill in reference slot and map slot.
            CALL NDF_UNMAP( DSA__REFID1(SLOT), 'DATA', STATUS )
            IF ( QTHERE )
     :         CALL NDF_UNMAP( DSA__REFID1(SLOT), 'QUALITY', STATUS )
            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_SCOPY( DSA__REFID1(SLOT),
     :         'DATA,QUALITY,NOTITLE,NOLABEL,NOHISTORY,NOEXTENSION()',
     :         PLACE, DSA__REFID2(SLOT), STATUS )
            IF ( QTHERE ) THEN
               CALL NDF_MAP( DSA__REFID2(SLOT), 'QUALITY', '_UBYTE',
     :            'UPDATE', QPTR, NELM, STATUS )
            ELSE
               CALL NDF_MAP( DSA__REFID2(SLOT), 'QUALITY', '_UBYTE',
     :            'WRITE/ZERO', QPTR, NELM, STATUS )
               CALL NDF_SBB( BADBIT, DSA__REFID2(SLOT), STATUS )
            END IF
            CALL NDF_MAP( DSA__REFID2(SLOT), 'DATA', NDFTYP, 'UPDATE',
     :         ADDRESS, NELM, STATUS )
            CALL DSA1_MRGFQ( NDFTYP, NELM, %VAL( CNF_PVAL(ADDRESS) ),
     :         %VAL( CNF_PVAL(QPTR) ), STATUS )
            CALL DSA1_CLEAN( SLOT, NDFTYP, NELM,
     :                       %VAL( CNF_PVAL(ADDRESS) ), STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__REFDPT(   SLOT ) = ADDRESS
               DSA__REFQPT(   SLOT ) = QPTR
               DSA__MAPUSD(  MSLOT ) = .TRUE.
               DSA__MAPID1(  MSLOT ) = DSA__REFID2(SLOT)
               DSA__MAPREF(  MSLOT ) = SLOT
               DSA__MAPNAM(  MSLOT ) = 'DATA,QUALITY'
            END IF


*  Else if read access, but no bad values present.
*  ===============================================

         ELSE

*        If no quality yet.
            IF ( .NOT. QTHERE ) THEN

*           Release original data.
*           Get a temporary NDF to hold the data and quality.
*           Copy the data.
*           Access the quality WRITE/ZERO.
*           Access the data copy.
*           Fill in reference slot and map slot.
               CALL NDF_UNMAP( DSA__REFID1(SLOT), 'DATA', STATUS )
               CALL NDF_TEMP( PLACE, STATUS )
               CALL NDF_SCOPY( DSA__REFID1(SLOT),
     :            'DATA,NOTITLE,NOLABEL,NOHISTORY,NOEXTENSION()',
     :            PLACE, DSA__REFID2(SLOT), STATUS )
               CALL NDF_MAP( DSA__REFID2(SLOT), 'QUALITY', '_UBYTE',
     :            'WRITE/ZERO', QPTR, NELM, STATUS )
               CALL NDF_SBB( BADBIT, DSA__REFID2(SLOT), STATUS )
               CALL NDF_MAP( DSA__REFID2(SLOT), 'DATA', NDFTYP, MODE,
     :            ADDRESS, NELM, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DSA__REFDPT(   SLOT ) = ADDRESS
                  DSA__REFQPT(   SLOT ) = QPTR
                  DSA__MAPUSD(  MSLOT ) = .TRUE.
                  DSA__MAPID1(  MSLOT ) = DSA__REFID2(SLOT)
                  DSA__MAPREF(  MSLOT ) = SLOT
                  DSA__MAPNAM(  MSLOT ) = 'DATA,QUALITY'
               END IF

*        Else.
            ELSE

*           Fill in reference slot and map slot.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DSA__REFDPT(   SLOT ) = ADDRESS
                  DSA__REFQPT(   SLOT ) = QPTR
                  DSA__MAPUSD(  MSLOT ) = .TRUE.
                  DSA__MAPID1(  MSLOT ) = DSA__REFID1(SLOT)
                  DSA__MAPREF(  MSLOT ) = SLOT
                  DSA__MAPNAM(  MSLOT ) = 'DATA,QUALITY'
               END IF

            END IF

         END IF


*  Else if update access to data with bad values.
*  ==============================================

      ELSE IF ( EMODE(:1) .EQ. 'U' ) THEN
         IF ( EXIST .OR. QTHERE ) THEN

*        If no quality yet, access it write/zero.
*        Merge quality into bad values.
*        Merge bad values into quality.
*        Clean the data.
*        Set bad pixel flag.
*        Fill in reference slot and map slot.
            IF ( .NOT. QTHERE ) THEN
               CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE',
     :            'WRITE/ZERO', QPTR, NELM, STATUS )
               CALL NDF_SBB( BADBIT, DSA__REFID1(SLOT), STATUS )
            END IF
            CALL DSA1_MRGFQ( NDFTYP, NELM, %VAL( CNF_PVAL(ADDRESS) ),
     :         %VAL( CNF_PVAL(QPTR) ), STATUS )
            CALL DSA1_CLEAN( SLOT, NDFTYP, NELM,
     :                       %VAL( CNF_PVAL(ADDRESS) ), STATUS )
            CALL NDF_SBAD( .FALSE., DSA__REFID1(SLOT), 'DATA', STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__REFDPT(   SLOT ) = ADDRESS
               DSA__REFQPT(   SLOT ) = QPTR
               DSA__MAPUSD(  MSLOT ) = .TRUE.
               DSA__MAPID1(  MSLOT ) = DSA__REFID1(SLOT)
               DSA__MAPREF(  MSLOT ) = SLOT
               DSA__MAPNAM(  MSLOT ) = 'DATA,QUALITY'
            END IF


*  Else if update access, but no bad values present.
*  =================================================

         ELSE

*        If no quality yet, access it write/zero.
*        Set bad pixel flag.
*        Fill in reference slot and map slot.
            IF ( .NOT. QTHERE ) THEN
               CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE',
     :            'WRITE/ZERO', QPTR, NELM, STATUS )
               CALL NDF_SBB( BADBIT, DSA__REFID1(SLOT), STATUS )
            END IF
            CALL NDF_SBAD( .FALSE., DSA__REFID1(SLOT), 'DATA', STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__REFDPT(   SLOT ) = ADDRESS
               DSA__REFQPT(   SLOT ) = QPTR
               DSA__MAPUSD(  MSLOT ) = .TRUE.
               DSA__MAPID1(  MSLOT ) = DSA__REFID1(SLOT)
               DSA__MAPREF(  MSLOT ) = SLOT
               DSA__MAPNAM(  MSLOT ) = 'DATA,QUALITY'
            END IF

         END IF

*  Else (write access)
*  ===================

      ELSE

*     If no quality yet, access it write/zero.
*     Else set it zero.
*     Set bad pixel flag.
*     Fill in reference slot and map slots.
         IF ( .NOT. QTHERE ) THEN
            CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE',
     :         'WRITE/ZERO', QPTR, NELM, STATUS )
         ELSE
            CALL DSA1_QZERO( NELM, %VAL( CNF_PVAL(QPTR) ), STATUS )
         END IF
         CALL NDF_SBB( BADBIT, DSA__REFID1(SLOT), STATUS )
         CALL NDF_SBAD( .FALSE., DSA__REFID1(SLOT), 'DATA', STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            DSA__REFDPT(   SLOT ) = ADDRESS
            DSA__REFQPT(   SLOT ) = QPTR
            DSA__MAPUSD(  MSLOT ) = .TRUE.
            DSA__MAPID1(  MSLOT ) = DSA__REFID1(SLOT)
            DSA__MAPREF(  MSLOT ) = SLOT
            DSA__MAPNAM(  MSLOT ) = 'DATA,QUALITY'
         END IF

      END IF


*  Return.
*  =======

 500  CONTINUE
      END

      SUBROUTINE DSA1_QZERO( NELM, QUAL, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER NELM
      BYTE QUAL( NELM )
      INTEGER STATUS

      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         QUAL(I) = 0
 1    CONTINUE

      END
