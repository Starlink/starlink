      SUBROUTINE DSA1_MDQF( SLOT, MODE, NDFTYP, ADDRESS, MSLOT, STATUS )
*+
*  Name:
*     DSA1_MDQF

*  Purpose:
*     Map an NDF's data accepting bad values, and map quality.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_MDQF( SLOT, MODE, NDFTYP, ADDRESS, MSLOT, STATUS )

*  Description:
*     This routine maps the main data array in an NDF, returning
*     the address of the dynamic memory array that may be used to
*     access it. The data array is accepted as it is, including bad
*     values.
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
*     13 Dec 1995 (hme):
*        For write and update access set the bad pixel flag to true.
*        This is in anticipation of the calling routine introducing
*        bad values. There is no great harm in this, since the flag
*        being true means only that there _may_ be bad values. This
*        setting is necessary, since dsa1_mdat and dsa1_mdq make the
*        opposite setting. If that setting were to persist the flag
*        might become wrong when an application uses flagged values and
*        actually introduces some.
*        For write access set a bad bit mask of 255 for the quality.
*     14 Dec 1995 (hme):
*        Had forgotten to set bad bit mask when update access creates a
*        new quality.
*     18 Dec 1995 (hme):
*        Had forgotten to set bad bit mask when read access creates a
*        temporary quality.
*        Update use of common block to enable unmapping. Data and quality
*        are regarded as one entity and will be unmapped in a single call.
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
*     2005 Aug 15 (TIMJ):
*        Use NUM__MAXUB for BADBIT initialiser. This is defined to be FF
*        whereas 255 is out of range for signed byte.
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
      INCLUDE 'PRM_PAR'          ! NUM__ constants

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
      INTEGER PLACE              ! NDF placeholder
      INTEGER NELM               ! Mapped array size
      INTEGER QPTR               ! Quality array pointer
      LOGICAL QTHERE             ! Whether or not a quality array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Preliminaries.
*  ==============

*  Find free map slot.
      CALL DSA1_MSNEW(  MSLOT, STATUS )

*  Find out if quality pre-exists.
      CALL NDF_STATE( DSA__REFID1(SLOT), 'QUALITY', QTHERE, STATUS )

*  Test whether the data array exists for update mode.  If it does not
*  exist, the effective mode is set to write otherwise this will fail.
*  This is to mimic the laissez-faire approach of DSA.   Make an upper
*  copy of the effective mode.
      CALL NDF_STATE( DSA__REFID1(SLOT), 'DATA', DTHERE, STATUS )
      EMODE = MODE
      CALL CHR_UCASE( EMODE )
      IF ( EMODE(:1) .EQ. 'U' .AND. .NOT. DTHERE ) EMODE = 'WRITE'


*  In essence we map first quality then data.  However, there are
*  certain guarantees about quality that DSA makes, but NDF doesn't.
*  For read access and absent quality, NDF would return an error.  So
*  we have to create a temporary NDF with zeros instead.  For write
*  access we have to initialise quality to zero.  For update access we
*  may have to initialise if quality does not yet exist.


*  If read access to absent quality.
*  =================================

      IF ( EMODE(:1) .EQ. 'R' ) THEN
         IF ( .NOT. QTHERE ) THEN

*        Get a temporary NDF to hold the data and quality.
*        Copy the data.
*        Access the quality WRITE/ZERO.
*        Access the data copy.
*        Fill in reference slot and map slot.
            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_SCOPY( DSA__REFID1(SLOT),
     :         'DATA,NOTITLE,NOLABEL,NOHISTORY,NOEXTENSION()',
     :         PLACE, DSA__REFID2(SLOT), STATUS )
            CALL NDF_MAP( DSA__REFID2(SLOT), 'QUALITY', '_UBYTE',
     :         'WRITE/ZERO', QPTR, NELM, STATUS )
            CALL NDF_SBB( BADBIT, DSA__REFID2(SLOT), STATUS )
            CALL NDF_MAP( DSA__REFID2(SLOT), 'DATA', NDFTYP, MODE,
     :         ADDRESS, NELM, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__REFDPT(   SLOT ) = ADDRESS
               DSA__REFQPT(   SLOT ) = QPTR
               DSA__MAPUSD(  MSLOT ) = .TRUE.
               DSA__MAPID1(  MSLOT ) = DSA__REFID2(SLOT)
               DSA__MAPREF(  MSLOT ) = SLOT
               DSA__MAPNAM(  MSLOT ) = 'DATA,QUALITY'
            END IF


*  Else if read access to existing quality.
*  ========================================

         ELSE

*        Map actual quality.
*        Map actual data.
*        Fill in reference slot and map slots.
            CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE',
     :         MODE, QPTR, NELM, STATUS )
            CALL NDF_MAP( DSA__REFID1(SLOT), 'DATA', NDFTYP, MODE,
     :         ADDRESS, NELM, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__REFDPT(   SLOT ) = ADDRESS
               DSA__REFQPT(   SLOT ) = QPTR
               DSA__MAPUSD(  MSLOT ) = .TRUE.
               DSA__MAPID1(  MSLOT ) = DSA__REFID1(SLOT)
               DSA__MAPREF(  MSLOT ) = SLOT
               DSA__MAPNAM(  MSLOT ) = 'DATA,QUALITY'
            END IF

         END IF


*  Else if update access to absent quality.
*  ========================================

      ELSE IF ( EMODE(:1) .EQ. 'U' ) THEN
         IF ( .NOT. QTHERE ) THEN

*        Map actual quality write/zero.
*        Map actual data.
*        Set bad pixel flag and bad bits mask.
*        Fill in reference slot and map slots.
            CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE',
     :         'WRITE/ZERO', QPTR, NELM, STATUS )
            CALL NDF_MAP( DSA__REFID1(SLOT), 'DATA', NDFTYP, MODE,
     :         ADDRESS, NELM, STATUS )
            CALL NDF_SBB( BADBIT, DSA__REFID1(SLOT), STATUS )
            CALL NDF_SBAD( .TRUE., DSA__REFID1(SLOT), 'DATA', STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__REFDPT(   SLOT ) = ADDRESS
               DSA__REFQPT(   SLOT ) = QPTR
               DSA__MAPUSD(  MSLOT ) = .TRUE.
               DSA__MAPID1(  MSLOT ) = DSA__REFID1(SLOT)
               DSA__MAPREF(  MSLOT ) = SLOT
               DSA__MAPNAM(  MSLOT ) = 'DATA,QUALITY'
            END IF


*  Else if update access to existing quality.
*  ==========================================

         ELSE

*        Map actual quality.
*        Map actual data.
*        Set bad pixel flag.
*        Fill in reference slot and map slots.
            CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE',
     :         MODE, QPTR, NELM, STATUS )
            CALL NDF_MAP( DSA__REFID1(SLOT), 'DATA', NDFTYP, MODE,
     :         ADDRESS, NELM, STATUS )
            CALL NDF_SBAD( .TRUE., DSA__REFID1(SLOT), 'DATA', STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__REFDPT(   SLOT ) = ADDRESS
               DSA__REFQPT(   SLOT ) = QPTR
               DSA__MAPUSD(  MSLOT ) = .TRUE.
               DSA__MAPID1(  MSLOT ) = DSA__REFID1(SLOT)
               DSA__MAPREF(  MSLOT ) = SLOT
               DSA__MAPNAM(  MSLOT ) = 'DATA,QUALITY'
            END IF

         END IF


*  Else if write access.
*  =====================

      ELSE

*        Map actual quality write/zero.
*        Map actual data with the effective mode, because the mode
*        might have been a supplied update mode for a new data array.
*        Map actual data (using the
*        Set bad pixel flag and bad-bits mask.
*        Fill in reference slot and map slots.
            CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE',
     :         'WRITE/ZERO', QPTR, NELM, STATUS )
            CALL NDF_MAP( DSA__REFID1(SLOT), 'DATA', NDFTYP, EMODE,
     :         ADDRESS, NELM, STATUS )
            CALL NDF_SBAD( .TRUE., DSA__REFID1(SLOT), 'DATA', STATUS )
            CALL NDF_SBB( BADBIT, DSA__REFID1(SLOT), STATUS )
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

      END
