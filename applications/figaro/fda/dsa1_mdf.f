      SUBROUTINE DSA1_MDF( SLOT, MODE, NDFTYP, ADDRESS, MSLOT, STATUS )
*+
*  Name:
*     DSA1_MDF

*  Purpose:
*     Map the main data array in an NDF, accepting bad values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_MDF( SLOT, MODE, NDFTYP, ADDRESS, SLOT, STATUS )

*  Description:
*     This routine maps the main data array in an NDF, returning
*     the address of the dynamic memory array that may be used to
*     access it. The data array is accepted as it is, including bad
*     values. If there is a quality array and if the access mode is
*     write or update, then the quality is deleted.

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
*     {enter_new_authors_here}

*  History:
*     25 Nov 1995 (hme):
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
*     14 Dec 1995 (hme):
*        Review again. The processing should not follow the DSA example,
*        but fewer, simpler, firmer principles.
*        Basically, first merge quality into bad values, then delete
*        quality. The principle involved here is that of equivalence of
*        bad values and quality.
*     18 Dec 1995 (hme):
*        Update use of common block to enable unmapping. Data and quality
*        are regarded as one entity and will be unmapped in a single call.
*     1996 July 12 (MJC):
*        Test for the existence of the data array in update mode.  DSA
*        is more relaxed and appears to allow update mode for a new
*        data array.  However, the NDF library gives an error.  So to
*        avoid changing numerous calls in applications the mode is
*        changed to write access in this case.
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
      INTEGER SLOT
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) NDFTYP

*  Arguments Returned:
      INTEGER ADDRESS
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL DTHERE             ! Whether or not there is a data array
      CHARACTER * ( NDF__SZACC ) EMODE ! Effective access mode
      INTEGER IGNORE             ! Mapped array size
      LOGICAL QTHERE             ! Whether or not a quality array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find a free map slot.
      CALL DSA1_MSNEW( MSLOT, STATUS )

*  Test whether the data array exists for update mode.  If it does not
*  exist, the effective mode is set to write otherwise this will fail.
*  This is to mimic the laissez-faire approach of DSA.   Make an upper
*  copy of the effective mode.
      CALL NDF_STATE( DSA__REFID1(SLOT), 'DATA', DTHERE, STATUS )
      EMODE = MODE
      CALL CHR_UCASE( EMODE )
      IF ( EMODE(:1) .EQ. 'U' .AND. .NOT. DTHERE ) EMODE = 'WRITE'

*  In this routine we can just map the NDF's data component.  This is
*  because the caller apparently can cope with bad values in the data.
*  Use the effective mode.
      CALL NDF_MAP( DSA__REFID1(SLOT), 'DATA', NDFTYP, EMODE,
     :   ADDRESS, IGNORE, STATUS )

*  If UW access.
*     Set bad pixel flag.
*     If quality exists, delete it.
*  Fill in reference slot and map slot.
      IF ( EMODE(:1) .EQ. 'U' .OR. EMODE(:1) .EQ. 'W' ) THEN
         CALL NDF_SBAD( .TRUE., DSA__REFID1(SLOT), 'DATA', STATUS )
         CALL NDF_STATE( DSA__REFID1(SLOT), 'QUALITY', QTHERE, STATUS )
         IF ( QTHERE )
     :      CALL NDF_RESET( DSA__REFID1(SLOT), 'QUALITY', STATUS )
      END IF
      IF ( STATUS .EQ. SAI__OK ) THEN
         DSA__REFDPT(SLOT) = ADDRESS
         DSA__MAPUSD(MSLOT) = .TRUE.
         DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
         DSA__MAPREF(MSLOT) = SLOT
         DSA__MAPNAM(MSLOT) = 'DATA'
      END IF

*  Return.
      END
