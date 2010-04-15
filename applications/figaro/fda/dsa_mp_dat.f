      SUBROUTINE DSA_MAP_DATA( DSAREF, MODE, TYPE,
     :   ADDRESS, MSLOT, STATUS )
*+
*  Name:
*     DSA_MAP_DATA

*  Purpose:
*     Map the main data array in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_MAP_DATA( DSAREF, MODE, TYPE, ADDRESS, MSLOT, STATUS )

*  Description:
*     This routine maps the main data array in an NDF. If the quality
*     array is to be mapped as well, then DSA_USE_QUALITY must be called
*     before this routine and DSA_MAP_QUALITY must be called after this
*     routine.
*
*     If neither DSA_USE_QUALITY nor DSA_USE_FLAGGED_VALUES have been
*     called before this routine, then the returned data array is
*     guaranteed to be free of bad values. This however means that any
*     bad values in the actual data have been filled in with fake values
*     that only appear to be good.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode, 'READ', 'WRITE', or 'UPDATE'.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type to be used to map the array. E.g. 'FLOAT' or
*        'DOUBLE'.
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
*     {enter_new_authors_here}

*  History:
*     24 Jun 1987 (ks):
*        Original version.
*     22 Jul 1988 (ks):
*        Data quality/flagged value handling added.  Call
*        to DSA_MAP_ARRAY modified.
*     08 Sep 1989 (ks):
*        Control of flagged value propagation added.  Call
*        to DSA_MAP_ARRAY modified.
*     13 Dec 1989 (ks):
*        Now sets `data flagged' flag in structure if
*        appropriate.  Also now uses DSA__ routines to
*        handle the details of structure contents.
*     03 May 1990 (ks):
*        Now clears the `data flagged' flag in the structure
*        if it was set but no elements were actually flagged.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     08 Oct 1992 (hme):
*        Unflag the data only if the
*        mode is not WRITE. Prologue now states correctly
*        that USE_FLAGS and USE_QUALITY are LOGICAL.
*     26th Oct 1994 (ks):
*        Now uses new calling sequence for DSA_MAP_ARRAY.
*      3rd Feb 1995 (ks):
*        Now supports files that have both flagged data and
*        quality arrays.
*     25 Nov 1995 (hme):
*        FDA library.
*     12 Feb 1996 (hme):
*        For UW access mark reference as data-modified. If caller is
*        ignorant of any kind of quality information, also mark quality
*        as modified, since it will be handled internally.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
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
      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER ADDRESS
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL MODIFD             ! Whether U or W access
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( NDF__SZTYP ) NDFTYP ! The type in NDF speak

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Find reference slot and translate data type.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL DSA1_NDFTYP( TYPE, NDFTYP, STATUS )

*  Is the access mode modifying?
      MODIFD = INDEX( 'UuWw', MODE(:1) ) .NE. 0

*  Call separate routines for each combination of using bad values
*  and/or quality.
*  DSA1_MDAT internally processes quality, therefore the
*  quality-modified needs to be set. For _MDQF and _MDQ that flag must
*  not be set here. It will be set by DSA_MAP_QUALITY if the caller
*  actually accesses the quality.
      IF ( DSA__REFBAD(SLOT) .AND. DSA__REFQUA(SLOT) ) THEN
         CALL DSA1_MDQF( SLOT, MODE, NDFTYP, ADDRESS, MSLOT, STATUS )
      ELSE IF ( DSA__REFBAD(SLOT) ) THEN
         CALL DSA1_MDF( SLOT, MODE, NDFTYP, ADDRESS, MSLOT, STATUS )
      ELSE IF ( DSA__REFQUA(SLOT) ) THEN
         CALL DSA1_MDQ( SLOT, MODE, NDFTYP, ADDRESS, MSLOT, STATUS )
      ELSE
         CALL DSA1_MDAT( SLOT, MODE, NDFTYP, ADDRESS, MSLOT, STATUS )
         IF ( MODIFD ) DSA__REFMDQ(SLOT) = .TRUE.
      END IF
      IF ( MODIFD ) DSA__REFMDD(SLOT) = .TRUE.

*  Tidy up.
 500  CONTINUE

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
