      SUBROUTINE DSA_SEEK_FLAGGED_VALUES( DSAREF, EXIST, STATUS )
*+
*  Name:
*     DSA_SEEK_FLAGGED_VALUES

*  Purpose:
*     Determine whether or not a data array contains bad values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SEEK_FLAGGED_VALUES( DSAREF, EXIST, STATUS )

*  Description:
*     This routine looks to see if the data array related to the given
*     DSA reference name contains bad values. The idea is that an
*     application can have a look before accessing the data.
*
*     Contrary to the previous implementation, this routine will check
*     for the actual presence of bad values. Furthermore, if a pixel is
*     flagged as bad by means of a quality array that counts usually as
*     bad values as well.
*
*     On the other hand, the answer is not quite definitive. Usually,
*     this routine is called before the data are accessed by the
*     calling routine. During data access a type conversion may occur,
*     which may introduce further bad values where the conversion is
*     not possible.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The DSA reference name associated with the NDF to be searched.
*     EXIST = LOGICAL (Returned)
*        True if the data array actually contains bad values, or if
*        an unmapped quality array indicates that there are bad pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     13 Jul 1988 (ks):
*        Original version.
*     02 Mar 1990 (ks):
*        Now uses DSA__SEEK_FLAGGED instead of assuming the
*        original Figaro file format.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     17 Feb 1995 (ks):
*        Now sets and checks QF_HANDLING.
*     25 Nov 1995 (hme):
*        FDA library.
*     09 Feb 1996 (hme):
*        If the data/quality buffer NDF exists, use it to ask about bad
*        values in its data. That's because that is where the mapped
*        data are, and those should be investigated.
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

*  Arguments Returned:
      LOGICAL EXIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )

*  Check the data component of the relevant NDF. Usually this is the
*  original NDF itself (DSA__REFID1), but in certain circumstances the
*  original data have been copied into a buffer NDF (DSA__REFID2) and
*  mapped from there.
      IF ( DSA__REFID2(SLOT) .NE. NDF__NOID ) THEN
         CALL NDF_BAD( DSA__REFID2(SLOT), 'DATA', .TRUE.,
     :      EXIST, STATUS )
      ELSE
         CALL NDF_BAD( DSA__REFID1(SLOT), 'DATA', .TRUE.,
     :      EXIST, STATUS )
      END IF

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
