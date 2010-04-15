      SUBROUTINE DSA_GET_RANGE( DSAREF, VMIN, VMAX, STATUS )
*+
*  Name:
*     DSA_GET_RANGE

*  Purpose:
*     Determine the maximum and minimum values for data in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_GET_RANGE( DSAREF, VMIN, VMAX, STATUS )

*  Description:
*     This routine determines the extreme values in the data array of
*     the NDF referred to by the DSA reference name. Since there never
*     is ready range information (in this implementation), the routine
*     has to access the data temporarily. This access is for data type
*     _REAL (FLOAT) and the extrema deteremination will ignore any
*     pixels that are bad or that become bad during conversion to this
*     data type.
*
*     If the range cannot be determined, both extrema will be returned
*     as zero.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     VMIN = REAL (Returned)
*        The smallest valid pixel value.
*     VMAX = REAL (Returned)
*        The largest valid pixel value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     30 Jul 1987 (ks):
*        Original version.
*     03 Feb 1989 (ks):
*        Now allows for data quality information.
*     08 Sep 1989 (ks):
*        Now controls flagged value propagation.
*     01 Mar 1990 (ks):
*        Now uses DSA__ routines rather than assuming the original
*        Figaro data format.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     26 Oct 1994 (ks):
*        Now uses new calling sequence for DSA_MAP_ARRAY.
*     21 Feb 1996 (hme):
*        FDA library.
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF

*  Arguments Returned:
      REAL VMIN
      REAL VMAX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL BAD                ! Whether bad pixels present
      INTEGER SLOT               ! Reference slot
      INTEGER INDF               ! NDF identifier
      INTEGER NELM               ! Array size
      INTEGER PNTR               ! Array pointer

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Find reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )

*  Clone the NDF identfier.
      CALL NDF_CLONE( DSA__REFID1(SLOT), INDF, STATUS )

*  Gain read access to data (quality implicitly processed into bad
*  values).
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'READ',
     :   PNTR, NELM, STATUS )

*  Check whether bad values present.
      CALL NDF_BAD( INDF, 'DATA', .TRUE., BAD, STATUS )

*  Find range.
      VMAX = 0.
      VMIN = 0.
      IF ( STATUS .EQ. SAI__OK )
     :   CALL DSA2_RANGEF( BAD, %VAL( CNF_PVAL(PNTR) ), 1, NELM, VMAX,
     :                     VMIN )

*  Annul the cloned identifier (implicit unmapping).
      CALL NDF_ANNUL( INDF, STATUS )

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
