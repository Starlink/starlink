      SUBROUTINE DSA_QUALITY_AND_FLAGS_OK( )
*+
*  Name:
*     DSA_QUALITY_AND_FLAGS_OK

*  Purpose:
*     Indicate that caller can handle both types of quality information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_QUALITY_AND_FLAGS_OK( )

*  Description:
*     The original versions of DSA did not allow a file to have both
*     data quality and flagged data values. Later versions removed this
*     restriction. This means that it is now legitimate for a program
*     to call both DSA_USE_QUALITY and DSA_USE_FLAGGED_VALUES and so
*     handle both sets of quality information explicitly. Doing so
*     makes for an efficient but rather complicated program and it is
*     probably better for a program to handle quality information
*     internally using one method or the other but not both. If a
*     program really wants to handle both types of information
*     internally it should call this routine first in order signal that
*     it is a modern application that knows what it's doing, and not
*     an old one that never expected that both kinds of quality
*     information could exist simultaneously. If presented with a file
*     containing both types of quality information, such a program may
*     call both routines but - having assumed only one would be called
*     - may then not process the data properly.
*
*     This flag is a global switch for all data accessed, not specific
*     to a certain NDF or DSA reference slot. Also, the flag cannot be
*     reset. It is reset by DSA_CLOSE. In order to avoid confusion
*     this routine should be called before any data or quality are
*     accessed from any NDF. Best call it immediately after DSA_OPEN.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     17 Feb 1995 (ks):
*        Original version.
*     28 Nov 1995 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*.

      DSA__BADQUA = .TRUE.

      END
