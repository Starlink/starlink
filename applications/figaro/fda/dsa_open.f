      SUBROUTINE DSA_OPEN( STATUS )
*+
*  Name:
*     DSA_OPEN

*  Purpose:
*     Open the DSA system for a Figaro application.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_OPEN( STATUS )

*  Description:
*     DSA_OPEN should be called at the start of any Figaro application
*     in order to initialise the DSA routines. It must be called
*     before any of the other DSA routines. In the FDA library this
*     routine refers to the DSA common block so that it is initialised
*     by DSA_BLOCK. That apart, this routine only begins a new NDF
*     context.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     09 Jun 1987 (ks):
*        Original version.
*     01 Mar 1988 (ks):
*        Initialisation of logical unit tables added.
*     04 Jul 1988 (ks):
*        Shape check flags added.
*     14 Jul 1988 (ks):
*        Bad (`flagged') value flag added, then removed.
*     12 Sep 1988 (ks):
*        References to discontinued common variables deleted.
*     29 Nov 1988 (ks):
*        FITS string buffer initialised.
*     11 Dec 1989 (ks):
*        LOG_DELETE added.
*     18 Jan 1990 (ks):
*        Call to DSA__SET_FORMAT added.
*     09 Feb 1990 (ks):
*        NDF FITS items added.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     17 Dec 1992. (ks):
*        Added initialisation of temporary name variables.
*        Also added SYS_STATUS.
*     17 Feb 1995 (ks):
*        Added QF_BOTH_OK.
*     24 Nov 1995 (hme):
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
      INCLUDE 'DSA_COMMON'       ! Global DSA variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL DSA_BLOCK

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

      CALL NDF_BEGIN

      END
