      SUBROUTINE snx_CHSET (I)
*+
*  Name:
*     CHSET

*  Purpose:
*     Select character set

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     I = INTEGER (Given)
*         Character set: 1='duplex', 2='complex'

*  Description:
*     This routine sets the variable MODE in the NCAR labelled
*     COMMON block /PUSER/.

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-APR-1986 (PTW):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE

      INTEGER I

*  NCAR labelled COMMON block
      INTEGER MODE
      COMMON /PUSER/ MODE


      MODE=I

      END
