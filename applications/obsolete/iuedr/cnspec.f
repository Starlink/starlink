      SUBROUTINE CNSPEC

*+
*
*   Name:
*      SUBROUTINE CNSPEC
*
*   Description:
*      This routine cancels the current Spectrum contents.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          07-DEC-87     IUEDR Vn. 1.4
*         Changed to include Aperture, Trailed and Date sensitivities.
*      Paul Rees          21-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     22-OCT-94     IUEDR Vn. 3.2
*
*   Method:
*      Propagate cancellation to all lower levels via structured calls.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global variables:
      INCLUDE 'CMSAVE'
      INCLUDE 'CMABS'
      INCLUDE 'CMRIP'
      INCLUDE 'CMHAL'
      INCLUDE 'CMCUT'
      INCLUDE 'CMWCOR'
      INCLUDE 'CMECOR'
      INCLUDE 'CMVEL'

      NOSPEC = .TRUE.
      NORDER = 0
      NOABS = .TRUE.
      NABS = 0
      NORIP = .TRUE.
      NOHAL = .TRUE.
      NRIPM = 0
      NRIPO = 0
      NOCUT = .TRUE.
      NOVEL = .TRUE.
      NOWCOR = .TRUE.
      NOECOR = .TRUE.

      CALL CNORD

      END
