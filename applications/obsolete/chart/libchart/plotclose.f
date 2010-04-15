      SUBROUTINE PLOT_CLOSE( STATUS )
*
* This subroutine is only put in to place all plotting calls in subroutines
*
*   Arguments:
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
* J.V.Carey
* 1984 July 20
*

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL SGS_CLOSE
      END
