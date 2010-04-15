      SUBROUTINE IIDATE(N, STATUS )
*+
*   This Routine Finds the Day in the 20th. Century
*   IDATE Returns Month,Day of Month and Year in 20th. Century
*   Taking March as Month 1 using Integer Arithmetic the Day
*   can be Calculated.
*
*   Returns
*   -------
*      N    - The Day Number
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     15-MAR-1993 (AJJB):
*        Functions LENG and LTOI removed from the end of this file and
*        put in their own files, called, unsurprisingly, LENG and LTOI.
*
*   Method due to W.F. Lupton
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL IDATE(J,I,K)
      IF (J.LE.2) THEN
         J=J+9
         K=K-1
      ELSE
         J=J-3
      ENDIF
      N=(1461*K)/4 + (153*J+2)/5 + I + 59
      END

