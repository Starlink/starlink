      SUBROUTINE CMD_HLP( NOPROMPT, HELP_TOPIC, HELP_PAR )
*+
*  Name:
*     SUBROUTINE CMD_HLP

*  Purpose:
*     Invoke help facility.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CMD_HLP( NOPROMPT, HELP_TOPIC, HELP_PAR )

*  Arguments:
*     NOPROMPT = LOGICAL (Given)
*        Whether user can interact with help facility.
*     HELPTOPIC = CHARACTER* ( * ) (Given)
*        Topic to be displayed if any.
*     HELP_PAR = CHARACTER* ( * ) (Given)
*        Further sub-topics to be displayed.

*  Authors:
*     ???
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     ??-???-?? (???):
*       Original version.
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      LOGICAL NOPROMPT

      CHARACTER*( * ) HELP_TOPIC
      CHARACTER*( * ) HELP_PAR

*  External References:
      INTEGER HLP_OUTSUB
      INTEGER HLP_INSUB
      INTEGER HLP_HELP
      INTEGER HLP_NAMETR

      EXTERNAL HLP_INSUB, HLP_OUTSUB, HLP_HELP, HLP_NAMETR

*  Local Constants:
      INTEGER LUCMD
      INTEGER LUMES
      PARAMETER ( LUCMD = 5, LUMES = 50 )

*  Local Variables:
      CHARACTER*255 HELP_STRING

      INTEGER STATUS   ! Input/Output status condition
*.

*   Build topic string from supplied information.
      HELP_STRING = HELP_TOPIC//' '//HELP_PAR

*   Invoke help facility.
      IF ( NOPROMPT ) THEN
         STATUS = HLP_HELP( HLP_OUTSUB, 80, HELP_STRING, LUMES,
     :                      'IUEDR_HELP:', 0,
     :                      HLP_INSUB, HLP_NAMETR )

      ELSE
         STATUS = HLP_HELP( HLP_OUTSUB, 80, HELP_STRING, LUMES,
     :                      'IUEDR_HELP:', 1,
     :                      HLP_INSUB, HLP_NAMETR )
      END IF

      END
