      SUBROUTINE PIDEIM( NAXIS1, NAXIS2, DZERO, DSCALE, DLIM, DLAB,
     :                   DUNT, A1LAB, A1UNT, A2LAB, A2UNT, TITLE,
     :                   STATUS )

*+
*  Name:
*     SUBROUTINE PIDEIM

*  Purpose:
*     Design and draw non-data part of image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PIDEIM( NAXIS1, NAXIS2, DZERO, DSCALE, DLIM, DLAB,
*    :             DUNT, A1LAB, A1UNT, A2LAB, A2UNT, TITLE, STATUS )

*  Arguments:
*     NAXIS1 = INTEGER (Given)
*        Size of x-axis (sample) in pixels.
*     NAXIS2 = INTEGER (Given)
*        Size of y-axis (line) in pixels.
*     DZERO = REAL*8 (Given)
*        Data zero point (intercept).
*     DSCALE = REAL*8 (Given)
*        Data scale factor (slope).
*     DLIM = INTEGER( 2 ) (Given)
*        Data value limits for the whole image.
*     DLAB = BYTE( * ) (Given)
*        Label for Data.
*     DUNT = BYTE( * ) (Given)
*        Units for Data.
*     A1LAB = BYTE( * ) (Given)
*        Label for x-axis.
*     A1UNT = BYTE( * ) (Given)
*        Units for x-axis.
*     A2LAB = BYTE( * ) (Given)
*        Label for y-axis.
*     A2UNT = BYTE( * ) (Given)
*        Units for y-axis.
*     TITLE = BYTE( * ) (Given)
*        Title for display.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The axes for the specified image are designed and drawn.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*   History:
*     01-MAY-82 (JRG):
*       AT4 version.
*     05-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*       Conversion to GKS 7.2 graphics.
*     09-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     06-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMIDEV'

*  Global Constants:
      INTEGER MAXLABEL    ! maximum length of label
      PARAMETER ( MAXLABEL = 40 )

*  Arguments Given:
      INTEGER NAXIS1      ! size of axis 1 (sample)
      INTEGER NAXIS2      ! size of axis 2 (line)

      REAL*8 DZERO        ! data zero point
      REAL*8 DSCALE       ! data scale factor

      INTEGER DLIM( 2 )   ! data limits (whole image)

      BYTE DLAB( * )      ! data label
      BYTE DUNT( * )      ! data units
      BYTE A1LAB( * )     ! axis 1 label
      BYTE A1UNT( * )     ! axis 1 units
      BYTE A2LAB( * )     ! axis 2 label
      BYTE A2UNT( * )     ! axis 2 units
      BYTE TITLE( * )     ! title for graph

*  Status:
      INTEGER STATUS      ! status return

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get parameters.
      CALL PIPAR( NAXIS1, NAXIS2, DZERO, DSCALE, DLIM, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Axis labels (etc).
         CALL str_MOVE( A1LAB, MAXLABEL, XLAB )
         CALL str_MOVE( A1UNT, MAXLABEL, XUN )
         CALL str_MOVE( A2LAB, MAXLABEL, YLAB )
         CALL str_MOVE( A2UNT, MAXLABEL, YUN )
         CALL str_MOVE( TITLE, MAXLABEL, GTITLE )
         NLEGND = 0
         CALL PIDEAX( STATUS )
      END IF

      END
