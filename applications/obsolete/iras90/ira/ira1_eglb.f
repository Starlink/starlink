      SUBROUTINE IRA1_EGLB( NC, SCS, NLABS, MAXLAB, LABS, TOL, LBND,
     :                      LAXMIN, STATUS )
*+
*  Name:
*     IRA1_EGLB

*  Purpose:
*     Produce longitude or latitude labels around the edge of the
*     plotting space.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_EGLB( NC, SCS, NLABS, MAXLAB, LABS, TOL, LBND, LAXMIN,
*                     STATUS )

*  Description:
*     This routine produces longitude or latitude around the edge of the
*     plotting space, using label information stored by routines
*     IRA1_DRMS and IRA1_DRPS.

*  Arguments:
*     NC = INTEGER (Given)
*        The axis index; 1 for longitude labels, 2 for latitude labels.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system. An abbreviationwill do. If no equinox
*        specifier is included, then a default one is used.
*     NLABS = INTEGER (Given)
*        The number of labels for which information is stored in LABS.
*     MAXLAB = INTEGER (Given)
*        The size of the LABS array.
*     LABS( MAXLAB, 5 ) = DOUBLE PRECISION (Given)
*        Label information; longitude or latitude value in (1),
*        image coordinates to put the label at in (2) and (3), and
*        unit vector along the meridian or parallel in (4) and (5).
*     TOL = DOUBLE PRECISION (Given)
*        The tolerance required for the displayed values.
*     LBND( 2 ) = REAL (Given)
*        Lower X and Y boundaries of plotting space.
*     LAXMIN = REAL (Given and Returned)
*        The lowest X coordinate covered by any coordinate labels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_DRVPO( 5 ) = REAL (Read)
*           Values defining the area occupied by the text, in the
*           order; (X,Y) at start of box, (X,Y) at end of box, height
*           of box (perpendicular to the line joing start and end of
*           box).

*  Arguments Given:
      INTEGER NC
      CHARACTER SCS*(*)
      INTEGER NLABS
      INTEGER MAXLAB
      DOUBLE PRECISION LABS( MAXLAB, 5 )
      DOUBLE PRECISION TOL
      REAL LBND( 2 )

*  Arguments Given and Returned:
      REAL LAXMIN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL SLA_DRANGE
      DOUBLE PRECISION SLA_DRANGE! Convert angle to +/- PI.

      EXTERNAL SLA_DRANRM
      DOUBLE PRECISION SLA_DRANRM! Convert angle to 0 - 2*PI.

*  Local Variables:
      DOUBLE PRECISION ABVAL     ! Sky longitude or latitude value.
      CHARACTER BJ*1             ! The type of epoch stored in EQU.
      CHARACTER CONSAV*20        ! Context of first displayed label.
      CHARACTER CONTXT*20        ! Label context.
      DOUBLE PRECISION EQU       ! Epoch from SCS.
      INTEGER I                  ! Loop count.
      REAL MINX                  ! Minimum X covered by the value just
                                 ! drawn.
      CHARACTER NAME*(IRA__SZSCS)! Full SCS name.
      INTEGER STYLE              ! Formatting style.
      REAL XVAL                  ! Single precision version of X
                                 ! coordinate.
      REAL XVEC                  ! X component of vector along the
                                 ! curve.
      REAL YVAL                  ! Single precision version of Y
                                 ! coordinate.
      REAL YVEC                  ! Y component of vector along the
                                 ! curve.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Identifiy the SCS, and set up the style.
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )
      IF( NAME( : 10 ) .EQ. 'EQUATORIAL' ) THEN
         STYLE = 2
      ELSE
         STYLE = 5
      END IF

*  Initialise the label contexts.
      CONTXT = ' '
      CONSAV = ' '

*  To ensure that the correct fields are displayed, labels should
*  always be produced in such an order that the absolute magnitude of
*  the sky coordinate values always increases. For this reason the
*  labels are produced in two batches, first those with zero or
*  positive values, and then those with negative values.  Loop round
*  producing each positive label.
      DO I = 1, NLABS

*  Check that the sky coordinate value is positive.
         ABVAL = LABS( I, 1 )
         IF( ABVAL .GE. 0.0D0 .AND. STATUS .EQ. SAI__OK ) THEN

*  Save the position and vector values.
            XVAL = REAL( LABS( I, 2 ) )
            YVAL = REAL( LABS( I, 3 ) )
            XVEC = REAL( LABS( I, 4 ) )
            YVEC = REAL( LABS( I, 5 ) )

*  Ensure that the values are in there normal ranges.
            IF( NC .EQ. 1 ) THEN
               ABVAL = SLA_DRANRM( ABVAL )
            ELSE
               ABVAL = SLA_DRANGE( ABVAL )
            END IF

*  Set up the correct justification.
            IF( XVAL .LE. LBND( 1 ) ) THEN
               CALL SGS_STXJ( 'CR' )

            ELSE IF( YVAL .LE. LBND( 2 ) ) THEN
               CALL SGS_STXJ( 'TC' )

            ELSE IF( YVEC .GT. ABS( XVEC ) ) THEN
               CALL SGS_STXJ( 'TC' )

            ELSE IF( YVEC .LT. -ABS( XVEC ) ) THEN
               CALL SGS_STXJ( 'BC' )

            ELSE IF( XVEC .LT. 0.0 ) THEN
               CALL SGS_STXJ( 'CL' )

            ELSE
               CALL SGS_STXJ( 'CR' )

            END IF

*  Plot the label.
            CALL IRA1_IDRVA( ABVAL, NAME, NC, XVAL, YVAL, STYLE, TOL,
     :                       CONTXT, STATUS )

*  Update the lowest X coordinate covered by any coordinate value.
            MINX = MIN( ACM_DRVPO( 1 ), ACM_DRVPO( 3 ) ) -
     :             0.5*ACM_DRVPO( 5 )
            LAXMIN = MIN( LAXMIN, MINX )

*  If this is the first label to be plotted, save the context.
            IF( CONSAV .EQ. ' ' ) CONSAV = CONTXT

         END IF

      END DO

*  Restore the context of the first label plotted.
      CONTXT = CONSAV

*  Now loop round producing each negative label. These are done in
*  reverse order to ensure that the absolute coordinate value
*  increases.
      DO I = NLABS , 1, -1

*  Check that the sky coordinate value is positive, and that all the
*  coordinate values are good.
         ABVAL = LABS( I, 1 )
         IF( ABVAL .LT. 0.0D0 .AND.
     :       STATUS .EQ. SAI__OK ) THEN

*  Save the position and vector values.
            XVAL = REAL( LABS( I, 2 ) )
            YVAL = REAL( LABS( I, 3 ) )
            XVEC = REAL( LABS( I, 4 ) )
            YVEC = REAL( LABS( I, 5 ) )

*  Ensure that the values are in there normal ranges.
            IF( NC .EQ. 1 ) THEN
               ABVAL = SLA_DRANRM( ABVAL )
            ELSE
               ABVAL = SLA_DRANGE( ABVAL )
            END IF

*  Set up the correct justification.
            IF( XVAL .LE. LBND( 1 ) ) THEN
               CALL SGS_STXJ( 'CR' )

            ELSE IF( YVAL .LE. LBND( 2 ) ) THEN
               CALL SGS_STXJ( 'TC' )

            ELSE IF( YVEC .GT. ABS( XVEC ) ) THEN
               CALL SGS_STXJ( 'TC' )

            ELSE IF( YVEC .LT. -ABS( XVEC ) ) THEN
               CALL SGS_STXJ( 'BC' )

            ELSE IF( XVEC .LT. 0.0 ) THEN
               CALL SGS_STXJ( 'CL' )

            ELSE
               CALL SGS_STXJ( 'CR' )

            END IF

*  Plot the label.
            CALL IRA1_IDRVA( ABVAL, NAME, NC, XVAL, YVAL, STYLE, TOL,
     :                       CONTXT, STATUS )

*  Update the lowest X coordinate covered by any coordinate value.
            MINX = MIN( ACM_DRVPO( 1 ), ACM_DRVPO( 3 ) ) -
     :             0.5*ACM_DRVPO( 5 )
            LAXMIN = MIN( LAXMIN, MINX )

         END IF

      END DO

      END
