      SUBROUTINE ECH_MERGE_ORDERS(
     :           NX,
     :           NY,
     :           IMAGE,
     :           ERR_IMAGE,
     :           BOX,
     :           CUTOFF,
     :           OUTPUT,
     :           ERR_OUTPUT,
     :           IFILT,
     :           OFILT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_MERGE_ORDERS

*  Purpose:
*     Adds in orders from scrunched echellograms to a 1-D spectrum.

*  Description:
*     Based on FIG_ECHADDIN.
*
*     Loop through all the orders in the input image. For each, calculate
*     a median filtered version of both the input and the current state
*     of the output. Then use the median filtered values as weights when
*     combining the orders from the input image and the output image. Where
*     the higher signal is less than CUTOFF times the lower signal, do not
*     take any contribution from the lower signal (since to do so would
*     probably degrade the signal to noise ratio).

*  Invocation:
*     CALL ECH_MERGE_ORDERS(
*     :    NX,
*     :    NY,
*     :    IMAGE,
*     :    ERR_IMAGE,
*     :    BOX,
*     :    CUTOFF,
*     :    OUTPUT,
*     :    ERR_OUTPUT,
*     :    IFILT,
*     :    OFILT,
*     :    STATUS
*     :   )

*  Parameters:
*     NX = INTEGER (Given)
*        Size of X-dimension of both the images.
*     NY = INTEGER (Given)
*        Size of Y-dimension of IMAGE.
*     IMAGE = REAL( NX, NY ) (Given)
*        Input image.
*     ERR_IMAGE = REAL( NX, NY ) (Given)
*        Input image variances.
*     BOX = INTEGER (Given)
*        Width of box used for median filtering of orders as part of
*        weights estimation.
*     CUTOFF = REAL (Given)
*        Maximum ratio of stronger signal to weaker signal that will still
*        allow a contribution from the weaker signal.
*     OUTPUT = REAL( NX ) (Returned)
*        The output spectrum containing the merged orders.
*     ERR_OUTPUT = REAL( NX ) (Returned)
*        The output spectrum variances.
*     IFILT = REAL
*
*     OFILT = REAL
*
*     STATUS = INTEGER (Given and Returned)
*        Global inherited status.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     26-JUN-1996 (MJC):
*       Separate file for this subroutine.  New prologue.
*     20-AUG-2004 (norman@astro.gla.ac.uk): replace Figaro routine
*       GEN_MEDFLT with kaplibs routine KPG1_BMEDR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      REAL ERR_IMAGE( NX, NY )
      INTEGER BOX
      REAL CUTOFF
      REAL IFILT( NX )
      REAL OFILT( NX )

*  Arguments Returned:
      REAL OUTPUT( NX )
      REAL ERR_OUTPUT( NX )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAXBOX
      PARAMETER ( MAXBOX = 501 )

*  Local Variables:
      REAL WORK( MAXBOX )
      INTEGER WORKI( MAXBOX )
      REAL IWEIGHT
      REAL OWEIGHT

      INTEGER I
      INTEGER J

      LOGICAL BADOUT            ! For call to KPG1_BMEDR (but unused)

      INCLUDE 'SAE_PAR'
*.

*  Loop through all the orders in the input image. For each, calculate
*  a median filtered version of both the input and the current state
*  of the output. Then use the median filtered values as weights when
*  combining the orders from the input image and the output image. Where
*  the higher signal is less than CUTOFF times the lower signal, do not
*  take any contribution from the lower signal (since to do so would
*  probably degrade the signal to noise ratio).
      DO I = 1, NX
         IFILT( I ) = 0.0
         OFILT( I ) = 0.0
      END DO

      STATUS = SAI__OK
      DO I = 1, NY
*     This used to call Figaro's GEN_MEDFLT routine, with arguments
*         CALL GEN_MEDFLT( IMAGE( 1, I ), NX, 1, BOX, 1, WORK, IFILT )
*         CALL GEN_MEDFLT( OUTPUT, NX, 1, BOX, 1, WORK, OFILT )
*     The kaplibs routine has a slightly different way of specifying the
*     size of the box, but since gen_medflt actually used a box with a
*     width rounded down to the next odd number, the result is the same
*     whether BOX is even or odd.
         CALL KPG1_BMEDR( .FALSE., .FALSE., .FALSE.,
     :        NX, 1, IMAGE(1,I), BOX/2, 0, 1,
     :        IFILT, BADOUT, WORK, WORKI, STATUS )
         CALL KPG1_BMEDR( .FALSE., .FALSE., .FALSE.,
     :        NX, 1, OUTPUT, BOX/2, 0, 1,
     :        OFILT, BADOUT, WORK, WORKI, STATUS )

         DO J = 1, NX
            IWEIGHT = MAX( IFILT( J ), 0.0 )
            OWEIGHT = MAX( OFILT( J ), 0.0 )
            IF ( IWEIGHT .GT. CUTOFF * OWEIGHT ) THEN
               OUTPUT( J ) = IMAGE( J, I )
               ERR_OUTPUT( J ) = ERR_IMAGE( J, I )

            ELSE IF ( OWEIGHT .GT. CUTOFF * IWEIGHT ) THEN
               CONTINUE

            ELSE IF ( IWEIGHT + OWEIGHT .LT. 1E-6 ) THEN
               CONTINUE

            ELSE
               OUTPUT( J ) = ( IWEIGHT * IMAGE( J, I ) +
     :                         OWEIGHT * OUTPUT( J ) ) /
     :                       ( IWEIGHT + OWEIGHT )
               ERR_OUTPUT( J ) = ( IWEIGHT * ERR_IMAGE( J, I ) +
     :                             OWEIGHT * ERR_OUTPUT( J ) ) /
     :                           ( IWEIGHT + OWEIGHT )
            END IF
         END DO
      END DO

      END
