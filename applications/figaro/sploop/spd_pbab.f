      SUBROUTINE SPD_PBAB( MODX, MODX2, MODF, MODF2, NPAR, MODPAR,
     :   MSKDIM, MSKUSE, MASK, STATUS )
*+
*  Name:
*     SPD_PBAB

*  Purpose:
*     Display zoomed spectrum and model.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_PBAB( MODX, MODX2, MODF, MODF2, NPAR, MODPAR,
*        MSKDIM, MSKUSE, MASK, STATUS )

*  Description:
*     This routine looks up the viewport specification for the spectrum
*     and model and displays the data.
*
*     The following curves are established and drawn:
*      - Bin-style bottom viewport: The registered y data versus x data.
*      - Dashed line bottom viewport: The model, if available. This is
*        sampled at internal x values using the given MODF routine.
*      - Crosses top viewport: Data minus model, if model available.
*        This is sampled at the registered x data, using the registered
*        y data and the given MODF routine.
*      - Error bars top viewport: Data minus model plus/minus the square
*        root of variance, if model and variance available. This is
*        sampled at the registered x data, using the registered y data
*        and variance and the given MODF routine.
*      - Line top viewport: Some high-order component of the model, if
*        available. This is sampled at interal x values using the given
*        MODF2 routine.
*      - Pedestals bottom viewport: These mark the extent of the mask,
*        if given.

*  Arguments:
*     MODX = LOGICAL (Given)
*        True if MODF given.
*     MODX2 = LOGICAL (Given)
*        True if MODF2 given.
*     MODF = EXTERNAL (Given)
*        Routine to evaluate model for a given x value array.
*        SUBROUTINE MODF( NX, X, NPAR, MODPAR, Y, STATUS )
*           NX = INTEGER (Given)
*           X( NX ) = REAL (Given)
*           NPAR = INTEGER (Given)
*           MODPAR( NPAR ) = REAL (Given)
*           Y( NX ) = REAL (Returned)
*           STATUS = INTEGER (Given and Returned)
*        This routine is used for the principal model, most obviously
*        the dashed line in the bottom view port.
*     MODF2 = EXTERNAL (Given)
*        Routine to evaluate model for a given x value array.
*        SUBROUTINE MODF2( NX, X, NPAR, MODPAR, Y, STATUS )
*           NX = INTEGER (Given)
*           X( NX ) = REAL (Given)
*           NPAR = INTEGER (Given)
*           MODPAR( NPAR ) = REAL (Given)
*           Y( NX ) = REAL (Returned)
*           STATUS = INTEGER (Given and Returned)
*        This routine is used for the secondary model, i.e. the
*        full-drawn line in the top viewport.
*     NPAR = INTEGER (Given)
*        The size of the MODPAR array, passed on to MODF, MODF2.
*     MODPAR( NPAR ) = REAL (Given)
*        The model parameters, passed on to MODF, MODF2. Note that both
*        routines have to use the same set of parameters.
*     MSKDIM = INTEGER (Given)
*        Size of MASK array. Must be even.
*     MSKUSE = INTEGER (Given)
*        Number of valid mask intervals. Give zero to indicate that no
*        mask array is available.
*     MASK( MSKDIM ) = REAL (Given)
*        The mask is used to draw a number of pedestals at the bottom of
*        the bottom viewport. The pedestals extend
*           from MASK(1) to MASK(1+MSKDIM/2),
*           from MASK(2) to MASK(2+MSKDIM/2),
*              ...
*           from MASK(MSKUSE) to MASK(MSKUSE+MSKDIM/2),
*        provided that the leading end is actually to the left of the
*        trailing end. Pedestals may overlap.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29 Apr 1994 (hme):
*        Original version.
*     2005 June 2 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'SPD_PCOM'         ! Specdre SPLOOP common block

*  Arguments Given:
      LOGICAL MODX
      LOGICAL MODX2
      EXTERNAL MODF
      EXTERNAL MODF2
      INTEGER NPAR
      REAL MODPAR( NPAR )
      INTEGER MSKDIM
      INTEGER MSKUSE
      REAL MASK( MSKDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER FITRES             ! Plot resolution for model
      PARAMETER ( FITRES = 501 )
      REAL LEFT, RIGHT,
     :   BOTTOM, TOP, DIVISN     ! Viewport location in view surface
      PARAMETER (
     :   LEFT = 0.10, RIGHT = 0.90,
     :   BOTTOM = 0.05, TOP = 0.95,
     :   DIVISN = 0.675 )

*  Local Variables:
      INTEGER I, J               ! Temporary integers
      REAL MASKY                 ! y level for mask pedestals
      REAL DELTA                 ! Viewport range
      REAL RMIN, RMAX            ! Temporary range
      REAL XFIT( FITRES )        ! Internal buffer to plot models
      REAL YDSH( FITRES )        ! Internal buffer to plot models
      REAL YLIN( FITRES )        ! Internal buffer to plot models

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check device and spectrum available.
      IF ( .NOT. DEVOPN .OR. .NOT. SPXST ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_PBAB_E01', 'SPD_PBAB: Error displaying ' //
     :      'spectrum. Device or spectrum not available.', STATUS )
         GO TO 500
      END IF


*  Work out arrays.
*  ================

*  Internal sampling.
      IF ( MODX .OR. MODX2 ) THEN
         DELTA = ( SPWIN(2) - SPWIN(1) ) / FLOAT(FITRES-1)
         DO 1 I = 1, FITRES
            XFIT(I) = SPWIN(1) + FLOAT(I-1) * DELTA
 1       CONTINUE
      END IF

*  Model with internal sampling.
      IF ( MODX )
     :   CALL MODF( FITRES, XFIT, NPAR, MODPAR, YDSH, STATUS )

*  Secondary model with internal sampling.
      IF ( MODX2 )
     :   CALL MODF2( FITRES, XFIT, NPAR, MODPAR, YLIN, STATUS )

*  With registered sampling: model minus data,
*  model minus data minus error, model minus data plus error.
*  This must be done at a lower level, since we have pointers only.
      IF ( MODX .AND. VARXST ) THEN
         CALL MODF( NDATA, %VAL( CNF_PVAL(XDAT) ), NPAR, MODPAR,
     :              %VAL( CNF_PVAL(YERU) ), STATUS )
         CALL VEC_SUBR( .TRUE., NDATA, %VAL( CNF_PVAL(YBIN) ),
     :                   %VAL( CNF_PVAL(YERU) ),
     :                   %VAL( CNF_PVAL(YCRS) ), I, J, STATUS )
         CALL VEC_SQRTR( .TRUE., NDATA, %VAL( CNF_PVAL(YVAR) ),
     :                   %VAL( CNF_PVAL(YERU) ), I, J, STATUS )
         CALL VEC_SUBR( .TRUE., NDATA, %VAL( CNF_PVAL(YCRS) ),
     :                   %VAL( CNF_PVAL(YERU) ),
     :                   %VAL( CNF_PVAL(YERL) ), I, J, STATUS )
         CALL VEC_ADDR( .TRUE., NDATA, %VAL( CNF_PVAL(YERU) ),
     :                   %VAL( CNF_PVAL(YCRS) ),
     :                   %VAL( CNF_PVAL(YERU) ), I, J, STATUS )

      ELSE IF ( MODX ) THEN
         CALL MODF( NDATA, %VAL( CNF_PVAL(XDAT) ), NPAR, MODPAR,
     :              %VAL( CNF_PVAL(YCRS) ), STATUS )
         CALL VEC_SUBR( .TRUE., NDATA, %VAL( CNF_PVAL(YBIN) ),
     :                  %VAL( CNF_PVAL(YCRS) ),
     :                  %VAL( CNF_PVAL(YCRS) ), I, J, STATUS )
      END IF

*  Work out top viewport y window.
*  ===============================

      IF ( MODX .OR. MODX2 ) THEN

*     If a linear curve exists.
         IF ( MODX2 ) THEN

*        Linear curve gives constraints for range.
            CALL SPD_PEAAR( .FALSE., FITRES, YLIN,
     :         SPWIN(5), SPWIN(6), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500

*        If error bars exist (and linear curve).
            IF ( MODX .AND. VARXST ) THEN
               CALL SPD_PEAAR( .TRUE., NDATA, %VAL( CNF_PVAL(YERL) ),
     :                         RMIN, RMAX, STATUS )
               IF ( STATUS .NE. SAI__OK ) GO TO 500
               SPWIN(5) = MIN( SPWIN(5), RMIN )
               SPWIN(6) = MAX( SPWIN(6), RMAX )
               CALL SPD_PEAAR( .TRUE., NDATA, %VAL( CNF_PVAL(YERU) ),
     :                         RMIN, RMAX, STATUS )
               IF ( STATUS .NE. SAI__OK ) GO TO 500
               SPWIN(5) = MIN( SPWIN(5), RMIN )
               SPWIN(6) = MAX( SPWIN(6), RMAX )

*        Else if crosses exist (and linear curve but not error bars).
            ELSE IF ( MODX ) THEN
               CALL SPD_PEAAR( .TRUE., NDATA, %VAL( CNF_PVAL(YCRS) ),
     :                         RMIN, RMAX, STATUS )
               IF ( STATUS .NE. SAI__OK ) GO TO 500
               SPWIN(5) = MIN( SPWIN(5), RMIN )
               SPWIN(6) = MAX( SPWIN(6), RMAX )
            END IF

*     Else if error bars exist (but no linear curve).
         ELSE IF ( MODX .AND. VARXST ) THEN
            CALL SPD_PEAAR( .TRUE., NDATA, %VAL( CNF_PVAL(YERL) ),
     :                      SPWIN(5), SPWIN(6), STATUS )
            CALL SPD_PEAAR( .TRUE., NDATA, %VAL( CNF_PVAL(YERU) ),
     :                      RMIN, RMAX, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500
            SPWIN(5) = MIN( SPWIN(5), RMIN )
            SPWIN(6) = MAX( SPWIN(6), RMAX )

*     Else if crosses exist (but neither linear curve nor error bars).
         ELSE IF ( MODX ) THEN
            CALL SPD_PEAAR( .TRUE., NDATA, %VAL( CNF_PVAL(YCRS) ),
     :                      SPWIN(5), SPWIN(6), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Else (nothing to draw in top viewport).
         ELSE
            SPWIN(5) = -1.
            SPWIN(6) = +1.
         END IF

         DELTA = SPWIN(6) - SPWIN(5)
         SPWIN(5) = SPWIN(5) - DELTA / 10.
         SPWIN(6) = SPWIN(6) + DELTA / 10.

      END IF


*  Clear the view surface.
*  =======================

      CALL PGPAGE


*  Top viewport.
*  =============

      IF ( MODX .OR. MODX2 ) THEN

*     Change to upper viewport.
         CALL PGSVP( LEFT, RIGHT, DIVISN, TOP )
         CALL PGSWIN( SPWIN(1), SPWIN(2), SPWIN(5), SPWIN(6) )

*     Plot the box.
         CALL PGBOX( 'BCTS', 0., 0, 'BCTSMV', 0., 0 )

*     Plot crosses and error bars.
         IF ( MODX )
     :      CALL SPD_PEAC( VARXST, .FALSE., .FALSE., .FALSE., MODX,
     :                     1, 5, NDATA, %VAL( CNF_PVAL(XDAT) ),
     :                     %VAL( CNF_PVAL(YCRS) ),
     :                      %VAL( CNF_PVAL(YERL) ),
     :                      %VAL( CNF_PVAL(YERU) ), 0., 0., STATUS )

*     Plot line.
         IF ( MODX2 )
     :      CALL SPD_PEAC( .FALSE., .FALSE., MODX2, .FALSE., .FALSE.,
     :         1, 0, FITRES, XFIT, YLIN, 0., 0., 0., 0.,
     :         STATUS )

      END IF


*  Bottom viewport.
*  ================

*  Change to lower viewport.
      CALL PGSVP( LEFT, RIGHT, BOTTOM, DIVISN )
      CALL PGSWIN( SPWIN(1), SPWIN(2), SPWIN(3), SPWIN(4) )

*  Plot the box.
      CALL PGBOX( 'BCTSN', 0., 0, 'BCTSNV', 0., 0 )

*  Plot the bin-style curve.
      CALL SPD_PEAC( .FALSE., .FALSE., .FALSE., SPXST, .FALSE., 1, 0,
     :               NDATA, %VAL( CNF_PVAL(XDAT) ),
     :               %VAL( CNF_PVAL(YBIN) ), 0., 0., 0., 0., STATUS )

*  Plot dashed curve.
      IF ( MODX )
     :   CALL SPD_PEAC( .FALSE., .FALSE., MODX, .FALSE., .FALSE., 2, 0,
     :                  FITRES, XFIT, YDSH, 0., 0., 0., 0., STATUS )

*  Plot mask pedestals.
      MASKY = 0.975 * SPWIN(3) + 0.025 * SPWIN(4)
      DO 2 I = 1, MSKUSE
         IF ( MASK(I) .LT. MASK(MSKDIM/2+I) ) THEN
            CALL PGMOVE( MASK(I),          SPWIN(3) )
            CALL PGDRAW( MASK(I),          MASKY    )
            CALL PGDRAW( MASK(MSKDIM/2+I), MASKY    )
            CALL PGDRAW( MASK(MSKDIM/2+I), SPWIN(3) )
         END IF
 2    CONTINUE


*  Return.
*  =======

 500  CONTINUE
      END
