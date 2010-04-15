      SUBROUTINE SPD_WAAD( ERROR, WIDTH, LIN, BIN, DOMARK, DASH, PGMARK,
     :   NELM, X, Y, YME, YPE, XMW, XPW, STATUS )
*+
*  Name:
*     SPD_WAAD

*  Purpose:
*     Plot set of data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WAAD( ERROR, WIDTH, LIN, BIN, DOMARK, DASH, PGMARK,
*        NELM, X, Y, YME, YPE, XMW, XPW, STATUS )

*  Description:
*     This routine plots a set of data points in various ways. Data
*     points are ignored if (i) the Y value is bad or (ii) error bars
*     are selected and the error bar start or end are bad. The data can
*     be plotted with line-style or bin-style connections, with five
*     different dash patterns. Data points can be plotted as markers.
*     Error and width bars can be added.
*
*     This routine is a copy of the same routine in UTIL. A copy exists
*     here to make the SPLOOP library self-contained.

*  Arguments:
*     ERROR = LOGICAL (Given)
*        True if error bars requested.
*     WIDTH = LOGICAL (Given)
*        True if pixel width bars requested.
*     LIN = LOGICAL (Given)
*        True if line-style connections requested.
*     BIN = LOGICAL (Given)
*        True if bin-style connections requested.
*     DOMARK = LOGICAL (Given)
*        True if markers requested.
*     DASH = INTEGER (Given)
*        PGPLOT dash pattern index.
*     PGMARK = INTEGER (Given)
*        PGPLOT marker number.
*     NELM = INTEGER (Given)
*        Size of arrays.
*     X( NELM ) = REAL (Given)
*        Array of X values.
*     Y( NELM ) = REAL (Given)
*        Array of Y values.
*     YME( NELM ) = REAL (Given)
*        Array of Y minus error.
*     YPE( NELM ) = REAL (Given)
*        Array of Y plus error.
*     XMW( NELM ) = REAL (Given)
*        Array of X minus half pixel width.
*     XPW( NELM ) = REAL (Given)
*        Array of X plus half pixel width.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     21 Sep 1991 (hme):
*        Original version.
*     23 Sep 1991 (hme):
*        Do not call PGBIN or PGLINE when IELM < 2.
*     26 Nov 1991 (hme):
*        Bug fix: PGERRX was called without y-array.
*     23 Mar 1992 (hme):
*        Instead of restoring dash pattern to 1, restore it to what it
*        was before this routine was called.
*     23 Jan 1993 (hme):
*        Fix bug where (last+1)st element was read from memory.
*     25 Jun 1993 (hme):
*        Use standard PGPLOT calls (short names).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primdat constants

*  Arguments Given:
      LOGICAL ERROR
      LOGICAL WIDTH
      LOGICAL LIN
      LOGICAL BIN
      LOGICAL DOMARK
      INTEGER DASH
      INTEGER PGMARK
      INTEGER NELM
      REAL X( NELM )
      REAL Y( NELM )
      REAL YME( NELM )
      REAL YPE( NELM )
      REAL XMW( NELM )
      REAL XPW( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER ISTA               ! Start of a good range
      INTEGER IELM               ! Length of a good range
      INTEGER ODASH              ! Old dash pattern

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the pre-plot dash pattern and initialise pattern to standard.
      CALL PGQLS( ODASH )
      CALL PGSLS( 1 )

*  Initialise counter.
      I = 1

*  If error bars selected (check YPE against VAL__BADR).
      IF ( ERROR ) THEN

*     While counter within array.
 1       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( I .LE. NELM ) THEN

*        While data are bad and counter within array.
 2          CONTINUE             ! Start of 'DO WHILE' loop
*           IF ( YPE(I) .EQ. VAL__BADR .AND. I .LE. NELM ) THEN
            IF ( I .LE. NELM ) THEN
               IF ( YPE(I) .EQ. VAL__BADR ) THEN

*              Increment counter
                  I = I + 1
                  GO TO 2
               END IF
            END IF

*        If counter exceeded array length, escape.
            IF ( I .GT. NELM ) GO TO 500

*        Store counter as begin of good range.
            ISTA = I

*        While data are good and counter within array.
 3          CONTINUE             ! Start of 'DO WHILE' loop
*           IF ( YPE(I) .NE. VAL__BADR .AND. I .LE. NELM ) THEN
            IF ( I .LE. NELM ) THEN
               IF ( YPE(I) .NE. VAL__BADR ) THEN

*              Increment counter
                  I = I + 1
                  GO TO 3
               END IF
            END IF

*        Counter is one behind end of good range. Store the number of
*        successive good data.
            IELM = I - ISTA

*        Plot the good range just found.
            CONTINUE

*           Plot the markers, error bars, width bars.
               IF ( DOMARK )
     :            CALL PGPT( IELM, X(ISTA), Y(ISTA), PGMARK )
               CALL PGERRY( IELM, X(ISTA), YME(ISTA), YPE(ISTA), 1. )
               IF ( WIDTH )
     :            CALL PGERRX( IELM, XMW(ISTA), XPW(ISTA), Y(ISTA), 0. )

*           Plot the line-style and bin-style connections.
               IF ( DASH .NE. 1 ) CALL PGSLS( DASH )
               IF ( LIN .AND. IELM .GE. 2 )
     :            CALL PGLINE( IELM, X(ISTA), Y(ISTA) )
               IF ( BIN .AND. IELM .GE. 2 )
     :            CALL PGBIN( IELM, X(ISTA), Y(ISTA), .TRUE. )
               IF ( DASH .NE. 1 ) CALL PGSLS( 1 )
            GO TO 1
         END IF

*  Else (no error bars selected; check Y against VAL__BADR).
      ELSE

*     While counter within array.
 4       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( I .LE. NELM ) THEN

*        While data are bad and counter within array.
 5          CONTINUE             ! Start of 'DO WHILE' loop
*           IF ( Y(I) .EQ. VAL__BADR .AND. I .LE. NELM ) THEN
            IF ( I .LE. NELM ) THEN
               IF ( Y(I) .EQ. VAL__BADR ) THEN

*              Increment counter
                  I = I + 1
                  GO TO 5
               END IF
            END IF

*        If counter exceeded array length, escape.
            IF ( I .GT. NELM ) GO TO 500

*        Store counter as begin of good range.
            ISTA = I

*        While data are good and counter within array.
 6          CONTINUE             ! Start of 'DO WHILE' loop
*           IF ( Y(I) .NE. VAL__BADR .AND. I .LE. NELM ) THEN
            IF ( I .LE. NELM ) THEN
               IF ( Y(I) .NE. VAL__BADR ) THEN

*              Increment counter
                  I = I + 1
                  GO TO 6
               END IF
            END IF

*        Counter is one behind end of good range. Store the number of
*        successive good data.
            IELM = I - ISTA

*        Plot the good range just found.
            CONTINUE

*           Plot the markers, error bars, width bars.
               IF ( DOMARK )
     :            CALL PGPT( IELM, X(ISTA), Y(ISTA), PGMARK )
               IF ( WIDTH )
     :            CALL PGERRX( IELM, XMW(ISTA), XPW(ISTA), Y(ISTA), 0. )

*           Plot the line-style and bin-style connections.
               IF ( DASH .NE. 1 ) CALL PGSLS( DASH )
               IF ( LIN .AND. IELM .GE. 2 )
     :            CALL PGLINE( IELM, X(ISTA), Y(ISTA) )
               IF ( BIN .AND. IELM .GE. 2 )
     :            CALL PGBIN( IELM, X(ISTA), Y(ISTA), .TRUE. )
               IF ( DASH .NE. 1 ) CALL PGSLS( 1 )
            GO TO 4
         END IF
      END IF

*  Tidy up.
 500  CONTINUE

*  Restore the pre-plot dash pattern.
      CALL PGSLS( ODASH )

*  Return.
      END
