      SUBROUTINE SPD_WZHAR( SPAXIS, TNPAR, PARA1,
     :   DIM12, DIM13, DIM14, DIM15, DIM16, DIM17,
     :   DIM22, DIM23, DIM24, DIM25, DIM26, DIM27,
     :   PARAMS, XVALS, DVALS, STATUS )
*+
*  Name:
*     SPD_WZHA{DR}

*  Purpose:
*     Add a Chebyshev series result component to a data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZHAR( SPAXIS, TNPAR, PARA1,
*        DIM12, DIM13, DIM14, DIM15, DIM16, DIM17,
*        DIM22, DIM23, DIM24, DIM25, DIM26, DIM27,
*        PARAMS, XVALS, DVALS, STATUS )

*  Description:
*     This routine uses the parameters PARA1 ... PARA1 + 10 from the
*     result array PARAMS as describing a finite Chebyshev series
*     component. For each element in the given and returned data array
*     DVALS, it uses the corresponding abscissa value from XVALS and
*     parameter set from PARAMS to add the component.
*
*     DVALS is usually a main NDF data array, XVALS is the corresponding
*     array of spectroscopic values from the Specdre Extension, and
*     PARAMS is the result NDF data array from the Specdre Extension.
*
*     The dimensions used for the arrays internally are different from
*     what they usually are in NDFs. This is so that the axes correspond
*     to each other and most dimensions match. The first dimension is
*     the total number of parameters for PARAMS, 1 for XVALS and DVALS.
*     The dimensions 2 to 7 are what normally would be dimensions 1 to
*     6 of XVALS and DVALS. In PARAMS they are what normally would be
*     dimensions 3 to 7 with an axis of length 1 inserted for the
*     spectroscopic axis.
*
*     This routine takes care of bad values in the data and parameter
*     arrys, though not in the array of abscissa values. Note that bad
*     parameters indicate a zero contribution from that component and
*     do not result in bad data.

*  Arguments:
*     SPAXIS = INTEGER (Given)
*        The number of the spectroscopic axis in the main NDF.
*     TNPAR = INTEGER (Given)
*        The total number of parameters in the result NDF. This is the
*        first dimension of PARAMS.
*     PARA1 = INTEGER (Given)
*        The index in 1...TNPAR of the first parameter of the result
*        component in question.
*     DIM1x = INTEGER (Given)
*        The dimensions 2 to 7 of DVALS and XVALS. These are the
*        dimensions 1 to 6 of the main NDF data array, which must be
*        less than 7-D.
*     DIM2x = INTEGER (Given)
*        The dimensions 2 to 7 of PARAMS. These are the dimensions 1 to 6
*        of the main NDF data array, only that the dimension along the
*        spectroscopic axis is set to 1 ( DIM2sp+1 = 1 ).
*     PARAMS( TNPAR, DIM2x ) = REAL (Given)
*        The parameter array. Usually this is the result NDF data array
*        corresponding to the main NDF data array DVALS. Here its
*        second dimension (always 1) is omitted and a dimension of 1
*        along the spectroscopic axis inserted.
*     XVALS( 1, DIM1x ) = REAL (Given)
*        The array of spectroscopic values. Usually this is the SPECVALS
*        data array corresponding to the main NDF data array DVALS. Here
*        its dimensions are one up and a first dimension of 1 is
*        inserted.
*     DVALS( 1, DIM1x ) = REAL (Given and Returned)
*        The array of data values. Usually this is the main NDF data
*        array. Here its dimensions are one up and a first dimension of
*        1 is inserted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     28 Jul 1992 (hme):
*        Original version.
*     03 May 1993 (hme):
*        No longer set data bad just because this component is not
*        defined. Rather an undefined component makes zero contribution
*        to the sum which data is.
*     27 Jan 1995 (hme):
*        Renamed from SPABKx.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      INTEGER SPAXIS
      INTEGER TNPAR
      INTEGER PARA1
      INTEGER DIM12, DIM13, DIM14, DIM15, DIM16, DIM17
      INTEGER DIM22, DIM23, DIM24, DIM25, DIM26, DIM27
      REAL PARAMS
     :   ( TNPAR, DIM22, DIM23, DIM24, DIM25, DIM26, DIM27 )
      REAL XVALS
     :   ( 1, DIM12, DIM13, DIM14, DIM15, DIM16, DIM17 )

*  Arguments Given and Returned:
      REAL DVALS
     :   ( 1, DIM12, DIM13, DIM14, DIM15, DIM16, DIM17 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I1, I2, I3, I4, I5, I6, I7 ! Indices for XVALS, DVALS
      INTEGER J( 7 )             ! Indices for PARAMS

*  Internal References:
      REAL SPD_UAAXR         ! Component ordinate value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop through data array, along axes 7 ... 2.
*  Axis 1 has length 1, so I1 == 1.
*  The indices J() for the parameter array are the same for axes 7 ...
*  2, except that J(SPAXIS+1) must always be 1.
      I1 = 1
      DO 1007 I7 = 1, DIM17
       J(7) = I7
       DO 1006 I6 = 1, DIM16
        J(6) = I6
        DO 1005 I5 = 1, DIM15
         J(5) = I5
         DO 1004 I4 = 1, DIM14
          J(4) = I4
          DO 1003 I3 = 1, DIM13
           J(3) = I3
           DO 1002 I2 = 1, DIM12
            J(2) = I2

*        All indices Ix, J(x) are ok now, except for spectroscopic
*        axis.
            J(SPAXIS+1) = 1

*        Add to the data array the ordinate value of the component.
*        The component ordinate value is returned by the function.
*        Its last argument is the appropriate abscissa value. The other
*        arguments of that function are XMIN, XMAX, and an array of
*        eight coefficients. In the PARAMS array, the first parameter
*        for this component is the order of the polynomial. But that is
*        not required by SPD_UAAXR, which always needs eight
*        coefficients.
*        If the data or any paramter are bad, then data is left
*        unchanged.
            IF ( DVALS(I1,I2,I3,I4,I5,I6,I7) .EQ. VAL__BADR ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 1+PARA1,J(2),J(3),J(4),J(5),J(6),J(7))
     :            .EQ. VAL__BADR ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 2+PARA1,J(2),J(3),J(4),J(5),J(6),J(7))
     :            .EQ. VAL__BADR ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 3+PARA1,J(2),J(3),J(4),J(5),J(6),J(7))
     :            .EQ. VAL__BADR ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 4+PARA1,J(2),J(3),J(4),J(5),J(6),J(7))
     :            .EQ. VAL__BADR ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 5+PARA1,J(2),J(3),J(4),J(5),J(6),J(7))
     :            .EQ. VAL__BADR ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 6+PARA1,J(2),J(3),J(4),J(5),J(6),J(7))
     :            .EQ. VAL__BADR ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 7+PARA1,J(2),J(3),J(4),J(5),J(6),J(7))
     :            .EQ. VAL__BADR ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 8+PARA1,J(2),J(3),J(4),J(5),J(6),J(7))
     :            .EQ. VAL__BADR ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 9+PARA1,J(2),J(3),J(4),J(5),J(6),J(7))
     :            .EQ. VAL__BADR ) THEN
               CONTINUE
            ELSE IF ( PARAMS(10+PARA1,J(2),J(3),J(4),J(5),J(6),J(7))
     :            .EQ. VAL__BADR ) THEN
               CONTINUE
            ELSE
               DVALS( I1, I2, I3, I4, I5, I6, I7 ) =
     :            DVALS( I1, I2, I3, I4, I5, I6, I7 ) +
     :            SPD_UAAXR(
     :               PARAMS( 1+PARA1, J(2),J(3),J(4),J(5),J(6),J(7) ),
     :               PARAMS( 2+PARA1, J(2),J(3),J(4),J(5),J(6),J(7) ),
     :               PARAMS( 3+PARA1, J(2),J(3),J(4),J(5),J(6),J(7) ),
     :               XVALS( I1, I2, I3, I4, I5, I6, I7 ) )
            END IF
 1002      CONTINUE
 1003     CONTINUE
 1004    CONTINUE
 1005   CONTINUE
 1006  CONTINUE
 1007 CONTINUE

*  Return.
      END
