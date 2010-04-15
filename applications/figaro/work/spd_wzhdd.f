      SUBROUTINE SPD_WZHDD( SPAXIS, TNPAR, PARA1,
     :   DIM12, DIM13, DIM14, DIM15, DIM16, DIM17,
     :   DIM22, DIM23, DIM24, DIM25, DIM26, DIM27,
     :   PARAMS, XVALS, DVALS, STATUS )
*+
*  Name:
*     SPD_WZHD{DR}

*  Purpose:
*     Add a polynomial result component to a data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZHDD( SPAXIS, TNPAR, PARA1,
*        DIM12, DIM13, DIM14, DIM15, DIM16, DIM17,
*        DIM22, DIM23, DIM24, DIM25, DIM26, DIM27,
*        PARAMS, XVALS, DVALS, STATUS )

*  Description:
*     This routine uses the parameters PARA1 ... PARA1 + 8 from the
*     result array PARAMS as describing a polynomial
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
*     arrays, though not in the array of abscissa values. Note that bad
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
*     PARAMS( TNPAR, DIM2x ) = DOUBLE PRECISION (Given)
*        The parameter array. Usually this is the result NDF data array
*        corresponding to the main NDF data array DVALS. Here its
*        second dimension (always 1) is omitted and a dimension of 1
*        along the spectroscopic axis inserted.
*     XVALS( 1, DIM1x ) = DOUBLE PRECISION (Given)
*        The array of spectroscopic values. Usually this is the SPECVALS
*        data array corresponding to the main NDF data array DVALS. Here
*        its dimensions are one up and a first dimension of 1 is
*        inserted.
*     DVALS( 1, DIM1x ) = DOUBLE PRECISION (Given and Returned)
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
*     24 Nov 1995 (hme):
*        Adapt the Chebyshev series routine for an ordinary polynomial.
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
      DOUBLE PRECISION PARAMS
     :   ( TNPAR, DIM22, DIM23, DIM24, DIM25, DIM26, DIM27 )
      DOUBLE PRECISION XVALS
     :   ( 1, DIM12, DIM13, DIM14, DIM15, DIM16, DIM17 )

*  Arguments Given and Returned:
      DOUBLE PRECISION DVALS
     :   ( 1, DIM12, DIM13, DIM14, DIM15, DIM16, DIM17 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I1, I2, I3, I4, I5, I6, I7 ! Indices for XVALS, DVALS
      INTEGER J( 7 )             ! Indices for PARAMS
      INTEGER K                  ! Loop index for polynomial value
      INTEGER ORDER              ! Order of polynomial
      DOUBLE PRECISION TEMP1               ! Buffer for polynomial value
      DOUBLE PRECISION TEMP0               ! Buffer for abscissa value

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
*        In the PARAMS array, the first parameter
*        for this component is the order of the polynomial.
*        If the data or any paramter are bad, then data is left
*        unchanged.
*        The polynomial is evaluated as follows: First the value is set
*        to the highest coefficient C_{ORDER+1}. Say, for second order
*        it must be the third coefficient or the fourth element in
*        PARAMS (the first being the order). So it is offset by three
*        from the first, hence 3+PARA1 = 1+ORDER+PARA1. Then a loop is
*        entered that goes down the exponents until the lowest
*        coefficient has been added. Of course before each coefficient
*        is added in, the previous value is multiplied by the argument.
*        P = C(ORDER+1)
*        DO 1 K = ORDER, 1, -1
*           P = P * XX
*           P = P + C(K)
*      1 CONTINUE
            ORDER = PARAMS( PARA1,J(2),J(3),J(4),J(5),J(6),J(7) )
            IF ( DVALS(I1,I2,I3,I4,I5,I6,I7) .EQ. VAL__BADD ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 1+PARA1,J(2),J(3),J(4),J(5),J(6),J(7) )
     :            .EQ. VAL__BADD ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 2+PARA1,J(2),J(3),J(4),J(5),J(6),J(7) )
     :            .EQ. VAL__BADD ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 3+PARA1,J(2),J(3),J(4),J(5),J(6),J(7) )
     :            .EQ. VAL__BADD ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 4+PARA1,J(2),J(3),J(4),J(5),J(6),J(7) )
     :            .EQ. VAL__BADD ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 5+PARA1,J(2),J(3),J(4),J(5),J(6),J(7) )
     :            .EQ. VAL__BADD ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 6+PARA1,J(2),J(3),J(4),J(5),J(6),J(7) )
     :            .EQ. VAL__BADD ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 7+PARA1,J(2),J(3),J(4),J(5),J(6),J(7) )
     :            .EQ. VAL__BADD ) THEN
               CONTINUE
            ELSE IF ( PARAMS( 8+PARA1,J(2),J(3),J(4),J(5),J(6),J(7) )
     :            .EQ. VAL__BADD ) THEN
               CONTINUE
            ELSE
               TEMP0 = XVALS( I1, I2, I3, I4, I5, I6, I7 )
               TEMP1 = PARAMS(
     :            1+ORDER+PARA1,J(2),J(3),J(4),J(5),J(6),J(7) )
               DO 1001 K = ORDER, 1, -1
                  TEMP1 = TEMP1 * TEMP0
                  TEMP1 = TEMP1 + PARAMS(
     :               K+PARA1,J(2),J(3),J(4),J(5),J(6),J(7) )
 1001          CONTINUE
               DVALS( I1, I2, I3, I4, I5, I6, I7 ) =
     :            DVALS( I1, I2, I3, I4, I5, I6, I7 ) + TEMP1
            END IF
 1002      CONTINUE
 1003     CONTINUE
 1004    CONTINUE
 1005   CONTINUE
 1006  CONTINUE
 1007 CONTINUE

*  Return.
      END
