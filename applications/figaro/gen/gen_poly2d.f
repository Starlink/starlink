      REAL FUNCTION GEN_POLY2D( X, Y, NCX, NCY, COEFF, CT )
*+
*  Name:
*     GEN_POLY2D

*  Purpose:
*     Evaluates a two dimensional polynomial.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = GEN_POLY2D( X, Y, NCX, NCY, COEFF, CT )

*  Description:
*     The evaluation is as follows -
*
*        RESULT = CT(1) + CT(2)*X + CT(3)*X**2 +
*            .... CT(NCX)*X**(NCX-1)
*
*     where the CTs are coefficients calculated as -
*
*        CT(n) = COEFF(1,n) + COEFF(2,n)*Y +
*           .... COEFF(NCY,n)*Y**(NCY-1)
*
*     This program is based on an original FORTRAN routine by JT, but
*     has COEFF reversed.
*     Note that this routine is equivalent to the following fortran
*     routine:
*
*        DO I = 1, NCX
*           CT(I) = GEN_EPOLYD( DBLE(Y), COEFF(1,I), NCY )
*        END DO
*        GEN_POLY2D = GEN_EPOLYD( DBLE(X), CT, NCX )
*
*     Well, so it said in the VAX macro routine, and in the
*     Sun Figaro C routine. Still, the 3.1-0 C routine was translated
*     into Fortran from the code.

*  Arguments:
*     X = REAL (Given)
*        The X value to be used.
*     Y = REAL (Given)
*        The Y value to be used.
*     NCX = INTEGER (Given)
*        The number of X-coefficients.
*     NCY = INTEGER (Given)
*        The number of Y-coefficients.
*     COEFF( NCY, NCX ) = DOUBLE PRECISION (Given)
*        Note the order of the coefficients in this array: COEFF(,NCX)
*        represents the CONSTANT terms in the X expansion, and
*        COEFF(NCY,) represents the CONSTANT terms in the for Y. This is
*        the reverse of what might be expected, and is required in order
*        that the function may make efficient use of the POLYD VAX.
*     CT( NCX ) = DOUBLE PRECISION (Given and Returned)
*        A work space.

*  Returned Value:
*     GEN_POLY2D = REAL

*  Authors:
*     ks:  Keith Shortridge (CIT)
*     ckl: {authors_name} (CIT)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     09 Dec 1982 (ks):
*        Original in VAX MACRO.
*     21 Nov 1988 (ckl):
*        Converted to C for SUN.
*     29 Sep 1992 (hme):
*        Always include math.h, always use float type.
*     25 Oct 1992 (hme):
*        Re-typed according to ANSI.
*     18 Mar 1993 (hme):
*        The circle has been closed now. Translated to Fortran. Might
*        have asked JT for the original.
*        Require the calling routine to supply a work space for NCX
*        double precision numbers.
*        There was a bug anyway, in that dvalue was calculated ncx
*        times (inside the loop), only the last one was correct.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      REAL X
      REAL Y
      INTEGER NCX
      INTEGER NCY
      DOUBLE PRECISION COEFF( NCY, NCX )

*  Arguments Given and Returned:
      DOUBLE PRECISION CT( NCX )

*  Local Variables:
      INTEGER I
      REAL VALUE
      DOUBLE PRECISION XVAL
      DOUBLE PRECISION YVAL

*  Internal References:
      DOUBLE PRECISION GEN_EPOLYD

*.

      VALUE  = 0.

      XVAL = DBLE( X )
      YVAL = DBLE( Y )

      DO 1 I = 1, NCX
         CT(I) = GEN_EPOLYD( YVAL, COEFF(1,I), NCY )
 1    CONTINUE

      GEN_POLY2D = GEN_EPOLYD( XVAL, CT, NCX )

      END
