      SUBROUTINE FTS1_I2VXR( BSWAP, WSWAP, EL, BUF, STATUS )
*+
*  Name:
*     FTS1_I2VXR

*  Purpose:
*     Converts a vector of 32-bit IEEE floating-point numbers to Vax-F
*     format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_I2VXR( BSWAP, WSWAP, EL, BUF, STATUS )

*  Description:
*     This is a dummy routine used to build the KAPPA FITS readers on
*     UNIX.

*  Arguments:
*     BSWAP = INTEGER (Given)
*        Whether or not byte swapping is to take place.  If and only if
*        BSWAP is 1 will the bytes be swapped.  Swapping must occur
*        either prior or within this routine to obtain the correct Vax-F
*        values.  An expression must not be given for this argument.
*     WSWAP = INTEGER (Given)
*        Whether or not word swapping is to take place.  If and only if
*        WSWAP is 1 will the words be swapped.  Swapping must occur
*        either prior or within this routine to obtain the correct Vax-F
*        values.  An expression must not be given for this argument.
*     EL = INTEGER (Given)
*        The number of IEEE numbers to be converted.
*     BUF( EL ) = DOUBLE PRECISION (Given and Returned)
*        On input the IEEE numbers to be converted.  On return these
*        are converted to Vax-F format.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     -  "IEEE Standard for Binary Floating-Point Arithmetic",
*     ANSI/IEEE 754, 1985.
*     -  "Floating Point Agreement for FITS", D.C. Wells & P. Grosbol, 
*     1990.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 January 25 (MJC):
*        Original version.
*     1993 January 7 (MJC):
*        Dummy version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing

*  Arguments Given:
      INTEGER
     :  EL

      LOGICAL
     :  BSWAP,
     :  WSWAP

*  Arguments Given and Returned:
      DOUBLE PRECISION BUF( EL )

*  Status:
      INTEGER STATUS           ! Global status

*. 

*  Do nothing.

      END
