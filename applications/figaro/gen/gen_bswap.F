#if HAVE_CONFIG_H
#  include "config.h"
#endif
      SUBROUTINE GEN_BSWAP( ARRAY, WORDS )
*+
*  Name:
*     GEN_BSWAP

*  Purpose:
*     Swap bytes of words.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GEN_BSWAP( ARRAY, WORDS )

*  Description:
*     This routine exchanges the byte order in an array of 2-byte words
*     from/to IBM order to/from local order.  This means no action on
*     Sun4, but exchanging the first and last byte on VAX, MIPS
*     (DECstation), Alpha AXP and Linux.

*  Arguments:
*     ARRAY( WORDS ) = INTEGER * 2 (Given and Returned)
*        The array of 2-byte words to be byte-swapped.
*     WORDS = INTEGER (Given)
*        The length of ARRAY.

*  Notes:
*     This routine has been updated to be platform independant using the
*     Fortran Pre-processor and the new autoconf build system.

*  Authors:
*     ks: Keith Shortridge (CIT, AAO)
*     ckl (CIT)
*     sns: Sam Southard (CIT)
*     bcd: Brad Carter (UNSW)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     aa: Alasdair Allan(University of Exeter, Starlink)
*     {enter_new_authors_here}

*  History:
*     11 Oct 1983 (ks):
*        Original version.
*     30 Apr 1986 (ks):
*        Psect name changed.
*     21 Nov 1988 (ckl):
*        Converted to C for SUN.
*     08 Apr 1990 (sns):
*        NULL version added for VMS.
*     05 Feb 1993 (bcd):
*        DECstation-specific version.
*     04 Mar 1993 (ks):
*        Now compiles for any machine, using the 'byteswap' flag.
*     19 Jul 1993 (hme):
*        Use f77 for portable interface to Fortran.
*     13 Aug 1993 (hme):
*        Translate to Fortran.  You never know how long or short an int
*        is in C.  On Alpha AXP a short is a big as a long on Sun4.
*     13 Jul 2004 (aa):
*        Updated to be platform independant using the Fortran Pre-processor
*        and the new autoconf build system.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER WORDS

*  Arguments Given and Returned:
      INTEGER * 2 ARRAY( WORDS )

#if WORDS_BIGENDIAN == 0

*  Local Variables:
      INTEGER I                  ! Lopp index
      INTEGER * 2 AWORD          ! A copy of an ARRAY element
      BYTE BWORD( 2 )            ! The bytes of AWORD
      BYTE ABYTE                 ! A copy of a BWORD element
      EQUIVALENCE ( AWORD, BWORD(1) )

*.

      DO 1 I = 1, WORDS
         AWORD    = ARRAY(I)
            ABYTE    = BWORD(1)
            BWORD(1) = BWORD(2)
            BWORD(2) = ABYTE
         ARRAY(I) = AWORD
 1    CONTINUE

#endif

      END
