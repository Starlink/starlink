      SUBROUTINE WBSWAP( ARRAY, LWORDS )
*+
*  Name:
*     GEN_WBSWAP

*  Purpose:
*     Swap bytes of long words.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GEN_WBSWAP( ARRAY, LWORDS )

*  Description:
*     This routine exchanges the byte order in an array of 4-byte words
*     from/to IBM order to/from local order.  This means no action on
*     Sun4. But on VAX, MIPS (DECstation), Alpha AXP and Linux the byte order
*     is reversed: The first and last byte are exchanged and the second,
*     and third are exchanged, too.

*  Arguments:
*     ARRAY( LWORDS ) = INTEGER (Given and Returned)
*        The array of 4-byte words to be byte-swapped.
*     LWORDS = INTEGER (Given)
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
*     timj: Tim Jenness (JAC, Hawaii)
*     rpt: Remo Tilanus (JAC, Hawaii)
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
*        is in C.  On Alpha AXP a short is a big as a long on Sun4..
*     3 May 1999 (RPT):
*        Rip-off Figaro C GEN_WBSWAP: strip
*        preprocessor code since SPECX allows optional swapping.
*     13 Jul 2004 (aa):
*        Updated to be platform independant using the Fortran Pre-processor
*        and the new autoconf build system.
*     26 Oct 2012 (timj):
*        Straight copy of Fortran version from Figaro with function renamed.
*        The previous C specx version used long integers and was not tied
*        to the size of an INTEGER*4. Simplest to just re-use the Figaro version.
*        Note that we remove the pre-processor checks as SPECX decides whether
*        to do the swap.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER LWORDS

*  Arguments Given and Returned:
      INTEGER ARRAY( LWORDS )

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER AWORD              ! A copy of an ARRAY element
      BYTE BWORD( 4 )            ! The bytes of AWORD
      BYTE ABYTE                 ! A copy of a BWORD element
      EQUIVALENCE ( AWORD, BWORD(1) )

*.

      DO 1 I = 1, LWORDS
         AWORD    = ARRAY(I)
            ABYTE    = BWORD(1)
            BWORD(1) = BWORD(4)
            BWORD(4) = ABYTE
            ABYTE    = BWORD(2)
            BWORD(2) = BWORD(3)
            BWORD(3) = ABYTE
         ARRAY(I) = AWORD
 1    CONTINUE

      END
