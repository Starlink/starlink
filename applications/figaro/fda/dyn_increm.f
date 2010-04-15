      INTEGER FUNCTION DYN_INCREMENT( OLD, TYPE, INCR )
*+
*  Name:
*     DYN_INCREMENT

*  Purpose:
*     Increment a dynamic memory element by a number of array elements.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = DYN_INCREMENT( OLD, TYPE, INCR )

*  Description:
*     If a given element of the conceptual array DYNAMIC_MEM may be
*     used to address a specific element of a dynamic memory array,
*     then this routine returns the element of DYNAMIC_MEM that may be
*     used to address a higher element in the dynamic array.  Using
*     this routine relieves the calling routine of the need to worry
*     about the details of the implementation - ie 'what type has
*     DYNAMIC_MEM been declared as?', 'how many bytes are there to an
*     element of a DOUBLE array?'. Note that this routine does not
*     output error messages or return any error status - if it does not
*     recognise the type it does nothing.

*  Arguments:
*     OLD = INTEGER (Given)
*        The element of DYNAMIC_MEM that corresponds to a specific
*        element of a dynamically allocated memory array.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of the dynamically allocated memory array.  This
*        should be one of 'FLOAT ', 'INT', 'DOUBLE', 'SHORT', 'CHAR',
*        'BYTE', or'USHORT'. Case is not significant.  If TYPE is none
*        of these, the result will quietly be set equal to OLD.
*     INCR = INTEGER (Given)
*        The number of elements of the dynamically allocated array by
*        which the current element number is to be incremented.

*  Returned Value:
*     DYN_INCREMENT = INTEGER
*        The element of DYNAMIC_MEM corresponding to the required
*        element of the dynamically allocated array.

*  Notes:
*     This routine is platform specific insofar as data types may have
*     different lengths in bytes on different machines.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     mjcl: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     21 Jul 1987 (ks):
*        Original version.
*     08 May 1990 (ks):
*        USHORT added (rather belatedly).
*     27 Jun 1992 (hme):
*        Port to Unix.
*     10 Aug 1993 (hme):
*        Replace CHR_UCASE with ICH_FOLD.
*     24 Nov 1995 (hme):
*        Use DSA_TYPES include file. Use CHR_UCASE.
*     30 Jul 1996 (mjcl):
*        Move DSA_TYPES include as it contains DATA statements.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER OLD
      CHARACTER * ( * ) TYPE
      INTEGER INCR

*  Local Variables:
      INTEGER I                  ! Loop index
      CHARACTER * ( 6 ) TYPEUC   ! Given type in upper case

*  Global Constants:
      INCLUDE 'DSA_TYPES'        ! DSA data types and their sizes

*.

      TYPEUC = TYPE
      CALL CHR_UCASE( TYPEUC )
      DO 1 I = 1, MAX_TYPES
         IF ( TYPEUC .EQ. TYPE_NAMES(I) ) THEN
            DYN_INCREMENT = OLD + INCR * TYPE_SIZE(I)
            GO TO 2
         END IF
 1    CONTINUE
 2    CONTINUE

      END
