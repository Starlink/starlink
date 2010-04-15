      SUBROUTINE DTA1_SPLIT( PATH, MXDIM,
     :   IDOT1, IDOT2, IBRA, NDIM, DIMS, STATUS )
*+
*  Name:
*     DTA1_SPLIT

*  Purpose:
*     Split DTA object name into constituents.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA1_SPLIT( PATH, MXDIM,
*        IDOT1, IDOT2, IBRA, NDIM, DIMS, STATUS )

*  Description:
*     This routine parses the given DTA object name and returns the
*     pointers into the string where the first and last dots are, and
*     where the cell or dimension information begins. The routine also
*     returns the cell/dimension as an array of integers.
*
*     A typical DTA object name might look like the following, and the
*     returned pointers point would be:
*
*        DSAREF.LEVEL1.LEVEL2[1,2,3].LEVEL3.COMPON[5,6,78]
*              ^                           ^      ^
*              IDOT1                       IDOT2  IBRA
*
*     The rational is as follows: The first component in a full DTA
*     object name is always the DSA reference name. Obtaining an HDS
*     locator for it is different than for any of the intermediate
*     levels. The last component may have to be treated separately, for
*     instance if it is to be created or deleted. In that case one needs
*     to locate its parent rather than the component itself. The final
*     cell/dimension array needs to be split off because one cannot get
*     a locator directly. One has to DAT_FIND the component and then to
*     DAT_CELL with the cell specified as a vector of integers.
*
*     This routine is not concerned with any cell specifications in the
*     interior of the object name. That will have to be sorted out by
*     the routine that digs its way down the various LEVELs.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The DTA object name.
*     MXDIM = INTEGER (Given)
*        The highest dimensionality the calling routine can cope with,
*        declared length of DIMS.
*     IDOT1 = INTEGER (Returned)
*        PATH(IDOT1:IDOT1) is the first dot in PATH. If there is no dot
*        at all, then this is returned as zero.
*     IDOT2 = INTEGER (Returned)
*        PATH(IDOT2:IDOT2) is the last dot in PATH (not the second
*        dot!). If there is no dot at all, then this is returned as
*        zero.
*     IBRA = INTEGER (Returned)
*        If the last non-blank character in PATH is a closing square
*        bracket, then PATH(IBRA:IBRA) is the last opening square
*        bracket. Otherwise it is returned as zero.
*     NDIM = INTEGER (Returned)
*        This indicates how many positive integers were found in
*        PATH(IBRA:). This is returned as zero if the object name does
*        not end with a closing square bracket.
*     DIMS( MXDIM ) = INTEGER (Returned)
*        The integer values found in PATH(IBRA:). If MXDIM is greater
*        than NDIM, then the remaining elements will be returned as
*        one.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     29 Feb 1996 (hme):
*        Original version.
*     05 Mar 1996 (hme):
*        Allow for temporary HDS structure during creation by structure
*        definition.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PATH
      INTEGER MXDIM

*  Arguments Returned:
      INTEGER IDOT1
      INTEGER IDOT2
      INTEGER IBRA
      INTEGER NDIM
      INTEGER DIMS( MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER IEND               ! End of string

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  IDOT1 is easy, just use the INDEX() function.
      IDOT1 = INDEX( PATH, '.' )

*  Parse from the end of the sting.
      IEND = CHR_LEN( PATH )

*  If there is a cell/dimension at the end, find its beginning.
      IF ( PATH(IEND:IEND) .EQ. ']' ) THEN
         IBRA = IEND - 1
 1       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( IBRA .GT. IDOT1 ) THEN
            IF ( PATH(IBRA:IBRA) .EQ. '[' ) GO TO 2
            IBRA = IBRA - 1
            GO TO 1
         END IF
 2       CONTINUE
         IF ( IBRA .LE. IDOT1 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T016', PATH )
            CALL ERR_REP( 'FDA_E081', 'DTA1_SPLIT: Error delimiting ' //
     :         'cell specification in DTA object name ^FDA_T016.',
     :         STATUS )
            GO TO 500
         END IF
      ELSE
         IBRA = 0
      END IF

*  Going backwards from the cell/dimension, find the last dot.
      IF ( IBRA .EQ. 0 ) THEN
         IDOT2 = IEND
      ELSE
         IDOT2 = IBRA - 1
      END IF
 3    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( IDOT2 .GT. IDOT1 ) THEN
         IF ( PATH(IDOT2:IDOT2) .EQ. '.' ) GO TO 4
         IDOT2 = IDOT2 - 1
         GO TO 3
      END IF
 4    CONTINUE

*  Convert the cell/dimension to an integer vector.
      IF ( IBRA .GT. 0 ) THEN
         CALL DTA1_DECDIM( PATH(IBRA+1:IEND-1), MXDIM,
     :      NDIM, DIMS, STATUS )
      ELSE
         NDIM = 0
         DO 5 I = 1, MXDIM
            DIMS(I) = 1
 5       CONTINUE
      END IF

*  Return.
 500  CONTINUE
      END
