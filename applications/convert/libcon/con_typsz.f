      SUBROUTINE  CON_TYPSZ (TYPE, NBYTES, STATUS)         
*+
*  Name:
*     CON_TYPSZ

*  Purpose:
*     Returns the number of bytes used to store an item of 
*     any of the HDS primitive data types.      


*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBROUTINE  CON_TYPSZ (TYPE, NBYTES, STATUS)         

*  Description:
*     If the input TYPE is one of the HDS primitive numeric data types,
*     i.e. one of _REAL, _DOUBLE, _INTEGER, _WORD, _UWORD, _BYTE or 
*      _UBYTE, then the number of bytes used by that data type is
*     returned as NBYTES. The values are those stored as the symbolic
*     constants VAL__NBx in the PRM_PAR include file. (See SUN/39.)
*     If the TYPE is not one of the above, an error results and
*     STATUS is set.

*  Arguments:
*     TYPE = CHARACTER (Given)
*        The data type.
*     NBYTES = INTEGER (Returned)
*        The number of bytes used for this data type.
*     STATUS = INTEGER (Given and returned)
*        Global status.

*  Author:
*     JM: Jo Murray (STARLINK)

*  History:
*     1990 Aug 27 (JM):
*        Original version.

*  Bugs:
*     None known.

*-

*  Type Definitions.
      IMPLICIT  NONE               ! No implicit typing.


*  Global constants.
      INCLUDE  'SAE_PAR'           ! SSE global definitions
      INCLUDE  'PRM_PAR'           ! PRIMDAT symbolic constants
      INCLUDE  'NDF_PAR'           ! NDF symbolic constants

*  External functions:
      LOGICAL CHR_SIMLR            ! Compare character strings

*  Arguments Given:
      CHARACTER*(NDF__SZTYP) TYPE  ! Data type

*  Arguments Returned:
      INTEGER   NBYTES             ! No. bytes required

*  Status:
      INTEGER   STATUS             ! Global status

*.

*  Check inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Assign number of BYTES appropriate for data type to NBYTES.
      IF (CHR_SIMLR (TYPE, '_REAL') ) THEN
         NBYTES = VAL__NBR
      ELSEIF (CHR_SIMLR (TYPE, '_DOUBLE') ) THEN
         NBYTES = VAL__NBD
      ELSEIF (CHR_SIMLR (TYPE, '_INTEGER') ) THEN
         NBYTES = VAL__NBI
      ELSEIF (CHR_SIMLR (TYPE, '_WORD') ) THEN
         NBYTES = VAL__NBW
      ELSEIF (CHR_SIMLR (TYPE, '_UWORD') ) THEN
         NBYTES = VAL__NBUW
      ELSEIF (CHR_SIMLR (TYPE, '_BYTE') ) THEN
         NBYTES = VAL__NBB
      ELSEIF (CHR_SIMLR (TYPE, '_UBYTE') ) THEN
         NBYTES = VAL__NBUB
      ELSE
*       If type not recognised, report error.
         STATUS = SAI__ERROR
         CALL ERR_REP ('CON_TYPSZ_INVTYP',
     :                  '^TYPE is not a primitive numeric type.',
     :                  STATUS)
      ENDIF

      END

