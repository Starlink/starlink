      SUBROUTINE CCD1_MEAN( PTYPE, BAD, IPVEC, SIZE, AVEACC, VALPIX,
     :                      STATUS )
*+
*  Name:
*     CCD1_MEAN

*  Purpose:
*     To pass data of type PTYPE to form the mean of the vectorised
*     array VEC

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MEAN( PTYPE, BAD, VEC, SIZE, AVEACC, VALPIX, STATUS )

*  Description:
*     This routine passes the vectorised array to the appropriate
*     typed routine.

*  Arguments:
*     PTYPE = CHARACTER * ( * ) (Given)
*        The type of the data pointed to by IPVEC.
*     BAD = LOGICAL (Given)
*        Flag set if there are bad pixels present in input array.
*     IPVEC = INTEGER (Given)
*        Pointer to the vectorised array of values.
*     SIZE = INTEGER (Given)
*        Size of the pointed to array.
*     AVEACC = DOUBLE PRECISION (Returned)
*        The average value of the array expressed in double precision.
*     VALPIX = INTEGER (Returned)
*        The number of non-bad pixels in the vectorised array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-MAY-1991 (PDRAPER):
*        Original Version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER PTYPE * ( * )
      LOGICAL BAD
      INTEGER SIZE
      INTEGER IPVEC

*  Arguments Returned:
      DOUBLE PRECISION AVEACC
      INTEGER VALPIX

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Pass the data to the appropriate routine.
      IF ( PTYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_MEANB( BAD, %VAL( IPVEC ), SIZE, AVEACC, VALPIX,
     :                    STATUS )
      ELSE IF ( PTYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_MEANUB( BAD, %VAL( IPVEC ), SIZE, AVEACC, VALPIX,
     :                     STATUS )
      ELSE IF ( PTYPE .EQ. '_WORD' ) THEN
         CALL CCG1_MEANW( BAD, %VAL( IPVEC ), SIZE, AVEACC, VALPIX,
     :                    STATUS )
      ELSE IF ( PTYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_MEANUW( BAD, %VAL( IPVEC ), SIZE, AVEACC, VALPIX,
     :                     STATUS )
      ELSE IF ( PTYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_MEANI( BAD, %VAL( IPVEC ), SIZE, AVEACC, VALPIX,
     :                    STATUS )
      ELSE IF ( PTYPE .EQ. '_REAL' ) THEN
         CALL CCG1_MEANR( BAD, %VAL( IPVEC ), SIZE, AVEACC, VALPIX,
     :                    STATUS )
      ELSE IF ( PTYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_MEAND( BAD, %VAL( IPVEC ), SIZE, AVEACC, VALPIX,
     :                    STATUS )
      ELSE

*  Bad data type, issue error message
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_MEAN_BAD',
     :   'Bad processing type, probable programming error.', STATUS )
      END IF

      END
* $Id$
