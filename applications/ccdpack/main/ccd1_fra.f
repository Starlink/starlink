      SUBROUTINE CCD1_FRA( TYPE, EL, IPARR, NFRAC, FRAC, BAD, CLFRAC,
     :                     VALUES, STATUS )
*+
*  Name:
*     CCD1_FRA

*  Purpose:
*     Finds values corresponding to specified fractions of an array's
*     ordered distribution.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCG1_FRA( TYPE, EL, IPARR, NFRAC, FRAC, BAD, CLFRAC, VALUES,
*                    STATUS )

*  Description:
*     This routine finds the values at defined fractions of an array's
*     ordered distribution, such as percentiles.  It simply acts as
*     a harness for the type-sensitive routines CCG1_FRACx.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        HDS data type of the data array.
*     EL = INTEGER (Given)
*        The number of elements of the array to be analysed.
*     IPARR = INTEGER (Given)
*        Pointer to array for which the chosen statistics are required.
*     NFRAC = INTEGER (Given)
*        Number of fractional positions.
*     FRAC( NFRAC ) = DOUBLE PRECISION (Given and Returned)
*        Fractional positions in the histogram in the range 0.0--1.0.
*        They must be given in ascending order.
*     BAD = LOGICAL (Given and Returned)
*        True when bad pixels may be present.
*     CLFRAC( NFRAC ) = DOUBLE PRECISION (Returned)
*        The clipped fractional positions in the histogram in the range
*        0.0--1.0 after iterative clipping of the histogram.
*     VALUES( NFRAC ) = DOUBLE PRECISION (Returned)
*        Values corresponding to the ordered fractional positions in
*        the histogram.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-AUG-2000 (MBT):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SAE definitions

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER EL
      INTEGER IPARR
      INTEGER NFRAC

*  Arguments Given and Returned:
      DOUBLE PRECISION FRAC( NFRAC )
      LOGICAL BAD

*  Arguments Returned:
      DOUBLE PRECISION CLFRAC( NFRAC )
      DOUBLE PRECISION VALUES( NFRAC )

*  Status:
      INTEGER STATUS

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Branch calling the correct version of CCG1_FRACx appropriate to the data.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_FRACB( EL, %VAL( IPARR ), NFRAC, FRAC, BAD, CLFRAC,
     :                    VALUES, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_FRACUB( EL, %VAL( IPARR ), NFRAC, FRAC, BAD, CLFRAC,
     :                     VALUES, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL CCG1_FRACW( EL, %VAL( IPARR ), NFRAC, FRAC, BAD, CLFRAC,
     :                    VALUES, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_FRACUW( EL, %VAL( IPARR ), NFRAC, FRAC, BAD, CLFRAC,
     :                     VALUES, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_FRACI( EL, %VAL( IPARR ), NFRAC, FRAC, BAD, CLFRAC,
     :                    VALUES, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL CCG1_FRACR( EL, %VAL( IPARR ), NFRAC, FRAC, BAD, CLFRAC,
     :                    VALUES, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_FRACD( EL, %VAL( IPARR ), NFRAC, FRAC, BAD, CLFRAC,
     :                    VALUES, STATUS )


*  Unsupported data type.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( 'CCD1_FRA', 
     :                 '  CCD1_FRA: Unsupported data type ^TYPE.', 
     :                 STATUS )
      END IF

      END
* $Id$
