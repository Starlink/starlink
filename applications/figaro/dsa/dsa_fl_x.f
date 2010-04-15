*+
*  Name:
*     DSA_FLAG_{BSIFDU}

*  Purpose:
*     Return flag value

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_FLAG_<X>( VALUE )

*  Description:
*     This routine returns the bad values for a certain numeric type.

*  Arguments:
*     VALUE = <TYPE> (Returned)
*        The flag (bad) value to be returned.

*  Notes:
*     This is a generic routine. But since Figaro's type names are
*     different from Starlink's, some care must be taken when
*     substituting <X>, <T> and <TYPE>. The following is a complete list
*     for this routine:
*               <X> <T>  <TYPE>
*     DSA_FCSTR  F   R    REAL
*     DSA_ICSTR  I   I    INTEGER
*     DSA_BCSTR  B   B    BYTE
*     DSA_DCSTR  D   D    DOUBLE PRECISION
*     DSA_UCSTR  U   UW   INTEGER * 2
*     DSA_SCSTR  S   W    INTEGER * 2

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     KS:  Keith Shortridge (AAO)
*     {enter_new_authors_here}

*  History:
*     25-AUG-1992 (HME):
*        Original version.
*     31-AUG-1992 (KS):
*        Format of INCLUDE files changed for use on VAX.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      SUBROUTINE DSA_FLAG_B( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      BYTE VALUE
*.
      VALUE = VAL__BADB

      END

      SUBROUTINE DSA_FLAG_S( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INTEGER * 2 VALUE
*.
      VALUE = VAL__BADW

      END

      SUBROUTINE DSA_FLAG_I( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INTEGER VALUE
*.
      VALUE = VAL__BADI

      END

      SUBROUTINE DSA_FLAG_F( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      REAL VALUE
*.
      VALUE = VAL__BADR

      END

      SUBROUTINE DSA_FLAG_D( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      DOUBLE PRECISION VALUE
*.
      VALUE = VAL__BADD

      END

      SUBROUTINE DSA_FLAG_U( VALUE )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INTEGER * 2 VALUE
*.
      VALUE = VAL__BADUW

      END
