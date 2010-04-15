      SUBROUTINE
     : CHI_AINITPAR( INPUT, STATUS )
*+
*  Name:
*     CHI_AINITPAR

*  Purpose:
*     Initialse common variables for a parse

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_AINITPAR( INPUT, STATUS )

*  Description:
*     Initialize common variables for a parse.
*

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue.

*  Anticipated Errors:
*     CHI__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     None

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHI_PAR'          ! Standard CHI constants
      INCLUDE 'CHIPAR_PAR'       ! Standard CHI constants
      INCLUDE 'CHI_ERR'          ! Standard CHI errors

*  Global Variables:
      INCLUDE 'CHIWRK_CMN'       ! CHI work variables

*    Local variables :
      integer i,j
      character*(chi__szcname) fnames(chi__numcols)
      character*(chi__szcfmt) fformats(chi__numcols)
      character*(chi__szcunit) fxunits(chi__numcols)
*      character*(chi__szcnval) fnulls(chi__numcols)
      character*(chi__szccmt) fcomments(chi__numcols)
      character*(1) ftypes(chi__numcols)
      integer numflds
      logical fmdataacc(chi__numcols)
      logical fdataacc(chi__numcols)

*  Arguments Given:
      CHARACTER * ( * ) INPUT

*  Status:
      INTEGER STATUS             ! Global status
*-

      IF (STATUS .NE. SAI__OK) RETURN
*
*   Get all the information about the fields.
*
      CALL CHI_GALLCD(INPUT, NUMFLDS, FNAMES, FFORMATS, FTYPES,
     :  FXUNITS, FCOMMENTS, FMDATAACC, FDATAACC, STATUS)
*
*   Copy the information into the common variables.
*
      DO I = 1, NUMFLDS
        IF (FTYPES(I) .EQ. 'C') THEN
          ETYPE(I) = C_TYPE
        ELSEIF (FTYPES(I) .EQ. 'D') THEN
          ETYPE(I) = D_TYPE
        ELSEIF (FTYPES(I) .EQ. 'I') THEN
          ETYPE(I) = I_TYPE
        ELSEIF (FTYPES(I) .EQ. 'L') THEN
          ETYPE(I) = L_TYPE
        ELSEIF (FTYPES(I) .EQ. 'R') THEN
          ETYPE(I) = R_TYPE
        ENDIF
        ENAME(I) = FNAMES(I)
        EFORMT(I) = FFORMATS(I)
        EUNIT(I) = FXUNITS(I)
*        ENULL(I) = FNULLS(I)
        ECOMNT(I) = FCOMMENTS(I)

      ENDDO
      DO I = 1, NUMFLDS
      ENDDO
      ENITEMS = NUMFLDS
*
      END
