
      SUBROUTINE
     : CHI_AINIT2PAR( INPUT1, INPUT2, FNAMES, FTYPES, STATUS )
*+
*  Name:
*     CHI_AINIT2PAR

*  Purpose:
*     Initialse common variables for a parse

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_AINIT2PAR( INPUT1, INPUT2, FNAMES, FTYPES, STATUS )

*  Description:
*     Initialize common variables for a parse.
*

*  Arguments:
*     INPUT1 = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the first catalogue.
*     INPUT2 = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the second catalogue.
*     FNAMES( CHI__NUMFLDS ) = CHARACTER * ( CHI__SZFNAME ) (Returned)
*        Names of the fields.
*     FTYPES( CHI__NUMFLDS ) = CHARACTER * ( 1 ) (Returned)
*        Types of the field.

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
      character*(chi__szcname) f1names(chi__numcols)
      character*(chi__szcname) f2names(chi__numcols)
      character*(1) f1types(chi__numcols)
      character*(1) f2types(chi__numcols)
      character*(chi__szcfmt) fformats(chi__numcols)
      character*(chi__szcunit) fxunits(chi__numcols)
*      character*(chi__szfnval) fnulls(chi__numcols)
      character*(chi__szccmt) fcomments(chi__numcols)
      integer numflds
      integer numflds1
      integer numflds2
      logical fmdataacc(chi__numcols)
      logical fdataacc(chi__numcols)


*  Arguments Given:
      CHARACTER * ( * ) INPUT1
      CHARACTER * ( * ) INPUT2
      CHARACTER * ( * ) FNAMES( * )
      CHARACTER * ( 1 ) FTYPES( * )

*  Status:
      INTEGER STATUS             ! Global status
*-

      IF (STATUS .NE. SAI__OK) RETURN
*
*   Get all the information about the first catalogues fields.
*
      CALL CHI_GALLCD(INPUT1, NUMFLDS1, F1NAMES, FFORMATS,
     :  F1TYPES, FXUNITS, FCOMMENTS, FMDATAACC, FDATAACC, STATUS)
*
*   Copy the information into the common variables.
*
      DO I = 1, NUMFLDS1
        IF (F1TYPES(I) .EQ. 'C') THEN
          ETYPE(I) = C_TYPE
        ELSEIF (F1TYPES(I) .EQ. 'D') THEN
          ETYPE(I) = D_TYPE
        ELSEIF (F1TYPES(I) .EQ. 'I') THEN
          ETYPE(I) = I_TYPE
        ELSEIF (F1TYPES(I) .EQ. 'L') THEN
          ETYPE(I) = L_TYPE
        ELSEIF (F1TYPES(I) .EQ. 'R') THEN
          ETYPE(I) = R_TYPE
        ENDIF
        CALL CHI_ACNAME(INPUT1, F1NAMES(I), FNAMES(I), STATUS)
        FTYPES(I) = F1TYPES(I)
        ENAME(I) = FNAMES(I)
        EFORMT(I) = FFORMATS(I)
        EUNIT(I) = FXUNITS(I)
*        ENULL(I) = FNULLS(I)
        ECOMNT(I) = FCOMMENTS(I)

      ENDDO
*
*   Get all the information about the first catalogues fields.
*
      CALL CHI_GALLCD(INPUT2, NUMFLDS2, F2NAMES, FFORMATS,
     :  F2TYPES, FXUNITS, FCOMMENTS, FMDATAACC, FDATAACC, STATUS)
*
*   Copy the information into the common variables.
*
      NUMFLDS = NUMFLDS1 + NUMFLDS2
      DO J = 1, NUMFLDS2
        I = J + NUMFLDS1
        IF (F2TYPES(J) .EQ. 'C') THEN
          ETYPE(I) = C_TYPE
        ELSEIF (F2TYPES(J) .EQ. 'D') THEN
          ETYPE(I) = D_TYPE
        ELSEIF (F2TYPES(J) .EQ. 'I') THEN
          ETYPE(I) = I_TYPE
        ELSEIF (F2TYPES(J) .EQ. 'L') THEN
          ETYPE(I) = L_TYPE
        ELSEIF (F2TYPES(J) .EQ. 'R') THEN
          ETYPE(I) = R_TYPE
        ENDIF
        CALL CHI_ACNAME(INPUT2, F2NAMES(J), FNAMES(I), STATUS)
        FTYPES(I) = F2TYPES(J)
        ENAME(I) = FNAMES(I)
        EFORMT(I) = FFORMATS(J)
        EUNIT(I) = FXUNITS(J)
*        ENULL(I) = FNULLS(J)
        ECOMNT(I) = FCOMMENTS(J)

      ENDDO
      ENITEMS = NUMFLDS
*
      END
