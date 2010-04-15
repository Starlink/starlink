      SUBROUTINE
     : CHP_AINITPAR( CD, STATUS )
*+
*  Name:
*     CHP_AINITPAR

*  Purpose:
*     Initialse common variables for a parse

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_AINITPAR( CD, STATUS )

*  Description:
*     Initialize common variables for a parse.
*

*  Arguments:
*     CD = INTEGER (Given)
*        Descriptor of the  catalogue.

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
      character*(1) fcoltypes(chi__numcols)
      character*(chi__szcfmt) fformats(chi__numcols)
      character*(chi__szcunit) fxunits(chi__numcols)
*      character*(chi__szfnval) fnulls(chi__numcols)
      character*(chi__szccmt) fcomments(chi__numcols)
      integer numflds
      logical fprefdis(chi__numcols)
      integer fcoldes(chi__numcols)
      integer farrshp(chi__numcols)
      integer farrdim(7,chi__numcols)
      logical fassert(chi__numcols)
      character * ( chi__szexp ) fassexp(chi__numcols)
      logical fdomchk(chi__numcols)
      integer fdatelm(chi__numcols)
      logical fvcflag(chi__numcols)
      character * ( chi__szexp ) fvcexp(chi__numcols)
      logical fdelind(chi__numcols)
      logical ffmatflag(chi__numcols)


*  Arguments Given:
      INTEGER CD

*  Status:
      INTEGER STATUS             ! Global status
*-

      IF (STATUS .NE. SAI__OK) RETURN
*
*   Get all the information about the first catalogues fields.
*
      CALL CHP_GALLCDB(CD, NUMFLDS, FNAMES, FFORMATS,
     :  FXUNITS, FCOMMENTS, FPREFDIS, FCOLTYPES, FCOLDES,
     :  FARRSHP, FARRDIM, FASSERT, FASSEXP, FDOMCHK, FDATELM,
     :  FVCFLAG, FVCEXP, FDELIND, FFMATFLAG, STATUS)
*
*   Copy the information into the common variables.
*
      DO I = 1, NUMFLDS
        IF (FCOLTYPES(I) .EQ. 'C') THEN
          ETYPE(I) = C_TYPE
        ELSEIF (FCOLTYPES(I) .EQ. 'D') THEN
          ETYPE(I) = D_TYPE
        ELSEIF (FCOLTYPES(I) .EQ. 'I') THEN
          ETYPE(I) = I_TYPE
        ELSEIF (FCOLTYPES(I) .EQ. 'L') THEN
          ETYPE(I) = L_TYPE
        ELSEIF (FCOLTYPES(I) .EQ. 'R') THEN
          ETYPE(I) = R_TYPE
        ENDIF
        ENAME(I) = FNAMES(I)
        EFORMT(I) = FFORMATS(I)
        EUNIT(I) = FXUNITS(I)
*        ENULL(I) = FNULLS(I)
        ECOMNT(I) = FCOMMENTS(I)

      ENDDO
*
      ENITEMS = NUMFLDS
*
      END
