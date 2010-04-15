      SUBROUTINE
     : CHP_DELPAR( INPUT, PNAME, STATUS )
*+
*  Name:
*     CHP_DELPAR

*  Purpose:
*     DELete a PARameter from a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_DELPAR( INPUT, PNAME, STATUS )
*
*  Description:
*     Delete a parameter from a catalogue.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue from which the parameter is to be
*        deleted.
*     PNAME = CHARACTER * ( CHP__SZPNAME ) (Given)
*        Name of the parameter to be deleted.
*     STATUS = INTEGER (Given and Returned)
*        Global status.
*
*
*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__PARNOTFND

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! Standard CHP dscf constants
      INCLUDE 'FIO_ERR'   ! Standard FIO errors
      INCLUDE 'CHP_ERR'   ! Standard CHP errors


*  Global Variables:
      INCLUDE 'CHP_CMN'   ! Standard CHP variables

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) PNAME

*  Status:
      INTEGER STATUS             ! Global status


*  Local Variables:
      INTEGER CD
      INTEGER PARCOUNT
      INTEGER NUMPARS
      LOGICAL PARNOTFND ! Parameter not yet found flag
*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      call chp_getcd(input, .TRUE., cd, status)
*
*  If this is the last parameter just delete it.
*
      if (PPname(cd,CPnumpars(cd)) .EQ. pname) then
        CPnumpars(cd) = CPnumpars(cd) - 1
      else
*
*  It is not the last parameter. Check through all the parameters and, if
*  the parameter to be deleted is found, copy the last parameter into its place
*  and reduce the number of parameters by 1
*
        parnotfnd = .TRUE.
        parcount = 1
        do while (parnotfnd .AND. parcount.LE. CPnumpars(cd))
          if (PPname(cd,parcount).EQ. pname) then
            parnotfnd = .FALSE.
            numpars = CPnumpars(cd)
            PPname(cd,parcount) = PPname(cd,numpars)
            PPformat(cd,parcount) = PPformat(cd,numpars)
            PPunit(cd,parcount) = PPunit(cd,numpars)
            PPcomment(cd,parcount) = PPcomment(cd,numpars)
            PPdatelm(cd,parcount) = PPdatelm(cd,numpars)
            PParrshp(cd,parcount) = PParrshp(cd,numpars)
            PParrdim(cd,parcount,1) = PParrdim(cd,numpars,1)
            PParrdim(cd,parcount,2) = PParrdim(cd,numpars,2)
            PParrdim(cd,parcount,3) = PParrdim(cd,numpars,3)
            PParrdim(cd,parcount,4) = PParrdim(cd,numpars,4)
            PParrdim(cd,parcount,5) = PParrdim(cd,numpars,5)
            PParrdim(cd,parcount,6) = PParrdim(cd,numpars,6)
            PParrdim(cd,parcount,7) = PParrdim(cd,numpars,7)
            PPpardes(cd,parcount) = PPpardes(cd,numpars)
            PPprefdis(cd,parcount) = PPprefdis(cd,numpars)
            PPdelind(cd,parcount) = PPdelind(cd,numpars)
            PPorig(cd,parcount) = PPorig(cd,numpars)
            PPdataacc(cd,parcount) = PPdataacc(cd,numpars)
            PPmdataacc(cd,parcount) = PPmdataacc(cd,numpars)
            PPcharval(cd,parcount) = PPcharval(cd,numpars)
            PPptrval(cd,parcount) = PPptrval(cd,numpars)
            CPnumpars(cd) = CPnumpars(cd) - 1
          endif
          parcount = parcount +1
        enddo
      endif
*
      END
