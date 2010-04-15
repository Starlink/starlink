      SUBROUTINE
     :  CHP_GALLPD(INPUT, NUMPARS, PNAMES, PFORMATS,
     : PUNITS, PCOMMENTS, PPREFDIS, PARDES, PMDATAACC, PDATAACC,
     : PDATELM, PDELIND, PFMATFLAG, STATUS)
*+
*  Name:
*     CHP_GALLPD

*  Purpose:
*     Get ALL Parameters Details.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_GALLPD (INPUT, NUMPARS, PNAMES, PFORMATS,
*     PUNITS, PCOMMENTS, PPREFDIS, PARDES, PMDATAACC, PDATAACC, PDATELM,
*     PDELIND, PFMATFLAG, STATUS)

*  Description :
*     Get the information about all the parameters. Each parameter has
*     associated with it a name, format, units, comment,
*     preferred dislay flag, parameter designation, access information, date
*     last modified and delete indicator.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue from which the parameter information is required.
*     NUMPARS = INTEGER (Returned)
*        Number of parameters in the catalogue.
*     PNAMES( CHP__NUMPARS ) = CHARACTER * ( CHP__SZPNAME ) (Returned)
*        Names of the parameters in the catalogue.
*     PFORMATS( CHP__NUMPARS ) = CHARACTER * ( CHI__SZPFMT ) (Returned)
*        Formats of the parameters in the catalogue.
*     PUNITS( CHP__NUMPARS ) = CHARACTER * ( CHP__SZPUNIT ) (Returned)
*        Units of the parameters in the catalogue.
*     PCOMMENTS( CHI__NUMPARS ) = CHARACTER * ( CHP__SZPCMT ) (Returned)
*        Comments of the parameters in the catalogue.
*     PPREFDIS( CHP__NUMPARS ) = LOGICAL (Returned)
*        Preferred display flag. TRUE for display.
*     PARDES( CHP__NUMPARS )  = INTEGER (Given)
*        Parameter designation.
*     PMDATAACC( CHP__NUMPARS ) = LOGICAL (Returned)
*        Access to metadata associated with the parameters in the catalogue.
*     PDATAACC( CHP__NUMPARS ) = LOGICAL (Returned)
*        Access to data associated with the parameters in the catalogue.
*     PDATELM( CHP__NUMPARS )  = INTEGER (Given)
*        Date that this parameter was last modified.
*     PDELIND( CHP__NUMPARS ) = LOGICAL (Returned)
*        Delete indicator flag. TRUE if the parameter can be deleted.
*     PFMATFLAG( CHP__NUMPARS ) = LOGICAL (Returned)
*        Format flag. TRUE if a non standard format is present.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHP__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     1-Oct-1993 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! Standard CHP constants

*  Global Variabless:
      INCLUDE 'CHP_CMN'   ! Standard CHP commons

*  Arguments Given:
      CHARACTER * ( * ) INPUT

*  Arguments Returned:
      INTEGER NUMPARS
      CHARACTER * ( * ) PNAMES( * )
      CHARACTER * ( * ) PFORMATS( * )
      CHARACTER * ( * ) PUNITS( * )
      CHARACTER * ( * ) PCOMMENTS( * )
      LOGICAL PPREFDIS( * )
      INTEGER PARDES( * )
      LOGICAL PMDATAACC( * )
      LOGICAL PDATAACC( * )
      INTEGER PDATELM( * )
      LOGICAL PDELIND( * )
      LOGICAL PFMATFLAG( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External references :

*  Local variables :
      integer cd   ! catalogue descriptor.
      integer cc   ! column count
      integer nvcc ! number of dependent columns count
      integer asc  ! array shape count

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*  Get the CD for the catalogue.
*
      call chp_getcd(input, .FALSE., cd, status)
*
*  Copy the contents of the common area
*
      numpars = CPnumpars(cd)
      do cc = 1, numpars
*        ptypes(cc) = PPtype(cd,cc)
        pnames(cc) = PPname(cd,cc)
        pformats(cc) = PPformat(cd,cc)
        punits(cc) = PPunit(cd,cc)
        pcomments(cc) = PPcomment(cd,cc)
        pmdataacc(cc) = PPmdataacc(cd,cc)
        pdataacc(cc) = PPdataacc(cd,cc)
        pdatelm(cc) = PPdatelm(cd,cc)
        pdelind(cc) = PPdelind(cd,cc)
        pprefdis(cc) = PPprefdis(cd,cc)
*        parrshp(cc) = PParrshp(cd,cc)
*        if (parrshp(cc) .ge. 1) then
*          do asc = 1, parrshp(cc)
*            parrdim(cc,asc) = PParrdim(cd,cc,asc)
*          enddo
*        endif
*        parrtyp(cc) = PParrtype(cd,cc)
*        pstruct(cc) = PPstruct(cd,cc)
      enddo
*
      END
