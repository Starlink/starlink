      SUBROUTINE
     : CHP_GONEPD(INPUT, PNAME, PFORMAT, PUNITS, PCOMMENT,
     : PPREFDIS, PPARTYPE, PARDES, PARRSHP, PARRDIM, PMDATAACC,
     : PDATAACC,
     : PDATELM, PDELIND, PNSFLAG, STATUS)
*+
*  Name:
*     CHP_GONEPD

*  Purpose:
*     Get ONE Parameters Details.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  CHP_GONEPD(INPUT, PNAME, PFORMAT, PUNITS, PCOMMENT,
*     PPREFDIS, PPARTYPE, PARDES, PARRSHP, PARRDIM, PMDATAACC, PDATAACC,
*     PDATELM, PDELIND, PFMATFLAG, STATUS)
*
*  Description :
*     Get the information about a parameter. A parameter has
*     associated with it a name, format, units, comment,
*     preferred dislay flag, parameter designation, access information, date
*     last modified, delete indicator and format flag.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue from which the parameter information is required.
*     PNAME = CHARACTER * ( CHP__SZPNAME ) (Given)
*        Name of the parameter.
*     PFORMAT = CHARACTER * ( CHI__SZPFMT ) (Returned)
*        Format of the parameter.
*     PUNITS = CHARACTER * ( CHP__SZPUNIT ) (Returned)
*        Units of the parameter.
*     PCOMMENT = CHARACTER * ( CHP__SZPCMT ) (Returned)
*        Comment of the parameter.
*     PPREFDIS = LOGICAL (Returned)
*        Preferred display flag. TRUE for display.
*     PARDES  = INTEGER (Returned)
*        Parameter designation.
*     PARRSHP  = INTEGER (Returned)
*        Parameter array shape.
*     PARRDIM(7)  = INTEGER (Returned)
*        Parameter array dimensions.
*     PMDATAACC = LOGICAL (Returned)
*        Access to metadata associated with the parameter.
*     PDATAACC = LOGICAL (Returned)
*        Access to data associated with the parameter.
*     PDATELM  = INTEGER (Given)
*        Date that this parameter was last modified.
*     PDELIND = LOGICAL (Returned)
*        Delete indicator flag. TRUE if the parameter can be deleted.
*     PFMATFLAG( CHP__NUMPARS ) = LOGICAL (Returned)
*        Format flag. TRUE if a non standard format is present.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__PARNOTFND

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
      CHARACTER * ( * ) PNAME

*  Arguments Returned:
      CHARACTER * ( * ) PFORMAT
      CHARACTER * ( * ) PUNITS
      CHARACTER * ( * ) PCOMMENT
      CHARACTER * ( 1 ) PPARTYPE
      LOGICAL PPREFDIS
      INTEGER PARDES
      INTEGER PARRSHP
      INTEGER PARRDIM( * )
      LOGICAL PMDATAACC
      LOGICAL PDATAACC
      INTEGER PDATELM
      LOGICAL PDELIND
      LOGICAL PNSFLAG

*  Status:
      INTEGER STATUS             ! Global status

*  External references :

*  Local variables :
      integer cd   ! catalogue descriptor.
      integer cc   ! column count
      integer nvcc ! number of dependent columns count
      integer asc  ! array shape count
      integer numpars

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
        if(PPname(cd,cc) .EQ. pname) then
          pformat = PPformat(cd,cc)
          punits = PPunit(cd,cc)
          pcomment = PPcomment(cd,cc)
          pprefdis = PPprefdis(cd,cc)
          ppartype = PPtype(cd,cc)
          pardes = PPpardes(cd,cc)
          pmdataacc = PPmdataacc(cd,cc)
          pdataacc = PPdataacc(cd,cc)
          pdatelm = PPdatelm(cd,cc)
          pdelind = PPdelind(cd,cc)
          parrshp = PParrshp(cd,cc)
          if (parrshp .ge. 1) then
            do asc = 1, parrshp
              parrdim(asc) = PParrdim(cd,cc,asc)
            enddo
          endif
          pnsflag = PPnsflag(cd,cc)
        endif
      enddo
*
      END
