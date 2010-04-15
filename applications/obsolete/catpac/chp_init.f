      SUBROUTINE
     : CHP_INIT( STATUS )
*+
*  Name:
*     CHP_INIT

*  Purpose:
*     Initialize the CHP system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_INIT( STATUS )
*
*  Description:
*     Initializes the CHP common area.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     None

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
*      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! CHP constants

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! CHP comon area

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER COUNT
      INTEGER COUNT1
      INTEGER COUNT2
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      do count = 1, CHP__NUMCATS
        do count1 = 1, CHP__NUMCOLS

          EPorig(count,count1) = 0
*
          EPtype(count, count1) = ' '
*
          call chr_fill(' ',EPname(count,count1))
          call chr_fill(' ',EPformat(count,count1))
          call chr_fill(' ',EPunits(count,count1))
          call chr_fill(' ',EPcomment(count,count1))
          EPmdataacc(count,count1) = .FALSE.
          EPdataacc(count,count1) = .FALSE.
          EPdatelm(count,count1) = 0
          EPvcflag(count,count1) = .FALSE.
          call chr_fill(' ',EPvcexp(count,count1))
          do count2 = 1, CHP__NUMCOLS
            call chr_fill(' ',EPvcdep(count,count1,count2))
          enddo
          EPnvcdep(count,count1) = 0
          EPdelind(count,count1) = .FALSE.
          EPprefdis(count,count1) = .FALSE.
          EParrshp(count,count1) = 0
          do count2 = 1, CHP__MXDIM
            EParrdim(count,count1,count2) = 0
          enddo
          EPassert(count, count1) = .FALSE.
          call chr_fill(' ',EPassexp(count,count1))
          EPdomchk(count,count1) = .FALSE.
          EPcoldes(count, count1) = 0
          EPnsflag(count, count1 ) = 0
          call chr_fill(' ',EPnsformat(count,count1))
          EPhidden(count,count1) = .FALSE.
        enddo

        do count1 = 1, CHP__NUMPARS

          PPorig(count,count1) = 0
          PPtype(count, count1) = ' '
          call chr_fill(' ',PPname(count,count1))
          call chr_fill(' ',PPunit(count,count1))
          call chr_fill(' ',PPformat(count,count1))
          call chr_fill(' ',PPcomment(count,count1))
          call chr_fill(' ',PPcharval(count,count1))
          PPptrval(count,count1) = 0
          PPmdataacc(count,count1) = .FALSE.
          PPdataacc(count,count1) = .FALSE.
          PPdatelm(count, count1) = 0
          PPdelind(count,count1) = .FALSE.
          PPprefdis(count,count1) = .FALSE.
          PParrshp(count,count1) = 0
          do count2 = 1, CHP__MXDIM
            PParrdim(count, count1, count2) = 0
          enddo
          PPpardes(count, count1) = .FALSE.
          PPnsflag(count, count1) = .FALSE.
          call chr_fill(' ',PPnsformat(count,count1))
        enddo

        CPnumext(count) = 0
        CPexttype(count) = .FALSE.
        call chr_fill(' ',CPsorjexp(count))
        call chr_fill(' ',CPcatnotes(count))
        do count1 = 1, CHP__MXJCATS
          call chr_fill(' ',CPorignames(count,count1))
        enddo
        CPnofndi(count) = 0
        do count1 = 1, CHP__NUMCOLS
          CPndorig(count,count1) = 0
          CPnddatelm(count,count1) = 0
          CPnumndcols(count,count1) = 0
          call chr_fill(' ',CP1icol(count,count1))
          call chr_fill(' ',CP2icol(count,count1))
          call chr_fill(' ',CP3icol(count,count1))
          call chr_fill(' ',CP4icol(count,count1))
          call chr_fill(' ',CP5icol(count,count1))
          call chr_fill(' ',CP6icol(count,count1))
          call chr_fill(' ',CP7icol(count,count1))
        enddo
        CPnofasorts(count) = 0
        do count1 = 1, CHP__NUMCOLS
          CPasorig(count,count1) = 0
          CPasdatelm(count,count1) = 0
          CPnumascols(count,count1) = 0
          call chr_fill(' ',CPfascol(count,count1))
          call chr_fill(' ',CPsascol(count,count1))
          call chr_fill(' ',CPtascol(count,count1))
          CPfasdir(count,count1) = 'T'
          CPsasdir(count,count1) = 'T'
          CPtasdir(count,count1) = 'T'
        enddo

        call chr_fill(' ',CPname(count))
        CPdelind(count) = .FALSE.
        CPnuments(count) = 0
        CPnumcols(count) = 0
        CPnumpars(count) = 0
      enddo
      END
