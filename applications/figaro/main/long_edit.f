      subroutine long_edit(status)
*+
*  Name:
*     LONG_EDIT

*  Purpose:
*     Edit results structure

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LONG_EDIT(STATUS)

*  Description:
*     The user is asked which option, and can either look at the
*     elements of the results and resvar arrays for a given location,
*     move planes of the results arrays around or sort components into
*     ascending order of wavelength.

*  Arguments:
*      STATUS = INTEGER (Given)
*        Global status

*  Global variables:
*      STAPTR = INTEGER (Given)
*        "Pointer" to fit status array (include file arc_dims)
*      D_RPTR = REAL (Given)
*        "Pointer" to fit results array (include file arc_dims)
*      D_VPTR = REAL (Given)
*        "Pointer" to variances on RESULTS (include file arc_dims)

*  Authors:
*     TNW: T.N.Wilkins (Durham)
*     {enter_new_authors_here}

*  History:
*     2-AUG-1993 (TNW):
*        Original version.
*     22-NOV-1993 (TNW):
*        Allow to sort in ascending or descending order
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      implicit none             ! No implicit typing
      include 'SAE_PAR'
      integer status
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      logical loop
      integer NDICT
      parameter (NDICT = 5)
      character*43 dict(NDICT)
      integer dumi,iopt
      character dumc
      real dumr

      integer OPT_LOOK, OPT_PLANES, OPT_ASC, OPT_DESC, OPT_EXIT
      parameter (OPT_LOOK = 1, OPT_PLANES = 2, OPT_ASC = 3,
     :     OPT_DESC = 4, OPT_EXIT = NDICT)
      data dict/'LOOK  : Look at data values',
     :     'PLANES     : Move planes of results around',
     :     'ASCENDING  : Sort components by wavelength',
     :     'DESCENDING : Sort components by wavelength',
     :     'EXIT       : Return to main menu'/

      loop = .true.
      do while(loop)
         call qmenu('Edit options',dict,NDICT,0,dumr,dumc,iopt,dumi,
     :        status)
         if((iopt.eq.OPT_EXIT).or.(status.ne.SAI__OK)) then
            return
         else if(iopt.eq.OPT_LOOK) then
            call look(%VAL(CNF_PVAL(d_rptr)),.false.,
     :                %VAL(CNF_PVAL(staptr)),%VAL(CNF_PVAL(d_vptr)))

         else if(iopt.eq.OPT_PLANES) then
            call fit_edit(%VAL(CNF_PVAL(d_rptr)),
     :                    %VAL(CNF_PVAL(d_vptr)),status)

         else if(iopt.eq.OPT_ASC) then
            call sort_planes(%VAL(CNF_PVAL(staptr)),
     :                       %VAL(CNF_PVAL(d_rptr)),
     :                       %VAL(CNF_PVAL(d_vptr)),.true.)

         else if(iopt.eq.OPT_DESC) then
            call sort_planes(%VAL(CNF_PVAL(staptr)),
     :                       %VAL(CNF_PVAL(d_rptr)),
     :                       %VAL(CNF_PVAL(d_vptr)),.false.)
         end if
      end do
      end
