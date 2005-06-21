      subroutine opt_prguess(deccntr,work)
*+
* Name:
*    OPT_PRGUESS

* Invocation:
*    CALL OPT_PRGUESS(DECCNTR,WORK)
*
* Description:
*    To print out the current guesses.
*
* Purpose:
*    To print out the current guesses.
*
* Arguments:
*    DECCNTR(*) = INTEGER ARRAY (Given)
*        fit coding
*    WORK(*) = REAL ARRAY (Workspace)
*        Workspace for scaling (needs to be large enough
*                         to hold unscaled parameters, usually n or n+1)
* Global variables:
*    MAX_CMP = INTEGER (Given)
*        Maximum number of fit components allowed (include file opt_cmn)
*    MAX_TIMES = INTEGER (Given)
*       Maximum number of fits we can store (include file opt_cmn)
*    TIMES = INTEGER (Given)
*      Number of slot containing guesses to print out
*
* Author:
*    T.N.Wilkins, Cambridge,  9-SEP-1991
*
* History:
*    T.N.Wilkins, Cambridge, 8-SEP-1992 MAX_CMP passed in common
*-
      implicit none
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'status_inc'
      include 'opt_cmn'
      real work(*)
      integer npcmp(0:5),ppcmp
      data npcmp/1,4,5,5,4,4/
*

      ppcmp = npcmp(deccntr(FIT_MODEL))

      call rescale_store(work,%VAL(CNF_PVAL(guessptr)),
     :                   deccntr(FIT_NCMP),times,ppcmp)
      call opt_wrfit(deccntr,work,work,.false.)
      end
