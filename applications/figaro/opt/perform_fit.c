#include "f77.h"
/*

  Declare functions

  Routines for E04GBF
*/
F77_SUBROUTINE(sk_func)(INTEGER(iflag),INTEGER(m),INTEGER(n),
			DOUBLE_ARRAY(xc),DOUBLE_ARRAY(rc),
			DOUBLE_ARRAY(ajc),INTEGER(ljc),INTEGER(miw)
			,INTEGER(liw),INTEGER(wn),INTEGER(niw));
F77_SUBROUTINE(ca_func)(INTEGER(iflag),INTEGER(m),INTEGER(n),
                        DOUBLE_ARRAY(xc),DOUBLE_ARRAY(rc),
                        DOUBLE_ARRAY(ajc),INTEGER(ljc),INTEGER(miw)
                        ,INTEGER(liw),INTEGER(wn),INTEGER(niw));
F77_SUBROUTINE(fs_func)(INTEGER(iflag),INTEGER(m),INTEGER(n),
                        DOUBLE_ARRAY(xc),DOUBLE_ARRAY(rc),
                        DOUBLE_ARRAY(ajc),INTEGER(ljc),INTEGER(miw)
                        ,INTEGER(liw),INTEGER(wn),INTEGER(niw));
F77_SUBROUTINE(fh_func)(INTEGER(iflag),INTEGER(m),INTEGER(n),
                        DOUBLE_ARRAY(xc),DOUBLE_ARRAY(rc),
                        DOUBLE_ARRAY(ajc),INTEGER(ljc),INTEGER(miw)
                        ,INTEGER(liw),INTEGER(wn),INTEGER(niw));
F77_SUBROUTINE(fw_func)(INTEGER(iflag),INTEGER(m),INTEGER(n),
                        DOUBLE_ARRAY(xc),DOUBLE_ARRAY(rc),
                        DOUBLE_ARRAY(ajc),INTEGER(ljc),INTEGER(miw)
                        ,INTEGER(liw),INTEGER(wn),INTEGER(niw));
F77_SUBROUTINE(mga_func)(INTEGER(iflag),INTEGER(m),INTEGER(n),
                        DOUBLE_ARRAY(xc),DOUBLE_ARRAY(rc),
                        DOUBLE_ARRAY(ajc),INTEGER(ljc),INTEGER(miw)
                        ,INTEGER(liw),INTEGER(wn),INTEGER(niw));
F77_SUBROUTINE(bl_func)(INTEGER(iflag),INTEGER(m),INTEGER(n),
                        DOUBLE_ARRAY(xc),DOUBLE_ARRAY(rc),
                        DOUBLE_ARRAY(ajc),INTEGER(ljc),INTEGER(miw)
                        ,INTEGER(liw),INTEGER(wn),INTEGER(niw));
F77_SUBROUTINE(error_e04kdf)(INTEGER(iflag),INTEGER(m),INTEGER(n),
                        DOUBLE_ARRAY(xc),DOUBLE_ARRAY(rc),
                        DOUBLE_ARRAY(ajc),INTEGER(ljc),INTEGER(miw)
                        ,INTEGER(liw),INTEGER(wn),INTEGER(niw));
/*
  Routines for E04KDF
*/
F77_SUBROUTINE(mgfun)(INTEGER(iflag),INTEGER(n),DOUBLE_ARRAY(xc),
		      DOUBLE(fc1),DOUBLE_ARRAY(gc1),INTEGER_ARRAY(iw)
		      ,INTEGER(liw),DOUBLE_ARRAY(w),INTEGER(lw));
F77_SUBROUTINE(mlfun)(INTEGER(iflag),INTEGER(n),DOUBLE_ARRAY(xc),
		     DOUBLE(fc1),DOUBLE_ARRAY(gc1),INTEGER_ARRAY(iw)
                      ,INTEGER(liw),DOUBLE_ARRAY(w),INTEGER(lw));
/*
  Routines for LMDER
*/
F77_SUBROUTINE(error_lmder)(INTEGER(m),INTEGER(n),DOUBLE_ARRAY(xc),
			    DOUBLE_ARRAY(rc),DOUBLE_ARRAY(fjac),
			    INTEGER(ldfjac),INTEGER(iflag));
F77_SUBROUTINE(lm_skf)(INTEGER(m),INTEGER(n),DOUBLE_ARRAY(xc),
                            DOUBLE_ARRAY(rc),DOUBLE_ARRAY(fjac),
                            INTEGER(ldfjac),INTEGER(iflag));
F77_SUBROUTINE(lm_caf)(INTEGER(m),INTEGER(n),DOUBLE_ARRAY(xc),
                            DOUBLE_ARRAY(rc),DOUBLE_ARRAY(fjac),
                            INTEGER(ldfjac),INTEGER(iflag));
F77_SUBROUTINE(lm_lof)(INTEGER(m),INTEGER(n),DOUBLE_ARRAY(xc),
                            DOUBLE_ARRAY(rc),DOUBLE_ARRAY(fjac),
                            INTEGER(ldfjac),INTEGER(iflag));
F77_SUBROUTINE(lm_mgf)(INTEGER(m),INTEGER(n),DOUBLE_ARRAY(xc),
                            DOUBLE_ARRAY(rc),DOUBLE_ARRAY(fjac),
                            INTEGER(ldfjac),INTEGER(iflag));
/*
  Declare the types for the arrays. FUNCTPTR doesn't have it's
  arguments given since they vary.
*/
typedef void (*FUNCPTR)();
typedef void (*CFUNCPTR)(FUNCPTR routine,REAL_ARRAY(guess),
                             INTEGER(n),INTEGER_ARRAY(deccntr),
                             INTEGER(npcmp),REAL_ARRAY(fitpar),
                             REAL_ARRAY(fiterr),INTEGER(fstat),
                             DOUBLE_ARRAY(work));
/*
  Routines which call the Nag (or whatever) routines
*/
F77_SUBROUTINE(gausid)(FUNCPTR routine,REAL_ARRAY(guess),INTEGER(n),
		       INTEGER_ARRAY(deccntr),INTEGER(npcmp),
		       REAL_ARRAY(fitpar),REAL_ARRAY(fiterr),
		       INTEGER(fstat),DOUBLE_ARRAY(work));
F77_SUBROUTINE(contr_mgauss)(FUNCPTR routine,REAL_ARRAY(guess),
			     INTEGER(n),INTEGER_ARRAY(deccntr),
			     INTEGER(npcmp),REAL_ARRAY(fitpar),
			     REAL_ARRAY(fiterr),INTEGER(fstat),
			     DOUBLE_ARRAY(work));
F77_SUBROUTINE(lm_gausid)(FUNCPTR routine,REAL_ARRAY(guess),INTEGER(n),
			  INTEGER_ARRAY(deccntr),INTEGER(npcmp),
			  REAL_ARRAY(fitpar),REAL_ARRAY(fiterr),
			  INTEGER(fstat),DOUBLE_ARRAY(work));
/*
  Arrays of pointers to subroutines

    control_routines are the routines which call the fitting routines
  (e04kdf etc.), and opt_routines are the routines passed as external
  to them.

    The first dimension of opt_routines gives the fitting method
  (e.g. using lmder) and the second gives the model. If no routine is
  available for the given combination, then a routine must be supplied
  which aborts the fitting.

*/
CFUNCPTR control_routines[] = {F77_EXTERNAL_NAME(gausid),
				F77_EXTERNAL_NAME(contr_mgauss),
				F77_EXTERNAL_NAME(lm_gausid)};
FUNCPTR opt_routines[3][7] = {{F77_EXTERNAL_NAME(mga_func),
			    F77_EXTERNAL_NAME(fs_func),
			    F77_EXTERNAL_NAME(fw_func),
			    F77_EXTERNAL_NAME(fh_func),
			    F77_EXTERNAL_NAME(bl_func),
			    F77_EXTERNAL_NAME(sk_func),
			    F77_EXTERNAL_NAME(ca_func)},
			    {F77_EXTERNAL_NAME(mgfun),
			    F77_EXTERNAL_NAME(error_e04kdf),
			    F77_EXTERNAL_NAME(error_e04kdf),
			    F77_EXTERNAL_NAME(error_e04kdf),
			    F77_EXTERNAL_NAME(mlfun),
			    F77_EXTERNAL_NAME(error_e04kdf),
			    F77_EXTERNAL_NAME(error_e04kdf)},
			    {F77_EXTERNAL_NAME(lm_mgf),
			    F77_EXTERNAL_NAME(error_lmder),
			    F77_EXTERNAL_NAME(error_lmder),
			    F77_EXTERNAL_NAME(error_lmder),
			    F77_EXTERNAL_NAME(lm_lof),
			    F77_EXTERNAL_NAME(lm_skf),
			    F77_EXTERNAL_NAME(lm_caf)}};
/*
  Positions in above list for subroutines of given models
  These are given by their values in DECCNTR, being
           0 - nothing
           1 - Gaussian
           2 - Skew Gaussian
           3 - Cauchy function
           4 - Centriod, not valid here
           5 - Lorentzian

*/
int fit_positions[] = {0,0,5,6,-1,4};
/*
  These must be 1 less than in status_inc (because C arrays start at 0
  whereas FORTRAN arrays normally start at 1).
*/
#define FIT_OPT 8
#define FIT_MODEL 1
#define FIT_TYPE 2

F77_SUBROUTINE(perform_fit)(INTEGER_ARRAY(deccntr),INTEGER(n),
			  REAL_ARRAY(fitpar),REAL_ARRAY(fiterr),
			  REAL_ARRAY(resstr),INTEGER(fstat),
			  DOUBLE_ARRAY(work),REAL_ARRAY(guess))
/*
*+
* Name:
*    PERFORM_FIT

* Invocation:
*    CALL PERFORM_FIT(DECCNTR,N,FITPAR,FITERR,RESSTR,FSTAT,WORK,GUESS)

* Purpose:
*    To invoke routines to perform the optimisation.

* Description:
*    This selects the appropriate routines to perform the optimisation, and
*    invokes the control routine (e.g. gausid) with the differentials/residuals
*    routine as an argument.

* Arguments:
*   DECCNTR(*) = INTEGER ARRAY (Given)
*     Fit coding
*   N = INTEGER (Given)
*     Number fit parameters
*   GUESS(NPC,*) = REAL ARRAY (Given)
*     Guesses to fit parameters
*   RESSTR(4,MAX_CMP,*) = REAL ARRAY (Given and returned)
*     Results store
*   FSTAT = INTEGER (Given and returned)
*     Fit status
*   FITPAR(N) = REAL ARRAY (Returned)
*     Fit parameters
*   FITERR(N) = REAL ARRAY (Returned)
*     Fit errors
*   WORK(*) = DOUBLE PRECISION ARRAY (Workspace)
*     WORK has to be as follows:
*         For GAUSID and DOUBLE_GAUSS:
*           m = DOUBLE PRECISION
*           + n*n = DOUBLE PRECISION
*           + (n+1)*(n+1) = DOUBLE PRECISION ARRAY
*           + m*n = DOUBLE PRECISION
*           + wndim = DOUBLE PRECISION (wndim defined as below)
*         For FIT_MGAUSS:
*           5 * n = DOUBLE PRECISION
*           n = INTEGER
*         For LM_GAUSID:
*           5*n + 2*m + m*n = DOUBLE PRECISION
*           n = INTEGER


* Langauge
*   ANSI C.

* Authors:
*    TNW: T.N.Wilkins, Durham

* History:
*    TNW: 29-JUNE-1993 Original version
*-
*/
{
  int FitOpt,FitModel, FitType, FitIndex;
  DECLARE_INTEGER(npcmp);
/*
  Number of parameters per component (includes simple base).
*/
  int parpcmp[] = {0,4,5,5,0,4};

  FitOpt = deccntr[FIT_OPT];
  FitModel = deccntr[FIT_MODEL];
  FitType = deccntr[FIT_TYPE];
/*
  Check for invalid values in DECCNTR
*/
  if((FitOpt < 1) || (FitOpt > 3)
     || (FitModel < 1) || (FitModel > 5) || (FitModel == 4)
     || (FitType < 1) || (FitType > 6))
    {
      *fstat = -10;
      return;
    }
  npcmp = parpcmp[FitModel];
  FitIndex = fit_positions[FitModel];
/*
  For doubles we have to consider fit type as well
*/
  if((FitType > 1) && (FitType < 5))
    {
      if(FitModel == 1)
	{
	  FitIndex += FitType - 1;
	}
      else
	{
	  FitIndex = 2; /* Error routine */
	}
    }
/*
  Call routine
*/
  control_routines[FitOpt-1](opt_routines[FitOpt-1][FitIndex],
			     REAL_ARRAY_ARG(guess),INTEGER_ARG(n),
			     INTEGER_ARRAY_ARG(deccntr),
			     INTEGER_ARG(&npcmp),REAL_ARRAY_ARG(fitpar),
			     REAL_ARRAY_ARG(fiterr),INTEGER_ARG(fstat),
			     DOUBLE_ARRAY_ARG(work));
}
