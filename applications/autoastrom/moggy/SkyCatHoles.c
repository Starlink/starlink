/* Small fragments of code to make up for the mildly filleted version
   of SkyCat libcat which this program uses.

   SkyCat's libcat (confusingly distinct from Starlink's libcat!) includes
   both some of Mark Calabretta's WCS code and some of Pat's SLALIB code,
   however these conflict with code included in Starlink libast_wcslib
   and libast_slalib.

   No problem: I can create a custom SkyCat libcat without the problematic
   code and link against that.  That fails, however, because of the way
   that wcslib and slalib have been integrated into these particular AST
   glue libraries.

   Code within SkyCat refers to a function slaDeuler, which presumably
   maps to sla_deuler, but that function is omitted from AST's sla.c.
   Also, the SkyCat code refers to functions glsfwd and glsrev, which, as
   I see from AST's wcsmap.c, have been renamed sflfwd and sflrev in
   newer versions.

   $Id$

*/

/* struct prjprm is defined textually in SkyCat's
   catlib-3.7/astrotcl/wcslib/src/proj.c, and a prototype is in
   catlib-3.7/astrotcl/wcslib/include/wcslib.h.  */
#include <astrotcl/wcslib.h>

/* Remap glsfwd and glsref to newer sflfwd and sflrev */
/* See WCS/AST proj.c */
int sflfwd (double phi, double theta,
	    struct prjprm *prj,
	    double *x, double *y);
int glsfwd (double phi, double theta,
	    struct prjprm *prj,
	    double *x, double *y)
{
    return sflfwd (phi, theta, prj, x, y);
}

int sflrev (double x, double y,
	    struct prjprm *prj,
	    double *phi, double* theta);
int glsrev (double x, double y,
	    struct prjprm *prj,
	    double *phi, double* theta)
{
    return sflrev (x, y, prj, phi, theta);
}


/* slaDeuler is missing from AST's sla.c.  The entry here is patterned
   after the functions in that file.  The prototype of slaDeuler is
   from catlib/astrotcl/wcslib/src/slasubs.c.  The only difference is that
   AST:sla.c declares the rotation matrices as `double rmat[3][3]',
   whereas catlib:slasubs.c declares them as `double (*rmat)[3]'.
   These are equivalent, since the first is equivalent to `rmat[][3]', 
   which is in turn equivalent to the second form (cf. K&R Sect.5.7) */
#include <f77.h>

F77_SUBROUTINE(sla_deuler) ( CHARACTER(ORDER),
			     DOUBLE(PHI),
			     DOUBLE(THETA),
			     DOUBLE(PSI),
			     DOUBLE_ARRAY(RMAT) );

void slaDeuler (char *order, double phi, double theta, double psi,
		double (*rmat)[3])
{
    DECLARE_CHARACTER(ORDER,4);
    DECLARE_DOUBLE(PHI);
    DECLARE_DOUBLE(THETA);
    DECLARE_DOUBLE(PSI);
    DECLARE_DOUBLE_ARRAY(RMAT,9);

    PHI   = phi;
    THETA = theta;
    PSI   = psi;

    cnf_exprt (order, ORDER, 4);

    F77_CALL (sla_deuler) ( CHARACTER_ARG(ORDER),
			    DOUBLE_ARG(&PHI),
			    DOUBLE_ARG(&THETA),
			    DOUBLE_ARG(&PSI),
			    DOUBLE_ARRAY_ARG(RMAT) );
}
