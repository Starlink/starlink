 /*
 				globals.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	global declarations.
*
*	Last modify:	28/11/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	"types.h"

/*----------------------- miscellaneous variables ---------------------------*/

sexcatstruct		cat;
prefstruct		prefs;
picstruct		thefield1, thefield2;
objstruct		flagobj;
obj2struct		flagobj2;
extern obj2struct	outobj2;
static obj2struct	*obj2 = &outobj2;
float			ctg[37], stg[37];
char			gstr[MAXCHAR];
static const char	notokstr[] = {" \t=,;\n\r\""};

/*------------------------------- functions ---------------------------------*/
extern void	analyse(picstruct *, picstruct *, int, objliststruct *),
		blankit(char *, int),
                endcat(void),
                closecheck(void),
		copydata(picstruct *, int, int),
		endfield(picstruct *),
		endobject(picstruct *, picstruct *, picstruct *, picstruct *,
			int, objliststruct *),
		error(int, char *, char *),
		examineiso(picstruct *, picstruct *, objstruct *,
			pliststruct *),
		fixexponent(char *),
		flagcleancrowded(int, objliststruct *),
		freeparcelout(void),
		getnnw(void),
		initcat(picstruct *),
		initglob(void),
		makeit(void),
		mergeobject(objstruct *, objstruct *),
		neurinit(void),
		neurclose(void),
		neurresp(double *, double *),
		preanalyse(int, objliststruct *, int),
		readcatparams(char *),
		readdata(picstruct *, PIXTYPE *, int),
		readidata(picstruct *, FLAGTYPE *, int),
		readimagehead(picstruct *),
		readprefs(char *, char **, char **, int),
		scanimage(picstruct *, picstruct *, picstruct **, int,
			picstruct *, picstruct *),
		sexcircle(PIXTYPE *bmp, int, int, double, double, double,
			PIXTYPE),
		sexdraw(PIXTYPE *bmp, int, int, double, double, PIXTYPE),
		sexellips(PIXTYPE *bmp, int, int, double, double, double,
			double, double, PIXTYPE, int),
		sexmove(double, double),
		swapbytes(void *, int, int),
		updateparamflags(void),
		useprefs(void),
		warning(char *, char *),
		writecat(int, objliststruct *);

extern float	hmedian(float *, int);

extern int	addobj(int, objliststruct *, objliststruct *),
		belong(int, objliststruct *, int, objliststruct *),
		cistrcmp(char *, char *, int),
		gatherup(objliststruct *, objliststruct *),
		parcelout(objliststruct *, objliststruct *);

extern void	*loadstrip(picstruct *, picstruct *);

extern char	*readfitshead(FILE *, char *, int *);

extern picstruct	*inheritfield(picstruct *infield, int flags),
			*newfield(char *, int );

