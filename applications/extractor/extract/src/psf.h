 /*
 				psf.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP & Leiden observatory
*
*	Contents:	Include file for psffit.c.
*
*	Last modify:	20/07/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*----------------------------- Internal constants --------------------------*/

#define	PSF_WIDTH	11	/* Width of the PSF-fitting area */
#define	PSF_HEIGHT	11	/* Height of the PSF-fitting area */
#define	PSF_MAXSHIFT	3.0	/* Max shift from initial guess (pixels)*/
#define	PSF_MINSHIFT	1e-3	/* Min shift from previous guess (pixels)*/
#define	PSF_NPIX	(PSF_WIDTH*PSF_HEIGHT)
#define PSF_NPSF	1	/* Number of fitted components */
#define PSF_NITER	20	/* Maximum number of iterations in fit */
#define PSF_NA		(3*PSF_NPSF)	/* Number of fitted parameters */
#define INTERPW		6	/* Interpolation function range (x) */
#define INTERPH		6	/* Interpolation function range (y) */

#define INTERPF(x)	(x==0.0?1.0:sin(PI*x)*sin(PI*x/3.0)/(PI*PI/3.0*x*x))
				/* Lanczos approximation */

/* NOTES:
One must have:	PSF_WIDTH >= 1
		PSF_HEIGHT >= 1
		PSF_MAXSHIFT > 0.0
		PSF_NPSF >= 1
		PSF_NITER >= 1
*/

/*--------------------------- structure definitions -------------------------*/

typedef struct
  {
  char		name[MAXCHAR];	/* Name of the file containing the PSF data */
  int		maskdim;	/* Dimensionality of the tabulated data */
  int		*masksize;	/* PSF mask dimensions */
  int		masknpix;	/* Total number of involved PSF pixels */
  float		*maskcomp;      /* Complete pix. data (PSF components) */
  double	*maskloc;	/* Local PSF */
  double	**context;	/* Contexts */
  t_type	*contexttyp;	/* Context types */
  char		**contextname;	/* Array of context key-names */
  double	*contextoffset;	/* Offset to apply to context data */
  double	*contextscale;	/* Scaling to apply to context data */
  struct poly	*poly;		/* Polynom describing the PSF variations */
  }	psfstruct;

/*----------------------------- Global variables ----------------------------*/
psfstruct	*thepsf;

/*------------------------------- functions ---------------------------------*/
extern void	psf_build(psfstruct *psf),
		psf_end(psfstruct *psf),
		psf_init(psfstruct *psf),
		psf_fit(psfstruct *psf, picstruct *field, picstruct *wfield,
		objstruct *obj),
		svdfit(double *a, double *b, int m, int n, double *sol,
			double **vout, double **wout),
		svdvar(double *vmat, double *wmat, int n, double *covmat);

extern int	psf_shift(psfstruct *psf, double *dest, int w, int h,
		double cdx, double cdy);

extern psfstruct	*psf_load(char *filename);

