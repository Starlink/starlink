 /*
 				back.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP/Leiden.
*
*	Contents:	functions dealing with background computation.
*
*	Last modify:	02/02/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*----------------------------- Internal constants --------------------------*/
#define	BACK_BUFSIZE		1048576		/* bkgnd buffer */
#define	QUANTIF_NSIGMA		5		/* histogram limits */
#define	QUANTIF_NMAXLEVELS	4096		/* max nb of quantif. levels */
#define	QUANTIF_AMIN		4		/* min nb of "mode pixels" */

/* NOTES:
One must have:		BACK_BUFSIZE >= MAXPICSIZE
			0 < QUANTIF_NSIGMA <= 10
			QUANTIF_AMIN > 0
*/

/*------------------------------- structures --------------------------------*/
/* Background info */
typedef struct structback
  {
  float		mode, mean, sigma;	/* Background mode, mean and sigma */
  LONG		*histo;			/* Pointer to a histogram */
  int		nlevels;		/* Nb of histogram bins */
  float		qzero, qscale;		/* Position of histogram */
  float		lcut, hcut;		/* Histogram cuts */
  int		npix;			/* Number of pixels involved */
  }	backstruct;


/*------------------------------- functions ---------------------------------*/
void		backstat(backstruct *, PIXTYPE *, size_t, int, int, int),
		backrmsline(picstruct *, int, PIXTYPE *),
		copyback(picstruct *infield, picstruct *outfield),
		endback(picstruct *),
		filterback(picstruct *),
		makeback(picstruct *),
		subbackline(picstruct *, int, PIXTYPE *);

float		backguess(backstruct *, float *, float *),
		localback(picstruct *, objstruct *),
		*makebackspline(picstruct *, float *);

extern PIXTYPE	back(picstruct *, int, int);
