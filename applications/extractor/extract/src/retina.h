 /*
 				retina.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP & Leiden Sterrewacht.
*
*	Contents:	include file related to retina.c.
*
*	Last modify:	07/12/96
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/
/*------------------------------- structures --------------------------------*/

typedef struct structreti
  {
/*---- convolution */
  float		*pix;		/* Pointer to the copy of the pixel array */
  int		width, height;	/* x,y size of the mask */
  int		npix;		/* Number of pixels in the retina */
  float		minnorm;	/* Minimum normalisation factor */
  struct structbpann	*bpann;	/* The neural network */
  }     retistruct;

retistruct	*theretina;

/*------------------------------- functions ---------------------------------*/

retistruct	*getretina(char *filename);
float		readretina(picstruct *, retistruct *, float, float);
void		endretina(retistruct *retina);

