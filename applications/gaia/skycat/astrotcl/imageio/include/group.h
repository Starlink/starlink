#define MAX_HDU_TRACKER 1000

typedef struct _HDUtracker HDUtracker;

struct _HDUtracker
{
  int nHDU;

  char *filename[MAX_HDU_TRACKER];
  int  position[MAX_HDU_TRACKER];

  char *newFilename[MAX_HDU_TRACKER];
  int  newPosition[MAX_HDU_TRACKER];
};

/* functions used internally in the grouping convention module */

int ffgtdc(int grouptype, int xtensioncol, int extnamecol, int extvercol,
	   int positioncol, int locationcol, int uricol, char *ttype[],
	   char *tform[], int *ncols, int  *status);

int ffgtgc(fitsfile *gfptr, int *xtensionCol, int *extnameCol, int *extverCol,
	   int *positionCol, int *locationCol, int *uriCol, int *grptype,
	   int *status);

int ffgmul(fitsfile *mfptr, int rmopt, int *status);

int ffgmf(fitsfile *gfptr, char *xtension, char *extname, int extver,	   
	  int position,	char *location,	long *member, int *status);

int ffgtrmr(fitsfile *gfptr, HDUtracker *HDU, int *status);

int ffgtcpr(fitsfile *infptr, fitsfile *outfptr, int cpopt, HDUtracker *HDU,
	    int *status);

int fftsad(fitsfile *mfptr, HDUtracker *HDU, int *newPosition, 
	   char *newFileName);

int fftsud(fitsfile *mfptr, HDUtracker *HDU, int newPosition, 
	   char *newFileName);

void ffgtcn(char *filename);

void prepare_keyvalue(char *keyvalue);
