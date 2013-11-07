
/* JCMT instruments */

typedef enum {
  SMF__INST_NONE,
  SMF__INST_SCUBA_2_450,
  SMF__INST_SCUBA_2_850,
  SMF__INST_HARP,
  SMF__INST_RXA,
  SMF__INST_RXWD,
  SMF__INST_RXWB
} smf_inst_t;


/* Struct to store information about the JSA tiling scheme for a given
   JCMT instrument. */

typedef struct smfJSATiling {
  smf_inst_t instrument;  /* Identifier for instrument */
  const char *name;       /* Instrument name */
  const char *subdir;     /* Sub-directory for instrument tiles */
  const char *type;       /* Data type for tile values */
  int var;                /* Should tiles have variance arrays? */
  int ntpf;               /* No. of tiles along one edge of an HEALPix facet */
  int ppt;                /* No. of pixels along one edge of a tile */
  int ntiles;             /* Number of tiles over whole sky */
  float fov;              /* Diameter of field of view (arcsec) */
} smfJSATiling;



/* Prototypes */
void         smf_jsainstrument( const char *param, AstFitsChan *fc,
                                smf_inst_t def, smfJSATiling *tiling,
                                int *status );
void         smf_jsatile( int itile, smfJSATiling *jsatiling,
                          int local_origin, AstFitsChan **fc,
                          AstFrameSet **fs, AstRegion **region, int lbnd[2],
                          int ubnd[2], int *status );
AstFitsChan *smf_jsatileheader( int itile, smfJSATiling *jsatiling,
                                int local_origin, int *status );
void         smf_jsatilei2xy( int itile, smfJSATiling *jsatiling, int *xt,
                              int *yt, int *fi, int *status );
int *        smf_jsatiles_region( AstRegion *region, smfJSATiling *tiling,
                                  int *ntile, int *status );
int *        smf_jsatiles_data( Grp *igrp, size_t size, smfJSATiling *tiling,
                                int *ntile, int *status );
int          smf_jsatilexy2i( int xt, int yt, smfJSATiling *jsatiling,
                              int *status );
void         smf_jsatiling( smf_inst_t instrument, smfJSATiling *jsatiling,
                            int *status );


