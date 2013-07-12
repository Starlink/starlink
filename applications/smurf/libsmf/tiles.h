
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


/* Struct to store information about the sky tiling scheme for a given
   JCMT instrument. */

typedef struct smfSkyTiling {
  smf_inst_t instrument;  /* Identifier for instrument */
  const char *name;       /* Instrument name */
  const char *subdir;     /* Sub-directory for instrument tiles */
  const char *type;       /* Data type for tile values */
  int var;                /* Should tiles have variance arrays? */
  int ntpf;               /* No. of tiles along one edge of an HPX facet */
  int ppt;                /* No. of pixels along one edge of a tile */
  int ntiles;             /* Number of tiles over whole sky */
  float fov;              /* Diameter of field of view (arcsec) */
} smfSkyTiling;



/* Prototypes */
void smf_skytiling( smf_inst_t instrument, smfSkyTiling *skytiling, int *status );
AstFitsChan *smf_skytileheader( int itile, smfSkyTiling *skytiling, int *status );
int smf_skytilexy2i( int xt, int yt, smfSkyTiling *skytiling, int *status );
void smf_skytile( int itile, smfSkyTiling *skytiling, AstFitsChan **fc, AstFrameSet **fs, AstRegion **region, int dim[2], int *status );
void smf_skytilei2xy( int itile, smfSkyTiling *skytiling, int *xt, int *yt, int *status );
int *smf_skytiles_region( AstRegion *region, smf_inst_t instrument, int *ntile, int *status );
int *smf_skytiles_data( Grp *igrp, size_t size, int *ntile, int *status );

