/* DS_LUT.H */



extern struct
{
      F77_LOGICAL_TYPE lut_flipped;	/* Is LUT flipped? */
      F77_LOGICAL_TYPE lut_dum1;
      F77_LOGICAL_TYPE lut_dum2;
      F77_LOGICAL_TYPE lut_dum3;
} F77_NAMED_COMMON(lutbcom);

extern struct
{
      F77_INTEGER_TYPE lut_ends;              /* LUT end types (1=black/white;2=wh/bl;3=col/col;4=wrap  */
      F77_INTEGER_TYPE lut_stored;            /* Number of stored LUTs  */
      F77_REAL_TYPE    lut_val[3][150];       /* RGB arrays  */
      F77_INTEGER_TYPE lut_num;               /* Present LUT number  */
      F77_REAL_TYPE    lut_sc;                /* LUT scale  */
      F77_REAL_TYPE    lut_ze;                /* LUT zero  */
      F77_INTEGER_TYPE numddcol;	      /* No of colours in LUT  */
      F77_INTEGER_TYPE depth;		      /* No of memory planes  */
} F77_NAMED_COMMON(lutacom);

