/****************************************
 IMAGE.H

*/

extern struct
{
      F77_INTEGER_TYPE  ipim;	 	/* Image pointer */
      F77_INTEGER_TYPE  nx;		/* Image X size */
      F77_INTEGER_TYPE  ny;		/* Image Y size */
      F77_REAL_TYPE     bs;		/* Image value scale */
      F77_REAL_TYPE     bz;		/* Image value zero */
      F77_INTEGER_TYPE  inval;		/* Image bad pixel value */
      F77_REAL_TYPE     rinval;		/* Image real bad pixel value */
} F77_NAMED_COMMON(imagecoma);

extern struct
{
      F77_CHARACTER_TYPE  imtitle[70];	/* Image title */
      F77_CHARACTER_TYPE  imtype[6];	/* Image type ('REAL':'INT':'SHORT') */
} F77_NAMED_COMMON(imagecomc);
