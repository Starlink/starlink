/*
 * f77_types.h: Provides a set of types for use when writing C routines that
 *              access srtandard FORTRAN 77 objects.
 *
 * Maintenance Log:
 *
 *  10/12/86  PJWR  Created.
 */

#ifndef F77_TYPE		/* Guarantee inclusion is unique. */
#define F77_TYPE

typedef long int	f77_integer;
typedef long int	f77_logical;
typedef float		f77_real;
typedef double		f77_double;
typedef struct
{
  float re, im;
}
f77_complex;
typedef char		f77_character;

#endif
