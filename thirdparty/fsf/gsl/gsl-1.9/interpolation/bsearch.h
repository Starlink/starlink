/* interpolation/bsearch.h
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2004 Gerard Jungman
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/* Author:  G. Jungman
 */
#ifndef __GSL_INTERP_BSEARCH_H__
#define __GSL_INTERP_BSEARCH_H__


/* Perform a binary search of an array of values.
 * 
 * The parameters index_lo and index_hi provide an initial bracket,
 * and it is assumed that index_lo < index_hi. The resulting index
 * is guaranteed to be strictly less than index_hi and greater than
 * or equal to index_lo, so that the implicit bracket [index, index+1]
 * always corresponds to a region within the implicit value range of
 * the value array.
 *
 * Note that this means the relationship of 'x' to x_array[index]
 * and x_array[index+1] depends on the result region, i.e. the
 * behaviour at the boundaries may not correspond to what you
 * expect. We have the following complete specification of the
 * behaviour.
 * Suppose the input is x_array[] = { x0, x1, ..., xN }
 *    if ( x == x0 )           then  index == 0
 *    if ( x > x0 && x <= x1 ) then  index == 0, and sim. for other interior pts
 *    if ( x == xN )           then  index == N-1
 *    if ( x > xN )            then  index == N-1
 *    if ( x < x0 )            then  index == 0 
 */
size_t
gsl_interp_bsearch(
  const double x_array[], double x,
  size_t index_lo,
  size_t index_hi
  );


#ifdef HAVE_INLINE
extern
inline
size_t
gsl_interp_bsearch(
  const double x_array[], double x,
  size_t index_lo,
  size_t index_hi
  )
{
  size_t ilo = index_lo;
  size_t ihi = index_hi;
  while(ihi > ilo + 1) {
    size_t i = (ihi + ilo)/2;
    if(x_array[i] > x)
      ihi = i;
    else
      ilo = i;
  }

  return ilo;
}
#endif /* HAVE_INLINE */


#endif /* __GSL_INTERP_BSEARCH_H__ */
