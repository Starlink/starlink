/* eigen/schur.h
 * 
 * Copyright (C) 2006 Patrick Alken
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

#ifndef __GSL_SCHUR_H__
#define __GSL_SCHUR_H__

#include <gsl/gsl_matrix.h>
#include <gsl/gsl_complex.h>

/*
 * Prototypes
 */

void gsl_schur_standardize(gsl_matrix *T, size_t row, gsl_complex *eval1,
                           gsl_complex *eval2, int update_t, gsl_matrix *Z);

#endif /* __GSL_SCHUR_H__ */
