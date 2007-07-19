static int
set (void *vstate, gsl_multifit_function_fdf * fdf, gsl_vector * x, gsl_vector * f, gsl_matrix * J, gsl_vector * dx, int scale)
{
  lmder_state_t *state = (lmder_state_t *) vstate;

  gsl_matrix *r = state->r;
  gsl_vector *tau = state->tau;
  gsl_vector *diag = state->diag;
  gsl_vector *work1 = state->work1;
  gsl_permutation *perm = state->perm;

  int signum;

  GSL_MULTIFIT_FN_EVAL_F_DF (fdf, x, f, J);

  state->par = 0;
  state->iter = 1;
  state->fnorm = enorm (f);

  gsl_vector_set_all (dx, 0.0);

  /* store column norms in diag */

  if (scale)
    {
      compute_diag (J, diag);
    }
  else
    {
      gsl_vector_set_all (diag, 1.0);
    }

  /* set delta to 100 |D x| or to 100 if |D x| is zero */

  state->xnorm = scaled_enorm (diag, x);
  state->delta = compute_delta (diag, x);

  /* Factorize J into QR decomposition */

  gsl_matrix_memcpy (r, J);
  gsl_linalg_QRPT_decomp (r, tau, perm, &signum, work1);

  return GSL_SUCCESS;
}
