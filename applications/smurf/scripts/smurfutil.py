'''
*+
*  Name:
*     smurfutil

*  Purpose:
*     A collection of smurf-specific functions called by other smurf
*     python scripts.

*  Language:
*     python (2.7 or 3.*)

*  Copyright:
*     Copyright (C) 2012-2013 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     4-SEP-2013 (DSB):
*        Original version
*     5-MAY-2015 (DSB):
*        Remove PCA facilities, and thus the dependencies on mdp and pyndf.

*-
'''

import math
import starutil
import numpy
from starutil import invoke
from starutil import get_task_par
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out


# -----------------------------------------------------------------------
def blanker( test, model, newtest ):
   """

   Blank out pixels in "test" that are not well correlated with "model",
   returning result in newtest.

   Invocation:
      result =  blanker( test, model, newtest )

   Arguments:
      test = string
         The name of an existing NDF.
      model = string
         The name of an existing NDF.
      newtest = string
         The name of an NDF to be created.

   Returned Value:
      A value between +1 and -1 indicating the degree of correlation
      between the model and test.

   """

#  We want statistics of pixels that are present in both test and model,
#  so first form a mask by adding them together, and then copy bad pixels
#  form this mask into test and model
   mask = "{0}/mask".format(NDG.tempdir)
   tmask = "{0}/tmask".format(NDG.tempdir)
   mmask = "{0}/mmask".format(NDG.tempdir)
   invoke( "$KAPPA_DIR/add in1={0} in2={1} out={2}".format(test,model,mask) )
   invoke( "$KAPPA_DIR/copybad in={0} ref={1} out={2}".format(test,mask,tmask) )
   invoke( "$KAPPA_DIR/copybad in={0} ref={1} out={2}".format(model,mask,mmask) )

#  Get the mean and standard deviation of the remaining pixels in the
#  test NDF.
   invoke( "$KAPPA_DIR/stats {0} clip=\[3,3,3\] quiet".format(tmask) )
   tmean = get_task_par( "mean", "stats" )
   tsigma = get_task_par( "sigma", "stats" )

#  Also get the number of good pixels in the mask.
   numgood1 = float( get_task_par( "numgood", "stats" ) )

#  Get the mean and standard deviation of the remaining pixels in the
#  model NDF.
   invoke( "$KAPPA_DIR/stats {0} clip=\[3,3,3\] quiet".format(mmask) )
   mmean = get_task_par( "mean", "stats" )
   msigma = get_task_par( "sigma", "stats" )

#  Normalize them both to have a mean of zero and a standard deviation of
#  unity.
   tnorm = "{0}/tnorm".format(NDG.tempdir)
   invoke( "$KAPPA_DIR/maths exp=\"'(ia-pa)/pb'\" ia={2} pa={0} pb={1} "
           "out={3}".format(tmean,tsigma,tmask,tnorm))

   mnorm = "{0}/mnorm".format(NDG.tempdir)
   invoke( "$KAPPA_DIR/maths exp=\"'(ia-pa)/pb'\" ia={2} pa={0} pb={1} "
           "out={3}".format(mmean,msigma,mmask,mnorm))

#  Find the difference between them.
   diff = "{0}/diff".format(NDG.tempdir)
   invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".format(mnorm,tnorm,diff) )

#  Remove pixels that differ by more than 0.5 standard deviations.
   mtmask = "{0}/mtmask".format(NDG.tempdir)
   invoke( "$KAPPA_DIR/thresh in={0} thrlo=-0.5 newlo=bad thrhi=0.5 "
           "newhi=bad out={1}".format(diff,mtmask) )

#  See how many pixels remain (i.e. pixels that are very similar in the
#  test and model NDFs).
   invoke( "$KAPPA_DIR/stats {0} quiet".format(mtmask) )
   numgood2 = float( get_task_par( "numgood", "stats" ) )

#  It may be that the two NDFs are anti-correlated. To test for this we
#  negate the model and do the above test again.
   mnormn = "{0}/mnormn".format(NDG.tempdir)
   invoke( "$KAPPA_DIR/cmult in={0} scalar=-1 out={1}".format(mnorm,mnormn) )

   diffn = "{0}/diffn".format(NDG.tempdir)
   invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".format(mnormn,tnorm,diffn ))

   mtmaskn = "{0}/mtmaskn".format(NDG.tempdir)
   invoke( "$KAPPA_DIR/thresh in={0} thrlo=-0.5 newlo=bad thrhi=0.5 "
           "newhi=bad out={1}".format(diffn,mtmaskn) )

   invoke( "$KAPPA_DIR/stats {0} quiet".format(mtmaskn) )
   numgood2n = float( get_task_par( "numgood", "stats" ) )

#  If we get more similar pixels by negating the model, the NDFs are
#  anti-correlated.
   if numgood2n > numgood2:

#  Take a copy of the supplied test NDF, masking out pixels that are not
#  anti-similar to the corresponding model pixels.
      invoke( "$KAPPA_DIR/copybad in={0} ref={2} out={1}".format(test,newtest,mtmaskn) )

#  The returned correlation factor is the ratio of the number of
#  anti-similar pixels to the total number of pixels which the two NDFs
#  have in common. But if there is not much difference between the number
#  of similar and anti-similar pixels, we assume there is no correlation.
      if numgood2n > 1.4*numgood2:
         res = -(numgood2n/numgood1)
      else:
         res = 0.0

#  If we get more similar pixels without negating the model, the NDFs are
#  correlated. Do the equivalent to the above.
   else:
      invoke( "$KAPPA_DIR/copybad in={0} ref={2} out={1}".format(test,newtest,mtmask) )
      if numgood2 > 1.4*numgood2n:
         res = numgood2/numgood1
      else:
         res = 0.0

#  If there are very few good pixels in common return zero correlation.
   if numgood1 < 150:
      res = 0.0

#  Return the correlation factor.
   return res


# -----------------------------------------------------------------------
def normer( model, test, cmin, newmodel ):
   """

   Normalise "model" to "test" returning result in "newmodel", so long as
   the "correlation factor" (determined by function blanker) of test and
   model is at least "cmin". Returns a boolean indicating the cmin value
   was reached.


   Invocation:
      result = normer( model, test, cmin, newmodel )

   Arguments:
      model = string
         The name of an existing NDF.
      test = string
         The name of an existing NDF.
      cmin = float
         The lowest acceptable absolute correlation factor.
      newmodel = string
         The name of an NDF to be created. The new NDF is only created if
         the cmin value is reached.

   Returned Value:
      A boolean indicating if the cmin value was reached.

   """

   btest = "{0}/btest".format(NDG.tempdir)
   if abs( blanker( test, model, btest ) ) > cmin:
      invoke( "$KAPPA_DIR/normalize in1={0} in2={2} out={1} device=!".format(model,newmodel,btest))
      return True
   else:
      return False


# -----------------------------------------------------------------------
def remove_corr( ins, masks ):
   """

   Masks the supplied set of Q or U images and then looks for and removes
   correlated components in the background regions.

   Invocation:
      result = remove_corr( ins, masks )

   Arguments:
      ins = NDG
         An NDG object specifying a group of Q or U images from which
         correlated background components are to be removed.
      masks = NDG
         An NDG object specifying a corresponding group of Q or U images
         in which source pixels are bad. These are only used to mask the
         images specified by "in". It should have the same size as "in".

   Returned Value:
      A new NDG object containing the group of corrected Q or U images.

   """

#  How many NDFs are we processing?
   nndf = len( ins )

#  Blank out sources by copy the bad pixels from "mask" into "in". We refer
#  to "q" below, but the same applies whether processing Q or U.
   msg_out( "   masking...")
   qm = NDG( ins )
   invoke( "$KAPPA_DIR/copybad in={0} ref={1} out={2}".format(ins,masks,qm) )

#  Find the most correlated pair of imagtes. We use the basic correlation
#  coefficient calculated by kappa:scatter for this.
   msg_out( "   Finding most correlated pair of images...")
   cmax = 0
   for i in range(0,nndf-1):
      for j in range(i + 1,nndf):
         invoke( "$KAPPA_DIR/scatter in1={0} in2={1} device=!".format(qm[i],qm[j]) )
         c = starutil.get_task_par( "corr", "scatter" )
         if abs(c) > abs(cmax):
            cmax = c
            cati = i
            catj = j

   if abs(cmax) < 0.3:
      msg_out("   No correlated images found!")
      return ins

   msg_out( "   Correlation for best pair of images = {0}".format( cmax ) )

#  Find images that are reasonably correlated to the pair found above,
#  and coadd them to form a model for the correlated background
#  component. Note, the holes left by the masking are filled in by the
#  coaddition using background data from other images.
   msg_out( "   Forming model...")

#  Form the average of the two most correlated images, first normalising
#  them to a common scale so that they both have equal weight.
   norm = "{0}/norm".format(NDG.tempdir)
   if not normer( qm[cati], qm[catj], 0.3, norm ):
      norm = qm[cati]

   mslist = NDG( [ qm[catj], norm ] )
   ave = "{0}/ave".format(NDG.tempdir)
   invoke( "$CCDPACK_DIR/makemos in={0} method=mean genvar=no usevar=no out={1}".format(mslist,ave) )

#  Loop round each image finding the correlation factor of the image and
#  the above average image.
   temp = "{0}/temp".format(NDG.tempdir)
   nlist = []
   ii = 0
   for i in range(0,nndf):
      c = blanker( qm[i], ave, temp )

#  If the correlation is high enough, normalize the image to the average
#  image and then include the normalised image in the list of images to be
#  coadded to form the final model.
      if abs(c) > 0.3:
         tndf = "{0}/t{1}".format(NDG.tempdir,ii)
         ii += 1
         invoke( "$KAPPA_DIR/normalize in1={1} in2={2} out={0} device=!".format(tndf,temp,ave))
         nlist.append( tndf )

   if ii == 0:
      msg_out("   No secondary correlated images found!")
      return ins

   msg_out("   Including {0} secondary correlated images in the model.".format(ii) )

#  Coadded the images created above to form the model of the correlated
#  background component. Fill any remaining bad pixels with artificial data.
   model = "{0}/model".format(NDG.tempdir)
   included = NDG( nlist )
   invoke( "$CCDPACK_DIR/makemos in={0} method=mean usevar=no genvar=no out={1}".format( included, temp ) )
   invoke( "$KAPPA_DIR/fillbad in={1} variance=no out={0} size=10 niter=10".format(model,temp) )

#  Now estimate how much of the model is present in each image and remove it.
   msg_out("   Removing model...")
   temp2 = "{0}/temp2".format(NDG.tempdir)
   qnew = NDG(ins)
   nbetter = 0
   for i in range(0,nndf):

#  Try to normalise the model to the current image. This fails if the
#  correlation between them is too low.
      if normer( model, qm[i], 0.3, temp ):

#  Remove the scaled model form the image.
         invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".format(ins[i],temp,temp2) )

#  We now check that removing the correlated background component has in
#  fact made the image flatter (poor fits etc can mean that images that
#  are poorly correlated to the model have a large amount of model
#  removed and so make the image less flat). FInd the standard deviation
#  of the data in the original image and in the corrected image.
         invoke( "$KAPPA_DIR/stats {0} quiet".format(ins[i]) )
         oldsig = get_task_par( "sigma", "stats" )

         invoke( "$KAPPA_DIR/stats {0} quiet".format(temp2) )
         newsig = get_task_par( "sigma", "stats" )

#  If the correction has made the image flatter, copy it to the returned NDG.
         if newsig < oldsig:
            nbetter += 1
            invoke( "$KAPPA_DIR/ndfcopy in={1} out={0}".format(qnew[i],temp2) )
         else:
            invoke( "$KAPPA_DIR/ndfcopy in={0} out={1}".format(ins[i],qnew[i]) )

#  If the input image is poorly correlated to the model, return the input
#  image unchanged.
      else:
         invoke( "$KAPPA_DIR/ndfcopy in={0} out={1}".format(ins[i],qnew[i]) )

   msg_out( "   {0} out of {1} images have been improved.".format(nbetter,nndf) )

#  Return the corrected images.
   return qnew











# -----------------------------------------------------------------------

def force_flat( ins, masks ):
   """

   Forces the background regions to be flat in a set of Q or U images.

   Invocation:
      result = force_flat( ins, masks )

   Arguments:
      in = NDG
         An NDG object specifying a group of Q or U images from which
         any low frequency background structure is to be removed.
      masks = NDG
         An NDG object specifying a corresponding group of Q or U images
         in which source pixels are bad. These are only used to mask the
         images specified by "in". It should have the same size as "in".

   Returned Value:
      A new NDG object containing the group of corrected Q or U images.

   """

#  How many NDFs are we processing?
   nndf = len( ins )

#  Blank out sources by copy the bad pixels from "mask" into "in".
   msg_out( "   masking...")
   qm = NDG( ins )
   invoke( "$KAPPA_DIR/copybad in={0} ref={1} out={2}".format(ins,masks,qm) )

#  Smooth the blanked NDFs using a 3 pixel Gaussian. Set wlim so that
#  small holes are filled in by the smoothing process.
   msg_out( "   smoothing...")
   qs = NDG( ins )
   invoke( "$KAPPA_DIR/gausmooth in={0} out={1} fwhm=3 wlim=0.5".format(qm,qs) )

#  Fill remaining big holes using artifical data.
   msg_out( "   filling...")
   qf = NDG( ins )
   invoke( "$KAPPA_DIR/fillbad in={0} out={1} niter=10 size=10 variance=no".format(qs,qf) )

#  Subtract the filled low frequency data form the original to create the
#  returned images.
   msg_out( "   removing low frequency background structure...")
   result = NDG( ins )
   invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".format(ins,qf,result) )

   return result




