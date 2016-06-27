#!/usr/bin/env python

'''
*+
*  Name:
*     JSATILEMOC

*  Purpose:
*     Create an image MOC based on a JSA tile.

*  Description:
*     This script takes a JSA tile and produces a MOC representation
*     of the area covered by good pixels.

*  Usage:
*     jsatilemoc in out [maxorder]

*  ADAM Parameters:
*     IN = NDF (Read)
*        An NDF (or FITS) file containing a JSA tile.  It must have
*        a TILENUM header indicating the JSA tile number accompanied
*        by a comment including the HEALPix Nside value.  The JSA tile
*        must not have been trimmed.  It could either be created
*        with the trimming options disabled, or be untrimmed using the
*        UNTRIM_JSA_TILES PICARD recipe.
*     MAXORDER = _INTEGER (Read)
*        The maximum HEALPix order to be included in the MOC. [29]
*     OUT = FITS (Read)
*        The output MOC FITS file name.

*  Copyright:
*     Copyright (C) 2013-2014 Science & Technology Facilities Council.
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
*-
'''

from math import log
import re

from astropy.io import fits
import numpy as np
from pymoc import MOC
from pymoc.io.fits import write_moc_fits

import starutil

tilenum_comment = re.compile('\(Nside=(\d+)\)')


def main():
    """Main routine for JSATILEMOC command."""

    starutil.glevel = starutil.NONE

    try:
        parsys = starutil.ParSys([
            starutil.Par0S('IN', 'Input JSA tile'),
            starutil.Par0S('OUT', 'Output MOC FITS file'),
            starutil.Par0I('MAXORDER', 'Maximum HEALPix order',
                           default=29, noprompt=True,
                           minval=0, maxval=29)
        ])

        jsa_tile_moc(in_=parsys['IN'].value,
                     out=parsys['OUT'].value,
                     max_order=parsys['MAXORDER'].value)

    finally:
        starutil.ParSys.cleanup()


def jsa_tile_moc(in_, out, max_order):
    """Create image MOC representation of a JSA tile."""

    if in_.lower().endswith('.fits') or in_.lower().endswith('.fit'):
        (data, tile_number, nside) = read_jsa_tile_fits(in_)
        good = np.logical_not(np.isnan(data))

    else:
        (data, tile_number, nside) = read_jsa_tile_sdf(in_)
        try:
            good = data != np.finfo(data.dtype).min
        except ValueError:
            good = data != np.iinfo(data.dtype).min

    (tile_order, map_suborder) = get_tile_params(data, nside)

    pixels = get_pixel_set(good, tile_number, map_suborder)

    moc = MOC(tile_order + map_suborder, pixels, moctype='image')

    moc.normalize(max_order=max_order)

    write_moc_fits(moc, out, clobber=True)


def read_jsa_tile_fits(filename):
    """Read a FITS file containing a JSA tile.

    Gets the TILENUM header and Nside value from the primary
    header.  Checks the CTYPE headers to determine the axis
    order, and transposes the data array if RA is not first.
    """

    hdulist = fits.open(filename, mode='readonly')
    header = hdulist[0].header
    data = np.squeeze(hdulist[0].data)
    hdulist.close()

    tile_number = header['TILENUM']
    match = tilenum_comment.search(header.comments['TILENUM'])
    if not match:
        raise Exception('Cannot find Nside in TILENUM comment')
    nside = int(match.group(1))

    if header['CTYPE1'] == 'RA---HPX' and header['CTYPE2'] == 'DEC--HPX':
        pass
    elif header['CTYPE2'] == 'RA---HPX' and header['CTYPE1'] == 'DEC--HPX':
        data = data.T
    else:
        raise Exception('Cannot determine if the axes are '
                        'the wrong way round or not.')

    return (data, tile_number, nside)


def read_jsa_tile_sdf(filename):
    """Read an SDF file containing a JSA tile.

    Fetches the tile number and Nside value from the FITS airlock.
    Checks the WCS labels to determine in which order the axes are,
    and transposes the data if RA is not first.
    """

    from starlink.ndfpack import Ndf

    ndf = Ndf(filename)
    header = fits.Header.fromstring(''.join(ndf.head['FITS']))
    data = np.squeeze(ndf.data)
    wcs = ndf.wcs

    tile_number = header['TILENUM']
    match = tilenum_comment.search(header.comments['TILENUM'])
    if not match:
        raise Exception('Cannot find Nside in TILENUM comment')
    nside = int(match.group(1))

    if wcs.Label_1 == 'Right ascension' and wcs.Label_2 == 'Declination':
        pass
    elif wcs.Label_2 == 'Right ascension' and wcs.Label_1 == 'Declination':
        data = data.T
    else:
        raise Exception('Cannot determine if the axes are '
                        'the wrong way round or not.')

    return (data, tile_number, nside)


def get_tile_params(data, nside):
    """Find the HEALPix parameters of the tile.

    Determines tile_order, the HEALPix level of the tile, and map_suborder,
    the difference of that and the HEALPix level of the pixels.
    """

    if data.shape[0] == data.shape[1]:
        map_size = data.shape[0]
    else:
        raise Exception('Map is not square')

    tile_order = log(nside, 2)
    if tile_order.is_integer():
        tile_order = int(tile_order)
    else:
        raise Exception('Nside is not a power of 2')

    map_suborder = log(map_size, 2)
    if map_suborder.is_integer():
        map_suborder = int(map_suborder)
    else:
        raise Exception('Map size is not a power of 2')

    return (tile_order, map_suborder)


def get_pixel_set(good, tile_number, map_suborder):
    """Make a set of the HEALPix nested indices for the
    true values in the array.

    Assumes that the map is square (get_tile_params will normally
    already have checked this).
    """

    pixels = set()
    map_size = good.shape[0]
    tile_prefix = tile_number << (2 * map_suborder)

    # Loop over all pixels and make a list of included pixels in
    # the nested numbering scheme.
    for x in range(0, map_size):
        # In the HPX projection the x component of the nested numbering
        # scheme runs from right to left, so invert the direction of x.
        px = map_size - x - 1

        # Interleaving performed using the "Binary Magic Numbers" method,
        # based on the public domain example (collection (C) 1997-2005
        # Sean Eron Anderson) available at:
        # http://graphics.stanford.edu/~seander/bithacks.html#InterleaveBMN
        px = (px | (px << 8)) & 0x00FF00FF
        px = (px | (px << 4)) & 0x0F0F0F0F
        px = (px | (px << 2)) & 0x33333333
        px = (px | (px << 1)) & 0x55555555

        for y in range(0, map_size):
            if good[y, x]:
                py = y
                py = (py | (py << 8)) & 0x00FF00FF
                py = (py | (py << 4)) & 0x0F0F0F0F
                py = (py | (py << 2)) & 0x33333333
                py = (py | (py << 1)) & 0x55555555

                pixels.add(tile_prefix | px | (py << 1))

    return pixels


if __name__ == '__main__':
    main()
