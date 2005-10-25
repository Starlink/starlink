#!/usr/bin/perl -w

# Auto Builder v1.0
#
# By Daniel P. Berrange <dan@berrange.com>
# Copyright (C) 2002-2004 Daniel P. Berrange
#
# Based on Rolling builds version 2.0
#
#   By Richard W.M. Jones <rich@annexia.org>
#     http://annexia.org/freeware/rollingbuild/

#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# $Id$

use strict;

use Config::Record;
use Test::AutoBuild;

use Getopt::Long;

# Configuration file.

my $config_file = $ENV{HOME} . "/auto-build.conf";
my $verbose = 0;

GetOptions ('config=s' => \$config_file, 
	    'verbose+' => \$verbose);

my $config = Config::Record->new(file => $config_file);

my $auto_build = Test::AutoBuild->new(config => $config,
				      verbose => $verbose);

exit $auto_build->run();

# end
