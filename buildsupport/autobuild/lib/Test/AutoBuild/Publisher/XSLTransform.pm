# -*- perl -*-
#
# Test::AutoBuild::Publisher::XSLTransform by Daniel Berrange <dan@berrange.com>
#
# XSLTransformright (C) 2002-2004 Daniel Berrange <dan@berrange.com>
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
#
# $Id$

=pod

=head1 NAME

Test::AutoBuild::Publisher::XSLTransform - Applies an XSL transform to an artifact

=head1 SYNOPSIS

  use Test::AutoBuild::Publisher::XSLTransform


=head1 DESCRIPTION

This module applies an XSL transformation to the artifact, saving the result
to the destination directory.

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Publisher::XSLTransform;

use strict;
use Carp qw(confess);
use Test::AutoBuild::Publisher;
use File::Path;
use vars qw(@ISA);

@ISA = qw(Test::AutoBuild::Publisher);


=pod

=item my $mod = Test::AutoBuild::Publisher::XSLTransform->new(  );

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = $class->SUPER::new(@_);

    bless $self, $class;

    return $self;
}


sub publish {
    my $self = shift;
    my $src = shift;
    my $dst = shift;
    
    my $xsl = $self->option("xsl-file");

    Test::AutoBuild::Lib::run("xsltproc $xsl $src > $dst");
}



1 # So that the require or use succeeds.

__END__

=back 4

=head1 AUTHORS

Daniel Berrange <dan@berrange.com>

=head1 COPYRIGHT

Copyright (C) 2002-2004 Daniel Berrange <dan@berrange.com>

=head1 SEE ALSO

L<perl(1)>

=cut
