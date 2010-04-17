#!/usr/bin/perl
#
# PoliticalSurvey/Statistics.pm:
# Statistical stuff for the political survey.
#
# Copyright (c) 2003 Chris Lightfoot. All rights reserved.
# Email: chris@ex-parrot.com; WWW: http://www.ex-parrot.com/~chris/
#
# $Id: Statistics.pm,v 1.2 2003/11/04 18:04:31 chris Exp $
#

package PoliticalSurvey::Statistics;

use strict;

use Error qw(:try);
#use Math::MatrixReal;

use Inline;

use PoliticalSurvey;
use PoliticalSurvey::Eigs;

# compute_marginal_dist RESULTS STATEMENT CONVERSE
# Return in list context the marginal distribution of answers for (optionally
# the CONVERSE form of) the given STATEMENT from RESULTS.
sub compute_marginal_dist ($$$) {
    my ($res, $stmt, $conv) = @_;

    my @dist = qw(0 0 0 0 0);

    foreach (@$res) {
        if (defined(my $v = $_->[1 + $conv]->[$stmt])) {
            ++$dist[2 + $v];
        }
    }

    my $sum = $dist[0] + $dist[1] + $dist[2] + $dist[3] + $dist[4];
    foreach (@dist) {
        $_ /= $sum;
    }

    return @dist;
}

# sample_from_dist DISTRIBUTION
# Return a single sample from the given DISTRIBUTION.
sub sample_from_dist ($) {
    my ($d) = @_;
    throw Error::Simple("distribution must have five elements") unless (@$d == 5);
    my $x = rand($d->[0] + $d->[1] + $d->[2] + $d->[3] + $d->[4]);
    my $y = 0;
    for (my $N = -2; $N <= 2; ++$N) {
        $y += $d->[$N + 2];
        return $N if ($x < $y);
    }
    return 2;
}

# compute_converse_mapping RESULTS STATEMENT
# Produce a best-fit line normal = a + b * converse, for the given STATEMENT
# based on the RESULTS, returning in list context a, b and the correlation
# coefficient r**2.
sub compute_converse_mapping ($$) {
    my ($res, $s) = @_;

    # x are the converse answers, y the normal answers.
    my ($sx, $sxx, $sy, $syy, $sxy, $n) = (0, 0, 0, 0, 0, 0);

    # XXX hack: pre-seed the data with a straight line
    foreach ([-2, 2], [0, 0], [2, -2]) {
        my ($x, $y) = @$_;
        $sx += $x;
        $sxx += $x ** 2;
        $sy += $y;
        $syy += $y ** 2;
        $sxy += $x * $y;
        ++$n;
    }

    foreach (@$res) {
        my ($x, $y);
        if (defined($y = $_->[1]->[$s]) and defined($x = $_->[2]->[$s])) {
            $sx += $x;
            $sxx += $x ** 2;
            $sy += $y;
            $syy += $y ** 2;
            $sxy += $x * $y;
            ++$n;
        }
    }

    my $xm = $sx / $n;
    my $ym = $sy / $n;

    my $ssxx = $sxx - $n * ($xm ** 2);
    my $ssyy = $syy - $n * ($ym ** 2);
    my $ssxy = $sxy - $n * ($xm * $ym);

    my $b = $ssxy / $ssxx;

    return ($ym - $b * $xm, $b, ($ssxy ** 2) / ($ssxx * $ssyy));
}

# compute_normal_results RESULTS
# Take RESULTS, and remap any answers which are available only in converse
# form to normal form. Return the normal results as a reference to a list.
sub compute_normal_results ($) {
    my ($ncres) = @_;

    my $i;
    my $ns = @{$ncres->[0]->[1]};

    my (@a, @b, @r2);
    for ($i = 0; $i < @{$ncres->[0]->[1]}; ++$i) {
        ($a[$i], $b[$i], $r2[$i]) = compute_converse_mapping($ncres, $i);
    }

    my @nres = ( );
    foreach (@$ncres) {
        my ($sess, $norm, $conv) = @$_;
        my @newnorm = ( );
        for ($i = 0; $i < $ns; ++$i) {
            if (defined($norm->[$i])) {
                $newnorm[$i] = $norm->[$i];
            } elsif (defined($conv->[$i])) {
                $newnorm[$i] = $a[$i] + $b[$i] * $conv->[$i];
            } else {
                $newnorm[$i] = undef;
            }
        }
        push(@nres, [$sess, [@newnorm]]);
    }

    return \@nres;
}

# compute_statement_dists RESULTS
# Return a reference to a list of the distributions of answers to statements
# in RESULTS.
sub compute_statement_dists ($) {
    my ($res) = @_;
    my @dd;
    for (my $s = 0; $s < @{$res->[0]->[1]}; ++$s) {
        push(@dd, [compute_marginal_dist($res, $s, 0)]);
    }
    return \@dd;
}

# compute_uncorrelated_results NUM DISTRIBUTIONS
# Compute NUM separate resuls for statements with the given marginal
# DISTRIBUTIONS.
sub compute_uncorrelated_results ($$) {
    my ($num, $dists) = @_;

    my @fake = ( );
    
    for (my $s = 0; $s < @$dists; ++$s) {
        my @dist = @{$dists->[$s]}; #compute_marginal_dist($real, $s, 0);
        for (my $i = 0; $i < $num; ++$i) {
            $fake[$i] = [0, []] if (!exists($fake[$i]));

            $fake[$i]->[1]->[$s] = sample_from_dist(\@dist);
        }
    }

    return \@fake;
}

# find_statement_sets RESULTS
# Return in list context the distinct sets of statements answered in the
# sessions of the (normal) RESULTS. Each return value is a comma-separated
# ordered list of statement IDs.
sub find_statement_sets ($) {
    my ($res) = @_;

    my %sets = ( );
    my $ns = @{$res->[0]->[1]};

    foreach my $r (@$res) {
        my $x = join(",", sort { $a <=> $b } grep { defined($r->[1]->[$_]) } (0..$ns - 1));
        $sets{$x} = 1;
    }

    return sort { length($a) <=> length($b) } keys %sets;
}

# compute_covariance RESULTS S1 S2
# Return the covariance between answers to statements S1 and S2 in RESULTS.
sub compute_covariance ($$$) {
    my ($res, $i, $j) = @_;

    # covariance = <xy> - <x><y>.
    my ($sx, $sy, $sxy, $n) = (0, 0, 0, 0);

    for (my $m = 0; $m < scalar(@$res); ++$m) {
        my $norm = $res->[$m]->[1];
        my ($x, $y);
        if (defined($x = $norm->[$i]) and defined($y = $norm->[$j])) {
            $sx += $x;
            $sy += $y;
            $sxy += $x * $y;
            ++$n;
        }
    }

    return ($sxy / $n) - $sx * $sy / ($n ** 2);
}

# compute_covariance_matrix RESULTS
# Return the covariance matrix for all pairs of statements in the RESULTS, as
# a reference to a list of lists.
sub compute_covariance_matrix ($) {
    my ($res) = @_;

    my $ns = @{$res->[0]->[1]};
    my $cov = [ ];

    for (my $i = 0; $i < $ns; ++$i) {
        for (my $j = 0; $j <= $i; ++$j) {
            $cov->[$j]->[$i] = $cov->[$i]->[$j] = compute_covariance($res, $i, $j);
#die "undefined covariance at $i, $j" unless defined($cov->[$i]->[$j]);
#print STDERR $cov->[$i]->[$j] - $cov->[$j]->[$i], "\n" if ($i > $j);
        }
    }

    return $cov;
}

# compute_eiegensystem COVARIANCE SET
# Compute the eigenvectors and eigenvalues of the subset of the COVARIANCE 
# matrix given by SET, a comma-separated list of statement IDs. Returns a
# reference to a list of [eigenvalue, [eigenvector component, ...]].
sub compute_eigensystem ($$) {
    my ($cov, $set) = @_;

    my @s = split(/,/, $set);
    my @i2s = map { $s[$_] } (0..$#s);

    my $covsubset;
    my ($i, $j);
    for ($i = 0; $i < @s; ++$i) {
        for ($j = 0; $j < @s; ++$j) {
            $covsubset->[$i]->[$j] = $cov->[$s[$i]]->[$s[$j]];
        }
    }

    my $elist = PoliticalSurvey::Eigs::compute_eigs(1, $covsubset);
    my @r = ( );

    foreach (@$elist) {
        my ($val, $vec) = @$_;
        my @v;
        for ($i = 0; $i < @s; ++$i) {
            $v[$i] = $vec->[$i2s[$i]];
        }
        push(@r, [$val, [@v]]);
    }

    return \@r;
}

# shuffle ARRAY
# Shuffle the elements of ARRAY. See perlfaq4.
sub shuffle ($) {
    my ($array) = @_;
    my $i;
    for ($i = @$array; --$i; ) {
        my $j = int(rand($i + 1));
        @$array[$i,$j] = @$array[$j,$i];
    }
}

1;

__END__
    my $mcov = new Math::MatrixReal(scalar(@s), scalar(@s));

    my ($i, $j);

    my @i2s = map { $s[$_] } (0..$#s);

    for ($i = 0; $i < @s; ++$i) {
        for ($j = 0; $j < @s; ++$j) {
            $mcov->assign(1 + $i, 1 + $j, $cov->[$s[$i]]->[$s[$j]]);
        }
    }
    throw Error::Simple("asymmetric covariance matrix") unless $mcov->is_symmetric();
    my ($l, $v) = $mcov->sym_diagonalize();

    # Flatten eigs, sort, return.
    my @ee;
    for ($i = 0; $i < @s; ++$i) {
        $ee[$i]->[0] = abs($l->element(1 + $i, 1));
        for (my $j = 0; $j < @s; ++$j) {
            $ee[$i]->[1]->[$s[$j]] = $v->element(1 + $j, 1 + $i);
        }
    }

    # Return eigenvectors in largest-first order.
    return [sort { $b->[0] <=> $a->[0] } @ee];
}

1;
