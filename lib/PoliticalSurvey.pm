#!/usr/bin/perl
#
# PoliticalSurvey.pm:
# Utility functions for the survey.
#
# Copyright (c) 2003 Chris Lightfoot. All rights reserved.
# Email: chris@ex-parrot.com; WWW: http://www.ex-parrot.com/~chris/
#
# $Id: PoliticalSurvey.pm,v 1.12 2003/11/18 23:43:37 chris Exp $
#

package PoliticalSurvey;

use strict;

use Error qw(:try);

package PoliticalSurvey::HTML;

use HTML::Entities qw(encode_entities);

sub html_head ($) {
    my ($title) = @_;
    encode_entities($title);
    return <<EOF;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<title>$title</title>
<link rel="stylesheet" type="text/css" media="screen" href="/std.css">
</head>
<body>
<h1>$title</h1>
EOF
}

sub html_tail () {
    # XXX should add validate link....
    return <<EOF
<hr>
<p class="smallprint">
Political Survey: an open, honest version of politicalcompass.org.<br>
Copyright © 2003 <a href="http://ex-parrot.com/~chris/">Chris Lightfoot</a>.
Available under a
<a href="http://creativecommons.org/licenses/by-nc-sa/1.0/">Creative Commons
Licence</a>.<br>
Sponsored by <a href="http://mythic-beasts.com/">Mythic Beasts Ltd.</a>
</p>
</body>
</html>
EOF
}

# new_url QUERY [PARAM VALUE]...
# Return the URL of the QUERY, with the given changes to PARAMs.
sub new_url ($%) {
    my ($q, %p) = @_;
    my $q2 = new CGI($q);
    foreach (keys %p) {
        $q2->param($_, $p{$_});
    }
    return $q2->self_url();
}

package PoliticalSurvey::DB;

use IO::File;
use Text::CSV;
use DBI;
use DBD::SQLite;

# Database file.
my $dbfile = '/path/to/politicalsurvey/politicalsurvey.sqlite';

# Fraction of questions to repeat for consistency checking.
my $repeatfrac = 0.1;

# database
# Open a connection to the database.
sub database () {
    return DBI->connect("dbi:SQLite:dbname=$dbfile", "", "", { RaiseError => 1, AutoCommit => 0 });
}

# select_single_row DBH STATEMENT [BINDVALS]
# Return in list context the columns returned by performing STATEMENT on DBH
# with BINDVALS.
sub select_single_row ($$;@) {
    my ($dbh, $stmt, @binds) = @_;
    my $x = $dbh->selectall_arrayref($stmt, {}, @binds);
    throw Error::Simple("statement `$stmt' returned " . scalar(@$x) . " rows in select_single_row, should be 1")
        unless (@$x == 1);
    return @{$x->[0]};
}

# select_single_value DBH STATEMENT [BINDVALS]
# As select_single_row, but return in scalar context the single value returned.
sub select_single_value ($$;@) {
    my ($dbh, $stmt, @binds) = @_;
    my @x = select_single_row($dbh, $stmt, @binds);
    throw Error::Simple("statement `$stmt' returned " . scalar(@x) . " columns in select_single_value, should be 1")
        unless (@x == 1);
    return $x[0];
}

# fill_table_from_csv DBH STATEMENT FILENAME FIELDS
# For each row in the named CSV file, execute the given STATEMENT, substituting
# values according to the columns listed in FIELDS, a reference to a list of
# 0-based indices.
sub fill_table_from_csv ($$$$) {
    my ($dbh, $stmt, $filename, $fields) = @_;
    my $f = new IO::File($filename, O_RDONLY) or die "$filename: open: $!";

    my $csv = new Text::CSV;
    my $line;
    my $n = 1;
    while (defined($line = $f->getline())) {
        $csv->parse($line) or die "$filename: line $n: parse error";
        
        my @ff = $csv->fields();
        my @vals;
        foreach (@$fields) {
            die "$filename: not enough columns" unless ($_ < @ff);
            push(@vals, $ff[$_]);
        }

        $dbh->do($stmt, {}, @vals);

        ++$n;
    }

    die "$filename: while reading: $!" if ($f->error());
    $f->close();
}

# get_statement_list DBH ID
# Return a reference a list of [statement ID, text, isconverse] for this survey.
sub get_statement_list ($$) {
    my ($dbh, $id) = @_;

    my $rand = new PoliticalSurvey::Random($id);

    my $ss = $dbh->selectall_arrayref('select id, normal, converse from statement where enabled = 1');

    # Shuffle @$ss according the the local random number generator. See
    # perlfaq4.
    my $i;
    for ($i = @$ss; --$i; ) {
        my $j = int(($i + 1) * $rand->number_real());
        @$ss[$i, $j] = @$ss[$j, $i];
    }

    # Should we use normal or converse forms of each statement?
    my @useconverse = map { ($rand->number_real() > 0.5 ? 1 : 0) } @$ss;

    # Now pick a few questions from near the beginning, and put the opposite
    # version at the end.
    my $nrepeats = int($repeatfrac * @$ss);
    my %repeated = ( );

    for ($i = 0; $i < $nrepeats; ++$i) {
        my $q;
        
        # Repeat each statement a maximum of once.
        do {
            $q = int($rand->number_real() * $nrepeats * 3.)
        } while (exists($repeated{$ss->[$q]->[0]}));

        push(@$ss, [(@{$ss->[$q]})]);
        push(@useconverse, $useconverse[$q] ? 0 : 1);
        $repeated{$ss->[$q]->[0]} = 1;
    }

    my @res = ( );
    for ($i = 0; $i < @$ss; ++$i) {
        push(@res, [$ss->[$i]->[0], ($useconverse[$i] ? $ss->[$i]->[2] : $ss->[$i]->[1]), $useconverse[$i]]);
    }

    return \@res;
}

# get_country_from_ip DBH IP
# Map the given IP address to a country code. Returns GB if not known.
sub get_country_from_ip ($$) {
    my ($dbh, $ip) = @_;
    use integer;
    throw Error::Simple("must specify an IP address") unless $ip;
    my @q = split(/\./, $ip);
    throw Error::Simple("`$ip' is not a valid IP address") unless (@q == 4 && 4 == scalar(grep { $_ >= 0 && $_ <= 255 } @q));
    my $v = ($q[0] << 24) | ($q[1] << 16) | ($q[2] << 8) | $q[3];
    my $x = $dbh->selectall_arrayref('select code from ipcountry where iplo >= ? and iphi <= ?', {}, $v, $v);
    return 'GB' if (!@$x);
    return $x->[0]->[0];
}

# get_countries_list DBH
# Return a reference to a list of [country code, name] in sorted order.
sub get_countries_list ($) {
    my ($dbh) = @_;
    return [ map { [$_->[0], $_->[1]] } @{ $dbh->selectall_arrayref('select code, name from country') } ];
}

# get_country_codes_list DBH
# Return a reference to a list of country codes, listed by a human-readable
# sorted order.
sub get_country_codes_list ($) {
    my ($dbh) = @_;
    return [ map { $_->[0] } @{ get_countries_list($dbh) } ]
}

# get_countries_hash DBH
# Return a reference to a hash of country code => country name.
sub get_countries_hash ($) {
    my ($dbh) = @_;
    return { map { $_->[0] => $_->[1] } @{ get_countries_list($dbh) } };
}

# save_session_data DBH ID IPFROM COUNTRY
# Save the given data in the session table. DOES NOT COMMIT!
sub save_session_data ($$$$) {
    my ($dbh, $id, $ipfrom, $country) = @_;
    $dbh->do('delete from session where id = ?', {}, $id);
    $dbh->do('insert into session (id, ipfrom, country, timewhen) values (?, ?, ?, ?)', {}, $id, $ipfrom, $country, time());

    return 1;
}

# save_answers DBH ID ANSWERS
# Save the results given in ANSWERS, which is a reference to a list of
# [statement ID, isconverse, value], returning false if there are not enough
# answers to complete the survey. DOES NOT COMMIT!
sub save_answers ($$$) {
    my ($dbh, $id, $answers) = @_;

    my %needids = map { $_->[0] => 1 } @{ $dbh->selectall_arrayref('select id from statement where enabled = 1') };

    foreach (@$answers) {
        delete($needids{$_->[0]});
    }

    return 0 if (scalar(keys %needids) > 0);

    $dbh->do('delete from answer where sessionid = ?', {}, $id);
    foreach (@$answers) {
        $dbh->do('insert into answer (sessionid, statementid, converse, value) values (?, ?, ?, ?)', {}, $id, $_->[0], $_->[1], $_->[2]);
    }

    return 1;
}


sub cumulativedist1 ($$) {
    my ($x, $dist) = @_;
    return 0 if ($x < -2.5);
    return 1 if ($x >  2.5);
    my $f = 0;
    my $i;
    for ($i = 0; $x > -1.5 + $i; ++$i) {
        $f += $dist->[$i];
    }
    $f += $dist->[$i] * ($x + 2.5 - $i);
    return $f;
}

sub cumulativedist2 ($$$$) {
    my ($x, $dist, $a, $b) = @_;

    throw Error::Simple("b must be < 0") unless ($b < 0);
    
    $b = abs($b);
    
    # x = a + b y so y = (x - a) / b
    my $xmin = $a - 2.5 * $b;
    my $xmax = $a + 2.5 * $b;
    return 0 if ($x < $xmin);
    return 1 if ($x > $xmax);

    my $f = 0;
    my $i;
    for ($i = 0; $x > $xmin + ($i + 1) * $b; ++$i) {
        $f += $dist->[4 - $i];
    }
    $f += $dist->[4 - $i] * ($x - $xmin - $i * $b) / $b;
    return $f;
}

sub ksintg ($$$$) {
    my ($dn, $dc, $a, $b) = @_;
    my $f = 0;
    for (my $x = -10; $x < 10; $x += 0.25) {
#printf STDERR "%f %f %f\n", $x,cumulativedist1($x, $dn), cumulativedist2($x, $dc, $a, $b);
        $f += cumulativedist1($x, $dn) - cumulativedist2($x, $dc, $a, $b);
    }
#die;
    return $f;
}

# get_converse_mapping DBH ID
# Fit a straight line normal_value = A + B * converse_answer to the normal
# vs. converse mapping for the given statement ID. Returns in list context
# A, B and the correlation coefficient R**2. XXX vestigial; should use the one
# in PolicitalSurvey::Statistics.
sub get_converse_mapping ($$) {
    my ($dbh, $id) = @_;
    my ($npts, $sxy, $sxx, $syy, $sx, $sy) = select_single_row($dbh, 'select numpts, sumxy, sumxx, sumyy, sumx, sumy from selfcovariance where statementid = ?', $id);
    
    if ($npts < 2 ) {
        # If we have fewer than two points, we can't fit a line. So just say
        # that the normal and converse questions are exactly inversely related.
        # This is pretty feeble but shouldn't matter once we have some more
        # data.
        return (0, -1, 1);
    } elsif ($npts < 10) {
        die "probably don't want to do that, in fact";
        # Use a different scheme where we try to match the marginal
        # distributions. The idea here is that we compute the cumulative
        # distributions for the two forms of the question, and search for
        # a mapping between the two which minimises the cumulative difference.
        # (Imagine a Kolmogorov-Smirnoff test or whatever.)
        my @dnorm = select_single_row($dbh, 'select nm2, nm1, n0, np1, np2 from distribution where statementid = ? and converse = 0', $id);
        my $total = $dnorm[0] + $dnorm[1] + $dnorm[2] + $dnorm[3] + $dnorm[4];
        foreach (@dnorm) {
            $_ /= $total;
        }
        my @dconv = select_single_row($dbh, 'select nm2, nm1, n0, np1, np2 from distribution where statementid = ? and converse = 1', $id);
        $total = $dconv[0] + $dconv[1] + $dconv[2] + $dconv[3] + $dconv[4];
        foreach (@dconv) {
            $_ /= $total;
        }

        my ($A, $B, $I) = (0, -1, 1e6);
        for (my $a = -2; $a <= 2; $a += 0.25) {
            for (my $b = -1.5; $b <= -0.5; $b += 0.1) {
                my $i = ksintg(\@dnorm, \@dconv, $a, $b);
                if (abs($i) < $I) { 
                    $A = $a;
                    $B = $b;
                    $I = abs($i);
                }
            }
        }

        return ($A, $B, -$I);
    } else {
        # Least-squares fit. Note that we want x = a + by, since y are the
        # converse answers and x the normal answers.
        my $ssxx = $sxx - ($sx ** 2) / $npts;
        my $ssyy = $syy - ($sy ** 2) / $npts;
        my $ssxy = $sxy - ($sx * $sy) / $npts;

        return (0, -1, 1) if ($ssxx == 0 || $ssyy == 0);
        
        my $b = $ssxy / $ssyy;
        my $a = ($sx - $b * $sy) / $npts;
        my $rr = ($ssxy ** 2) / ($ssxx * $ssyy);
        return ($a, $b, $rr);
    }
}

# get_answers DBH ID
# Return a reference to a hash mapping statement ID to value for the session
# with the given ID, remapping converse to normal statements if necessary.
sub get_answers ($$) {
    my ($dbh, $sessionid) = @_;

    my $sth = $dbh->prepare('select statementid, converse, value from answer where sessionid = ? order by converse');

    my %res = ( );
    $sth->execute($sessionid);
    while (my ($id, $conv, $val) = $sth->fetchrow_array()) {
        next if exists ($res{$id});
        if ($conv && !exists($res{$id})) {
            my ($a, $b, $r2) = get_converse_mapping($dbh, $id);
            $val = $a + $b * $val;
        }
        $res{$id} = $val;
    }

    return \%res;
}

# get_statementset DBH ANSWERS
# ANSWERS is a reference to a hash whose keys are statement IDs. Return the ID
# of the largest recorded statement set which is a subset of the statements in
# ANSWERS.
sub get_statementset ($$) {
    my ($dbh, $answers) = @_;

    # First try this exact set.
    my $mm = join(',', sort { $a <=> $b } keys %$answers);
    my $x = $dbh->selectall_arrayref('select id from statementset where members = ?', {}, $mm);
    if (@$x > 0) {
        # XXX shouldn't be > 1
        return $x->[0]->[0];
    }

    # No dice. Go through all available sets and pick the largest which
    # overlaps.
    $x = $dbh->selectall_arrayref('select id, members from statementset');
    my ($ln, $lid) = (0, undef);
    foreach (@$x) {
        my ($id, $members) = @$_;
        my %m = map { $_ => 1 } split(/,/, $members);
        # Must not return a set containing statements not in ANSWERS.
        next if (scalar(grep { !exists($answers->{$_}) } keys %m) > 0);
        # Obtain number of answers in this set which are in ANSWERS.
        my $N = scalar(grep { exists($m{$_}) } keys %$answers);
        if ($N > $ln) {
            $ln = $N;
            $lid = $id;
        }
    }

    throw Error::Simple("cannot find a valid statement set") unless defined($lid);

    return $lid;
}

# get_statementset_create DBH ANSWERS
# ANSWERS is a reference to a hash whose keys are statement IDs. Return the ID
# of a statment set containing exactly those IDs, creating one if none exists.
# Does commit!
sub get_statementset_create ($$);
sub get_statementset_create ($$) {
    my ($dbh, $answers) = @_;

    my $mm = join(',', sort { $a <=> $b } keys %$answers);

    my $x = $dbh->selectall_arrayref('select id from statementset where members = ?', {}, $mm);
    if (@$x > 0) {
        # XXX shouldn't be > 1
        return $x->[0]->[0];
    } else {
        $dbh->do('insert into statementset (members) values (?)', {}, $mm);
        $dbh->commit();
        return get_statementset_create($dbh, $answers);
    }
}

# get_statementset_by_id DBH ID
# Return a reference to a list of the members of the statement set with the
# given ID.
sub get_statementset_by_id ($$) {
    my ($dbh, $setid) = @_;
    my $ss = select_single_value($dbh, 'select members from statementset where id = ?', $setid);
    return [split(/,/, $ss)];
}

# get_all_answers DBH
# Return a reference to a list of the answers given by all sessions; each
# element of the list is a list of [session ID, [normal answers], [converse
# answers]], storing undef for an answer if it is not present.
sub get_all_answers ($) {
    my ($dbh) = @_;
 
    my @res = ( );
    my %idmap = ( );
    my $stmt = $dbh->prepare('select sessionid, statementid, converse, value from answer');
    
    $stmt->execute();
    
    my $maxs = 0;

    while (my ($sess, $s, $conv, $val) = $stmt->fetchrow_array()) {
        if (!exists($idmap{$sess})) {
            $idmap{$sess} = scalar(@res);
            push(@res, [$sess, [], []]);
        }

        $res[$idmap{$sess}]->[1 + $conv]->[$s] = $val;

        $maxs = $s if ($s > $maxs);
    }

    # Make sure that all arrays are padded with undef.
    foreach (@res) {
        for (my $i = 0; $i <= $maxs; ++$i) {
            $_->[1]->[$i] = undef if (!exists($_->[1]->[$i]));
            $_->[2]->[$i] = undef if (!exists($_->[2]->[$i]));
        }
    }

    return \@res;
}

# get_eigenvalues DBH SET
# Return in list context the first two eigenvalues of SET.
sub get_eigenvalues ($$) {
    my ($dbh, $set) = @_;
    return map { $_->[0] } @{$dbh->selectall_arrayref('select val from eigenvalue where setid = ? order by val desc limit 2', {}, $set)};
}

# get_eigenvector DBH SET N
# Return the N'th eigenvector of SET as a reference to an array of statement ID
# -> value, or undef if none is given. Entries in the eigenvector for
# statements not present in the SET are undef. Eigenvectors are numbered from
# zero.
sub get_eigenvector ($$$) {
    my ($dbh, $set, $n) = @_;

    my %members = map { $_ => 1 } @{get_statementset_by_id($dbh, $set)};
    
    my $stmt = $dbh->prepare('select i, x from eigenvector where setid = ? and n = ?');
    
    $stmt->execute($set, $n);
    my $res = [ ];

    while (my ($i, $x) = $stmt->fetchrow_array()) {
        $res->[$i] = $x if (exists($members{$i}));
    }

    return $res;
}

# set_eigenvector DBH SET N VALUE ELEMENTS
# Set the N'th eigenvector for SET to ELEMENTS; also sets the eigenvalue to
# VALUE. Does not commit!
sub set_eigenvector ($$$$$) {
    my ($dbh, $set, $n, $val, $elts) = @_;

    my @members = @{get_statementset_by_id($dbh, $set)};
  
    $dbh->do('delete from eigenvalue where setid = ? and n = ?', {}, $set, $n);

    $dbh->do('insert into eigenvalue (setid, n, val) values (?, ?, ?)', {}, $set, $n, $val);
    
    $dbh->do('delete from eigenvector where setid = ? and n = ?', {}, $set, $n);

    foreach (@members) {
        $dbh->do('insert into eigenvector (setid, n, i, x) values (?, ?, ?, ?)', {}, $set, $n, $_, $elts->[$_]);
    }
}

# get_position_2d DBH SESSION
# Return in list context the position on the first two principal axes of the
# answers in the given SESSION, and the normalised positions.
sub get_position_2d ($$) {
    my ($dbh, $sess) = @_;
    my $ans = get_answers($dbh, $sess);
    my $setid = get_statementset($dbh, $ans);
    my $e1 = get_eigenvector($dbh, $setid, 0);
    my $e2 = get_eigenvector($dbh, $setid, 1);
    my $size = scalar @{get_statementset_by_id($dbh, $setid)};
    my ($x, $y) = (0, 0);
    foreach (keys %$ans) {
        $x += $ans->{$_} * $e1->[$_];
        $y += $ans->{$_} * $e2->[$_];
    }
    return ($x, $y, $x / (2 * sqrt($size)), $y / (2 * sqrt($size)));
}

package PoliticalSurvey::Random;

use constant IA => 16807;
use constant IQ => 127773;
use constant IR => 2836;
use constant IM => 2147483647;
use constant MASK => 123459876;

# new SEED
# Create a new random number generator with the given seed.
sub new ($$) {
    my ($class, $seed) = @_;
    throw Error::Simple("seed must be a positive integer") unless ($seed eq int($seed) && $seed > 0);
    ++$seed if ($seed == MASK);
    return bless({seed => $seed}, $class);
}

# number
# Get the next random number. NR section 7.1
sub number ($) {
    my $self = shift;
    my $ans;
    use integer;
    $self->{seed} ^= MASK;
    my $k = $self->{seed} / IQ;
    $self->{seed} = IA * ($self->{seed} - $k * IQ) - IR * $k;
    $self->{seed} += IM if ($self->{seed} < 0);
    $self->{seed} -= IM if ($self->{seed} > IM);        # XXX ?
    $ans = $self->{seed};
    $self->{seed} ^= MASK;
    return $ans;
}

# number_real
# Get the next random number, as a real between 0 and 1.
sub number_real ($) {
    my $self = shift;
    return (1. * $self->number()) / IM;
}

1;
