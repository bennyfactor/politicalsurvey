#!/usr/bin/perl
#
# PoliticalSurvey/Graphics.pm:
# Graphical plots for the survey.
#
# Copyright (c) 2003 Chris Lightfoot. All rights reserved.
# Email: chris@ex-parrot.com; WWW: http://www.ex-parrot.com/~chris/
#
# $Id: Graphics.pm,v 1.6 2003/11/18 23:43:37 chris Exp $
#

package PoliticalSurvey::Graphics;

use strict;

use Error qw(:try);

use GD;
use PoliticalSurvey;

# plot_histogram GD X Y W H U V COLOUR BINS [LABELCOLOUR LABELS]
# Plot a histogram of the data in BINS, which is a reference to an array. U and
# V give the directions of the ordinate and coordinate axes relative to the
# point (X, Y); W and H are the space available for the ordinate and coordinate
# axes in the U and V directions. Bars are plotted in the given COLOUR. LABELS
# is an optional reference to a list of labels for the individual bars.
sub plot_histogram ($$$$$$$$$;$$) {
    my ($gd, $x, $y, $w, $h, $u, $v, $col, $data, $lcol, $labels) = @_;

    my ($ux, $uy, $vx, $vy) = (@$u, @$v);

    my $barwidth = $w / @$data;
    my $max = 0.;
    my @barheight = map { $max = $_ if ($_ > $max); $_ * $h } @$data;

    return if ($max == 0);

    for (my $i = 0; $i < @$data; ++$i) {
        my $p = new GD::Polygon();
        $p->addPt($x + $i * $ux * $barwidth, $y + $i * $uy * $barwidth);
        $p->addPt($x + $i * $ux * $barwidth + $vx * $barheight[$i] / $max, $y + $i * $uy * $barwidth + $vy * $barheight[$i] / $max);
        $p->addPt($x + ($i + 1) * $ux * $barwidth + $vx * $barheight[$i] / $max, $y + ($i + 1) * $uy * $barwidth + $vy * $barheight[$i] / $max);
        $p->addPt($x + ($i + 1) * $ux * $barwidth, $y + ($i + 1) * $uy * $barwidth);

        $gd->filledPolygon($p, $col);

        if ($labels) {
            $gd->string(gdTinyFont, $x + ($i + 0.5) * $ux * $barwidth, $y + ($i + 0.5) * $uy * $barwidth, $labels->[$i], $lcol);
        }
    }
}

# string_centered GD FONT X Y HORIZ VERT STRING COLOUR [ROTATED]
# Draw STRING optionally HORIZontally and VERTically centered at (X, Y) on GD.
sub string_centered ($$$$$$$$;$) {
    my ($gd, $f, $x, $y, $hc, $vc, $s, $c, $rot) = @_;
    $rot ||= 0;
    if (!$rot) {
        $x -= $f->width * length($s) / 2. if ($hc);
        $y -= $f->height / 2. if ($vc);
        $gd->string($f, $x, $y, $s, $c);
    } else {
        $x -= $f->height / 2 if ($hc);
        $y += $f->width * length($s) / 2 if ($hc);
        $gd->stringUp($f, $x, $y, $s, $c);
    }
}

# statement_dist_plot DBH STATEMENTID
# Return a GD object showing the distribution of answers for the given
# statement in its normal and converse forms, and a scatter-plot of the
# distribution of paired answers when we repeat the question.
sub statement_dist_plot ($$) {
    my ($dbh, $id) = @_;

    # +          .---------------.     -         +
    #            |  normal label |     | labelh
    #            +---------------+     -
    #            |  normal hist  |     |
    #            |               |     | histh
    # .---+------+---------------+--.  - 
    # |c  | c    |               |  |
    # |o  | o    |               |  |
    # |n l| n h  |   normal/     |t |
    # |v a| v i  |   converse    |i +---------------.
    # |e b| e s  |               |c | joint label   |
    # |r e| r t  |   scatter     |s +---------------+ -
    # |s l| s    |       plot    |  | joint hist    | |
    # |e  | e    |               |  |               | | histh
    # `---+------+---------------+--+---------------' -
    #            |     tics      |                    | ticsh
    # +          `---------------'                  + -
    #            |---- histw ----|--|
    #                              ticsw
    
    use constant histw => 150;
    use constant histh => 80;
    use constant labelh => 20;
    use constant ticsw => 50;
    use constant ticsh => 40;

    my $gd = new GD::Image(labelh + histh + 2 * histw + ticsw, labelh + histh + histw + ticsh);
    my $white = $gd->colorAllocate(255, 255, 255);
    my $black = $gd->colorAllocate(0, 0, 0);
    my $lgrey = $gd->colorAllocate(220, 220, 220);
    my $dgrey = $gd->colorAllocate(150, 150, 150);

    # Colours we use to represent numbers of responses.
    my @shade = ( );

    for (my $s = 255; $s > 128; --$s) {
        push(@shade, $gd->colorAllocate($s, $s, 255));
    }
    
    my $i;

    my (@ndist) = PoliticalSurvey::DB::select_single_row($dbh, 'select nm2, nm1, n0, np1, np2 from distribution where statementid = ? and converse = 0', $id);
    my (@cdist) = PoliticalSurvey::DB::select_single_row($dbh, 'select nm2, nm1, n0, np1, np2 from distribution where statementid = ? and converse = 1', $id);

    # Label normal/converse marginal distributions.
    string_centered($gd, gdLargeFont, labelh + histh + histw / 2, labelh / 2, 1, 1, 'Normal', $black);
    string_centered($gd, gdLargeFont, labelh / 2, labelh + histh + histw / 2, 1, 1, 'Converse', $black, 1);

    # Tics to line up with scatter plot and histograms.
    my $d = histw / 5;
    my $dy = 8;
    my ($x, $y) = (labelh + histh, labelh + histh + histw + 5);

    # X
    string_centered($gd, gdTinyFont, $x + 0.5 * $d, $y + $dy, 1, 0, 'disagree', $black);
    string_centered($gd, gdTinyFont, $x + 0.5 * $d, $y + 10 + $dy, 1, 0, 'strongly', $black);
    
    string_centered($gd, gdTinyFont, $x + 1.5 * $d, $y, 1, 0, 'disagree', $black);
    
    string_centered($gd, gdTinyFont, $x + 2.5 * $d, $y + $dy, 1, 0, 'no', $black);
    string_centered($gd, gdTinyFont, $x + 2.5 * $d, $y + 10 + $dy, 1, 0, 'opinion', $black);
    
    string_centered($gd, gdTinyFont, $x + 3.5 * $d, $y, 1, 0, 'agree', $black);
    
    string_centered($gd, gdTinyFont, $x + 4.5 * $d, $y + $dy, 1, 0, 'agree', $black);
    string_centered($gd, gdTinyFont, $x + 4.5 * $d, $y + 10 + $dy, 1, 0, 'strongly', $black);

    # Y
    ($x, $y) = (labelh + histh + histw + 5, labelh + histh + histw);

    string_centered($gd, gdTinyFont, $x, $y - 0.5 * $d - $dy / 2, 0, 1, 'disagree', $black);
    string_centered($gd, gdTinyFont, $x, $y - 0.5 * $d + $dy / 2, 0, 1, 'strongly', $black);

    string_centered($gd, gdTinyFont, $x, $y - 1.5 * $d, 0, 1, 'disagree', $black);
    
    string_centered($gd, gdTinyFont, $x, $y - 2.5 * $d, 0, 1, 'no opinion', $black);

    string_centered($gd, gdTinyFont, $x, $y - 3.5 * $d, 0, 1, 'agree', $black);

    string_centered($gd, gdTinyFont, $x, $y - 4.5 * $d - $dy / 2, 0, 1, 'agree', $black);
    string_centered($gd, gdTinyFont, $x, $y - 4.5 * $d + $dy / 2, 0, 1, 'strongly', $black);

    # Marginal distributions.
    plot_histogram($gd, labelh + histh, labelh + histh, histw, histh, [1, 0], [0, -1], $lgrey, \@ndist);
    plot_histogram($gd, labelh + histh, labelh + histh + histw, histw, histh, [0, -1], [-1, 0], $lgrey, \@cdist);

    # Now plot the scatter diagram.
    my $s = $dbh->prepare(q{select a1.value, a2.value from session, answer as a1, answer as a2
                                where a1.sessionid = session.id
                                  and a2.sessionid = a1.sessionid
                                  and a1.statementid = ?
                                  and a1.statementid = a2.statementid
                                  and a1.converse = 0
                                  and a2.converse = 1});

    $s->execute($id);

    # ($x, $y) is now the centre of the scatter plot.
    $x -= histw / 2 + 5;
    $y -= histw / 2;

    my @scatter = ([0, 0, 0, 0, 0],
                   [0, 0, 0, 0, 0],
                   [0, 0, 0, 0, 0],
                   [0, 0, 0, 0, 0],
                   [0, 0, 0, 0, 0]);

    my $max = 0;

    while (my ($ax, $ay) = $s->fetchrow_array()) {
        my $m = ++$scatter[$ax + 2]->[$ay + 2];
        $max = $m if ($m > $max);
    }

    if ($max > 0) {
        for (my $X = -2; $X <= 2; ++$X) {
            for (my $Y = -2; $Y <= 2; ++$Y) {
                my $s = $shade[int($#shade * $scatter[$X + 2]->[$Y + 2] / $max)];
                $gd->filledRectangle($x + ($X - 0.5) * $d, $y - ($Y + 0.5) * $d,
                                     $x + ($X + 0.5) * $d, $y - ($Y - 0.5) * $d, $s);
            }
        }
    }
    
=pod
        my ($X, $Y) = ($x + $d * $ax, $y - $d * $ay);
        $X += (rand() - 0.5) * $d;
        $Y += (rand() - 0.5) * $d;
        $gd->line($X - 3, $Y, $X + 3, $Y, $dgrey);
        $gd->line($X, $Y - 3, $X, $Y + 3, $dgrey);
    }
=cut

    # Now plot the best-fit line on the scatter diagram.
    my ($a, $b, $r2) = PoliticalSurvey::DB::get_converse_mapping($dbh, $id);
    for (my $inv = -2; $inv < 2; $inv += 0.1) {
        my $nor = $a + $b * $inv;
        my ($x1, $y1) = ($x + $d * $nor, $y - $d * $inv);
        my ($x2, $y2) = ($x + $d * ($nor + 0.1 * $b), $y - $d * ($inv + 0.1));

        if ($x1 > $x - 2 * $d and $x1 < $x + 2 * $d
            and $x2 > $x - 2 * $d and $x2 < $x + 2 * $d) {
            $gd->line($x1, $y1, $x2, $y2, $black);
        }
    }

    return $gd;
}

# results_plot DBH SESSION
# Plot the results for SESSION.
sub results_plot ($$) {
    my ($dbh, $sess) = @_;
    my ($x, $y) = PoliticalSurvey::DB::get_position_2d($dbh, $sess);
    my $setid = PoliticalSurvey::DB::get_statementset($dbh, PoliticalSurvey::DB::get_answers($dbh, $sess));
    my $nstmts = scalar @{PoliticalSurvey::DB::get_statementset_by_id($dbh, $setid)};
    my $max = 2. * sqrt($nstmts);
    my ($e1, $e2) = PoliticalSurvey::DB::get_eigenvalues($dbh, $setid);

    # Font for drawing labels.
    my $f = gdSmallFont;
    
    # Size of scatter plot. This is squashed to reflect the ratio of the
    # eigenvalues for the first two principal axes.
    use constant width => 500;

    my $sw = width;
    my $sh = width * sqrt($e2 / $e1);

    # Width of whole diagram.
    my $w = $sw + (my $dx = $f->width() * length("left")) + $f->width() * length("right");
    my $h = $sh + (my $dy = $f->height()) + $f->height();

    my $gd = new GD::Image($w, $h);

    my $white = $gd->colorAllocate(255, 255, 255);
    my $black = $gd->colorAllocate(0, 0, 0);
    my $lgrey = $gd->colorAllocate(220, 220, 220);
    my $dgrey = $gd->colorAllocate(150, 150, 150);
    my $red   = $gd->colorAllocate(180, 0, 0);

    # Axes.
    $gd->line($dx + $sw / 2, $dy, $dx + $sw / 2, $dy + $sh, $dgrey);
    $gd->line($dx, $dy + $sh / 2, $dx + $sw, $dy + $sh / 2, $dgrey);

    # Labels.
    string_centered($gd, $f, $dx + $sw / 2, 0, 1, 0, "pragmatic", $black, 0);
    string_centered($gd, $f, $dx + $sw / 2, $dy + $sh, 1, 0, "idealistic", $black, 0);
    string_centered($gd, $f, 0, $dy + $sh / 2, 0, 1, "left", $black, 0);
    string_centered($gd, $f, $dx + $sw, $dy + $sh / 2, 0, 1, "right", $black, 0);

    # XXX plot some kind of calibration crap here.
###     my $stmt = $dbh->prepare('select id from session where id <> ?');
###     $stmt->execute($sess);
###     while (my ($id2) = $stmt->fetchrow_array()) {
###         my ($x2, $y2) = PoliticalSurvey::DB::get_position_2d($dbh, $id2);
###         my ($X, $Y) = ((1 + $x2 / sqrt($nstmts)) * width / 2., (1 - $y2 / sqrt($nstmts)) * $h / 2.);
###         $gd->setPixel($X, $Y, $dgrey);
###     }

    # Draw this user's result.
    my ($X, $Y) = ($dx + (1 + $x / $max) * $sw / 2.,
                   $dy + (1 - $y / $max) * $sh / 2.);

    $gd->line($X - 4, $Y, $X + 4, $Y, $red);
    $gd->line($X, $Y - 4, $X, $Y + 4, $red);

    $gd->rectangle($X - 6, $Y - 6, $X + 6, $Y + 6, $red);

    my @celebs = @{$dbh->selectall_arrayref('select sessionid, name from celebrity')};
    foreach (@celebs) {
        my ($id, $name) = @$_;
        my ($x, $y) = PoliticalSurvey::DB::get_position_2d($dbh, $id);
        my ($X, $Y) = ($dx + (1 + $x / $max) * $sw / 2.,
                       $dy + (1 - $y / $max) * $sh / 2.);

        $gd->line($X - 1, $Y, $X + 1, $Y, $dgrey);
        $gd->line($X, $Y - 1, $X, $Y + 1, $dgrey);

        $gd->stringUp($f, $X - $f->height() / 2,
                        $y > 0 ? $Y - 3
                               : $Y + 3 + length($name) * $f->width(), $name, $dgrey);
    }

    return $gd;
}

1;
