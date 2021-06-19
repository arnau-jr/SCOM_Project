#!usr/bin/gnuplot
set terminal postscript enhanced color eps

set output "results/plots/evolution.eps"

set xlabel "t (adim.)"
set ylabel "# Infected"

unset key

plot "results/evolution.dat" u 1:3 w lp

