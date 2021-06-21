#!usr/bin/gnuplot
set terminal postscript enhanced color eps

set output "results/plots/evolution.eps"

set xlabel "t (adim.)"
set ylabel "# Infected"

unset key

plot "results/evolution.dat" u 1:3 w lp


set output "results/plots/histrograms.eps"

set xlabel "t (adim.)"
unset ylabel 

set xrange [*:*]
set yrange [*:*] 

set key

plot "results/evolution_histo.dat" u 1:2 w lp t "Susceptible", "" u 1:3 w lp t "Infected", "" u 1:4 w lp t "Recovered"

set output "results/plots/rec_lambda.eps"

set xlabel "lambda (adim.)"
set ylabel "R (t->∞)" 

set xrange [*:*]
set yrange [*:*] 

unset key

plot "results/rec_lambda.dat" u 1:2 w lp
