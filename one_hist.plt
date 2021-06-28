#!usr/bin/gnuplot
set terminal postscript enhanced color eps 
set terminal pngcairo enhanced size 1280,960 font "Arial, 28" lw 2

set encoding utf8

set output path."_histogram.png"

set style line 1 pt 7 ps 1 lc rgbcolor "#118ab2"  # Recovered
set style line 2 pt 7 ps 1 lc rgbcolor "#ef476f"  # Infected
set style line 3 pt 7 ps 1 lc rgbcolor "#06d6a0"  # Susceptible

set xlabel "t (adim.)"
set xrange [:20]

set ylabel "Fraction of population"

set key horizontal outside center top font ", 24"

plot path u 1:(($2+$3+$4)/($2+$3+$4)) w filledcurves x1 ls 1 t "Susceptible", \
       "" u 1:(($3+$4)/($2+$3+$4))    w filledcurves x1 ls 3 t "Recovered", \
       "" u 1:($3/($2+$3+$4))         w filledcurves x1 ls 2 t "Infected"