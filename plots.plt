#!usr/bin/gnuplot
set terminal postscript enhanced color eps 
set terminal pngcairo enhanced size 1280,960 font "Arial, 20" lw 2

set encoding utf8

# For EPS
# set style line 1 pt 7 ps 0.5 lc rgbcolor "#118ab2"  # Recovered
# set style line 2 pt 7 ps 0.5 lc rgbcolor "#ef476f"  # Infected
# set style line 3 pt 7 ps 0.5 lc rgbcolor "#06d6a0"  # Susceptible

# For pngcairo
set style line 1 pt 7 ps 1 lc rgbcolor "#118ab2"  # Recovered
set style line 2 pt 7 ps 1 lc rgbcolor "#ef476f"  # Infected
set style line 3 pt 7 ps 1 lc rgbcolor "#06d6a0"  # Susceptible


####################
# LAMBDA EVOLUTION #
####################

# set output directory."/plots/rec_lambda.eps"
set output directory."/plots/rec_lambda.png"

set xlabel "λ (adim.)"
set ylabel "R (t→∞)" 

set xrange [*:*]
set yrange [*:*] 

unset key

plot directory."/rec_lambda.dat" u 1:2 w lp ls 1 lw 2 ps 1.5

###################
# LAMBDA INFECTED #
###################

set output directory."/plots/lambda_infect.png"

set xlabel "t (adim.)"
set xrange [0:20]
set key inside top right vertical
set ylabel "# Infected"

set palette defined ( 0 "#118ab2", 0.5 "#06d6a0", 1 "#ef476f" )
unset colorbox

plot for [i=1:25:2] sprintf(directory."/lambda_%5.3f_evolution_histo.dat", (i-1)/100. + 0.001) u 1:3:0 \
                    w l lw 1.5 lc palette frac ((i-1)/ 25.) t sprintf("λ = %5.3f", (i-1)/100. + 0.001)

############################
# LAMBDA FILLED HISTOGRAMS #
############################

# set output directory."/plots/filled_hist.eps"
set output directory."/plots/lambda_all_hist.png"

unset xlabel
unset ylabel 

set multiplot layout 3,3
unset key

set rmargin 1
set lmargin 2.5
set tmargin 1
set bmargin 1
set xtics format "%g" font ",12" offset 0,0.5
set ytics format "%g" font ",12" offset 0.4,0
set tics front

set title offset 0,-0.7

lambdas = "0.001 0.011 0.031 0.051 0.091 0.131 0.151 0.191 0.241"

do for [i in lambdas] {
    set title "λ = ".i font ",14"
    plot directory."/lambda_".i."_evolution_histo.dat" u 1:(($2+$3+$4)/($2+$3+$4)) w filledcurves x1 ls 1 t "Susceptible", \
                                                    "" u 1:(($3+$4)/($2+$3+$4))    w filledcurves x1 ls 3 t "Recovered", \
                                                    "" u 1:($3/($2+$3+$4))         w filledcurves x1 ls 2 t "Infected"
}
