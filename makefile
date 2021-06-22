objects = main.o network.o sir.o mtfort90.o
comp = gfortran
OPT = -O3 -fbounds-check

main.x: $(objects)
	mkdir -p $(dir_net)
	$(comp) -o main.x $(OPT) $(objects)

network.o: network.f90
	$(comp) -c $(OPT) network.f90

sir.o : sir.f90 network.o mtfort90.o
	$(comp) -c $(OPT) sir.f90 network.f90 mtfort90.f90

mtfort90.o : mtfort90.f90
	$(comp) -c $(OPT) mtfort90.f90

main.o: main.f90 network.o sir.o
	$(comp) -c $(OPT) main.f90 network.f90 sir.f90

sim: main.x
	mkdir -p $(dir_net)
	./main.x $(dir_net) $(N_sample) $(offset)
	make plots

plots:
	mkdir -p $(dir_net)/plots
	gnuplot -e "directory='$(dir_net)" plots.plt

clean:
	rm -f $(objects)

