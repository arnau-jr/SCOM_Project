objects = test_net.o network.o
comp = gfortran
OPT = -O3

test.x: $(objects)
	$(comp) -o test.x $(OPT) $(objects)

network.o: network.f90
	$(comp) -c $(OPT) network.f90

test_net.o: test_net.f90 network.o
	$(comp) -c $(OPT) test_net.f90 network.f90 
clean:
	rm -f $(objects)
