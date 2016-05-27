# Markov Chain Estimator - Victhor Simões Sartório

FC=gfortran
FF=-O2 -march=native -Wall -Wextra -Wpedantic

OBJS=file_handler.o sorting.o estimator.o main.o

# Build system

all: mce

mce: $(OBJS)
	$(FC) $(FF) -o mce $(OBJS)

main.o: main.f95 file_handler.mod estimator.mod
	$(FC) $(FF) -c main.f95

estimator.mod: estimator.o

estimator.o: estimator.f95 file_handler.mod sorting.mod
	$(FC) $(FF) -c estimator.f95

sorting.mod: sorting.o

sorting.o: sorting.f95
	$(FC) $(FF) -c sorting.f95

file_handler.mod: file_handler.o

file_handler.o: file_handler.f95
	$(FC) $(FF) -c file_handler.f95

# Utilities

rebuild: clean all

clean:
	rm -f *.o *.mod

test: mce
	./mce markov.dat
