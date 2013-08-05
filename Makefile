all: bin/kuruc

bin/kuruc:
	make -C src

clean:
	make -C src clean

