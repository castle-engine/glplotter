.PHONY: compile
compile:
	./compile.sh

# Run also "dircleaner . clean" here to really clean
.PHONY: clean
clean:
	rm -f glplotter glplotter.exe
	rm -Rf glplotter.app
	rm -f gen_function gen_function.exe
	rm -Rf gen_function.app
