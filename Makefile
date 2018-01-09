.PHONY: compile
compile:
	./compile.sh

.PHONY: clean
clean:
	castle-engine clean
	rm -f glplotter glplotter.exe
	rm -Rf glplotter.app
	rm -f gen_function gen_function.exe
	rm -Rf gen_function.app
