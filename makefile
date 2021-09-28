main:
	cd build || mkdir build
	cp *.ml build
	cd build && ocamlopt -o main ChainOp.ml ByteOp.ml Logging.ml BinFile.ml main.ml
	rm ./build/*.ml

clean:
	 rm -rf build