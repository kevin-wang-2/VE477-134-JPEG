main:
	cd build || mkdir build
	cp *.ml build
	cd build && ocamlopt -o main ListOp.ml ChainOp.ml ByteOp.ml Logging.ml BinFile.ml Huffman.ml main.ml
	rm ./build/*.ml

clean:
	 rm -rf build