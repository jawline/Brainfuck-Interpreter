all: build

build:
	mkdir -p ./bin/
	ghc --make -O3 -isrc -outputdir ./bin/ -o ./bin/main src/Main.hs

test: build
	echo "a" | ./bin/main tests/test.bf

clean:
	rm -rf ./bin/
