all: src/monkeys.hs
	@mkdir -p ./bin
	@ghc src/monkeys.hs -hidir ./bin -odir ./bin -o ./bin/app

run: 
	@./bin/app

clean:
	rm -rf ./bin
