all: src/calories.hs
	@mkdir -p ./bin
	@ghc src/calories.hs -hidir ./bin -odir ./bin -o ./bin/app

run: 
	@./bin/app

clean:
	rm -rf ./bin
