hello: Main.hs FGLUT.hs GameStates.hs SnakeGame.hs SnakeRender.hsa Randomize.hs
	mkdir -p build
	cd build; \
	cp -l ../*.hs ./; \
	ghc -O -o hello ../Main.hs
	mv build/hello ./hello
