build/hello: Main.hs FGLUT.hs GameStates.hs SnakeGame.hs SnakeRender.hs
	mkdir -p build
	cd build; \
	lndir ..; \
	ghc -O -o hello ../Main.hs
