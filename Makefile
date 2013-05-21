build/hello: Main.hs FGLUT.hs GameStates.hs SnakeGame.hs SnakeRender.hs Randomize.hs Utils.hs
	mkdir -p build
	cd build; \
	ln -s ../*.hs ./; \
	ghc -O -o hello ../Main.hs
