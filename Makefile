install:
	cd app && elm package install
	cd server && npm install

start-app:
	cd app && elm-live Bingo.elm --open --output=bingo.js

start-server:
	cd server && node server.js