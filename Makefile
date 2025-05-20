all: reactor

RUN_IN_DOCKER = docker-compose run --user $$(id -u)

build:
	docker-compose build elm-container

compile: build
	${RUN_IN_DOCKER} elm-container make src/Main.elm

reactor: build
	${RUN_IN_DOCKER} -p 8000:8000 elm-container reactor

test: build
	${RUN_IN_DOCKER} --entrypoint elm-test elm

test-watch: build
	${RUN_IN_DOCKER} --entrypoint elm-test elm --watch

bash: build
	${RUN_IN_DOCKER} --entrypoint bash elm.container
