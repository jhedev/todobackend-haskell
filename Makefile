all: scotty.docker servant.docker snap.docker spock.docker yesod.docker

%.bin:
	stack build todobackend-$(basename $@)

%.docker: %.bin
	docker build -f docker/Dockerfile-$(basename $@) -t jhedev/todobackend-haskell:$(basename $@) .

push:
	docker push jhedev/todobackend-haskell

