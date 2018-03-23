default:
	@jbuilder build --only-packages jbuild-create @install

install:
	@jbuilder install

uninstall:
	@jbuilder uninstall

clean:
	@rm -rf _build
