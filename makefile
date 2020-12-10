setup:
	stack exec -- twelve init
	cp assets/* _templates/

clean:
	rm -rf _templates _site twelve.json
