extras = \"bash\" \"j\"

j-docker-manifest :
	guix package -L . -A ^j- | cut -f 1 > $@
	jconsole -js \
		"pkgs=: (<@(' \"'&,)@(,&'\"'));._2 (1!:1) < '$@'" \
		"cmd=: '(specifications->manifest ''($(extras) '" \
		"cmd=: cmd,(; pkgs),'))'" \
		"cmd 1!:2 < '$@'" \
		"exit 0"

j-docker : j-docker-manifest
	guix pack -L . -m $< -f docker -r j-docker
