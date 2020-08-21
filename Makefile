extras = \"bash\" \"j\"

j-manifesto :
	guix package -L . -A ^j- | cut -f 1 > $@
	jconsole -js \
		"pkgs=: (<@(' \"'&,)@(,&'\"'));._2 (1!:1) < '$@'" \
		"cmd=: '(specifications->manifest ''($(extras) '" \
		"cmd=: cmd,(; pkgs),'))'" \
		"cmd 1!:2 < '$@'" \
		"exit 0"
