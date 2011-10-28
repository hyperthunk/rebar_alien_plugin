## Using a {Dir, explicit} alien_deps configuration 

If you run `rebar compile -v` in this project then you'll notice that the 
whilst the `inttest` subdirectory is registered in our `rebar.config` as an
`alien_dir`, it is not offered up for pre-processing. This is because it has 
been configured as an *explicit* alien directory - which means it will only be
processed by the plugin, when the current (i.e., active) rebar command is one
which it's handler spec (in `alien_conf`) recognises.
