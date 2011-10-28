-module(foo).
    -export(['mvn:test'/2]).
    'mvn:test'(Config, AppFile) ->
        rebar_alien_plugin:execute_command("inttest", Config, AppFile).