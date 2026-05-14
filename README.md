# lookup

This is a project i did for myself. If you actually want to use it you should
probably use the version from the rust_rewrite. Features will probably be
implemented first in the ocaml version from the main branch but the rust_rewrite
is working on windows as well as linux and macos. You need to add it to your
path. I personally created a function in my shell of choice to cd easily into
known directories.

## TODO

- [x] add flag for case insensitive search
    - [x] has to return the path with correct cases
    - [x] has to work with single needle
    - [x] has to work with multiple needles
- [x] remove or add slashes at the end consequently when adding an entry
- [x] add flag to list all matches
- [x] add flag to list known locations
- [x] add flag to remove matches from known locations
- [x] add flag to remove all paths that don't exist anymore
- [x] add flag to print version
- [x] add env variable to change the path of the known location file
- [x] if file with known locations doesn't exist yet, it will be created

