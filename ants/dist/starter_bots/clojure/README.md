# aichallenge-ants-clj

AI Challenge 'Ants' -- Winter/Spring 2011

Package origin repo: https://github.com/i0cus/aichallenge-ants-clj

## Usage

Contest engine is prepared to accept `leiningen` project and build it
using dependencies provided by uploader. Easy!

Create your bot, adjust `project.clj` to match one provided.

Remove compiled artifacts & deps with:

    lein clean

Then fetch the deps from local repo with:

    lein deps

All that's left is to zip the directory and upload. Contest engine will
hopefully pick it up and build a pretty uberjar.


## License

Copyright (C) 2011 Slawek Gwizdowski

Distributed under the Eclipse Public License, the same as Clojure.

