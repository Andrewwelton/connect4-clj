# connect4

This is a connect 4 game written in Clojure.
It communicates and plays via an IRC connection. Requires someone either on IRC feeding correct responses, or another bot from this project.

Handles V1 of the protocol.

Coded for CIS 4910 - Functional Programming @ University of Guelph.

## Usage

Your best bet is to just run this through Leiningen

### Bugs

Have to manually accept or decline a win from opponent. Something odd happens when trying to do this automagically from the IRC callback.

## License

Feel free to use this as you see fit. Realize this was a University project and if you blatantly copy this for an assignment of your own, I am not responsible for anything that happens to you.
