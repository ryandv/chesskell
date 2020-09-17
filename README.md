Chesskell
=========

LIVE DEMO: http://chess.ryandv.me (Running on Heroku, please wait as the dyno spins up)

This is a personal learning project for Haskell. It exposes a simple chess AI using the minimax algorithm with alpha-beta pruning via a JSON API.

Kudos to [Josh](https://github.com/lime-green) for the awesome front-end user interface.

#### Update

As of ~2019, chesskell now uses [bitboard representations](https://www.chessprogramming.org/Bitboards) of the game's current occupancy to achieve search times that are within the request timeout of a free Heroku dyno at a modest level of depth.
