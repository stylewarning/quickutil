# Quickutil-Server

"quickutil-server" is a component for web frontend.

## Usage

    (ql:quickload :quickutil-server)
    (quickutil-server:start :port 8080)
    
    (quickutil-server:stop)

## Production use

    $ make start SERVER_PORT=8080 SWANK_PORT=4005
