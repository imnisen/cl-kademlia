

# Cl-Kademlia

Cl-Kademlia is a implementation of [kademlia algorithm](https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf)

Inspired by [python kademlia](https://github.com/bmuller/kademlia), however make the "udp over rpc" own, drop the spiders stuff.

The code is messy now. I'll keep polishing it while possible :)


## Usage


## Installation


## Author

-   Nisen (imnisen@gmail.com)


## TODO -LIST

-   [X] Debug and finish server's get method and set method
-   [X] Try to abstract bootstrap, set-key and get-key method
-   [X] Refactor rpc method famework
-   [ ] Add async and concurrerent rpc call
-   [ ] Handle rpc error condtions
-   [ ] Add server stop method
-   [ ] Refactor kbucket's nodes data type, make it easy use
-   [ ] Add test case
-   [ ] Add logic: When new node found, call store key/value when need
-   [ ] Make initialize class order correct
-   [ ] Make some macro to clean code
-   [ ] Add logic: clean data when expired; republish data periodicity
-   [ ] Add logic: each RPC receiver will ping the sender to check address
-   [ ] Clean debug code

