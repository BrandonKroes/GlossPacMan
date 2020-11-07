# Pac-Man 

### How to play
PacMan can be played with the WASD-keys. To (un)pause the game, you can press the p-key.
The four different ghost try to hunt down PacMan and each ghost has a different tactic. If you collect a coin, you can briefly hunt the ghosts and earn more points. The goal is to collect all coins without being caught. After a game, the highscore list will be updated and this file can be found in HighScores/highscore.txt

### Implemented optional requirements
We have chosen to implement 2 different requirements, namely complex graphics and different enemies.
###### Complex  Graphics
For  the  criteria  complex  graphics,  we  have  made  the enviroment work with PNG images from file storage.  This required significant work, because by default FilePaths are absolute and we needed to make them relative.  Both for usage across PCs and to allow a stack build commando to run without an absolute path.

###### Different enemies
For Pac-Man, we want to include four different enemies. We have implemented four different ghosts which have each a different strategy to catch Pac-Man. Also, after the game proceeds, more ghosts come into the field and try to catch PacMan. Every ghost has 2 different methods of catching PacMan: Chase and Scatter. The ghosts can also be frightened, then they run away from PacMan in a random direction 

### Running PacMan game
To build the game: `stack build`
To run the build: `stack exec pacman-exe`

If you get a GLUT related error, this seemed to solve the problem for us: https://stackoverflow.com/questions/8956387/cant-get-freeglut-to-work-with-haskell-on-windows


